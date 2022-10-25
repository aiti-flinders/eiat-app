inputUI <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    tabPanel("Summary Tables",
             dataTableOutput(ns("input_table")),
             dataTableOutput(ns("input_table_sector"))
    ),
    tabPanel("Summary Graphs",
             selectInput(ns("input_type"),
                         label = NULL,
                         choices = c("Direct Capital Expenditure", "Direct Capital Expenditure by Industry")),
             plotOutput(ns("plot")),
             h3("Download Graphs"),
             download_graph_ui(id)

    )
  )

}

inputServer <- function(id, region, impact) {
  moduleServer(
    id,
    function(input, output, session) {

      create_plot <- reactive({
        validate(need(input$input_type, message = FALSE))
        if (input$input_type == "Direct Capital Expenditure") {
          impact() %>%
            as_tibble(rownames = "sector") %>%
            pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
            group_by(year) %>%
            summarise(expenditure = sum(expenditure), .groups = "drop") %>%
            ggplot(aes(x = as.factor(year), y = expenditure), fill = aiti_blue) +
            geom_col() +
            labs(x = NULL,
                 title = glue("Direct Capital Expenditure ($M) by Year in {region()}")) +
            scale_y_continuous(labels = scales::dollar_format(suffix = "M"))
        } else {
          impact() %>%
            as_tibble(rownames = "sector") %>%
            pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
            filter(expenditure > 0) %>%
            ggplot(aes(x = as.factor(year), y = expenditure, fill = sector)) +
            geom_col() +
            labs(x = NULL,
                 title = glue("Direct Capital Expenditure ($M) by Industry in {region()}")) +
            scale_y_continuous(labels = scales::dollar_format(suffix = "M")) +
            guides(fill = guide_legend(nrow = 2))

        }
      })


      output$plot <- renderPlot({
        create_plot()
      })

      output$input_table <- renderDataTable({
        impact() %>%
          as_tibble(rownames = "sector") %>%
          summarise(across(where(is.double), sum)) %>%
          mutate(Region = region(),
                 .before = 1)
      })


      output$input_table_sector <- renderDataTable({
        impact() %>%
          rbind(colSums(impact())) %>%
          as_tibble(rownames = "Industry Sector") %>%
          filter(rowSums(across(where(is.double))) > 0)
      })

      output$download_plot <- downloadHandler(
        filename = function() {
          paste0(input$filename, ".", input$filetype)
        },
        content = function(file) {
          thematic_save_plot(create_plot(),
                             device = default_device(input$filetype),
                             filename = file)
        }
      )
    }
  )
}
