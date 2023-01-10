inputUI <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    tabPanel("Summary Tables",
             uiOutput(ns("title1")),
             dataTableOutput(ns("input_table")),
             downloadUI("input_table_download", button_text = "Download Input Data"),
             uiOutput(ns('title2')),
             dataTableOutput(ns("input_table_sector"))
    ),
    tabPanel("Summary Graphs",
             selectInput(ns("input_type"),
                         label = NULL,
                         choices = c("Direct Capital Expenditure", "Direct Capital Expenditure by Industry")),
             plotOutput(ns("plot"), height = "500px"),
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
            ggplot(aes(x = as.factor(year), y = expenditure)) +
            geom_col() +
            theme_aiti(flipped = TRUE) +
            labs(x = NULL,
                 y = NULL,
                 title = glue("Direct Capital Expenditure ($M): {region()}")) +
            scale_y_continuous(labels = scales::dollar_format(suffix = "M", scale = 0.1))
        } else {
          impact() %>%
            as_tibble(rownames = "sector") %>%
            pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
            filter(expenditure != 0) %>%
            ggplot(aes(x = as.factor(year), y = expenditure, fill = sector)) +
            geom_col() +
            theme_aiti(flipped = TRUE) +
            labs(x = NULL,
                 y = NULL,
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
                 .before = 1) %>%
          datatable(rownames = FALSE)
      })


      output$input_table_sector <- renderDataTable({
        impact() %>%
          as_tibble(rownames = "Industry Sector") %>%
          filter(rowSums(across(where(is.double))) != 0) %>%
          adorn_totals() %>%
          datatable(rownames = FALSE)
      })



      output$title1 <- renderUI({
        h1(glue("Direct Capital Expenditure ($M) in {region()}"))
      })
      output$title2 <- renderUI({
        h1(glue("Direct Capital Expenditure ($M) by Industry in {region()}"))
      })
      output$download_plot <- downloadHandler(
        filename = function() {
          paste0(input$filename, ".", input$filetype)
        },
        content = function(file) {
          ggsave(create_plot(),
                 device = input$filetype,
                 units = "cm",
                 filename = file,
                 width = input$width * 2.54 / 300,
                 height = input$height * 2.54 / 300)
        }


      )
    }
  )
}
