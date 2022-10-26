AnnualUI <- function(id) {

  ns <- NS(id)

  tabPanel("Annual Impacts",
           uiOutput(ns("year_radio")),
           uiOutput(ns("title")),
           dataTableOutput(ns("annual_table")),
           plotOutput(ns("annual_plot"))
  )

}

AnnualServer <- function(id, tab, region, impact) {

  moduleServer(
    id,
    function(input, output, session) {

      impact_data <- reactive({


        impact_analysis(region = region(),
                        impacts = impact()) %>%
          .[[tab]]
      })

      output$title <- renderUI({

        req(input$year_radio)

        switch(tab,
               "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")),
               "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {input$year_radio}"))
        )


      })

      output$year_radio <- renderUI({
        validate(
          need(sum(impact()) > 0, FALSE)
        )
        choices <- 2022:(2022 + ncol(impact()) - 1)
        radioButtons(inputId = session$ns("year_radio"),
                     label = NULL,
                     choices = choices,
                     inline = TRUE)
      })

      output$annual_table <- renderDataTable({

        validate(
          need(sum(impact()) > 0, "Enter data in Project Setup > Data Input to calculate economic impacts. "),
          need(input$year_radio, FALSE)
        )

        if (tab == "emp") {
          disp <- function(table) {
            formatRound(table, c(2:4), 0)
          }
        } else {
          disp <- function(table) {
            formatCurrency(table, c(2:4), currency = "$M", before = FALSE, digits = 1)
          }
        }

        impact_data() %>%
          filter(year == input$year_radio) %>%
          pivot_wider(id_cols = c(year, Sector),
                      names_from = type,
                      values_from = value) %>%
          select(Sector, contains(c("Direct",
                                    "Flow on",
                                    "Total"))) %>%
          janitor::adorn_totals() %>%
          datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"),
                    rownames = FALSE) %>%
          disp()
      })

      output$annual_plot <- renderPlot({
        validate(
          need(sum(impact()) > 0, FALSE),
          need(input$year_radio, FALSE)
        )

        if (tab == "emp") {
          x_axis <- scale_x_continuous(labels = scales::comma_format())
          title <-  glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")
        } else {
          x_axis <- scale_x_continuous(labels = scales::dollar_format(suffix = "M"))
          title <- glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {input$year_radio}")
        }

        impact_data() %>%
          filter(grepl("Direct|Flow on", type),
                 year == input$year_radio)  %>%
          ggplot(aes(x = value, y = reorder(Sector, value))) +
          geom_col(aes(fill = type)) +
          labs(x = NULL,
               title = title) +
          x_axis +
          theme_aiti(flipped = TRUE)


      })


    }
  )
}
