TotalUI <- function(id) {

  ns <- NS(id)

  tabPanel("Total Impacts",
           uiOutput(ns("title")),
           dataTableOutput(ns("total_table")),
           plotOutput(ns("total_plot"))
  )

}

TotalServer <- function(id, tab, region, impact) {

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

        if (tab == "emp") {
          h3(glue("Employment Impacts by Industry (FTE) in {region()}"))
        }

      })



      output$total_table <- renderDataTable({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup > Data Input to calculate economic impacts. ")
        }

        impact_data() %>%
          group_by(Sector, type) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(Sector = factor(Sector, levels = rownames(impact()))) %>%
          arrange(Sector) %>%
          pivot_wider(id_cols = c(Sector),
                      names_from = type,
                      values_from = value) %>%
          select(Sector, contains(c("Direct",
                                    "Flow on",
                                    "Total"))) %>%
          janitor::adorn_totals(fill = "Total") %>%
          datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"),
                    rownames = FALSE) %>%
          formatRound(c(2:4), 0)
      })

      output$total_plot <- renderPlot({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup > Data Input to calculate economic impacts. ")
        }

        impact_data() %>%
          filter(grepl("Direct|Flow on", type)) %>%
          group_by(year, type) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          ggplot(aes(x = year, y = value, fill = type)) +
          geom_col() +
          theme_aiti(flipped = TRUE) +
          labs(x = NULL)


      })


    }
  )
}
