AnnualUI <- function(id) {

  ns <- NS(id)

  tabPanel("Annual Impact Results",
           uiOutput(ns("title")),
           uiOutput(ns("year_radio")),
           dataTableOutput(ns("annual_table")),
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

        if (is.null(input$year_radio)) {
          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}")))

        } else {

        switch(tab,
               "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")),
               "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {input$year_radio}")))
        }


      })

      output$year_radio <- renderUI({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts.")
        }
        choices <- 2023:(2023 + ncol(impact()) - 1)
        radioButtons(inputId = session$ns("year_radio"),
                     label = NULL,
                     choices = choices,
                     inline = TRUE)
      })

      output$annual_table <- renderDataTable({

        req(input$year_radio)


        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts. ")
        }

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
                    rownames = FALSE,
                    extensions = "Buttons",
                    options = list(dom = "Bt",
                                   buttons = c("copy", "csv", "excel", "pdf", "print"))) %>%
          disp()
      })




    }
  )
}

AnnualGraphUI <- function(id) {
  ns <- NS(id)

  tabPanel("Annual Impact Graphs",
           uiOutput(ns("title")),
           uiOutput(ns("year_radio")),
           plotOutput(height = '500px',ns("annual_plot")),
           download_graph_ui(id)
  )

}

AnnualGraphServer <- function(id, tab, region, impact) {

  moduleServer(
    id,
    function(input, output, session)  {

      impact_data <- reactive({
        impact_analysis(region = region(),
                        impacts = impact()) %>%
          .[[tab]]
      })

      output$title <- renderUI({

        if (is.null(input$year_radio)) {

          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}")))

        } else {

        switch(tab,
               "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")),
               "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {input$year_radio}")))
        }


      })

      output$year_radio <- renderUI({

        if (all(impact() == 0)) {
          validate(FALSE)
        }
        choices <- 2023:(2023 + ncol(impact()) - 1)
        radioButtons(inputId = session$ns("year_radio"),
                     label = NULL,
                     choices = choices,
                     inline = TRUE)
      })

      create_plot <- reactive({

        if (tab == "emp") {
          x_axis <- scale_x_continuous(labels = scales::comma_format(),
                                       expand = expansion(mult = c(0, 0.1)))
          title <-  glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")
          fct_lvls <- c("Flow on Employment", "Direct Employment")

        } else {
          x_axis <- scale_x_continuous(labels = scales::dollar_format(suffix = "M"),
                                       expand = expansion(mult = c(0, 0.1)))
          title <- glue("Gross Regional Product Impacts by Industry ($M): {region()} {input$year_radio}")
          fct_lvls <- c("Flow on GRP", "Direct GRP")
        }

        impact_data() %>%
          filter(grepl("Direct|Flow on", type),
                 year == input$year_radio)  %>%
          mutate(type = factor(type, levels = fct_lvls)) %>%
          ggplot(aes(x = value, y = reorder(Sector, value))) +
          geom_col(aes(fill = type)) +
          labs(x = NULL,
               y = NULL,
               title = title) +
          x_axis +
          theme_aiti(colour = "grey",
                     flipped = TRUE) +
          scale_fill_aiti()
      })

      output$annual_plot <- renderPlot({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts.")
        }

        create_plot()




      })

      output$download_plot <- downloadHandler(
        filename = function() {

          fname <- ifelse(is.null(input$filename), "eiat-download", input$filename)

          paste0(fname, ".", input$filetype)
        },
        content = function(file)
          ggsave(filename = file,
                 plot = create_plot() + labs(caption = paste0("Produced with EIAT Version: ", as.character(packageVersion("eiat")))),
                 dpi = "screen",
                 device = input$filetype,
                 units = "px",
                 width = input$width,
                 height = input$height)
      )
    }
  )
}
