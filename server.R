function(input, output, session) {

  options(DT.options = list(pageLength = 19, dom = "t"))

  observeEvent(input$random, {

    m <- matrix(round(runif(19*10, min = 0, max = 100), 2), nrow = 19, ncol = 10, dimnames = list(LETTERS[1:19], 2022:2031))

    updateMatrixInput(session, "industry_input", m)
  })

  region_selected <- reactive(input$lga)
  output$region_selected <- renderText({
    region_selected()
  })

  observe({
    lga <- if (is.null(input$state)) character(0) else {
      regions %>%
        filter(state_name == input$state) %>%
        pull(lga) %>%
        unique() %>%
        sort()
    }
    still_selected <- isolate(input$lga[input$lga %in% lga])
    updateSelectInput(session, "lga", choices = lga, selected = still_selected)
  })



# Employment --------------------------------------------------------------


  output$employment <- DT::renderDataTable({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["emp"]] %>%
      mutate(value = sprintf(value, fmt = "%.2f")) %>%
      filter(year == input$select_year_fte) %>%
      pivot_wider(id_cols = c(year, Sector), names_from = type, values_from = value) %>%
      select(Sector, Direct.Employment., Flow.on.Employment., Total.Employment.) %>%
      datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"), rownames = FALSE)
  })

  output$employment_plot <- renderPlot({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["emp"]] %>%
      group_by(type, year) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      ggplot(aes(x = year, y = value, col = type, group = type)) +
      geom_line() +
      theme_aiti(legend = "bottom") +
      labs(x = NULL,
           title = glue("Employment Impact (FTE) in {input$lga}"))

  })

  output$employment_plot_sector <- renderPlot({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["emp"]] %>%
      group_by(year, Sector) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      ggplot(aes(x = year, y = value, fill = Sector)) +
      geom_col(position = "dodge") +
      theme_aiti(legend = "bottom") +
      labs(x = NULL,
           title = glue("Employment Impact (FTE) by Industry in {input$lga}"))
  })


# GRP ---------------------------------------------------------------------




  output$grp <- DT::renderDataTable({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      mutate(value = sprintf(value, fmt = "%.2f")) %>%
      filter(year == input$select_year_grp) %>%
      pivot_wider(id_cols = c(year, Sector), names_from = type, values_from = value) %>%
      select(Sector, Direct.GRP., Flow.on.GRP., Total.GRP.) %>%
      datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"), rownames = FALSE)
  })

  output$grp_table <- renderTable({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      pivot_wider(names_from = year) %>%
      group_by(type) %>%
      summarise(across(where(is.double), sum), .groups = "drop") %>%
      filter(type %in% c("Direct.GRP.", "Flow.on.GRP.", "Total.GRP."))
  })

  output$grp_table_sector <- renderTable({
    d <- impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      pivot_wider(names_from = year) %>%
      group_by(Sector) %>%
      summarise(across(where(is.double), sum), .groups = "drop")

    rbind(d, c(NA, colSums(d[, 2:11])))
  })



  output$grp_plot <- renderPlot({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      group_by(type, year) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      ggplot(aes(x = year, y = value, col = type, group = type)) +
      geom_line() +
      theme_aiti(legend = "bottom") +
      labs(title = glue("Gross Regional Impact ($M) in {input$lga}"))
  })

  output$grp_plot_sector <- renderPlot({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      group_by(year, Sector) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      ggplot(aes(x = year, y = value, fill = Sector)) +
      geom_col(position = "dodge") +
      theme_aiti(legend = "bottom") +
      labs(x = NULL,
           title = glue("Gross Regional Impact ($M) by Industry in {input$lga}"))
  })


# Output ------------------------------------------------------------------

  output$output<- DT::renderDataTable({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["output"]] %>%
      mutate(value = sprintf(value, fmt = "%.2f")) %>%
      filter(year == input$select_year_output) %>%
      pivot_wider(id_cols = c(year, Sector), names_from = type, values_from = value) %>%
      select(Sector, Direct.Output., Flow.on.Output., Total.Output.) %>%
      datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"), rownames = FALSE)
  })



  output$output_plot <- renderPlot({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["output"]] %>%
      group_by(type, year) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      ggplot(aes(x = year, y = value, col = type, group = type)) +
      geom_line() +
      theme_aiti(legend = "bottom") +
      labs(title = glue("Output ($M) in {input$lga}"))
  })

  output$output_plot_sector <- renderPlot({
    impact_analysis(region = input$lga,
                    years = 2022:2031,
                    impacts = input$industry_input) %>%
      .[["output"]] %>%
      group_by(year, Sector) %>%
      summarise(value = sum(value), .groups = "drop") %>%
      ggplot(aes(x = year, y = value, fill = Sector)) +
      geom_col(position = "dodge") +
      theme_aiti(legend = "bottom") +
      labs(x = NULL,
           title = glue("Output ($M) by Industry in {input$lga}"))
  })

# Inputs ------------------------------------------------------------------


  output$input_plot <- renderPlot({
    input$industry_input %>%
      as_tibble(rownames = "sector") %>%
      pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
      group_by(year) %>%
      summarise(expenditure = sum(expenditure), .groups = "drop") %>%
      ggplot(aes(x = as.factor(year), y = expenditure)) +
      geom_col() +
      theme_aiti() +
      labs(x = NULL,
           title = glue("Direct Capital Expenditure ($M) by Year in {input$lga}")) +
      scale_y_continuous(labels = scales::dollar_format(suffix = "M"))
  })

  output$input_table <- renderTable({
    input$industry_input %>%
      as_tibble(rownames = "sector") %>%
      summarise(across(where(is.double), sum)) %>%
      mutate(Region = input$lga,
             .before = 1)
  })


  output$input_plot_sector <- renderPlot({
    input$industry_input %>%
      as_tibble(rownames = "sector") %>%
      pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
      filter(expenditure > 0) %>%
      ggplot(aes(x = as.factor(year), y = expenditure, fill = sector)) +
      geom_col() +
      theme_aiti(legend = "bottom") +
      labs(x = NULL,
           title = glue("Direct Capital Expenditure ($M) by Industry in {input$lga}")) +
      scale_y_continuous(labels = scales::dollar_format(suffix = "M"))
  })

  output$input_table_sector <- renderTable({
    input$industry_input %>%
      rbind(colSums(input$industry_input)) %>%
      as_tibble(rownames = "Industry Sector") %>%
      filter(rowSums(across(where(is.double))) > 0)
  })

  # output$input_table_title <- renderUI({
  #   h3(glue("Direct Capital Expenditure ($M) in {input$lga}"))
  # })

  output$input_table_sector_title <- renderUI({
    h3(glue("Direct Capital Expenditure ($M) by Industry in {input$lga}"))
  })

# Titles ------------------------------------------------------------------


  titleServer("input_table_title", text = "Direct Capital Expenditure ($M)", region_selected)
  titleServer("input_table_sector_title", text = "Direct Capital Expenditure by Industry ($M)", region_selected)
  titleServer("employment_summary_title", text = "Employment Impacts by Industry (FTE)", region_selected)
  titleServer("grp_summary_title", text = "Gross Regional Product Impacts by Industry ($M)", region_selected)
  titleServer("output_summary_title", text = "Output by Industry ($M)", region_selected)


  # output$matrix <- renderUI({
  #   years <- input$syear:input$eyear
  #   if (input$show_names == "Sector") {dim_x <- eiat:::anzsic_swap$letter} else {dim_x <- eiat:::anzsic_swap$name}
  #   matrixInput("industry_input",
  #               value = matrix(1, nrow = 19, ncol = length(years), dimnames = list(dim_x, years)),
  #               class = "numeric")
  # })
  #
  # output$start_year <- renderUI({
  #   numericInput("syear", "Select Start Year: ", value = format(Sys.Date(), "%Y"))
  # })
  #
  # output$end_year <- renderUI({
  #   numericInput("eyear", "Select End Year: ", value = as.numeric(format(Sys.Date(), "%Y")) + 1 , max = input$syear + 10, min = input$syear)
  # })
  #
  # output$year_select <- renderUI({
  #   radioButtons("select_year", label = "Select Year: ", choices = input$syear:input$eyear)
  # })
  #


# Base Data ---------------------------------------------------------------


  output$national_io <- renderTable(
    national_19
  )
  output$regional_employment <- renderTable(
    lq_models[[input$lga]][c("Local Employment", "Total Employment"),] %>%
      as_tibble(rownames = "employment_type") %>%
      pivot_longer(-employment_type,
                   names_to = "Sector") %>%
      pivot_wider(names_from = employment_type) %>%
      mutate(across(where(is.double), round)) %>%
      filter(Sector %in% LETTERS[1:19])

  )

  output$regional_io <- renderTable(
    rownames = TRUE,
    digits = 0,
    lq_models[[input$lga]]
  )

}
