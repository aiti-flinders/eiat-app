function(input, output, session) {

  thematic::thematic_shiny()

  options(DT.options = list(pageLength = 20,
                            dom = "t"))


  # File Upload -------------------------------------------------------------

  user_matrix <- reactive({
    req(input$upload)

    ext <- tools::file_ext(input$upload$name)

    uploaded_file <- vroom::vroom(input$upload$datapath,
                                  delim = ",")

    uploaded_data <- uploaded_file %>%
      select(where(is.double)) %>%
      mutate(across(where(is.double), ~ifelse(is.na(.x), 0, .x)))

    n_col <- ncol(uploaded_data)

    m <- matrix(sapply(uploaded_data, cbind, simplify = TRUE),
                nrow = 19,
                ncol = n_col,
                dimnames = list(eiat:::anzsic_swap$name,
                                2022:(2022 + n_col - 1)))

    m


  })





  # File Download (Template) ------------------------------------------------

  output$download <- downloadHandler(
    filename = function() {
      "EIAT-Template.csv"
    },
    content = function(file) {
      out <- matrix(0, nrow = 19, ncol = input$years, dimnames = list(eiat:::anzsic_swap$name, 2022:(2022 + input$years - 1)))
      out <- as_tibble(out, rownames = "Sector")
      vroom::vroom_write(out, file, delim = ",")
    }
  )


  # Matrix ------------------------------------------------------------------

  # Keep track of what has been typed in
  # When ADDING years - bind a matrix with all values 0 for the NEW years.
  # If initial matrix is 2022, 2023, 2024 and need to add 2 columns (from 3 years -> 5 years), new matrix
  # has 2 columns with names 2025, 2026





  observeEvent(input$years, {

    req(input$years)

    if (is.null(input$upload)) {
      entered <- input$industry_input
      t <- ifelse(is.null(input$years), 1, input$years - ncol(entered))
      if (t > 0) { # If we're adding years
        new_cols <- input$years - ncol(entered)
        colnames_from <- max(as.numeric(colnames(entered))) + 1
        colnames_to <- max(as.numeric(colnames(entered))) + new_cols
        new_matrix <- matrix(0,
                             nrow = 19,
                             ncol = new_cols,
                             dimnames = list(eiat:::anzsic_swap$name,  colnames_from:colnames_to)
        )
        m <- cbind(entered, new_matrix)
      } else if (t < 0) { #If we're removing years
        m <- entered[, 1:input$years, drop = FALSE]
      } else { # t == 0 on initialisation
        m <- input$industry_input
      }

    } else {
      m <- user_matrix()
    }
    updateMatrixInput(session, "industry_input", m)
  })




  observeEvent(input$clear, {

    col_names <- 2022:(2022 + input$years - 1)

    m <- matrix(0, nrow = 19, ncol = input$years, dimnames = list(eiat:::anzsic_swap$name, col_names))
    updateMatrixInput(session, "industry_input", m)

  })

  observeEvent(input$upload, {
    updateNumericInput(session, "years", value = NCOL(user_matrix()))
    updateMatrixInput(session, "industry_input", user_matrix())
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



  # Modules --------------------------------------------------------------
  AnnualServer("employment", tab = "emp", reactive(input$lga), reactive(input$industry_input))
  AnnualServer("grp", tab = "grp", reactive(input$lga), reactive(input$industry_input))
  TotalServer("employment_total", tab = "emp", reactive(input$lga), reactive(input$industry_input))
  TotalServer("grp_total", tab = "grp", reactive(input$lga), reactive(input$industry_input))


  # Report ------------------------------------------------------------------

  output$report <- downloadHandler(
    filename = "report.html",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)

      params <- list(title = input$project_name,
                     description = input$project_desc,
                     author = input$project_analyst,
                     data = input$industry_input)

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )


  # GRP ---------------------------------------------------------------------




  output$grp <- DT::renderDataTable({
    impact_analysis(region = input$lga,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      mutate(value = sprintf(value, fmt = "%.2f")) %>%
      filter(year == input$select_year_grp) %>%
      pivot_wider(id_cols = c(year, Sector), names_from = type, values_from = value) %>%
      select(Sector, `Direct GRP`, `Flow on GRP`, `Total GRP`) %>%
      datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"), rownames = FALSE)
  })

  output$grp_table <- renderTable({
    impact_analysis(region = input$lga,
                    impacts = input$industry_input) %>%
      .[["grp"]] %>%
      pivot_wider(names_from = year) %>%
      group_by(type) %>%
      summarise(across(where(is.double), sum), .groups = "drop") %>%
      filter(type %in% c("Direct GRP",
                         "Flow on GRP",
                         "Total GRP"))
  })

  output$grp_table_sector <- renderTable({
    d <- impact_analysis(region = input$lga,
                         impacts = input$industry_input) %>%
      .[["grp"]] %>%
      pivot_wider(names_from = year) %>%
      group_by(Sector) %>%
      summarise(across(where(is.double), sum), .groups = "drop")

    rbind(d, c(NA, colSums(d[, 2:11])))
  })



  output$grp_plot <- renderPlot({
    impact_analysis(region = input$lga,
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
