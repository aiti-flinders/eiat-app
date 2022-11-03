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





  # File Downloads  ------------------------------------------------

  # Template
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

  # Objects
  downloadServer("download_national_io", "National I-O (19 Sector).csv", national_19)
  downloadServer("download_regional_employment", "Regional Employment.csv", regional_employment())
  downloadServer("download_expenditure", "Expenditure Plot.png", )


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
    filename = paste0(input$filename, ".pdf"),
    content = function(file) {
      tempReport <- file.path(tempdir(), c("report.Rmd", "preamble-latex.tex"))
      file.copy("report.Rmd", tempReport[1], overwrite = TRUE)
      file.copy("preamble-latex.tex", tempReport[2], overwrite = TRUE)

      params <- list(title = input$project_name,
                     description = input$project_desc,
                     author = input$project_analyst,
                     data = input$industry_input,
                     region = input$lga,
                     include_tables = input$report_tables,
                     include_graphs = input$report_graphs)

      rmarkdown::render(tempReport[1],
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )





  # Inputs ------------------------------------------------------------------

  inputServer("input_summary", reactive(input$lga), reactive(input$industry_input))


  # Base Data ---------------------------------------------------------------


  output$national_io <- DT::renderDataTable(
    datatable(national_19,
              rownames = FALSE,
              options = list(pageLength = 30,
                             scrollX = TRUE)) %>%
      formatRound(columns = c(2:length(national_19)), digits = 0)
  )

  regional_employment <- reactive({
    lq_models[[input$lga]][c("Local Employment", "Total Employment"),] %>%
    as_tibble(rownames = "employment_type") %>%
    pivot_longer(-employment_type,
                 names_to = "Sector") %>%
    pivot_wider(names_from = employment_type) %>%
    mutate(across(where(is.double), round)) %>%
    filter(Sector %in% eiat:::anzsic_swap$name)
  })

  output$regional_employment <- renderDataTable(
    datatable(regional_employment(),
              rownames = FALSE)


  )


  output$regional_io <- renderDataTable(
    datatable(lq_models[[input$lga]],
              rownames = TRUE,
              options = list(pageLength = 30,
                             scrollX = TRUE)) %>%
      formatRound(columns = c(1:27),
                  digits = 0)
  )

  output$multipliers <- renderDataTable(
    multipliers(input$region) %>%
    datatable(rownames = TRUE) %>%
      formatRound(columns = c(2:length(.)), digits = 3)
  )

}
