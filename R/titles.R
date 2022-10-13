titleUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("title"))
}

titleServer <- function(id, text, region_selected) {
  moduleServer(
    id,
    function(input, output, session) {

      output$title <- renderUI({
        h3(glue('{text} in {region_selected()}'))

    })
    }
  )
}
