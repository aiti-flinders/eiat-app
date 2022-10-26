library(eiat)
library(DT)
library(dplyr)
library(tidyr)
library(shinyMatrix)
library(ggplot2)
library(aititheme)
library(thematic)
library(glue)
library(janitor)

regions <- eiat:::get_available_regions(2021)

ggplot2::theme_set(aititheme::theme_aiti())
thematic::thematic_shiny()

download_graph_ui <- function(id) {
  fluidRow(
    column(width = 6,
           textInput(NS(id, "filename"),
                     "Filename",
                     placeholder = "Type a filename")
    ),
    column(width = 6,
           radioButtons(NS(id, "filetype"),
                        "File extension",
                        choices = c("png",
                                    "jpeg",
                                    "pdf")
           )
    ),
    column(width = 6,
           numericInput(NS(id, "width"),
                        "Plot width (pixels)",
                        value = 1000,
                        min = 1000
           )
    ),
    column(width = 6,
           numericInput(NS(id, "height"),
                        "Plot height (pixels)",
                        value = 500,
                        min = 500
           )
    ),
    column(width = 12,
           downloadButton(NS(id, "download_plot"), "Download chart", class = "download-button"),
    )
  )
}
