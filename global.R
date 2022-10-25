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
                        "Plot width (in cm)",
                        value = 21,
                        min = 10
           )
    ),
    column(width = 6,
           numericInput(NS(id, "height"),
                        "Plot height (cm)",
                        value = 13,
                        min = 8
           )
    ),
    column(width = 12,
           downloadButton(NS(id, "download_plot"), "Download chart", class = "download-button"),
    )
  )
}
