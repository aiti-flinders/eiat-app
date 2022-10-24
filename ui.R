navbarPage("Economic Impact Assessment Tool",
           fluid = TRUE,
           theme = bslib::bs_theme(bootswatch = "flatly"),
           tabPanel("Description",
                    fluidRow(
                      column(width = 7, id = "grey",
                             tags$style("#grey {background-color: white;}"),
                             h3("Economic Impact Analysis Tool"),
                             p("EIAT is an online input-output (I-O) analysis tool for Local Goverment Areas across Australia.
                      It has been developed by the Australian Industrial Transformation Institute at Flinders University,
                      in conjunction with AURIN. This tool is designed to enable users to conduct regional economic impact analyses."),
                      h3("Input-Output Models"),
                      p("Input-Output models provide a standard approach for the estimation of the economic impact of a particular activity
                      (e.g. construction of a new infrastructure project). A regional economic impact statement regarding the impact of
                      major projects and policies has become a critical part of regional development analysis, and is an extensive component
                      of the applied economic literature. The linkages between employment opportunities and residents - and business to business linkages affect
                      urban design and transport systems, infrasftructure demand, and regional taxes, amongst others."),
                      h3("Using the Economic Impact Analysis Tool"),
                      tags$b("The quality of an economic impact assessment is dependent on the quality of input data.", style = 'color:red'),
                      p("The analyst is required to enter the direct capital expenditures assosciated with the investment project of interest.
                             These expenditures must be expressed in millions of dollars ($M) in basic or producer prices which exclude margins,
                             taxes, and subsidies. Only expenditure which is expected to occur inside the region should be entered. Expenditures that
                             occur outside of the region should be excluded from the analysis. This includes expenditure which may be allocated to a region but
                             must be imported from outside the region. The tool is agnostic to input data. Any reporting of potential economic impacts
                             should also include a summary of the data used to generate the impact."),
                      p("For simple projects, the user may enter data directly into the Data Input panel in Project Setup. For more complex analyses,
                             the user may download an excel template, enter the expenditure, and upload to the tool."),
                      h3("Important Assumptions"),
                      p("The use of an input-output model imposes a number of assumptions which must be considered in interpreting the predicted impacts.
                             They include:",
                        tags$ol(
                          tags$li("Increases in demand in the region are serviced by industries with constant proportions, and no significant price adjustments occur."),
                          tags$li("Industries have a linear production function, which implies constant returns to scales and fixed input proportions."),
                          tags$li("Firms within a sector are homogenous, which implies they produce a fixed set of products that are not produced by any other sector,
                                       and that the input structure of the firms are the same."),
                          tags$li("The model is a static model that does not take into account the dynamic processes involved in the adjustment to an external change.")
                        )
                      ),
                      h3("Required Information"),
                      p("Before using input-output analysis to estimate the economic impact of regional expenditure, the user is required to collect information.
                             The analyst must know the magnitude of various expenditures and where they occur. Also needed is information on how the sectors recieving this
                             expenditure share their expenditures among the various sectors from whom they buy, and so on, for the further expenditure rounds. While private
                             and public stakeholders are welcome to use this powerful tool to conduct input-output analysis, it is recommended that expert consultants are
                             engaged for a full and detailed report on the estimations of economic impacts and the interpretations.")

                      ),
                      column(width = 5, id = "blue",
                             tags$style("#blue {background-color: white;}"),
                             h3("Tool Summary"),
                             p("This tool draws on data from the 2021 Census industry of employment data and the 2019/20 national input-output table.
                                  Regional transaction tables are derived using a location quotient model. The source code for the regional
                                  transaction tables is available at", a(href = 'https://github.com/aiti-flinders/eiat', 'the eiat GitHub'), "."),
                             h3("Version Info"),
                             p("The current version of this tool is", tags$b(as.character(packageVersion('eiat'))))
                      )
                    )
           ),
           tabPanel("Project Setup",
                    tabsetPanel(
                      tabPanel("Parameters",

                               h3("Region Selection"),
                               p("Select the region for which you would like to conduct the economic impact assessment. Regions are Local Government Areas
                                 as represented by the Australian Statistical Geography Standard (ASGS) Edition 3."),
                               fluidRow(
                                 column(width = 6,
                                        selectInput("state", "Select State", unique(regions$state_name))
                                 ),
                                 column(width = 6,
                                        selectInput("lga", "Select Region: ", unique(regions$lga))
                                 )
                               ),
                               h3("Model Parameters"),
                               splitLayout(
                                 numericInput("productivity", "Productivity multiplier (NYI)", value = 1, max = 2, step = 0.01),
                                 numericInput("inflation", "Inflation multiplier (NYI)", value = 1, max = 2, step = 0.01)
                               )


                      ),
                      tabPanel("Data Input",
                               p("You may enter input price data data directly into the table below.
                                 This may be your preferred method if the economic impact analysis is relatively simple.
                                 Impact analyses extending beyond the current year can be conducted by selecting a timespan below.",
                                 br(),
                                 "Economic impacts up to 10 years in the future can be calculated."),
                               numericInput("years", "Select the number of years : ", value = 1, min = 1, max = 10, step = 1),
                               p("For more complicated analyses, you may wish to download a template using the button below. This will
                                 download a file called 'EIAT-Template.csv', which can be opened in Microsoft Excel, OpenOffice, or Google
                                 Sheets. You may give the file any name which you prefer, but the extension should remain 'Comma Separated Values (.csv)'.
                                 Data created using the template can then be uploaded, using the upload data button.",
                                 "Please check that"),
                               matrixInput("industry_input",
                                           class = "numeric",
                                           cols = list(
                                             names = TRUE,
                                             extend = TRUE,
                                             editableNames = FALSE,
                                             delta = 0,
                                             delete = TRUE
                                           ),
                                           rows = list(
                                             names = TRUE,
                                             extend = TRUE,
                                             editableNames = FALSE,
                                             delta = 0,
                                             delete = FALSE
                                           ),
                                           value = matrix(0, nrow = 19, ncol = 1, dimnames = list(eiat:::anzsic_swap$name, 2022))
                               ),
                               splitLayout(
                                 downloadButton("download", "Download Excel Template", style = 'margin-top:25px'),
                                 fileInput("upload", "Upload Data", accept = "csv"),
                                 actionButton("clear", "Clear ALL Entered Data")

                               )



                      )

                    )

           ),
           tabPanel("Input Summary",
                    plotOutput("input_plot"),
                    plotOutput("input_plot_sector"),
                    titleUI("input_table_title"),
                    tableOutput("input_table"),
                    titleUI("input_table_sector_title"),
                    tableOutput("input_table_sector")
           ),

           navbarMenu("Economic Impacts",
                      tabPanel("Employment Impacts",
                               tabsetPanel(
                                 AnnualUI("employment"),
                                 TotalUI("employment_total")
                               )
                      ),
                      tabPanel("Gross Regional Product Impacts",
                               tabsetPanel(
                                 AnnualUI("grp"),
                                 TotalUI("grp_total")
                               )
                      )
           ),
           tabPanel("Report",
                    h3("Project Details"),
                    p("Enter a name for your project, as well as a short description. These inputs will be used to generate a report of
                                 your economic impact assessment. These inputs are optional."),
                    fluidRow(
                      column(width = 4,
                             textInput("project_name", "Project Name: ", placeholder = "Enter Project Name")
                      ),
                      column(width = 4,
                             textInput("project_analyst", "Project Analyst: ", placeholder = "Enter Analyst Name")
                      ),
                      column(width = 4,
                             textAreaInput("project_desc", "Project Description: ", placeholder = "Enter Project Description")

                      )
                    ),
                    h3("What to include in the report"),
                    checkboxInput("report_input", "Summarise Input Data (NYI)"),
                    checkboxInput("report_employment", "Summarise Employment Impacts (NYI)"),
                    checkboxInput("report_grp", "Summarise Gross Regional Product Impacts (NYI)"),
                    downloadButton("report", "Generate Report")
           ),

           navbarMenu("Base Data",
                      tabPanel("National I-O Table",
                               tableOutput("national_io")),
                      tabPanel("Regional Employment",
                               tableOutput("regional_employment")),
                      tabPanel("Regional I-O Table",
                               tableOutput("regional_io")
                      )
           )
)


