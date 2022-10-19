navbarPage("Economic Impact Assessment Tool",
           fluid = TRUE,
           tabPanel("Description",
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
                           br(),
                           p("The analyst is required to enter the direct capital expenditures assosciated with the investment project of interest.
                             These expenditures must be expressed in millions of dollars ($M) in basic or producer prices which exclude margins,
                             taxes, and subsidies. Only expenditure which is expected to occur inside the region should be entered. Expenditures that
                             occur outside of the region should be excluded from the analysis. This includes expenditure which may be allocated to a region but
                             must be imported from outside the region. The tool is agnostic to input data. Any reporting of potential economic impacts
                             should also include a summary of the data used to generate the impact."),
                           br(),
                           p("For simple projects, the user may enter data directly into the Data Input panel in Project Setup. For more complex analyses,
                             the user may download an excel template, enter the expenditure, and upload to the tool.")

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
           ),
           tabPanel("Project Setup",
                    tabsetPanel(
                      tabPanel("Parameters",
                               h3("Project Details"),
                               p("Enter a name for your project, as well as a short description. These inputs will be used to generate a report of
                                 your economic impact assessment. These inputs are optional."),
                               fluidRow(
                                 column(width = 6,
                                        textInput("project_name", "Project Name: (NYI)", placeholder = "Enter Project Name")
                                 ),
                                 column(width = 6,
                                        textInput("project_desc", "Project Description: (NYI)", placeholder = "Enter Project Description")
                                 )
                               ),
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
                               ),
                               h3("BYO Data"),
                               numericInput("years", "Number of years: ", value = 1, max = 10, step = 1),
                               splitLayout(
                                 downloadButton("download", "Download Excel Template", style = 'margin-top:25px'),
                                 fileInput("upload", "Upload Data")
                               )


                      ),
                      tabPanel("Data Input",
                               uiOutput("m"),
                               actionButton("random", "Generate Random Data"),
                               actionButton("clear", "Clear ALL Entered Data")
                      ),
                      tabPanel("Data Input Summary",
                               plotOutput("input_plot"),
                               plotOutput("input_plot_sector"),
                               titleUI("input_table_title"),
                               tableOutput("input_table"),
                               titleUI("input_table_sector_title"),
                               tableOutput("input_table_sector")
                      )
                    )

           ),
           navbarMenu("Employment",
                      tabPanel("Employment Summary",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("select_year_fte", label = "Select Year: ", choices = 2022:2031)),
                                 mainPanel(
                                   titleUI("employment_summary_title"),
                                   DT::dataTableOutput("employment")
                                 )
                               )
                      ),
                      tabPanel("Employment Graphs",
                               plotOutput("employment_plot"),
                               plotOutput("employment_plot_sector")
                      ),
                      tabPanel("Employment Tables",
                               tableOutput("employment_table")
                      )
           ),
           navbarMenu("Gross Regional Product",
                      tabPanel("Gross Regional Product Summary",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("select_year_grp", label = "Select Year: ", choices = 2022:2031)),
                                 mainPanel(
                                   titleUI("grp_summary_title"),
                                   DT::dataTableOutput("grp")
                                 )
                               )
                      ),
                      tabPanel("Gross Regional Product Graphs",
                               plotOutput("grp_plot"),
                               plotOutput("grp_plot_sector")
                      ),
                      tabPanel("Gross Regional Product Tables",
                               tableOutput("grp_table"),
                               tableOutput("grp_table_sector")
                      )
           ),
           navbarMenu("Output",
                      tabPanel("Output Table",
                               sidebarLayout(
                                 sidebarPanel(
                                   radioButtons("select_year_output", label = "Select Year: ", choices = 2022:2031)),
                                 mainPanel(
                                   titleUI("output_summary_title"),
                                   DT::dataTableOutput("output")
                                 )
                               )
                      ),
                      tabPanel("Output Graphs",
                               plotOutput("output_plot"),
                               plotOutput("output_plot_sector")
                      )
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


