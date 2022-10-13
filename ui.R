navbarPage("Economic Impact Assessment Tool",
           navbarMenu("Input",
                      tabPanel("Input Data",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput("state", "Select State", unique(regions$state_name)),
                                   selectInput("lga", "Select Region: ", unique(regions$lga)
                                   ),
                                   #radioButtons("show_names", label = "Show sectors as: ", choices = c("Sector", "Name"))
                                 ),
                                 mainPanel(
                                   matrixInput("industry_input",
                                               value = matrix(0, nrow = 19, ncol = 10, dimnames = list(LETTERS[1:19], 2022:2031)),
                                               class = "numeric"),
                                   actionButton("random", "Generate Random Data"),

                                 )
                               )
                      ),
                      tabPanel("Input Graphs",
                               plotOutput("input_plot"),
                               plotOutput("input_plot_sector")),
                      tabPanel("Input Tables",
                               titleUI("input_table_title"),
                               tableOutput("input_table"),
                               titleUI("input_table_sector_title"),
                               tableOutput("input_table_sector"))
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


