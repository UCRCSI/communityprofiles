

source("global.R")
# START OF UI -------------------------------------------------------------
shinyUI(
    fluidPage(
        tags$head(includeScript("google-analytics.js")),
    theme = "customcss.css",
    tags$div(
        class = "header", checked = NA,
        tags$img(src = "racial_data_logo.png",
                 style = "align:center; display: block; width:250px; min-width: 200px max; margin-left: auto; margin-right: auto; margin-top:30px; margin-bottom:30px;")
    ),
    fluidRow(
    column(3, offset = 1,
           # useShinyalert(),  # Set up shinyalert
           # actionButton("help_me", "Help Me!"),
           br(),
           titlePanel("Community Profiles"),
            # selectizeInput("geo_choice", "Choose a geographic area",
            #                choices = geo_labels,
            #                options = list(
            #                    placeholder = '"e.g. Colorado or Seattle or CD 5 (Florida)',
            #                    onInitialize = I('function() { this.setValue(""); }')
            #                )),
           selectizeInput("group_choice", "Choose Detailed Ethnic Groups (up to 5 groups)",
                          choices = group_labels,
                          multiple= T,
                          options = list(
                              placeholder = 'i.e. Asian Indian Alone, Mexican, South American',
                              maxItems = 5,
                              onInitialize = I('function() { this.setValue(""); }')
                          )),
           actionButton("create_report", "Create Report"),
           conditionalPanel("input.create_report != 0",
                            br(),
           downloadButton("report", "Download Report (PDF)")),
           # downloadButton("rawdata", "Download Raw Data (xlsx)")
           conditionalPanel("input.create_report != 0",
                            br(),
                            downloadButton("rawdata", "Download raw data (Excel)"))
           
        ),

# MAIN PANEL --------------------------------------------------------------

    column(8,
        mainPanel(
            tabsetPanel(
                tabPanel("Profile",
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            br(),
            h1(htmlOutput("selected_topic"), align = "center"),
            # fluidRow(
            #     column(
            #         12, align = "left",
            #         conditionalPanel("input.create_report != 0",
            #             h2("Total Population", align = "center"),
            #             dataTableOutput("total_pop"))
            #     )
            # ),
            fluidRow(
              column(
                12, align = "left",
                conditionalPanel("input.create_report != 0",
                                 h2("Total Population", align = "center"),
                                 h6(htmlOutput("totpop_notes"),align = "center"),
                                 dataTableOutput("total_pop"))
              )
            ),
            fluidRow(
              column(
                12, align = "left",
                conditionalPanel("input.create_report != 0",
                                 h2("Population Growth", align = "center"),
                                 h6(htmlOutput("growth_notes"),align = "center"),
                                 dataTableOutput("growth"))
              )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("Educational Attainment", align = "center"),
                                     h6(htmlOutput("edu_notes"),align = "center"),
                                     dataTableOutput("edu"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("Income", align = "center"),
                                     h6(htmlOutput("income_notes"),align = "center"),
                                     dataTableOutput("income"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("Age", align = "center"),
                                     h6(htmlOutput("age_notes"),align = "center"),
                                     dataTableOutput("age"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("Language", align = "center"),
                                     h6(htmlOutput("lep_notes"),align = "center"),
                                     dataTableOutput("lep"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("Health Insurance", align = "center"),
                                     h6(htmlOutput("ins_notes"),align = "center"),
                                     dataTableOutput("ins"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("Homeownership", align = "center"),
                                     h6(htmlOutput("home_notes"),align = "center"),
                                     dataTableOutput("home"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                                     h2("CVAP", align = "center"),
                                     h6(htmlOutput("cvap_notes"),align = "center"),
                                     dataTableOutput("cvap"))
                )
            )
            ),
        tabPanel("FAQ",
                 br(),
                 h3("Where does this data come from?"),
                    tags$ul(
                        tags$li("This tool pulls data from the 2017 American Community Survey 5 Year",
                                tags$a(href = "https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml",
                                       "Tables."))),
                 h3("Why are groups missing for certain topics (i.e. Korean alone by Median HH Income)?"),
                 tags$ul(
                     tags$li("The publicly available 5-Year tables do not disaggreagate many topics by detailed origin group. However, we are in the process of adding support for these groups by using the ACS PUMS data. Stay Tuned!")),
                 h3("What is the difference between Alone and Combo?"),
                 tags$ul(
                     tags$li("See here for the Census definition of", tags$a(href = "https://factfinder.census.gov/help/en/race_alone.htm","race alone.")),
                     tags$li("See here for the Census definition of", tags$a(href = "https://factfinder.census.gov/help/en/race_alone_or_in_combination.htm","race combo."))),
                 h3("How did you make this tool?"),
                 tags$ul(
                     tags$li("This tool was built by",tags$a(href="https://www.sonoshah.com/","Sono Shah"), "and", tags$a(href="https://www.linkedin.com/in/yimingshao/","Sunny Shao"), "using R and Shiny.")))
            )
        )
    )
    ),
br(),
    fluidRow(
        column(2,offset=6,
        tags$footer(
            tags$p("About",tags$a(href = "http://www.racialdata.com/about", "Racial Data")),
                    align = "center"))
    )
    )
)
