library(shiny)
library(tidyverse)
library(DT)
library(shinyWidgets)
library(highcharter)
library(shinyalert)


# Loading Cleaned Data ----------------------------------------------------
dta <- read_rds("dta/geo_profile_dta_acs1yr.rds")
dta %>% distinct(geo_display_label) -> geo_choices
dta %>% distinct(group_name) -> group_choices
topic_choices <- c("Population","Income","Educational Attainment","Age","Language","Health Insurance", "Homeownership")



# START OF UI -------------------------------------------------------------
shinyUI(
    fluidPage(
    # theme = "bootstrap.css",
    theme = "customcss.css",
    tags$div(
        class = "header", checked = NA,
        tags$img(src = "aapidata.png",
                 style = "align:center; display: block; width:250px; min-width: 200px max; margin-left: auto; margin-right: auto; margin-top:30px; margin-bottom:30px;")
    ),
    fluidRow(
    column(3, offset = 1,
           useShinyalert(),  # Set up shinyalert
           # actionButton("help_me", "Help Me!"),
           br(),
           # titlePanel("Geographic Profiles"),
            selectizeInput("geo_choice", "Choose a geographic area",
                           choices = geo_choices,
                           options = list(
                               placeholder = 'i.e. Riverside County, Colorado',
                               onInitialize = I('function() { this.setValue(""); }')
                           )),
           selectizeInput("group_choice", "Choose Racial/Ethnic Groups",
                          choices = group_choices,
                          multiple= T,
                          options = list(
                              placeholder = 'i.e. Asian Alone, Hispanic, Chinese',
                              maxItems = 3,
                              onInitialize = I('function() { this.setValue(""); }')
                          )),
           # checkboxGroupInput(
           #     "topic_choice", label = h3("Select Estimates"),
           #     choices = topic_choices,
           #     selected = 1
           # ),
           pickerInput(
               inputId = "topic_choice",
               label = h3("Select Estimates"),
               choices = topic_choices,
               options = list(
                   `actions-box` = TRUE,
                   size = 12,
                   `selected-text-format` = "count > 3"
               ),
               multiple = TRUE
           )
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
            h1(textOutput("selected_topic"), align = "center"),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Population', input.topic_choice) > -1",
                        hr(),
                        h2("Total Population", align = "center"),
                        dataTableOutput("tot_pop")
                    )
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Income', input.topic_choice) > -1",
                        hr(),
                        h2("Income", align = "center"),
                        dataTableOutput("income")
                    )
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Educational Attainment', input.topic_choice) > -1",
                        hr(),
                        h2("Educational Attainment", align = "center"),
                        dataTableOutput("edu")
                    )
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Age', input.topic_choice) > -1",
                        hr(),
                        h2("Age Distribution", align = "center"),
                        dataTableOutput("age")
                    )
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Language', input.topic_choice) > -1",
                        hr(),
                        h2("Language", align = "center"),
                        h3(actionButton("help_me", "What does Limited English Proficient Mean?"),align="center"),
                        dataTableOutput("lang")
                    )
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Health Insurance', input.topic_choice) > -1",
                        hr(),
                        h2("Health Insurance", align = "center"),
                        dataTableOutput("healthins")
                    )
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel(
                        "$.inArray('Homeownership', input.topic_choice) > -1",
                        hr(),
                        h2("Homeownership", align = "center"),
                        dataTableOutput("homeowner")
                    )
                )
            )
        ),
        tabPanel("FAQ",
                 br(),
                 h2("Where does this data come from?"),
                    tags$ul(
                        tags$li("This tool pulls data from the 2017 American Community Survey 1 Year",
                                tags$a(href = "https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml",
                                       "Selected Population Tables"))),
                 h2("Why are some groups missing?"),
                 tags$ul(
                     tags$li("For some groups with smaller populations, the Census will not report data...."))),
        tabPanel("testplot",
                 fluidRow(
                     column(
                         12, align = "left",
                         conditionalPanel(
                             "$.inArray('Population', input.topic_choice) > -1",
                             hr(),
                             h2("Total Population", align = "center"),
                             highchartOutput("plt_totpop",height = "500px"))
                     )
                 ),
                 fluidRow(
                     column(
                         12, align = "left",
                         conditionalPanel(
                             "$.inArray('Income', input.topic_choice) > -1",
                             hr(),
                             h2("Income", align = "center"),
                             highchartOutput("plt_income",height = "500px"))
                         )
                     )
                 )
            )
        )
    )
    ),
    fluidRow(
        column(1,offset=6,
        tags$footer("A Project of",
                    tags$a(href = "http://www.aapidata.com", "AAPI Data"),
                    align = "center"))
    )
    )
)
