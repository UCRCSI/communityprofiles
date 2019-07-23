library(shiny)
library(tidyverse)
library(DT)

options(shiny.sanitize.errors = FALSE)


# Loading Cleaned Data ----------------------------------------------------
# dta <- read_rds("dta/geo_profile_dta_acs1yr.rds")
dta <- read_rds("dta/geo_profile_acs17_Sunnyclean.RDS")


group_choices <- factor(dta$group_name, levels = c("All races", "Am Indian Alaska Native alone", "Asian alone", "Black alone", "NHPI alone", "Hispanic", "White (Not-Hispanic) alone", "Am Indian Alaska Native combo", "Asian combo", "Black combo", "NHPI combo", "White (Not-Hispanic)  combo"))

dta %>% distinct(geo_display_label) %>% pull() -> geo_choices
topic_choices <- c("Population","Income","Educational Attainment","Age","Language","Health Insurance", "Homeownership")



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
           titlePanel("Geographic Profiles"),
            selectizeInput("geo_choice", "Choose a geographic area",
                           choices = geo_choices,
                           options = list(
                               placeholder = '"e.g. Colorado or Seattle or CD 5 (Florida)',
                               onInitialize = I('function() { this.setValue(""); }')
                           )),
           selectizeInput("group_choice", "Choose Racial/Ethnic Groups",
                          choices = group_choices,
                          multiple= T,
                          options = list(
                              placeholder = 'i.e. Asian Alone, Hispanic, Chinese',
                              maxItems = 5,
                              onInitialize = I('function() { this.setValue(""); }')
                          )),
           actionButton("create_report", "Create Report"),
           conditionalPanel("input.create_report != 0",
                            br(),
           downloadButton("report", "Download"))
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
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        h2("Total Population", align = "center"),
                        dataTableOutput("tot_pop"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        hr(),
                        h2("Income", align = "center"),
                        dataTableOutput("income"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        hr(),
                        h2("Educational Attainment", align = "center"),
                        h5(tags$em("Population 25 Years and over"),align = "center"),
                        dataTableOutput("edu"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        hr(),
                        h2("Age Distribution", align = "center"),
                        dataTableOutput("age"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        hr(),
                        h2("Language", align = "center"),
                        dataTableOutput("lang"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        hr(),
                        h2("Health Insurance", align = "center"),
                        dataTableOutput("healthins"))
                )
            ),
            fluidRow(
                column(
                    12, align = "left",
                    conditionalPanel("input.create_report != 0",
                        hr(),
                        h2("Homeownership", align = "center"),
                        dataTableOutput("homeowner"))
            )
        )),
        tabPanel("FAQ",
                 br(),
                 h3("Where does this data come from?"),
                    tags$ul(
                        tags$li("This tool pulls data from the 2017 American Community Survey 1 Year",
                                tags$a(href = "https://factfinder.census.gov/faces/nav/jsf/pages/index.xhtml",
                                       "Selected Population Tables."))),
                 h3("Why are some groups missing?"),
                 tags$ul(
                     tags$li("Although the 1-Year estimates offer the most recent data, they do not offer data for smaller geographic areas."),
                     tags$li("For more information about the differences between ACS 1-Year and 5-Year estimates, please visit",
                             tags$a(href="https://www.census.gov/programs-surveys/acs/guidance/estimates.html", "here."))),
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
    ),br(),
    fluidRow(
        column(2,offset=6,
        tags$footer("A Project of",tags$a(href = "http://www.aapidata.com", "AAPI Data"),
                    align = "center"))
    )
    )
)
