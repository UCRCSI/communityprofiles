library(shiny)
library(tidyverse)
library(DT)
options(shiny.sanitize.errors = FALSE)

# Loading Data ------------------------------------------------------------
# dta <- read_rds("dta/geo_profile_dta_acs1yr.rds")
dta <- read_rds("dta/geo_profile_acs17_Sunnyclean.RDS")
dta %>% distinct(geo_display_label) -> geo_choices

# Setting up groups of topics ---------------------------------------------
income_vars <- c("est_vc319","est_vc354","est_vc355")
pop_vars <- c("est_vc03")
educational_attainment <- c("est_vc135","est_vc136","est_vc137","est_vc138","est_vc139")
age_distribution <- c("est_vc27","est_vc16","est_vc17","est_vc18","est_vc19","est_vc20","est_vc21","est_vc22","est_vc23","est_vc24")
language <- c("est_vc233","est_vc234","est_vc235")
health_ins <- c('est_vc360','est_vc361','est_vc362')
homeowner <- c("est_vc390","est_vc391")

# START OF SERVER ---------------------------------------------------------
shinyServer(function(input, output,session) {

observe({
        updateSelectizeInput(session,
                             "geo_choice",
                             choices = unique(geo_choices))
    })

observe({
        updateSelectizeInput(
            session,
            "group_choice",
            choices = dta %>%
                filter(geo_display_label == input$geo_choice) %>%
                select(group_name) %>%
                unique() %>% .[[1]]
        )


    })



# Topic-Specific Data Cleaners --------------------------------------------
observeEvent(input$create_report, {


dta_totpop <- reactive({
    dta <- dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice) %>%
        select(group_name,pop_vars) %>%
        rename(`Total Population` = est_vc03) %>%
        gather(key = estimate_type, value,-group_name) %>%
        rename(Estimate= estimate_type) %>%
        spread(group_name, value)
    return(dta)

})

dta_income <- reactive({
        dta %>%
            filter(geo_display_label == input$geo_choice) %>%
            filter(group_name %in% input$group_choice) %>%
            select(group_name,income_vars) %>%
            rename(`Median HH Income` = est_vc319,
                   `Median earnings Full-Time Male` = est_vc354,
                   `Median earnings Full-Time Female` = est_vc355) %>%
            gather(key = estimate_type, value,-group_name) %>%
            rename(Estimate = estimate_type) %>%
            spread(group_name, value) -> dta
        dta$Estimate <- factor(dta$Estimate, levels=c("Median Income", "Median earnings Full-Time Female", "Median earnings Full-Time Male"))
        dta <- dta %>% arrange(Estimate)
        return(dta)

})

dta_edu <- reactive({
    dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice) %>%
        select(group_name,educational_attainment) %>%
        rename(`Less than HS` = est_vc135,
               `HS or GED` = est_vc136,
               `Some College or AA` = est_vc137,
               `Bachelors Degree` = est_vc138,
               `Graduate or Professional Degree` = est_vc139) %>%
        gather(key = estimate_type, value,-group_name) %>%
        # mutate(value = value/100) %>%
        rename(Estimate= estimate_type) %>%
        spread(group_name, value) -> dta

    dta$Estimate <- factor(dta$Estimate, levels=c("Less than HS", "HS or GED", "Some College or AA", "Bachelors Degree", "Graduate or Professional Degree"))
    dta <- dta %>% arrange(Estimate)
    return(dta)

})

dta_age <- reactive({
    dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice) %>%
        select(group_name, age_distribution) %>%
        rename(`Median Age` = est_vc27,
               `Under 5 years` = est_vc16,
               `5 to 17 years` = est_vc17,
               `18 to 24 years` = est_vc18,
               `25 to 34 years` = est_vc19,
               `35 to 44 years` = est_vc20,
               `45 to 54 years` = est_vc21,
               `55 to 64 years` = est_vc22,
               `65 to 74 years` = est_vc23,
               `75 years and over` = est_vc24) %>%
        gather(key = estimate_type, value,-group_name) %>%
        # mutate(value = value/100) %>%
        rename(Estimate= estimate_type) %>%
        spread(group_name, value) -> dta

    dta$Estimate <- factor(dta$Estimate, levels=c("Median Age", "Under 5 years", "5 to 17 years", "18 to 24 years", "25 to 34 years", "35 to 44 years", "45 to 54 years", "55 to 64 years", "65 to 74 years", "75 years and over"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

dta_lang <- reactive({
    dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice) %>%
        select(group_name,language) %>%
        rename(`Speak only English at Home` = est_vc233,
               `Speak Language other than English at Home` = est_vc234,
               `Limited English Proficient` = est_vc235) %>%
        gather(key = estimate_type, value,-group_name) %>%
        # mutate(value = value/100) %>%
        rename(Estimate = estimate_type) %>%
        spread(group_name, value) -> dta

    dta$Estimate <- factor(dta$Estimate, levels=c("Speak only English at Home", "Speak Language other than English at Home", "Limited English Proficient"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

dta_healthins <- reactive({
    dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice) %>%
        select(group_name,health_ins) %>%
        rename(`With Private Health Insurance` = est_vc360,
               `With Public Coverage` = est_vc361,
               `No Health Insurance` = est_vc362) %>%
        gather(key = estimate_type, value,-group_name) %>%
        # mutate(value = value/100) %>%
        rename(Estimate= estimate_type) %>%
        spread(group_name, value) -> dta
    dta %>% datatable()
    ## Reordering dta$Estimate
    dta$Estimate <- factor(dta$Estimate, levels=c("With Private Health Insurance", "With Public Coverage", "No Health Insurance"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

dta_home <- reactive({
    dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice) %>%
        select(group_name,homeowner) %>%
        rename(`Owner-Occupied` = est_vc390,
               `Renter-Occupied` = est_vc391) %>%
        gather(key = estimate_type, value,-group_name) %>%
        # mutate(value = value/100) %>%
        rename(Estimate= estimate_type) %>%
        spread(group_name, value) -> dta
    ## Reordering dta$Estimate
    dta$Estimate <- factor(dta$Estimate, levels=c("Total Housing Occupied Units", "Owner-Occupied", "Renter-Occupied"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

# Preparing Output Objects ------------------------------------------------

output$tot_pop <- DT::renderDataTable({
    DT::datatable(dta_totpop(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$income <- DT::renderDataTable({
    DT::datatable(dta_income(),
                  rownames = FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$edu <- DT::renderDataTable({
    DT::datatable(dta_edu(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$age <-  DT::renderDataTable({
    DT::datatable(dta_age(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        # formatPercentage(c(2,3,4,5,6)) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$lang <- DT::renderDataTable({
    DT::datatable(dta_lang(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        # formatPercentage(c(2,3,4,5,6)) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$healthins <- DT::renderDataTable({
    DT::datatable(dta_healthins(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        # formatPercentage(c(2,3,4,5,6)) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$homeowner <- DT::renderDataTable({
    DT::datatable(dta_home(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        # formatPercentage(c(2,3,4,5,6)) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

dta_download <- reactive({
    dta <- dta %>%
        filter(geo_display_label == input$geo_choice) %>%
        filter(group_name %in% input$group_choice)
    return(dta)
})

# Other Output ------------------------------------------------------------
output$selected_topic <- renderText({
    if(input$geo_choice == ""){
        return()
    }
    paste("Geographic Profile for: ", tags$strong(input$geo_choice,style ="color: #EFA875;"))
})

output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename <-  paste(input$geo_choice," Geographic Profile.pdf"),
    content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report2.Rmd")
        templogo <- file.path(tempdir(), "racial_data_logo_dark.png")
        tempheader <- file.path(tempdir(), "header_test.html")
        file.copy("report2.Rmd", tempReport, overwrite = TRUE)
        file.copy("www/racial_data_logo_dark.png", templogo, overwrite = TRUE)
        file.copy("header_test.html", tempheader, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(geo = input$geo_choice,
                       groups = input$group_choice,
                       dataset = dta_download())

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
    }
)


})

})
