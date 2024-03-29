
source("global.R")
# Load Data ---------------------------------------------------------------

raw <- read_rds("dta/final_dta.rds")
raw <- raw %>%
  gather(estimate, value,-group)

# labels <- read_rds("dta/group_lookup.rds")
# labels <- labels %>% 
#   arrange(desc(variable))
# group_labels <- labels$group

list_pop <- c("pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", 
         "pop_2015", "pop_2016", "pop_2017")
list_growth <- c("growth", "growth_17", "growth_16", 
            "growth_15", "growth_14", "growth_13", "growth_12", "growth_11")
list_edu <- c("edu1", "edu2", "edu3", "edu4")
list_lep <- c("eng", "other_lang", "lep")
list_ins <- c("no_ins", "pri_ins")
list_home <- c("owner", "renter")
list_nativity <- c("native", "fb")
list_cvap <- c("cit_18", "non_cit", "cvap")


dta_totpop <- raw %>% filter(estimate %in% list_pop | estimate %in% list_growth)
dta_income <- raw %>% filter(estimate == "income")
dta_age <- raw %>% filter(estimate == "median_age")
dta_edu <- raw %>% filter(estimate %in% list_edu)
dta_lep <- raw %>% filter(estimate %in% list_lep)
dta_ins <- raw %>% filter(estimate %in% list_ins)
dta_home <- raw %>% filter(estimate %in% list_home)
dta_nativity <- raw %>% filter(estimate %in% list_nativity)
dta_cvap <- raw %>% filter(estimate %in% list_cvap)

# pop_compare <- read_rds("dta/pop_racial.rds")
# income_compare <- read_rds("dta/income_racial.rds")
# age_compare <- read_rds("dta/age_racial.rds")
# edu_compare <- read_rds("dta/edu_racial.rds")
# ins_compare <- read_rds("dta/ins_racial.rds")
# home_compare <- read_rds("dta/home_racial.rds")
# cvap_compare <- read_rds("dta/cvap_racial.rds")
# lep_compare <- read_rds("dta/lep_racial.rds")

# START OF SERVER ---------------------------------------------------------
shinyServer(function(input, output,session) {
observe({
        updateSelectizeInput(session,
                             "group_choice",
                             choices = group_labels,
                             server = TRUE)
    })




# Topic-Specific Data Cleaners --------------------------------------------
  
observeEvent(input$create_report, {

profile_name <- reactive({
    names(group_labels)[group_labels == input$group_choice]
})


# excel export ------------------------------------------------------------

selected_data <- reactive({
  fulldata <- raw %>% 
    filter(!estimate %in% c("kpov", "spov", "pov")) 
  
  fulldata$estimate <- fct_recode(fulldata$estimate,
                                  "Population 2017" = "pop_2017",
                                  "Population 2016" = "pop_2016",
                                  "Population 2015" = "pop_2015",
                                  "Population 2014" = "pop_2014",
                                  "Population 2013" = "pop_2013",
                                  "Population 2012" = "pop_2012",
                                  "Population 2011" = "pop_2011",
                                  "Population 2010" = "pop_2010",
                                  "Pop Growth 2017-2010" = "growth",
                                  "Pop Growth 2017-2016" = "growth_17",
                                  "Pop Growth 2016-2015" = "growth_16", 
                                  "Pop Growth 2015-2014" = "growth_15",
                                  "Pop Growth 2014-2013" = "growth_14", 
                                  "Pop Growth 2013-2012" = "growth_13",
                                  "Pop Growth 2012-2011" = "growth_12", 
                                  "Pop Growth 2011-2010" = "growth_11",
                                  "BA or Higher" = "edu4",
                                  "HS Grad" = "edu2",
                                  "Less HS" = "edu1",
                                  "Some College or AA" = "edu3",
                                  "Median HH Income" = "income",
                                  "Median Age" = "median_age",
                                  "Speak only English" = "eng",
                                  "Limited English Proficient" = "lep",
                                  "Speak Language other than English at home" = "other_lang",
                                  "No Health Insurance" = "no_ins",
                                  "With Private Insurance" = "pri_ins",
                                  "Owner" = "owner",
                                  "Renter" = "renter",
                                  "Foreign Born" = "fb",
                                  "Native Born" = "native",
                                  "Citizen Age Voting Population" = "cvap",
                                  "Citizen under 18" = "cit_18",
                                  "Non-Citizen Population" = "non_cit")
  ## Reordering fulldata$estimate
  fulldata$estimate <- factor(fulldata$estimate, levels=c("Median HH Income", "Median Age", 
                                                          "Population 2017", "Population 2016", 
                                                          "Population 2015", "Population 2014", 
                                                          "Population 2013", "Population 2012", 
                                                          "Population 2011", "Population 2010", 
                                                          "Pop Growth 2017-2010", "Pop Growth 2017-2016", 
                                                          "Pop Growth 2016-2015", "Pop Growth 2015-2014", 
                                                          "Pop Growth 2014-2013", "Pop Growth 2013-2012", 
                                                          "Pop Growth 2012-2011", "Pop Growth 2011-2010", 
                                                          "Less HS", "HS Grad", "Some College or AA", 
                                                          "BA or Higher",  
                                                          "Speak only English", 
                                                          "Speak Language other than English at home", 
                                                          "Limited English Proficient", 
                                                          "Native Born", "Foreign Born", 
                                                          "Citizen Age Voting Population",
                                                          "Citizen under 18",
                                                          "Non-Citizen Population",
                                                          "No Health Insurance", 
                                                          "With Private Insurance", 
                                                          "Owner", "Renter"))
  
  dta <- fulldata %>% 
    filter(group %in% input$group_choice) %>%
    spread(group, value) %>% 
    rename(Estimate = estimate)

  return(dta)
})


# pop ---------------------------------------------------------------------


table_totpop <- reactive({
    dta <-  dta_totpop %>%
      filter(group %in% input$group_choice) %>%
      select(group, estimate, value) %>%
      rename(topic = estimate,
             estimate = value) %>% 
      mutate(estimatenew = case_when(
        topic %in% list_pop ~paste(scales::comma(round(estimate))),
        topic %in% list_growth ~paste(as.character(round(estimate*100, 1)), " %", sep = ""),
        group == "Mongolian alone" ~NA_character_)) %>% 
      select(-estimate) %>% 
      spread(group, estimatenew) %>% 
      rename(Estimate = topic) %>% 
      filter(Estimate %in% list_pop)
    
    dta$Estimate <- fct_recode(dta$Estimate,
                                "Population 2017" = "pop_2017",
                                "Population 2016" = "pop_2016",
                                "Population 2015" = "pop_2015",
                                "Population 2014" = "pop_2014",
                                "Population 2013" = "pop_2013",
                                "Population 2012" = "pop_2012",
                                "Population 2011" = "pop_2011",
                                "Population 2010" = "pop_2010")
    
    dta$Estimate <- factor(dta$Estimate, levels=c("Population 2017",
                                                  "Population 2016",
                                                  "Population 2015",
                                                  "Population 2014",
                                                  "Population 2013",
                                                  "Population 2012",
                                                  "Population 2011",
                                                  "Population 2010"))

    dta <- dta %>% arrange(Estimate)
    return(dta)
})

# growth ------------------------------------------------------------------

table_growth <- reactive({
  dta <-  dta_totpop %>%
    filter(group %in% input$group_choice) %>%
    select(group, estimate, value) %>%
    rename(topic = estimate,
           estimate = value) %>% 
    mutate(estimatenew = case_when(
      topic %in% list_pop ~paste(scales::comma(estimate)),
      topic %in% list_growth ~paste(as.character(round(estimate*100, 1)), " %", sep = ""),
      group == "Mongolian alone" ~NA_character_)) %>% 
    select(-estimate) %>% 
    spread(group, estimatenew) %>% 
    rename(Estimate = topic) %>% 
    filter(Estimate %in% list_growth)
  
  dta$Estimate <- fct_recode(dta$Estimate,
                             "Pop Growth 2017-2010" = "growth",
                             "Pop Growth 2017-2016" = "growth_17",
                             "Pop Growth 2016-2015" = "growth_16", 
                             "Pop Growth 2015-2014" = "growth_15",
                             "Pop Growth 2014-2013" = "growth_14", 
                             "Pop Growth 2013-2012" = "growth_13",
                             "Pop Growth 2012-2011" = "growth_12", 
                             "Pop Growth 2011-2010" = "growth_11")
  
  dta$Estimate <- factor(dta$Estimate, levels=c("Pop Growth 2017-2010",
                                                "Pop Growth 2017-2016",
                                                "Pop Growth 2016-2015", 
                                                "Pop Growth 2015-2014",
                                                "Pop Growth 2014-2013", 
                                                "Pop Growth 2013-2012",
                                                "Pop Growth 2012-2011", 
                                                "Pop Growth 2011-2010"))
  
  dta <- dta %>% arrange(Estimate)
  return(dta)
})

# edu ---------------------------------------------------------------------

table_edu <- reactive({
   
    dta_edu %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
        rename(topic = estimate,
               estimate = value) %>% 
        mutate(estimate = case_when(is.na(estimate) ~ "-",
                                    estimate == 0 ~ "-",
                                    TRUE ~ as.character(round(estimate*100,1)))) %>%
        mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
        select(-estimate) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic) -> dta

    dta$Estimate <- fct_recode(dta$Estimate,
                              "BA or Higher" = "edu4",
                              "HS Grad" = "edu2",
                              "Less HS" = "edu1",
                              "Some College or AA" = "edu3")
    ## Reordering dta$topic
    dta$Estimate <- factor(dta$Estimate, levels=c("Less HS", "HS Grad", "Some College or AA", "BA or Higher"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

# income ------------------------------------------------------------------

table_income <- reactive({
    dta <-  dta_income %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
      rename(topic = estimate,
             estimate = value) %>% 
        mutate(estimatenew = as.character(scales::dollar(round(estimate)))) %>%
        select(-estimate) %>%
        mutate(topic = "Median HH Income") %>%
        # mutate(estimatenew = paste(estimatenew, "<br>+/-")) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic)

    return(dta)
})


# age ---------------------------------------------------------------------

table_age <- reactive({
    dta <-  dta_age %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
        rename(topic = estimate,
             estimate = value) %>%
        mutate(topic = "Median Age",
               estimatenew = as.character(estimate)) %>%
        select(-estimate) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic)
    
    # dta$Estimate <- fct_recode(dta$Estimate,
    #                            "Median Age" = "median_age")
    # ## Reordering dta$topic
    # dta$Estimate <- factor(dta$Estimate, levels=c("Median Age"))
    # dta <- dta %>% arrange(Estimate)

    return(dta)
})

# lep ---------------------------------------------------------------------

table_lep <- reactive({
    dta_lep %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
        rename(topic = estimate,
           estimate = value) %>%    
        mutate(estimate = case_when(is.na(estimate) ~ "-",
                                    estimate == 0 ~ "-",
                                    TRUE ~ as.character(round(estimate*100,1)))) %>%
        mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
        select(-estimate) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic) -> dta
    ## Recoding dta$topic
    dta$Estimate <- fct_recode(dta$Estimate,
                              "Speak only English" = "eng",
                              "Limited English Proficient" = "lep",
                              "Speak Language other than English at home" = "other_lang")
    ## Reordering dta$topic
    dta$Estimate <- factor(dta$Estimate, levels=c("Speak only English", "Speak Language other than English at home", "Limited English Proficient"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})


# insurance ---------------------------------------------------------------

table_ins <- reactive({
    dta_ins %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
        rename(topic = estimate,
               estimate = value) %>%
        mutate(estimate = case_when(is.na(estimate) ~ "-",
                                    estimate == 0 ~ "-",
                                    TRUE ~ as.character(round(estimate*100,1)))) %>%
        mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
        select(-estimate) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic) -> dta
  ## Recoding dta$topic
  dta$Estimate <- fct_recode(dta$Estimate,
                             "No Health Insurance" = "no_ins",
                             "With Private Insurance" = "pri_ins")
  ## Reordering dta$topic
  dta$Estimate <- factor(dta$Estimate, levels=c("No Health Insurance", "With Private Insurance"))
    return(dta)
})


# home ownership ----------------------------------------------------------

table_home <- reactive({
    dta_home %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
        rename(topic = estimate,
               estimate = value) %>%
        mutate(estimate = case_when(is.na(estimate) ~ "-",
                                    estimate == 0 ~ "-",
                                    TRUE ~ as.character(round(estimate*100,1)))) %>%
        mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
        select(-estimate) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic) -> dta


    dta$Estimate <- fct_recode(dta$Estimate,
                               "Owner" = "owner",
                               "Renter" = "renter")
    return(dta)
})


# nativity ----------------------------------------------------------------
table_nativity <- reactive({
  dta_nativity %>%
    filter(group %in% input$group_choice) %>%
    select(group, estimate, value) %>%
    rename(topic = estimate,
           estimate = value) %>%
    mutate(estimate = case_when(is.na(estimate) ~ "-",
                                estimate == 0 ~ "-",
                                TRUE ~ as.character(round(estimate*100,1)))) %>%
    mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
    select(-estimate) %>%
    spread(group, estimatenew) %>%
    rename(Estimate = topic) -> dta
  
  dta$Estimate <- fct_recode(dta$Estimate,
                             "Foreign Born" = "fb",
                             "Native Born" = "native")
  ## Reordering dta$label.x
  dta$Estimate <- factor(dta$Estimate, levels=c(  "Native Born", "Foreign Born"))
  dta <- dta %>% arrange(Estimate)
  return(dta)
})


# cvap --------------------------------------------------------------------

table_cvap <- reactive({
    dta_cvap %>%
        filter(group %in% input$group_choice) %>%
        select(group, estimate, value) %>%
        rename(topic = estimate,
               estimate = value) %>%
        mutate(estimate = case_when(is.na(estimate) ~ "-",
                                    estimate == 0 ~ "-",
                                    TRUE ~ as.character(round(estimate*100,1)))) %>%
        mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
        select(-estimate) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic) -> dta

    dta$Estimate <- fct_recode(dta$Estimate,
                               "Citizen Voting Age Population" = "cvap",
                               "Citizen (under 18 years old)" = "cit_18",
                               "Non-Citizen Population" = "non_cit")
    ## Reordering dta$label.x
    dta$Estimate <- factor(dta$Estimate, levels=c("Citizen Voting Age Population",
                                                  "Citizen (under 18 years old)",
                                                  "Non-Citizen Population"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

dta_download <- reactive({
    dta_all <- list(tot_pop = table_totpop(),
                    growth =  table_growth(),
                    edu = table_edu(),
                    income = table_income(),
                    age = table_age(),
                    lep = table_lep(),
                    ins= table_ins(),
                    home = table_home(),
                    nativity = table_nativity(),
                    cvap = table_cvap())
    return(dta_all)
})


# Preparing Output Objects ------------------------------------------------

output$growth <- DT::renderDataTable({
  DT::datatable(table_growth(),
                rownames = FALSE,
                escape=FALSE,
                extensions = 'Buttons',
                style = 'bootstrap', class = 'table-bordered',
                options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatStyle(c(2,3,4,5,6), `text-align` = 'center')
  
})


output$total_pop <- DT::renderDataTable({
    DT::datatable(table_totpop(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})


output$edu <- DT::renderDataTable({
    DT::datatable(table_edu(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$income <- DT::renderDataTable({
    DT::datatable(table_income(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$age <- DT::renderDataTable({
    DT::datatable(table_age(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$lep <- DT::renderDataTable({
    DT::datatable(table_lep(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$ins <- DT::renderDataTable({
    DT::datatable(table_ins(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$home <- DT::renderDataTable({
    DT::datatable(table_home(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

output$nativity <- DT::renderDataTable({
  DT::datatable(table_nativity(),
                rownames = FALSE,
                escape=FALSE,
                extensions = 'Buttons',
                style = 'bootstrap', class = 'table-bordered',
                options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatStyle(c(2,3,4,5,6), `text-align` = 'center')
  
})

output$cvap <- DT::renderDataTable({
    DT::datatable(table_cvap(),
                  rownames = FALSE,
                  escape=FALSE,
                  extensions = 'Buttons',
                  style = 'bootstrap', class = 'table-bordered',
                  options = list(dom = 't', pageLength = 25, buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
        formatStyle(c(2,3,4,5,6), `text-align` = 'center')

})

# Other Output ------------------------------------------------------------

output$totpop_notes <- renderText({
  if(input$group_choice == ""){
    return()
  }
  paste("Universe: Total Population<br>Source: 2017-2010 ACS table B02015, B02016, B03001, B03002 and B02001")
})

output$growth_notes <- renderText({
  if(input$group_choice == ""){
    return()
  }
  paste("Universe: Total Population<br>Source: 2017-2010 5-year ACS table B02015, B02016, B03001, B03002 and B02001")
})


output$edu_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Age 25+<br>Source: 2017 5-year ACS table B15002, C15002B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$income_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Households<br>Source: 2017 5-year ACS table B19013, B19013B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$age_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Total Population<br>Source: 2017 5-year ACS table B01002, B01002B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$lep_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: 5 years +<br>Source: 2017 5-year ACS table B16005, B16005B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$ins_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Civilian noninstitutionalized population<br>Source: 2017 5-year ACS table B27001, C27001B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$home_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Occupied housing units<br>Source: 2017 5-year ACS table B25003, B25003B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$nativity_notes <- renderText({
  if(input$group_choice == ""){
    return()
  }
  paste("Universe: Total population<br>Source: 2017 5-year ACS table B05003, B05003B/C/D/E/H/I (racial groups) & PUMS data (ethnic groups)")
})

output$cvap_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Total population<br>Source: 2017 5-year ACS Citizen Voting Age Population Special Tabulation (racial data) & PUMS data (ethnic groups)")
})


output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename <-  paste(profile_name()," Community Profile.pdf"),
    content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        templogo <- file.path(tempdir(), "racial_data_logo_dark.png")
        tempheader <- file.path(tempdir(), "header_test.html")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        file.copy("www/racial_data_logo_dark.png", templogo, overwrite = TRUE)
        file.copy("header_test.html", tempheader, overwrite = TRUE)
        # Set up parameters to pass to Rmd document
        params <- list(group = profile_name(),
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

output$rawdata <- downloadHandler(
  filename = function() { "Community Profiles.xlsx"},
  content = function(file) {
    wb <- createWorkbook()
    addWorksheet(wb, sheetName = "data", gridLines = T)
    insertImage(wb, sheet = "data", "www/racial_data_logo_dark.png", startRow = 1, startCol = 1,width =3, height = 1.5)
    #local test
    # writeDataTable(wb, x = dta, sheet = "data", startCol = 1, startRow = 8, colNames = T)
    # setColWidths(wb, sheet = "data", cols = 1:6, widths = "35")
    # excel_style()#check golobal.R for detailed styling function
    # saveWorkbook(wb, file = "dta/test.xlsx", overwrite = T)
    #server script
    writeDataTable(wb, x = selected_data(), sheet = "data", startCol = 1, startRow = 8, 
                   bandedRows=F, firstColumn=T, colNames = T)
    setColWidths(wb, sheet = "data", cols = 1:6, widths = "35")
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 2)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 3)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 4)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 5)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 6)
    
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 2)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 3)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 4)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 5)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 6)
    
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 2)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 3)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 4)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 5)
    addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 6)
    
    saveWorkbook(wb, file = file, overwrite = T)

    }
)


})

})
