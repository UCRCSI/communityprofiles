
source("global.R")
# Load Data ---------------------------------------------------------------

raw <- read_rds("dta/final_dta.rds")
raw <- raw %>%
  gather(estimate, value,-group)

# labels <- read_rds("dta/group_lookup.rds")
# labels <- labels %>% 
#   arrange(desc(variable))
# group_labels <- labels$group

dta_totpop <- raw %>% filter(estimate %in% c("pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", 
                                    "pop_2015", "pop_2016", "pop_2017", "growth_17", "growth_16", 
                                    "growth_15", "growth_14", "growth_13", "growth_12", "growth_11"))

dta_income <- raw %>% filter(estimate == "income")
dta_age <- raw %>% filter(estimate == "median_age")
dta_edu <- raw %>% filter(estimate %in% c("edu1", "edu2", "edu3", "edu4"))
dta_lep <- raw %>% filter(estimate %in% c("eng", "other_lang", "lep"))
dta_ins <- raw %>% filter(estimate %in% c("no_ins", "pri_ins"))
dta_home <- raw %>% filter(estimate %in% c("owner", "renter"))
dta_cvap <- raw %>% filter(estimate %in% c("cvap", "native", "fb"))

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


table_totpop <- reactive({
    dta <-  dta_totpop %>%
      filter(group %in% input$group_choice) %>%
      select(group, estimate, value) %>%
      rename(topic = estimate,
             estimate = value) %>% 
      mutate(estimatenew = case_when(
        topic %in% c("pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", 
                     "pop_2015", "pop_2016", "pop_2017") ~paste(scales::comma(round(estimate))),
        topic %in% c("growth_17", "growth_16", "growth_15", "growth_14", 
                     "growth_13", "growth_12", "growth_11") ~paste(as.character(round(estimate*100, 1)), " %", sep = ""),
        group == "Mongolian alone" ~NA_character_)) %>% 
      select(-estimate) %>% 
      spread(group, estimatenew) %>% 
      rename(Estimate = topic) %>% 
      filter(Estimate %in% c("pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", 
                             "pop_2015", "pop_2016", "pop_2017"))
    
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
table_growth <- reactive({
  dta <-  dta_totpop %>%
    filter(group %in% input$group_choice) %>%
    select(group, estimate, value) %>%
    rename(topic = estimate,
           estimate = value) %>% 
    mutate(estimatenew = case_when(
      topic %in% c("pop_2010", "pop_2011", "pop_2012", "pop_2013", "pop_2014", 
                   "pop_2015", "pop_2016", "pop_2017") ~paste(scales::comma(estimate)),
      topic %in% c("growth_17", "growth_16", "growth_15", "growth_14", 
                   "growth_13", "growth_12", "growth_11") ~paste(as.character(round(estimate*100, 1)), " %", sep = ""),
      group == "Mongolian alone" ~NA_character_)) %>% 
    select(-estimate) %>% 
    spread(group, estimatenew) %>% 
    rename(Estimate = topic) %>% 
    filter(Estimate %in% c("growth_17", "growth_16", "growth_15", "growth_14", 
                           "growth_13", "growth_12", "growth_11"))
  
  dta$Estimate <- fct_recode(dta$Estimate,
                             "Pop Growth 2017-2016" = "growth_17",
                             "Pop Growth 2016-2015" = "growth_16", 
                             "Pop Growth 2015-2014" = "growth_15",
                             "Pop Growth 2014-2013" = "growth_14", 
                             "Pop Growth 2013-2012" = "growth_13",
                             "Pop Growth 2012-2011" = "growth_12", 
                             "Pop Growth 2011-2010" = "growth_11")
  
  dta$Estimate <- factor(dta$Estimate, levels=c("Pop Growth 2017-2016",
                                                "Pop Growth 2016-2015", 
                                                "Pop Growth 2015-2014",
                                                "Pop Growth 2014-2013", 
                                                "Pop Growth 2013-2012",
                                                "Pop Growth 2012-2011", 
                                                "Pop Growth 2011-2010"))
  
  dta <- dta %>% arrange(Estimate)
  return(dta)
})
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

table_income <- reactive({
    dta <-  dta_income %>%
        filter(group %in% input$group_choice) %>%
        select(group,estimate, value) %>%
      rename(topic = estimate,
             estimate = value) %>% 
        mutate(estimatenew = as.character(scales::dollar(estimate))) %>%
        select(-estimate) %>%
        mutate(topic = "Median HH Income") %>%
        # mutate(estimatenew = paste(estimatenew, "<br>+/-")) %>%
        spread(group, estimatenew) %>%
        rename(Estimate = topic)

    return(dta)
})
# table_income <- reactive({
#     dta <-  dta_income %>%
#         filter(variable %in% input$group_choice) %>%
#         select(label,estimate, moe) %>%
#         mutate(estimatenew = case_when( moe >= (.25*estimate) ~ "-",
#                                         is.na(estimate) ~ "-",
#                                         is.na(moe) ~ as.character(scales::dollar(estimate)),
#                                         TRUE ~ as.character(scales::dollar(estimate)))) %>%
#         mutate(moeNew = case_when( moe >= (.25*estimate) ~ "-",
#                                    is.na(moe) ~ "-",
#                                    TRUE ~ as.character(scales::dollar(moe)))) %>%
#         select(-moe,-estimate) %>%
#         mutate(topic = "Median HH Income") %>%
#         mutate(estimatenew = paste(estimatenew, "<br>+/-",moeNew)) %>%
#         select(-moeNew) %>%
#         spread(label, estimatenew) %>%
#         rename(Estimate = topic)
# 
#     return(dta)
# })

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
                               "Citizen Age Voting Population" = "cvap",
                               "Foreign Born" = "fb",
                               "Native Born" = "native")
    ## Reordering dta$label.x
    dta$Estimate <- factor(dta$Estimate, levels=c(  "Native Born" ,   "Foreign Born",  "Citizen Age Voting Population"))
    dta <- dta %>% arrange(Estimate)
    return(dta)
})

table_raw <- reactive({
  dta <-  raw %>%
    # filter(group %in% input$group_choice) %>%
    select(group, estimate, value) %>%
    rename(topic = estimate,
           estimate = value) %>% 
    spread(group, estimate) %>% 
    rename(Estimate = topic)
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
  paste("Universe: Total Population")
})

output$growth_notes <- renderText({
  if(input$group_choice == ""){
    return()
  }
  paste("Universe: Total Population")
})


output$edu_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Age 25+")
})

output$income_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Households")
})

output$age_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Total Population")
})

output$lep_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: 5 years +")
})

output$ins_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Civilian noninstitutionalized population")
})

output$home_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Occupied housing units")
})

output$cvap_notes <- renderText({
    if(input$group_choice == ""){
        return()
    }
    paste("Universe: Total population")
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
  content = function(file) {write_xlsx(raw, path = file)}
)

# output$extract <- downloadHandler(
#   filename = function() {paste("community_profiles.xlsx", "2019", sep="")},
#   content = function(file) {
#     excelfile <- createWorkbook()
#     addWorksheet(wb = excelfile, sheetName = "raw_data", gridLines = TRUE)
#     # img <- system.file("einstein.jpg", package = "openxlsx")
#     insertImage(excelfile, "raw_data", "www/aapidata.png", 
#                 startRow = 1,  startCol = 1, width = 2.5, height = 1.5)
#     # addWorksheet(wb = excelfile, sheetName = "raw_data", gridLines = TRUE)
#     writeData(wb = excelfile, sheet = "raw_data", x = dta_download, startCol = 1, startRow = 9)
#     saveWorkbook(excelfile, filename,  overwrite = TRUE)
#     }
# 
# )

})

})
