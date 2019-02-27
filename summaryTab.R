
# report page control defined here----

output$summaryPage <- renderUI({
  
  tagList(
    column(8, uiOutput("summaryOutputs")),
    column(4, h2("Report builder"),
           
           # write the stuff in here
           
           # I've had a good idea where the report updates the numbers
           # of responses, service areas, that kind of thing, as you go
           
           # Quarterly
           # Access to services, Pharmacy
           # Diversity
           # Exception based reporting
           # Show zero returning teams more easily (maybe outside the feedback tracker)
           
           selectInput("reportTime", "Time frame", 
                       choices = c("Quarterly" = "quarterly", 
                                   "Yearly (not implemented)" = "yearly", 
                                   "Custom" = "custom")),
           
           selectInput("serviceArea", "Service area", 
                       choices = c("Division", "Directorate", "Team",
                                   "Current selection", "Custom (not yet implemented)")),
           
           uiOutput("reportCustomAreaSelector"),
           
           downloadButton("downloadDoc", "Download report")
    )
  )
})

# main value box output interface defined here----

output$summaryOutputs = renderUI({
  
  tagList(
    htmlOutput("summary_text"),
    
    fluidRow(
      
      valueBoxOutput("sqBox", width = 3), # service quality
      
      valueBoxOutput("fftBox", width = 3), # fft
      
      valueBoxOutput("numberResponsesBox", width = 3), # number respones
      
      valueBoxOutput("zeroRespondingTeams", width = 3) # breakdown of teams responding (zero responding teams, <3 responding teams)
      
    ),
    
    fluidRow(
      
      valueBoxOutput("impCritOneBox", width = 3), # minmally critical
      
      valueBoxOutput("impCritTwoBox", width = 3), # mildy critical
      
      valueBoxOutput("impCritThreeBox", width = 3), # highly critical
      
      valueBoxOutput("changeInCriticality", width = 3) # trend in criticality
    ),
    
    fluidRow(
      
      valueBoxOutput("topCompliment1", width = 3),
      
      valueBoxOutput("topCompliment2", width = 3),
      
      valueBoxOutput("topCompliment3", width = 3),
      
      valueBox("+7%", "Food- general", width = 3, icon = icon("smile"))
    ),
    
    fluidRow(
      
      valueBoxOutput("topCriticism1", width = 3),
      
      valueBoxOutput("topCriticism2", width = 3),
      
      valueBoxOutput("topCriticism3", width = 3),
      
      valueBox("-11%", "Involvement- general", width = 3, icon = icon("frown"))
    ),
    
    fluidRow(
      
      valueBox("96%", "Listening", width = 3, color = "green"), # best score
      
      valueBox("84%", "Would you recommend?", width = 3, color = "red"), # worst score
      
      valueBox("+7%", "Communication", width = 3, color = "green"),
      
      valueBox("-11%", "Service quality", width = 3, color = "red")
    )
  )
})

# this is the reactive interface referred to in the first bit of the code

output$reportCustomAreaSelector <- renderUI({
  
  if(input$serviceArea == "Trust"){
    
    return()
  } else if(input$serviceArea == "Division"){
    
    selectInput("report_division", "Division", 
                list("Local Partnerships- Mental Healthcare" = 0,
                     "Local Partnerships- Community Healthcare" = 2, "Forensic" = 1),
                multiple = TRUE)
    
  } else if(input$serviceArea == "Directorate"){
    
    finalTable = dirTable %>%
      filter(!DirC %in% c(0, 40))
    
    # finally pull the directorates and names
    
    directorates = finalTable %>%
      pull(DirC)
    
    names(directorates) = finalTable %>%
      pull(DirT)
    
    selectInput("report_directorate", "Choose directorate(s)",
                directorates, multiple = TRUE)
    
  } else if(input$serviceArea == "Team"){
    
    teams <- trustData %>% 
      filter(Date > input$dateRange[1], Date < input$dateRange[2]) %>% 
      distinct(TeamC) %>% 
      inner_join(
        counts %>% 
          group_by(TeamC) %>%
          slice(which.max(as.Date(date_from)))
      )
    
    if(nrow(teams) < 1) return()
    
    ### removing all missing names and sort
    
    teams = teams %>% 
      filter(!is.na(TeamN)) %>% 
      arrange(TeamN)
    
    team_numbers <- teams %>% pull(TeamC)
    
    names(team_numbers) <- teams %>% pull(TeamN)
    
    tagList(p("Please note that there are many teams listed within 
              this control. You can type to search or you may prefer to use 
              the custom selector"),
            selectInput("report_team", "Choose team(s)",
                        team_numbers, multiple = TRUE, selected = "All")
    )
  } else if(input$serviceArea == "Custom"){
    
    p("This feature is not yet implemented")
  }
})

# report page outputs defined here----

output$downloadDoc <- downloadHandler(
  filename = "CustomReport.docx",
  content = function(file){
    
    # determine which report we're rendering
    
    report_name = input$reportTime
    
    # Set up parameters to pass to Rmd document
    
    # we're going to need to know WHICH UI has been rendered
    # and pick values from there- it could be 
    # report_division, report_directorate, report_team
    # let's just pretend it's report_division for now
    
    params <- list(division = input$report_division,
                   carerSU = input$carerSU)
    
    # params <- list(division = "Hello")
    
    # render(paste0("reports/", report_name, ".Rmd"), output_format = "word_document",
    #        quiet = TRUE, envir = environment())
    
    render(paste0("reports/", report_name, ".Rmd"), output_format = "word_document",
           quiet = TRUE, params = params,
           envir = new.env(parent = globalenv())
    )
    
    # copy docx to 'file'
    file.copy(paste0("reports/", report_name, ".docx"), file, overwrite = TRUE)
  }
)

# reactive function to produce data summary to pass to value boxes and to report

dataSummary <- reactive({
  
  suceData = passData()[["currentData"]]
  
  if(is.null(suceData)){
    
    return(NULL)
    
  } else {
    
    # FFT score
    
    promoterScores = suceData[, "Promoter2"]
    
    if(length(promoterScores[!is.na(promoterScores)]) > 0) {
      
      FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                    sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
      
    }
    
    # Quality score
    
    SQ = round(mean(suceData[, "Service"], na.rm = TRUE) * 20, 0)
    
    # Number of responses
    
    NR = nrow(suceData)
    
    NSQ = length(suceData$Service[!is.na(suceData$Service)])
    
    NFFT = length(suceData$Promoter2[!is.na(suceData$Promoter2)])
    
    # number of comments
    
    IC = length(suceData[, "Improve"][!is.na(suceData[, "Improve"])])
    BC = length(suceData[, "Best"][!is.na(suceData[, "Best"])])
    
    # were you aware of how to raise a concern yes/ no/ maybe
    
    complaint_numbers <- map_int(c("D", "N", "Y"), function(x){
      
      suceData %>% 
        filter(Complaint == x) %>% 
        nrow()
    })
    
    # criticality
    
    improve_numbers <- map_int(c(1, 2, 3), function(x){
      
      suceData %>% 
        filter(ImpCrit == x) %>% 
        nrow()
    })
    
    # criticality
    
    best_numbers <- map_int(c(1, 2, 3), function(x){
      
      suceData %>% 
        filter(BestCrit == x) %>% 
        nrow()
    })
    
    # name of the area
    
    # if(is.null(input$selTeam)){
    #   
    theArea = "the selected area"
    #   
    # } else {
    #   
    #   ### look up team names
    #   
    #   teams <- as.tibble(input$selTeam) %>%  
    #     inner_join(
    #       counts %>% 
    #         group_by(TeamC) %>%
    #         slice(which.max(as.Date(date_from)))
    #     )
    #   
    #   team_names <- teams %>% pull(TeamN)
    #   
    # }
    
    return(
      list("theArea" = theArea, "NR" = NR, "IC" = IC, "BC" = BC, "NFFT" = NFFT,
           "FFT" = FFT, "NSQ" = NSQ, "SQ" = SQ, "complaint_numbers" = complaint_numbers,
           "improve_numbers" = improve_numbers, "best_numbers" = best_numbers)
    )
  }
})

# value boxes rendered here----

# 1st row

output$sqBox <- renderValueBox({
  
  valueBox(value = paste0(dataSummary()[["SQ"]], "%"), 
           subtitle = HTML(paste0("Service quality<br>(", dataSummary()[["NSQ"]], " responses)")),
           icon = icon("thumbs-up")
  )
})

output$fftBox <- renderValueBox({
  
  valueBox(value = paste0(dataSummary()[["FFT"]], "%"), 
           subtitle = HTML(paste0("Would you recommend?<br>(", dataSummary()[["NFFT"]], " responses)")),
           icon = icon("smile")
  )
})

output$numberResponsesBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["NR"]], 
           subtitle = HTML("Number of<br>responses"),
           icon = icon("book-open")
  )
})

output$zeroRespondingTeams <- renderValueBox({
  
  # need to filter counts according to division/ directorate/ team
  
  all_teams = counts %>% # if the whole Trust is selected do this
    filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
    pull(TeamC) %>% 
    unique()
  
  if(!is.null(input$Division)){ 
    
    all_teams = counts %>% 
      filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
      filter(Division %in% input$Division) %>% 
      pull(TeamC) %>% 
      unique()
  } 
  
  if(!is.null(input$selDirect)){ # otherwise look at the directorate code
    
    all_teams = counts %>% 
      filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
      filter(Directorate %in% input$selDirect) %>% 
      pull(TeamC) %>% 
      unique()
  }
  
  if(!is.null(input$selTeam)){
    
    all_teams = counts %>% 
      filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
      filter(TeamC %in% input$TeamC) %>% 
      pull(TeamC) %>% 
      unique()
  }
  
  df <- passData()[["currentData"]] %>% 
    mutate(quarter_date = floor_date(Date, "quarter")) %>% 
    mutate(TeamC = factor(TeamC, levels = all_teams)) %>% 
    group_by(TeamC, quarter_date) %>% 
    summarise(n = n()) %>%
    ungroup() %>% 
    complete(TeamC, quarter_date) %>%
    arrange(TeamC, quarter_date)
  
  zero_teams <- df %>% filter(!is.na(TeamC)) %>% 
    fill(TeamC) %>% 
    spread(quarter_date, n, fill = 0) %>%
    mutate(row_total = rowSums(select(., -1))) %>% 
    filter(row_total == 0) %>% 
    nrow()
  
  valueBox(value = zero_teams,
           subtitle = HTML("Zero responding<br>teams")
  )
})

# second row

output$impCritOneBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["improve_numbers"]][1], 
           subtitle = HTML("Minimally critical<br>comments"),
           color = "green"
  )
})

output$impCritTwoBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["improve_numbers"]][2], 
           subtitle = HTML("Moderately critical<br>comments"),
           color = "orange"
  )
})

output$impCritThreeBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["improve_numbers"]][3], 
           subtitle = HTML("Highly critical<br>comments"),
           color = "red"
  )
})

output$changeInCriticality <- renderValueBox({
  
  req(passData()[["comparisonData"]])
  
  # take the current period and compare with three months previously
  
  current_criticality <- passData()[["currentData"]] %>% 
    filter(ImpCrit %in% 1:3) %>% 
    pull(ImpCrit) %>% 
    mean()
  
  previous_criticality <- passData()[["comparisonData"]] %>% 
    filter(ImpCrit %in% 1:3) %>% 
    pull(ImpCrit) %>% 
    mean()
  
  change <- round((current_criticality - previous_criticality) / current_criticality * 100, 1)
  
  req(change)
  
  valueBox(value = paste0(change, "%"), 
           subtitle = HTML("Change in<br>criticality"),
           color = ifelse(change >= 0, "green", "red")
  )
})

# third row

# write a function to return the nth element of improve/ best thing table

returnTopComments <- function(nth_row, type){
  
  if(type == "Improve"){
    
    check1 <- passData()[["currentData"]] %>% 
      filter(!is.na(Imp1)) %>% 
      left_join(categoriesTable, by = c("Imp1" = "Number")) %>% 
      select(Category, Super)
    
    check2 <- passData()[["currentData"]] %>% 
      filter(!is.na(Imp2)) %>% 
      left_join(categoriesTable, by = c("Imp2" = "Number")) %>% 
      select(Category, Super)
  }
  
  if(type == "Best"){
    
    check1 <- passData()[["currentData"]] %>% 
      filter(!is.na(Best1)) %>% 
      left_join(categoriesTable, by = c("Best1" = "Number")) %>% 
      select(Category, Super)
    
    check2 <- passData()[["currentData"]] %>% 
      filter(!is.na(Best2)) %>% 
      left_join(categoriesTable, by = c("Best2" = "Number")) %>% 
      select(Category, Super)
  }
  
  check_final <- rbind(check1, check2)
  
  count_table <- check_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup()
  
  count_sum <- sum(count_table$n)
  
  count_table %>% 
    mutate(percent = round(n / count_sum * 100, 1)) %>% 
    arrange(-percent) %>% 
    slice(nth_row)
}

output$topCompliment1 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(1, "Best")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("smile"))
})

output$topCompliment2 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(2, "Best")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("smile"))
})

output$topCompliment3 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(3, "Best")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("smile"))
})

# change in compliments

# fourth row

output$topCriticism1 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(1, "Improve")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("frown"))
})

output$topCriticism2 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(2, "Improve")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("frown"))
})

output$topCriticism3 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(3, "Improve")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("frown"))
})

# valueBoxOutput("impCritPercentageBox", width = 3), # number comments
# 
# valueBoxOutput("numberImproveBox", width = 3), # number comments
#
# valueBoxOutput("numberBestBox", width = 3), # number comments
#
# valueBoxOutput("criticalBox"), # criticality summary
# 
# valueBoxOutput("teamsRespondingBox"), # breakdown of teams responding (zero responding teams, <3 responding teams)
# 
# valueBoxOutput("bestScoreBox"), # best score
# 
# valueBoxOutput("worstScoreBox") # worst score

# this is the function that draws the parameterized rmarkdown

observe({
  
  searchString <- parseQueryString(session$clientData$url_search)
  
  cat(searchString[["division"]])
  
  # # update inputs according to query string
  # if(length(searchString) > 0){ # if the searchString exists
  #   # deal with first query which indicates the audience
  #   if(searchString[[1]] == "nhs"){ # for NHS users do the following
  #     updateCheckboxGroupInput(session, "domainShow",
  #                              choices = list("NHS users" = "nhs.uk",
  #                                             "Other" = "Other"), selected = c("nhs.uk"))
  #   }
  #   # do they want a smooth?
  #   if(searchString[[2]] == "yes"){
  #     updateTabsetPanel(session, "theTabs", selected = "trend")
  #     updateCheckboxInput(session, inputId = "smooth",
  #                         value = TRUE)
  #   }
  # }
  # 
  # input$Division
  # 
  # input$selDirect
  # 
  # input$selTeam
  
})


