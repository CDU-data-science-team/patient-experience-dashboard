
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
           
           selectInput("reportTime", "Time frame", choices = c("Quarterly" = "quarterly", "Yearly" = "yearly", "Custom" = "custom")),
           
           selectInput("serviceArea", "Service area", choices = c("Trust", "Division", "Directorate", "Team", "Custom")),
           
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
      
      valueBox("???", "Something else here", icon = icon("question"), width = 3)
      
    ),
    
    fluidRow(
      
      valueBoxOutput("impCritOneBox", width = 3), # number comments
      
      valueBoxOutput("impCritTwoBox", width = 3), # number comments
      
      valueBoxOutput("impCritThreeBox", width = 3), # number comments
      
      valueBoxOutput("impCritPercentageBox", width = 3), # number comments
      
      valueBoxOutput("numberBestBox", width = 3), # number comments
      
      valueBoxOutput("criticalBox", width = 3), # criticality summary
      
      valueBoxOutput("teamsRespondingBox", width = 3), # breakdown of teams responding (zero responding teams, <3 responding teams)
      
      valueBoxOutput("bestScoreBox", width = 3), # best score
      
      valueBoxOutput("worstScoreBox", width = 3) # worst score
      
      # trend in scores
      
      # trend in criticality
      
      # trend in categories
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

output$downloadDoc <- downloadHandler(filename = "CustomReport.docx",
                                      content = function(file){
                                        
                                        # determine which report we're rendering
                                        
                                        report_name = input$reportTime
                                        
                                        render(paste0("reports/", report_name, ".Rmd"), output_format = "word_document",
                                               quiet = TRUE, envir = environment())
                                        
                                        # copy docx to 'file'
                                        file.copy(paste0("reports/", report_name, ".docx"), file, overwrite = TRUE)
                                      }
)

# reactive function to produce data summary to pass to value boxes and to report

dataSummary <- reactive({
  
  suceData = passData()[["suce"]]
  
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
    
    if(is.null(input$selTeam)){
      
      theArea = "the selected area"
      
    } else {
      
      ### look up team names
      
      teams <- as.tibble(input$selTeam) %>%  
        inner_join(
          counts %>% 
            group_by(TeamC) %>%
            slice(which.max(as.Date(date_from)))
        )
      
      team_names <- teams %>% pull(TeamN)
      
      # teams = input$selTeam
      # 
      # nameteams = lapply(teams, function(x) tail(trustData$TeamN[which(trustData$TeamC == x)], 1))
      
      theArea = paste(team_names, collapse = ", ")
    }
    
    return(
      list("theArea" = theArea, "NR" = NR, "IC" = IC, "BC" = BC, "NFFT" = NFFT,
           "FFT" = FFT, "NSQ" = NSQ, "SQ" = SQ, "complaint_numbers" = complaint_numbers,
           "improve_numbers" = improve_numbers, "best_numbers" = best_numbers)
    )
  }
})

# text summary----

output$summary_text = renderText({
  
  if(is.null(dataSummary())){
    
    myString = "Within the selected time and area there were no responses"
  } else {
    
    myString = paste0("<p>Within ", dataSummary()[["theArea"]], " in the selected time there were ", dataSummary()[["NR"]],
                      " responses.</p><br>",
                      "<p>There were ", dataSummary()[["IC"]], " 'What could we do better' responses and ", dataSummary()[["BC"]],
                      " 'What did we do well' responses</p><br>",
                      ifelse(dataSummary()[["NFFT"]] > 9,
                             paste0("<p>The Friends and Family Test Score is the proportion of patients
        who are extremely likely or likely to recommend a service. In the selected period of time it was ",
                                    dataSummary()[["FFT"]], "% (based on ", dataSummary()[["NFFT"]], " responses.)", "</p><br>"), ""),
                      ifelse(dataSummary()[["NSQ"]] > 9,
                             paste0("<p>Service quality rating was ", dataSummary()[["SQ"]], "% (based on ", dataSummary()[["NSQ"]],
                                    " responses.)</p>"), ""),
                      ifelse(sum(dataSummary()[["complaint_numbers"]]) > 3,
                             paste0("<p>", dataSummary()[["complaint_numbers"]][3], " individuals reported that they knew how to make a complaint, ",
                                    dataSummary()[["complaint_numbers"]][2], " reported that they did not know how to make a complaint, and ", 
                                    dataSummary()[["complaint_numbers"]][1], " reported that they were unsure if they knew.</p>"), "")
    )
  }
  
  HTML(myString)
  
})

# value boxes rendered here----

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

output$impCritPercentageBox <- renderValueBox({
  
  crit_numbers <- dataSummary()[["improve_numbers"]]
  
  percentages = round(
    c(
      crit_numbers[1] / sum(crit_numbers) * 100,
      crit_numbers[2] / sum(crit_numbers) * 100,
      crit_numbers[3] / sum(crit_numbers) * 100
    ), 0)
  
  valueBox(value = paste0(percentages[1], "/", percentages[2], "/", percentages[3], "%"),
  # valueBox(value = tags$p(paste0(percentages[1], "%/ ", percentages[2], "%/ ", percentages[3], "%"), style = "font-size: 0%;"),
           subtitle = HTML("Percentages<br>min/ mod/ high")
  )
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