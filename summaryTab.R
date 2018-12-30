
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
           
           selectInput("reportTime", "Time frame", choices = c("Quarterly", "Yearly", "Custom")),
           
           selectInput("serviceArea", "Service area", choices = c("Trust", "Division", "Directorate", "Team", "Custom")),
           
           uiOutput("reportCustomAreaSelector"),
           
           downloadButton("downloadDoc", "Download report")
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
                                        
                                        report_name <- switch(input$serviceArea, 
                                                              "Trust" = "trust_report", 
                                                              "Division" = "division_report", 
                                                              "Directorate" = "directorate_report",
                                                              "Team" = "team_report", 
                                                              "Custom" = "custom_report")
                                        
                                        render(paste0("reports/", report_name, ".Rmd"), output_format = "word_document",
                                               quiet = TRUE, envir = environment())
                                        
                                        # copy docx to 'file'
                                        file.copy(paste0(report_name, ".docx"), file, overwrite = TRUE)
                                        
                                      }
)

# text summary

output$summary_text = renderText({
  
  suceData = passData()[["suce"]]
  
  if(is.null(suceData)){
    
    myString = "Within the selected time and area there were no responses"
    
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
    
    # name of the area
    
    if(is.null(input$Division)){
      
      theArea = "the selected area"
      
    } else if(is.null(input$selDirect)){
      
      theArea = "the selected area"
      
    } else if(is.null(input$selTeam)){
      
      theArea = "the selected area"
      
    } else if(input$selTeam == 99){
      
      theArea = "the selected area"
      
    } else {
      
      ### look up team names
      
      teams = input$selTeam
      
      nameteams = lapply(teams, function(x) tail(trustData$TeamN[which(trustData$TeamC == x)], 1))
      
      theArea = paste(nameteams, collapse = ", ")
    }
    
    myString = paste0("<p>Within ", theArea, " in the selected time there were ", NR,
                      " responses.</p><br>",
                      "<p>There were ", IC, " 'What could we do better' responses and ", BC,
                      " 'What did we do well' responses</p><br>",
                      ifelse(NFFT > 9,
                             paste0("<p>The Friends and Family Test Score is the proportion of patients
        who are extremely likely or likely to recommend a service. In the selected period of time it was ",
                                    FFT, "% (based on ", NFFT, " responses.)", "</p><br>"), ""),
                      ifelse(NSQ > 9,
                             paste0("<p>Service quality rating was ", SQ, "% (based on ", NSQ,
                                    " responses.)</p>"), ""),
                      ifelse(sum(complaint_numbers) > 3,
                             paste0("<p>", complaint_numbers[3], " individuals reported that they knew how to make a complaint, ",
                                    complaint_numbers[2], " reported that they did not know how to make a complaint, and ", 
                                    complaint_numbers[1], " reported that they were unsure if they knew.</p>"), "")
    )
    
    HTML(myString)
  }
})


output$summaryOutputs = renderUI({
  
  htmlOutput("summary_text")
})