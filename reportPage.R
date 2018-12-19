
# report page control defined here----

output$reportPage <- renderUI({
  
  tagList(
    h2("Report builder"),
    
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
    
    uiOutput("reportCustomAreaSelector")
  )
  
})

# this is the reactive interface referred to in the first bit of the code

output$reportCustomAreaSelector <- renderUI({
  
  if(input$serviceArea == "Trust"){
    
    return()
  }
  
  if(input$serviceArea == "Division"){
    
    selectInput("report_division", "Division", 
                list("Local Partnerships- Mental Healthcare"= 0,
                     "Local Partnerships- Community Healthcare" = 2, "Forensic" = 1),
                multiple = TRUE)
  }
  
  if(input$serviceArea == "Directorate"){
    
  finalTable = dirTable %>%
    filter(!DirC %in% c(0, 40))
  
  # finally pull the directorates and names
  
  directorates = finalTable %>%
    pull(DirC)
  
  names(directorates) = finalTable %>%
    pull(DirT)
  
  selectInput("report_directorate", "Choose directorate(s)",
              directorates, multiple = FALSE)
  }
  
  if(input$serviceArea == "Team"){
    
    return()
  }
  
  if(input$serviceArea == "Custom"){
    
    p("This feature is not yet implemented")
  }
})

# report page outputs defined here----