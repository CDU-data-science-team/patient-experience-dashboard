
output$allImproveComments = renderText({
  
  df <- allComments(passData()[["currentData"]], input$sortCategoryCriticality, "Improve")
  
  if(input$sortCategoryCriticality == "Category"){
    
    finalText = map(unique(df$Super), function(x) {
      
      commentsFrame = df %>% 
        filter(Super == x) %>% 
        ungroup() %>% 
        select(Improve, Location) %>% 
        set_names(c("Comment", "Location"))
      
      paste0("<h3>", x, "</h3>", 
             paste0("<p>", commentsFrame$Comment, " (", 
                    commentsFrame$Location, ")</p>", collapse = "")
      )
    })
    
    return(unlist(finalText))
    
  } else {
    nameVector = c("Mildly critical", "Somewhat critical", "Very critical")
    
    finalText = map(3 : 1, function(x) {
      
      commentsFrame = df %>% 
        filter(ImpCrit == x) %>% 
        select(Improve, Location) %>% 
        set_names(c("Comment", "Location"))
      
      if(nrow(commentsFrame) > 0){
        
        paste0("<h3>", nameVector[x], "</h3>",
               paste0("<p>", commentsFrame$Comment, " (",
                      commentsFrame$Location, ")</p>", collapse = "")
        )
      }
    })
    return(unlist(finalText))
  }
})

output$allBestComments = renderText({
  
  df <- allComments(passData()[["currentData"]], input$sortCategoryCriticality, "Best")
  
  if(input$sortCategoryCriticality == "Category"){
    
    finalText <- map(unique(df$Super), function(x) {
      
      commentsFrame <- df %>% 
        filter(Super == x) %>% 
        ungroup() %>% 
        select(Best, Location) %>% 
        set_names(c("Comment", "Location"))
      
      paste0("<h3>", x, "</h3>", 
             paste0("<p>", commentsFrame$Comment, " (", 
                    commentsFrame$Location, ")</p>", collapse = "")
      )
    })
    
    return(unlist(finalText))
    
  } else {
    
    nameVector = c("Mildly complimentary", "Somewhat complimentary", "Very complimentary")
    
    finalText = map(3 : 1, function(x) {
      
      commentsFrame = df %>% 
        filter(BestCrit == x) %>% 
        select(Best, Location) %>% 
        set_names(c("Comment", "Location"))
      
      if(nrow(commentsFrame) > 0){
        
        paste0("<h3>", nameVector[x], "</h3>",
               paste0("<p>", commentsFrame$Comment, " (",
                      commentsFrame$Location, ")</p>", collapse = "")
        )
      }
    })
    return(unlist(finalText))
  }
})

# download all comments button

output$downloadAllComments <- downloadHandler(
  filename = "AllComments.docx",
  content = function(file){
    
    if(nrow(passData()[["currentData"]]) > 10000){
      
      showModal(
        modalDialog(
          title = "Error!",
          HTML("Too many comments, please reduce your selection"),
          easyClose = TRUE
        )
      )
      
      return()
    }
    
    # Set up parameters to pass to Rmd document
    
    # find which areas are selected
    
    report_area <- case_when(
      isTruthy(input$selTeam) ~ "team",
      isTruthy(input$selDirect) ~ "directorate",
      isTruthy(input$Division) ~ "division",
      TRUE ~ "trust"
    )
    
    if(report_area == "trust"){
      
      area_name <- "the whole Trust"
      
      params <- list(division = "NA",
                     carerSU = input$carerSU,
                     area_name = area_name,
                     date_from = input$dateRange[1],
                     date_to = input$dateRange[2],
                     comment_sort = input$sortCategoryCriticality)
    }
    
    if(report_area == "division"){
      
      area_name <- c("Local Partnerships- Mental Healthcare", 
                     "Forensic Services", 
                     "Local Partnerships- Community Healthcare")[as.numeric(input$Division) + 1]
      
      params <- list(division = input$Division,
                     carerSU = input$carerSU,
                     area_name = area_name,
                     date_from = input$dateRange[1],
                     date_to = input$dateRange[2],
                     comment_sort = input$sortCategoryCriticality)
    }
    
    if(report_area == "directorate"){
      
      first_date <- input$dateRange[1]
      
      end_date <- input$dateRange[2]
      
      number_rows = trustData %>%
        filter(Directorate %in% input$selDirect) %>% 
        filter(Date >= first_date, Date <= end_date) %>% 
        nrow()
      
      area_name <- dirTable %>% 
        filter(DirC %in% input$selDirect) %>% 
        pull(DirT) %>% 
        paste(collapse = ", ")
      
      params <- list(directorate = input$selDirect,
                     carerSU = input$carerSU,
                     area_name = area_name,
                     date_from = input$dateRange[1],
                     date_to = input$dateRange[2],
                     comment_sort = input$sortCategoryCriticality)
    }
    
    if(report_area == "team"){
      
      area_name_team <- passData()[["currentData"]] %>% 
        pull(TeamN) %>% 
        unique() %>% 
        paste(collapse = ", ")
      
      params <- list(team = input$selTeam,
                     carerSU = input$carerSU,
                     area_name = area_name_team,
                     date_from = input$dateRange[1],
                     date_to = input$dateRange[2],
                     comment_sort = input$sortCategoryCriticality)
    }
    
    render("reports/AllComments.Rmd", output_format = "word_document",
           quiet = TRUE, params = params,
           envir = new.env(parent = globalenv()))
    
    # copy docx to 'file'
    file.copy("reports/AllComments.docx", file, overwrite = TRUE)
    
  }
)
