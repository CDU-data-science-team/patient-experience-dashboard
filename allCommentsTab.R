
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
    
    params <- generate_rmd_parameters()
    
    render("reports/AllComments.Rmd", output_format = "word_document",
           quiet = TRUE, params = params,
           envir = new.env(parent = globalenv()))
    
    # copy docx to 'file'
    file.copy("reports/AllComments.docx", file, overwrite = TRUE)
    
  }
)
