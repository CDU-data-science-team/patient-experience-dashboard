
output$allImproveComments = renderText({
  
  # this first bit is for if they are ordering by category
  
  if(input$sortCategoryCriticality == "Category"){
    
    # going to left join the improve codes to the category table to produce
    # a fully labelled dataframe with all comments, sub and super categories
    
    df = passData()[["currentData"]] %>% 
      filter(!is.na(Imp1), !is.na(Improve)) %>% 
      left_join(categoriesTable, by = c("Imp1" = "Number")) %>%  
      select(key : Time, Improve: CommentCoderBest, 
             Location, Division, Directorate, Division2 : type) %>% 
      filter(!is.na(Super)) %>% 
      group_by(Super) %>% 
      mutate(count = n()) %>% 
      arrange(-count, Location)
    
    finalText = map(unique(df$Super), function(x) {
      
      commentsFrame = df %>% filter(Super == x) %>% select(Improve, Location)
      
      paste0("<h3>", x, "</h3>", 
             paste0("<p>", commentsFrame$Improve, " (", 
                    commentsFrame$Location, ")</p>", collapse = "")
      )
    })
    
    return(unlist(finalText))
  }
  
  # now let's do a whole other thing for if they want to sort by criticality
  
  if(input$sortCategoryCriticality == "Criticality"){
    
    # going to left join the improve codes to the category table to produce
    # a fully labelled dataframe with all comments, sub and super categories
    
    df = passData()[["currentData"]] %>% 
      filter(!is.na(ImpCrit), !is.na(Improve)) %>% 
      select(key : Time, Improve: CommentCoderBest, 
             Location, Division, Directorate, Division2) %>% 
      arrange(Location)
    
    # make a vector to store the names in
    
    nameVector = c("Mildly critical", "Somewhat critical", "Very critical")
    
    finalText = map(3 : 1, function(x) {
      
      commentsFrame = df %>% filter(ImpCrit == x) %>% select(Improve, Location)
      
      if(nrow(commentsFrame) > 0){
        
        paste0("<h3>", nameVector[x], "</h3>", 
               paste0("<p>", commentsFrame$Improve, " (", 
                      commentsFrame$Location, ")</p>", collapse = "")
        )
      }
    })
    
    return(unlist(finalText))
  }
})

# this code is, very shamefully, just copied from above and changed to 
# best thing. It's just easier than making it into a function

output$allBestComments = renderText({
  
  # this first bit is for if they are ordering by category
  
  if(input$sortCategoryCriticality == "Category"){
    
    # going to left join the best codes to the category table to produce
    # a fully labelled dataframe with all comments, sub and super categories
    
    df = passData()[["currentData"]] %>% 
      filter(!is.na(Best1), !is.na(Best)) %>% 
      left_join(categoriesTable, by = c("Best1" = "Number")) %>%  
      select(key : Time, Improve: CommentCoderBest, 
             Location, Division, Directorate, Division2 : type) %>% 
      filter(!is.na(Super)) %>% 
      group_by(Super) %>% 
      mutate(count = n()) %>% 
      arrange(-count, Location)
    
    finalText = map(unique(df$Super), function(x) {
      
      commentsFrame = df %>% filter(Super == x) %>% select(Best, Location)
      
      paste0("<h3>", x, "</h3>", 
             paste0("<p>", commentsFrame$Best, " (", 
                    commentsFrame$Location, ")</p>", collapse = "")
      )
    })
    
    return(unlist(finalText))
  }
  
  # now let's do a whole other thing for if they want to sort by criticality
  
  if(input$sortCategoryCriticality == "Criticality"){
    
    # going to left join the best codes to the category table to produce
    # a fully labelled dataframe with all comments, sub and super categories
    
    df = passData()[["currentData"]] %>% 
      filter(!is.na(BestCrit), !is.na(Best)) %>% 
      select(key : Time, Improve: CommentCoderBest, 
             Location, Division, Directorate, Division2) %>% 
      arrange(Location)
    
    # make a vector to store the names in
    
    nameVector = c("Mildly complimentary", "Somewhat complimentary", "Very complimentary")
    
    finalText = map(3 : 1, function(x) {
      
      commentsFrame = df %>% filter(BestCrit == x) %>% select(Best, Location)
      
      if(nrow(commentsFrame) > 0){
        
        paste0("<h3>", nameVector[x], "</h3>", 
               paste0("<p>", commentsFrame$Best, " (", 
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
    
    render("reports/AllComments.Rmd", output_format = "word_document",
           quiet = TRUE, envir = environment())
    
    # copy docx to 'file'
    file.copy("reports/AllComments.docx", file, overwrite = TRUE)
    
  }
)
