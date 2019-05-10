
output$commentSearchSelector <- renderUI({
  
  # I may be able to have more than one on the screen at once
  # let's look at that some time
  
  # 1. top 6 themes- preprocess
  
  comment_table <- returnTopComments(passData()[["currentData"]], 1 : 6, "Both")[["return_table"]]
  
  list_names <- comment_table$Number
  
  names(list_names) = paste0(comment_table$Super, ": ", comment_table$Category)
  
  # 2. Criticality- preprocess
  
  criticality_choices <- list("Highly Complimentary" = 3, "Fairly Complimentary" = 2, 
                              "Mildly Complimentary" = 1, "Mildly critical" = -1, 
                              "Fairly Critical" = -2, "Highly Critical" = -3)
  
  # 4. Pre selected search- preprocess
  
  tagList(
    
    box(width = 12,
        # 1. top 6 themes
        checkboxGroupButtons("topSixThemes", "Top six themes", 
                             choices = list_names,
                             selected = list_names, status = "default", size = "lg",
                             direction = "vertical", justified = FALSE, individual = FALSE),
        
        # 2. criticality levels
        
        checkboxGroupButtons("criticalityLevels", "Criticality levels", 
                             choices = criticality_choices,
                             selected = criticality_choices, status = "default", size = "lg",
                             direction = "vertical", justified = FALSE, individual = FALSE),
        
        # 3. Search
        
        textInput("searchTextInclude", "Enter search terms separated by commas"),
        
        textInput("textSearchExclude", HTML("Enter terms to <em>exclude</em> separated by commas"))
    )
  )
})

output$commentSearchOutput <- renderUI({
  
  tagList(
    fluidRow(
      column(4, uiOutput("commentSearchSelector")),
      column(8, 
             box(width = 12,
                 textOutput("beeswarmText"), plotOutput("beeswarmComments", click = "beeswarm_click")
             ),
             box(width = 12,
                 fluidRow(
                   column(6, h2("What could we do better?"), htmlOutput("showImprove")),
                   column(6, h2("What did we do well?"), htmlOutput("showBest"))
                 )
             )
      )
    )
  )
})

fixSearchString <- reactive({
  
  trimws(unlist(strsplit(input$searchTextInclude, ",")))
})

fixSearchExclude <- reactive({
  
  trimws(unlist(strsplit(input$textSearchExclude, ",")))
})

output$showImprove <- renderText({
  
  if(length(fixSearchString()) > 0){
    
    improve_data <- passData()[["currentData"]] %>% 
      filter(grepl(paste(fixSearchString(), collapse = "|"), Improve))
  } else {
    
    improve_data <- passData()[["currentData"]]
  }
  
  if(length(fixSearchExclude()) > 0){
    
    improve_data <- improve_data %>% 
      filter(!grepl(paste(fixSearchExclude(), collapse = "|"), Improve))
  }
  
   improve_data %>%
    mutate(ImpCrit = -ImpCrit) %>% 
    filter(Imp1 %in% input$topSixThemes |
             Imp2 %in% input$topSixThemes) %>%
    filter(ImpCrit %in% input$criticalityLevels) %>% 
    filter(!is.na(Improve)) %>%
    pull(Improve) %>%
    paste0("<p>", ., "</p>", collapse = "")
})

output$showBest <- renderText({
  
  if(!is.null(input$searchTextInclude)){
    
    best_data <- passData()[["currentData"]] %>% 
      filter(grepl(paste(fixSearchString(), collapse = "|"), Best))
  } else {
    
    best_data <- passData()[["currentData"]]
  }

  best_data %>%
    filter(Best1 %in% input$topSixThemes |
             Best2 %in% input$topSixThemes) %>%
    filter(BestCrit %in% input$criticalityLevels) %>% 
    filter(!is.na(Best)) %>%
    pull(Best) %>%
    paste0("<p>", ., "</p>", collapse = "")
})

# this is being cached so the plot click can access it

beeswarmGraph <- reactive({
  
  combined <- rbind(
    
    passData()[["currentData"]] %>% 
      select(Date, Imp1, ImpCrit, Improve) %>% 
      gather(key, value, -Date, -ImpCrit, -Improve) %>% 
      rename(Crit = ImpCrit, Comment = Improve) %>% 
      mutate(Crit = -Crit),
    
    passData()[["currentData"]] %>% 
      select(Date, Best1, BestCrit, Best) %>% 
      gather(key, value, -Date, -BestCrit, -Best) %>% 
      rename(Crit = BestCrit, Comment = Best)
  )
  
  if(!is.null(input$searchTextInclude)){
    
    combined <- combined %>% 
      filter(grepl(input$searchTextInclude, Comment))
  }
  
  combined %>% 
    filter(!is.na(Crit), !is.na(Comment)) %>% 
    filter(value %in% input$topSixThemes) %>% 
    filter(Crit %in% input$criticalityLevels) %>% 
    mutate(Crit = factor(Crit, levels = c(-3, -2, -1, 1, 2, 3))) %>% 
    mutate(y_axis = 0, y_axis = jitter(y_axis))
})

output$beeswarmComments <- renderPlot({
  
  beeswarmGraph() %>% 
    ggplot(aes(x = Date, y = y_axis, colour = Crit)) + 
    geom_point() + 
    scale_colour_brewer(palette = "Spectral") + 
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
})

output$beeswarmText <- renderText({
  
  nearPoints(beeswarmGraph(), input$beeswarm_click, threshold = 100, maxpoints = 1)$Comment
})



