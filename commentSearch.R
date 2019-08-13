
output$commentSearchSelector <- renderUI({
  
  # I may be able to have more than one on the screen at once
  # let's look at that some time
  
  # 1. top 6 themes- preprocess
  
  comment_table <- returnTopComments(passData()[["currentData"]], 1 : 6, "Both")[["return_table"]]
  
  list_names <- comment_table$Number
  
  names(list_names) = paste0(comment_table$Super, ":<br>", comment_table$Category)
  
  # 2. Criticality- preprocess
  
  criticality_choices <- list("Highly Complimentary" = 3, "Fairly Complimentary" = 2, 
                              "Mildly Complimentary" = 1, "Mildly critical" = -1, 
                              "Fairly Critical" = -2, "Highly Critical" = -3)
  
  # 4. Pre selected search- preprocess
  
  tagList(
    
    box(width = 6, height = 130, checkboxGroupInput("filterCommentsBy", "Filter comments by...", 
                                                    choices = c("Text search", "Criticality", "Themes"),
                                                    selected = "Text search")),
    box(width = 6, height = 130, actionButton("showTimeline", "Show timeline")),
    
    box(width = 12,
        
        # 1. Search
        
        conditionalPanel("input.filterCommentsBy.includes('Text search')",
                         textInput("searchTextInclude", "Enter search terms separated by commas"),
                         textInput("textSearchExclude", HTML("Enter terms to <em>exclude</em> separated by commas"))),
        
        # 2. criticality levels
        
        conditionalPanel("input.filterCommentsBy.includes('Criticality')",
                         checkboxGroupButtons("criticalityLevels", "Criticality levels (all selected by default)", 
                                              choices = criticality_choices,
                                              selected = criticality_choices, status = "default", size = "lg",
                                              direction = "vertical", justified = FALSE, individual = FALSE)),
        
        # 3. top 6 themes
        
        conditionalPanel("input.filterCommentsBy.includes('Themes')",
                         checkboxGroupButtons("topSixThemes", "Top six themes (all selected by default)", 
                                              choices = list_names,
                                              selected = list_names, status = "default", size = "lg",
                                              direction = "vertical", justified = FALSE, individual = FALSE))
    )
  )
})

output$commentSearchOutput <- renderUI({
  
  tagList(
    fluidRow(
      column(4, uiOutput("commentSearchSelector")),
      column(8, box(width = 12,
                    fluidRow(
                      column(6, h2("What could we do better?"), htmlOutput("showImprove")),
                      column(6, h2("What did we do well?"), htmlOutput("showBest"))
                    )
      )
      )
    )
  )
})

observeEvent(input$showTimeline, {
  
  showModal(
    modalDialog(
      textOutput("beeswarmText"), 
      plotOutput("beeswarmComments", click = "beeswarm_click"),
      size = "l")
  )
})

output$showImprove <- renderText({
  
  improve_data <- passData()[["currentData"]]
  
  if("Text search" %in% input$filterCommentsBy){
    
    if(isTruthy(input$searchTextInclude)){ # or overwrite if search string exists
      
      improve_data <- improve_data %>%
        filter(grepl(paste(
          trimws(unlist(strsplit(input$searchTextInclude, ","))), 
          collapse = "|"), Improve))
    }
    
    if(isTruthy(input$textSearchExclude)){
      
      improve_data <- improve_data %>%
        filter(!grepl(paste(
          trimws(unlist(strsplit(input$textSearchExclude, ","))), 
          collapse = "|"), Improve))
    }
  }
  
  improve_data <- improve_data %>%
    mutate(ImpCrit = -ImpCrit)
  
  if("Criticality" %in% input$filterCommentsBy){
    
    improve_data <- improve_data %>%
      filter(ImpCrit %in% input$criticalityLevels)
  }
  
  if("Themes" %in% input$filterCommentsBy){
    
    improve_data <- improve_data %>%
      filter(Imp1 %in% input$topSixThemes |
               Imp2 %in% input$topSixThemes)
  }
  
  improve_data <- improve_data %>% 
    filter(!is.na(Improve))
  
  # req(nrow(improve_data) > 0)
  
  paste0("<p>", improve_data$Improve, " (", 
         improve_data$Location, ")</p>", collapse = "")
})

output$showBest <- renderText({
  
  improve_data <- passData()[["currentData"]]
  
  if("Text search" %in% input$filterCommentsBy){
    
    if(isTruthy(input$searchTextInclude)){ # or overwrite if search string exists
      
      improve_data <- improve_data %>%
        filter(grepl(paste(
          trimws(unlist(strsplit(input$searchTextInclude, ","))), 
          collapse = "|"), Best))
    }
    
    if(isTruthy(input$textSearchExclude)){
      
      improve_data <- improve_data %>%
        filter(!grepl(paste(
          trimws(unlist(strsplit(input$textSearchExclude, ","))), 
          collapse = "|"), Best))
    }
  }
  
  if("Criticality" %in% input$filterCommentsBy){
    
    improve_data <- improve_data %>%
      filter(BestCrit %in% input$criticalityLevels)
  }
  
  if("Themes" %in% input$filterCommentsBy){
    
    improve_data <- improve_data %>%
      filter(Best1 %in% input$topSixThemes |
               Best2 %in% input$topSixThemes)
  }
  
  improve_data <- improve_data %>% 
    filter(!is.na(Best))
  
  paste0("<p>", improve_data$Best, " (", 
         improve_data$Location, ")</p>", collapse = "")
})

# this is being cached so the plot click can access it

beeswarmGraph <- reactive({
  
  combined <- rbind(
    
    passData()[["currentData"]] %>% 
      select(Date, Imp1, ImpCrit, Improve, Location) %>% 
      gather(key, value, -Date, -ImpCrit, -Improve, -Location) %>% 
      rename(Crit = ImpCrit, Comment = Improve) %>% 
      mutate(Crit = -Crit),
    
    passData()[["currentData"]] %>% 
      select(Date, Best1, BestCrit, Best, Location) %>% 
      gather(key, value, -Date, -BestCrit, -Best, -Location) %>% 
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
  
  beeswarm_df <- nearPoints(beeswarmGraph(), input$beeswarm_click, threshold = 100, maxpoints = 1)
  
  req(nrow(beeswarm_df) > 0)
  
  with(beeswarm_df, paste0(Comment, " (", Location, ")"))
  
})



