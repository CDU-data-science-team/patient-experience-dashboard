
# Generate stacked plot of the requested variable

myStack <- reactive({
  
  req(passData())
  
  theQuestions = input$selQuestions
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(passData()[["suce"]][, input$selQuestions], 1:2,
                               function(x) round(x + .01)))
  
  # count the missing responses
  
  missnum = apply(fixedData, 2, function(x) sum(!is.na(x)))
  
  fixedData[, missnum > 2] %>%
    gather(L1, value) %>% 
    filter(!is.na(value)) %>%
    left_join(select(questionFrame, code, value), by = c("L1" = "code")) %>%
    select(-L1) %>%
    group_by(value.y) %>%
    count(value.x) %>%
    mutate(prop = prop.table(n) * 100) %>%
    filter(!is.na(prop)) %>% 
    ungroup() %>%
    ggplot(aes(x = value.y, y = prop, fill = factor(value.x))) +
    geom_bar(position = "fill", stat = "identity") + ylab("Proportion responding") + 
    scale_fill_manual(values = rainbow(5), "Response", limits = c(1:5), breaks = c(5:1),
                      labels = c("Excellent", "Good", "Fair", "Poor", "Very poor")) +
    scale_y_continuous(labels = percent_format()) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    scale_x_discrete() + coord_flip() + xlab("Question")
})

output$StackPlot <- renderPlot({
  
  missnum = apply(passData()[["suce"]][, input$selQuestions], 2, function(x) sum(!is.na(x)))
  
  validate(
    need(length(names(missnum[missnum > 2])) > 2, "Not enough data")
  )
  
  print(myStack())
  
})

# produce click interaction to bring up table

observeEvent(input$stacked_suce_click, {
  
  showModal(
    modalDialog(
      dataTableOutput("stackedTableSuceModal"),
      textOutput("fftScore"),
      size = "l")
  )
})

output$stackedTableSuceModal <- renderDT({
  
  theQuestions = input$selQuestions
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(passData()[["suce"]][, unlist(theQuestions)], 1:2,
                               function(x) round(x + .01)))
  
  missnum = apply(fixedData, 2, function(x) sum(!is.na(x)))
  
  fixedData[, missnum > 2] %>%
    gather(L1, value) %>% 
    filter(!is.na(value)) %>%
    left_join(select(questionFrame, code, value), by = c("L1" = "code")) %>%
    select(-L1) %>%
    group_by(value.y) %>%
    count(value.x) %>%
    mutate(prop = prop.table(n) * 100) %>%
    select(-n) %>%
    spread(value.x, prop) %>%
    ungroup() %>%
    setNames(c("Question", rev(c("Excellent", "Good", "Fair", "Poor", "Very poor")))) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(Score = (Excellent * 5 + Good * 4 + Fair * 3 + Poor * 2 + `Very poor` * 1) / 5) %>%
    datatable() %>%
    formatRound(TRUE, 1)
})

# generate scores to put underneath the stacked plot

output$fftScore = renderText({
  
  promoterScores = passData()[["suce"]][, "Promoter2"]
  
  req(length(promoterScores[!is.na(promoterScores)]) > 0)
  
  FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
  
  theString = paste0("FFT score calculated as a percentage of 
                     those who would/ would not recommed ", FFT, "%")
  
  return(HTML(theString))
})

# Generate line plot for trend

myTrend = reactive({
  
  if(min(passData()$Time, na.rm = TRUE) == Inf){
    
    df <- data.frame()
    
    p = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
    
    p = p + annotate("text", label = "Not enough data in the specified time and",
                     x = 2, y = 2, size = 7, colour = "red") +
      annotate("text", label = "location, please broaden your search terms",
               x = 2, y = 1, size = 7, colour = "red")
    
    p = p + options(theme = new_theme_empty)
    
    return(p)
  }
  
  mytime = min(passData()$Time, na.rm=TRUE) : max(passData()$Time, na.rm=TRUE)
  
  # different questions needed for carers' survey when Advanced controls not selected
  
  if(input$carerSU == "carer" & !input$custom){
    
    theQuestions = as.list(questionFrame[questionFrame$carers == 1, "code"])
    
    names(theQuestions) = questionFrame[questionFrame$carers == 1, "value"]
    
  } else {
    
    theQuestions = input$selQuestions
  }
  
  # if there's enough data
  
  if(nrow(passData()) > 10 & length(unlist(theQuestions) > 0) & max(mytime) - min(mytime) > 1){
    
    ### main data first
    
    helpTrend = function(x) { # x is a dataframe
      
      # prepare
      
      md = melt(x, measure.vars = unlist(theQuestions),
                id.vars = c("Time"))
      
      md$variable = factor(md$variable,
                           labels = names(myQuestions()[which(unlist(myQuestions()) %in%
                                                                unlist(theQuestions))]))
      
      # generate sums and lengths of variables
      
      mycast = data.frame(cast(md, variable + Time ~., sum, na.rm=TRUE),
                          cast(md, variable + Time ~., function(x) sum(!is.na(x)))[,3])
      
      # remove missing Time rows
      
      names(mycast) = c("variable", "Time", "sum", "length")
      
      mycast2 = data.frame(cast(md, variable + Time ~., mean, na.rm=TRUE))
      
      names(mycast2) = c("variable", "Time", "value")
      
      # merge with an empty frame to fill in the blanks
      
      mymerge = data.frame("variable" = as.vector(sapply(unlist(theQuestions),
                                                         function(x) rep(x, length(mytime)))),
                           "Time" = rep(mytime, length(unlist(theQuestions))),
                           "sum" = NA,
                           "length" = NA)
      
      theMerge = merge(mycast, mymerge, all=TRUE, by=c("variable", "Time"))[,1:4]
      
      theMerge = theMerge[!is.na(theMerge$Time),]
      
      # find all the cells that are below 5 responses and merge with the next cell along
      
      while(sum(theMerge$length.x < 5, na.rm=TRUE) > 0){
        
        myloc = which(theMerge$length.x == min(theMerge$length.x, na.rm = TRUE))[1]
        
        if(theMerge$Time[myloc] != max(theMerge$Time, na.rm = TRUE)){ # merge up if it's not the end of the time series
          
          theMerge[(myloc + 1), 3] = sum(theMerge[myloc, 3], theMerge[(myloc + 1), 3])
          
          theMerge[(myloc + 1), 4] = sum(theMerge[myloc, 4], theMerge[(myloc + 1), 4])
          
          theMerge[myloc, 3:4] = NA
          
        } else {
          
          theMerge[myloc, 3] = sum(theMerge[myloc, 3], theMerge[(myloc - 1), 3])
          
          theMerge[myloc, 4] = sum(theMerge[myloc, 4], theMerge[(myloc - 1), 4])
          
          theMerge[(myloc - 1), 3:4] = NA
          
        }
        
      } # end of while
      
      # calculate the mean response and fix the table ready for plotting
      
      theMerge$value = theMerge$sum.x / theMerge$length.x
      
      theMerge = theMerge[order(as.numeric(theMerge$Time)), ]
      
      theMerge$Time = factor(theMerge$Time, levels = unique(as.numeric(theMerge$Time)),
                             labels = names(timelabels[unique(as.numeric(theMerge$Time))]))
      
      return(theMerge)
      
    }
    
    myfinal = helpTrend(passData())
    
    ### now comparison data
    
    if(input$comparison == TRUE & input$Division != 9 &
       length(unique(myfinal$Time[!is.na(myfinal$value)])) > 1 &
       !(as.numeric(input$Division) == 2 &
         sum(unlist(theQuestions) %in%
             c("Service", "Listening", "Communication", "Respect", "InvCare")) == 0)){
      
      mycomp = helpTrend(compData())
      
      changeloc = cumsum(!is.na(myfinal$value)) != 0
      
      myfinal = myfinal[changeloc,]
      
      mycomp = subset(mycomp, Time %in% myfinal$Time)
      
      p = ggplot(myfinal, aes(x = Time, y = value * 20)) + geom_blank() +
        geom_line(aes(group = variable, colour = variable),
                  linetype = 1, data = na.omit(myfinal)) +
        geom_point(aes(group = variable, colour = variable), data = na.omit(myfinal)) +
        annotate("text", x = myfinal$Time[!duplicated(myfinal$Time)],
                 y = floor(min(myfinal$value * 20, na.rm=TRUE)*.8),
                 label = myfinal$length[!duplicated(myfinal$Time)], angle = 90) +
        geom_line(aes(group = variable, colour = variable), linetype = 2,
                  data = na.omit(mycomp)) +
        geom_point(aes(group = variable, colour = variable), data = na.omit(mycomp)) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("% score") +
        labs(colour = "Question")
      
    } else {
      
      if(length(unique(myfinal$Time[!is.na(myfinal$value)])) > 1){
        
        changeloc = cumsum(!is.na(myfinal$value)) != 0
        
        myfinal = myfinal[changeloc, ]
        
        p = ggplot(myfinal, aes(x = Time, y = value * 20)) + geom_blank() +
          geom_line(aes(group = variable, colour = variable), linetype = 1,
                    data = na.omit(myfinal)) +
          geom_point(aes(group = variable, colour = variable), data = na.omit(myfinal)) +
          annotate("text", x = myfinal$Time[!duplicated(myfinal$Time)],
                   y = floor(min(myfinal$value * 20, na.rm=TRUE)*.8),
                   label = myfinal$length[!duplicated(myfinal$Time)], angle = 90) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("% score") +
          labs(colour = "Question")
        
      } else {
        
        df <- data.frame()
        
        p = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
        
        p = p + annotate("text", label = "Not enough data in the specified time and",
                         x = 3, y = 2, size = 7, colour = "red") +
          annotate("text", label = "location, please broaden your search terms",
                   x = 3, y = 1, size = 7, colour = "red")
        
        p = p + options(theme = new_theme_empty)
        
      }
      
      
    }
    
  } else { # end of if(nrow(passData()) > 10)
    
    df <- data.frame()
    
    p = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
    
    p = p + annotate("text", label = "Not enough data in the specified time and",
                     x = 2, y = 2, size = 7, colour = "red") +
      annotate("text", label = "location, please broaden your search terms",
               x = 2, y = 1, size = 7, colour = "red")
    
    p = p + options(theme = new_theme_empty)
    
  }
  
})

output$TrendPlot <- renderPlot({
  
  print(myTrend())
  
})
