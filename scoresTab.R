
# Generate stacked plot of the requested variable

myStack <- reactive({
  
  req(passData())
  
  theQuestions = c("Service", "Promoter", "Listening", "Communication", "Respect", "Positive")
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(passData()[, theQuestions], 1:2,
                               function(x) round(x + .01)))
  
  # count the missing responses
  
  missnum = apply(fixedData, 2, function(x) sum(!is.na(x)))
  
  if(length(names(missnum[missnum > 2])) < 3){
    
    return(NULL)
  }
  
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
  
  validate(
    need(myStack(), "Not enough data")
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
  
  theQuestions = c("Service", "Promoter", "Listening", "Communication", "Respect", "Positive")
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(passData()[, unlist(theQuestions)], 1:2,
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
    datatable(rownames = FALSE) %>%
    formatRound(TRUE, 1)
})

# generate scores to put underneath the stacked plot

output$fftScore = renderText({
  
  promoterScores = passData()[, "Promoter2"]
  
  req(length(promoterScores[!is.na(promoterScores)]) > 0)
  
  FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
  
  theString = paste0("FFT score calculated as a percentage of 
                     those who would/ would not recommed ", FFT, "%")
  
  return(HTML(theString))
})

# Generate line plot for trend

myTrend = reactive({
  
  theQuestions = c("Service", "Promoter", "Listening", "Communication", "Respect", "Positive")
  
  sample_data <- passData()
  
  sample_data$Quarter = yq(paste0(year(sample_data$Date), ": Q", quarter(sample_data$Date)))
  
  mean_score <- sample_data %>% 
    select(c("Quarter", "Service", "Promoter", "Listening", "Communication", "Respect", "Positive")) %>% 
    group_by(Quarter) %>% 
    summarise_if(is.numeric, function(x) mean(x, na.rm = TRUE) * 20)
  
  minimum_value = mean_score %>% 
    select(c("Service", "Promoter", "Listening", "Communication", "Respect", "Positive")) %>% 
    min(na.rm = TRUE) %>% 
    `-`(20)
  
  number_scores <- sample_data %>% 
    select(c("Quarter", "Service", "Promoter", "Listening", "Communication", "Respect", "Positive")) %>% 
    group_by(Quarter) %>% 
    summarise_all(function(x) length(x[!is.na(x)]))
  
  mean_score[number_scores < 3] = NA
  
  mean_score %>% 
    gather(Question, value, -Quarter) %>% 
    ggplot(aes(x = Quarter, y = value, group = Question, colour = Question)) +
    geom_line() + 
    geom_point() +
    ylim(minimum_value, 100) 
})

output$trendPlot <- renderPlot({
  
  validate(
    need(myTrend(), "Not enough data")
  )
  
  print(myTrend())
  
})
