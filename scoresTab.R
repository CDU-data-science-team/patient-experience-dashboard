
# Generate stacked plot of the requested variable

output$StackPlot <- renderPlot({
  
  validate(
    need(passData()[["currentData"]], "Not enough data")
  )
  
  stack_function(passData()[["currentData"]])
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
  
  theQuestions = c("Service", "Promoter", "Listening", "Communication", "Respect", "InvCare", "Positive")
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(passData()[["currentData"]][, unlist(theQuestions)], 1:2,
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
    mutate(value.x = factor(value.x, levels = 1:5)) %>% 
    spread(value.x, prop, drop = FALSE) %>%
    ungroup() %>%
    setNames(c("Question", rev(c("Excellent", "Good", "Fair", "Poor", "Very poor")))) %>% 
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    mutate(Score = (Excellent * 5 + Good * 4 + Fair * 3 + Poor * 2 + `Very poor` * 1) / 5) %>%
    datatable(rownames = FALSE) %>%
    formatRound(TRUE, 1)
})

# generate scores to put underneath the stacked plot

output$fftScore = renderText({
  
  promoterScores = passData()[["currentData"]][, "Promoter2"]
  
  req(length(promoterScores[!is.na(promoterScores)]) > 0)
  
  FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
  
  theString = paste0("FFT score calculated as a percentage of 
                     those who would/ would not recommed ", FFT, "%")
  
  return(HTML(theString))
})

# Generate line plot for trend

output$trendPlot <- renderPlot({
  
  validate(
    need(passData()[["trendData"]], "Not enough data")
  )
  
  trend_function(passData()[["trendData"]])
  
})
