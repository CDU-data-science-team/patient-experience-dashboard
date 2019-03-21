
# all report functions go in here, out the way of Shiny code

# function to return values to value box and to reports

reportFunction <- function(report_data){
  
  suceData = report_data
  
  if(is.null(suceData)){
    
    return(NULL)
    
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
    
    # criticality
    
    improve_numbers <- map_int(c(1, 2, 3), function(x){
      
      suceData %>% 
        filter(ImpCrit == x) %>% 
        nrow()
    })
    
    # criticality
    
    best_numbers <- map_int(c(1, 2, 3), function(x){
      
      suceData %>% 
        filter(BestCrit == x) %>% 
        nrow()
    })
    
    return(
      list("NR" = NR, "IC" = IC, "BC" = BC, "NFFT" = NFFT,
           "FFT" = FFT, "NSQ" = NSQ, "SQ" = SQ, "complaint_numbers" = complaint_numbers,
           "improve_numbers" = improve_numbers, "best_numbers" = best_numbers)
    )
  }
}

# stacked plot

stack_function <- function(stack_data){
  
  theQuestions = c("Service", "Promoter", "Listening", "Communication", "Respect", "InvCare", "Positive")
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(stack_data[, theQuestions], 1:2,
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
}

# trend plot

trend_function <- function(trend_data){
  
  theQuestions = c("Service", "Promoter", "Listening", "Communication", "Respect", "InvCare", "Positive")
  
  sample_data <- trend_data
  
  sample_data$Quarter = yq(paste0(year(sample_data$Date), ": Q", quarter(sample_data$Date)))
  
  mean_score <- sample_data %>% 
    select(c("Quarter", theQuestions)) %>% 
    group_by(Quarter) %>% 
    summarise_if(is.numeric, function(x) mean(x, na.rm = TRUE) * 20)
  
  minimum_value = mean_score %>% 
    select(theQuestions) %>% 
    min(na.rm = TRUE) %>% 
    `-`(20)
  
  number_scores <- sample_data %>% 
    select(c("Quarter", theQuestions)) %>% 
    group_by(Quarter) %>% 
    summarise_all(function(x) length(x[!is.na(x)]))
  
  mean_score[number_scores < 3] = NA
  
  mean_score %>% 
    gather(Question, value, -Quarter) %>% 
    left_join(select(questionFrame, code, value), by = c("Question" = "code")) %>% 
    ggplot(aes(x = Quarter, y = value.x, group = value.y, colour = value.y)) +
    geom_line() +  geom_point() +
    ylab("%") + theme(legend.title=element_blank()) +
    ylim(minimum_value, 100) 
}

