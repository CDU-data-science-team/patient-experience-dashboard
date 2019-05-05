
# all report functions go in here, out the way of Shiny code

# function to return values to value box and to reports

reportFunction <- function(report_data){
  
  suceData = report_data
  
  if(is.null(suceData)){
    
    return(NULL)
    
  } else {
    
    # FFT score
    
    promoterScores = suceData[, "Promoter2"]
    
    if(length(promoterScores[!is.na(promoterScores)]) > 2) {
      
      FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                    sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
    } else {
      
      FFT = NULL
    }
    
    # Quality score
    
    serviceScores <- suceData[, "Service"]
    
    if(length(serviceScores[!is.na(serviceScores)]) > 2){
      
      SQ = round(mean(suceData[, "Service"], na.rm = TRUE) * 20, 0)
    } else {
      
      SQ = NULL
    }

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
  
  # useful test- poor data
  # trend_data <- trustData %>% filter(TeamC %in% 505, Date > Sys.Date() - 365 * 2)
  
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
    filter(!is.na(value)) %>% 
    left_join(select(questionFrame, code, value), by = c("Question" = "code")) %>% 
    ggplot(aes(x = Quarter, y = value.x, group = value.y, colour = value.y)) +
    geom_line() +  geom_point() +
    ylab("%") + theme(legend.title=element_blank()) +
    ylim(minimum_value, 100) 
}

# this function returns all comments sorted by category/ criticality & could improve/ do well

allComments <- function(comment_data, category_criticality, improve_do_well){
  
  if(improve_do_well == "Improve"){
    
    improve_well_code <- "Imp1"
    improve_well_crit <- "ImpCrit"
    
  } else {
    
    improve_well_code <- "Best1"
    improve_well_crit <- "BestCrit"
    
  }
  
  if(category_criticality == "Category"){
    
    # going to left join the improve codes to the category table to produce
    # a fully labelled dataframe with all comments, sub and super categories
    
    df = comment_data %>% 
      filter(!is.na(!!(sym(improve_well_code))), !is.na(!!(sym(improve_do_well)))) %>% 
      left_join(categoriesTable, by = setNames("Number", improve_well_code)) %>%  
      select(key : Time, Improve : CommentCoderBest, 
             Location, Division, Directorate, Division2 : type) %>% 
      filter(!is.na(Super)) %>% 
      group_by(Super) %>% 
      mutate(count = n()) %>% 
      arrange(-count, Location)
    
    return(df)
  }
  
  # now let's do a whole other thing for if they want to sort by criticality
  
  if(category_criticality == "Criticality"){
    
    df = comment_data %>%
      filter(!is.na(!!(sym(improve_well_crit))), !is.na(!!(sym(improve_do_well)))) %>%
      select(key : Time, Improve : CommentCoderBest,
             Location, Division, Directorate, Division2) %>%
      arrange(Location)
    
    return(df)
  }
}

# this is a data generation function for all of the reports

report_data <- function(division = "NA", directorate = "NA", team = "NA",
                        date_from = NULL, date_to = NULL, carerSU = NULL,
                        area_name = NULL, comment_sort = NULL, comment_summary = NULL){
  
  first_date <- date_from
  
  end_date <- date_to
  
  # filter by area and name the areas
  
  if(division != "NA"){
    
    suceData = trustData %>%
      filter(Division %in% division)
    
  } else if(directorate != "NA"){
    
    suceData = trustData %>%
      filter(Directorate %in% directorate)
    
  } else if(team != "NA"){
    
    suceData = trustData %>%
      filter(TeamC %in% team)
  } else {
    
    suceData = trustData
  }
  
  # now filter by SU/ carer
  
  if(carerSU == "SU"){
    
    suceData = suceData %>%
      filter(is.na(formtype) | formtype != "SUCE")
    
  } else if(carerSU == "carer"){
    
    suceData = suceData %>%
      filter(formtype == "carer")
    
  } else if(carerSU == "bothCarerSU"){
    
    # nothing!
  }
  
  # filter by time last and produce another dataset with 2 years in
  
  two_year_data <- suceData %>% 
    filter(Date > end_date - 365 * 2)
  
  suceData <- suceData %>% 
    filter(Date >= first_date, Date <= end_date)
  
  report_information <- reportFunction(suceData)
  
  return(list("suceData" = suceData, "two_year_data" = two_year_data,
              "report_information" = report_information))
  
}

# produce all the variables and then place them inline below the chunk

returnTopComments <- function(the_data, nth_row, type){
  
  if(type == "Improve"){
    
    check1 <- the_data %>% 
      filter(!is.na(Imp1)) %>% 
      left_join(categoriesTable, by = c("Imp1" = "Number")) %>% 
      select(Category, Super)
    
    check2 <- the_data %>% 
      filter(!is.na(Imp2)) %>% 
      left_join(categoriesTable, by = c("Imp2" = "Number")) %>% 
      select(Category, Super)
  }
  
  if(type == "Best"){
    
    check1 <- the_data %>% 
      filter(!is.na(Best1)) %>% 
      left_join(categoriesTable, by = c("Best1" = "Number")) %>% 
      select(Category, Super)
    
    check2 <- the_data %>% 
      filter(!is.na(Best2)) %>% 
      left_join(categoriesTable, by = c("Best2" = "Number")) %>% 
      select(Category, Super)
  }
  
  check_final <- rbind(check1, check2)
  
  if(nrow(check_final) < 10){
    
    return(NULL)
  }
  
  count_table <- check_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup()
  
  # this is the bit that returns the nth top comment
  
  return_table <- count_table %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(-percent) %>% 
    slice(nth_row)
  
  # return three comments that are exemplars of that comment
  
  comment_numbers <- return_table %>% 
    left_join(categoriesTable, by = c("Category", "Super")) %>% 
    pull(Number)
  
  if(type == "Improve"){
    
    return_comments <- the_data %>% 
      filter(Imp1 %in% comment_numbers |
               Imp2 %in% comment_numbers) %>% 
      filter(!is.na(Improve)) %>% 
      sample_n(ifelse(nrow(.) >= 3, 3, nrow(.))) %>%  
      pull(Improve) %>% 
      paste0("<p>", 1 : length(.), ": ", ., "</p>", collapse = "")
  }
  
  if(type == "Best"){
    
    return_comments <- the_data %>% 
      filter(Best1 %in% comment_numbers |
               Best2 %in% comment_numbers) %>% 
      filter(!is.na(Best)) %>% 
      sample_n(ifelse(nrow(.) >= 3, 3, nrow(.))) %>%  
      pull(Best) %>% 
      paste0("<p>", 1 : length(.), ": ", ., "</p>", collapse = "")
  }
  
  return(list("return_table" = return_table, 
              "return_comments" = return_comments))
}


