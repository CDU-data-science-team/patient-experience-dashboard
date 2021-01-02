
# all report functions go in here, out the way of Shiny code

# function to return values to value box and to reports

reportFunction <- function(report_data){
  
  suceData = report_data
  
  if(is.null(suceData)){
    
    return(NULL)
    
  } else {
    
    # FFT score
    
    promoterScores = suceData$Promoter2
    
    if(length(promoterScores[!is.na(promoterScores)]) > 2) {
      
      FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                    sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
    } else {
      
      FFT = NULL
    }
    
    # Quality score
    
    serviceScores <- suceData$Service
    
    if(length(serviceScores[!is.na(serviceScores)]) > 2){
      
      SQ = round(mean(serviceScores, na.rm = TRUE) * 20, 0)
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

stack_function <- function(stack_data, type){
  
  if(type == "suce_dashboard"){
    
    theQuestions = c("Service", "Promoter", "Positive", "Respect", "Privacy")
  }
  
  if(type == "carer_dashboard"){
    
    theQuestions = c("InvCare", "Listening", "Communication", "SupportServices")
  }
  
  if(type == "suce_report"){
    
    theQuestions <- c("Service", "InvCare", "Positive", "Promoter", "Listening", 
                      "Communication", "Respect")
  }
  
  if(type == "carer_report"){
    
    theQuestions <- c("SupportServices", "Promoter", "Listening", 
                      "Communication", "Respect", "Privacy")
  }
  
  # remove decimals from historic data
  
  fixedData = data.frame(apply(stack_data[, theQuestions], 1 : 2,
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
    scale_fill_viridis_d("Response", limits = c(1:5), breaks = c(5:1),
                      labels = c("Excellent", "Good", "Fair", "Poor", "Very poor"),
                      direction = -1) +
    scale_y_continuous(labels = percent_format()) +
    guides(fill = guide_legend(reverse = TRUE)) + 
    scale_x_discrete() + coord_flip() + xlab("Question")
}

# trend plot

trend_function <- function(trend_data, type){
  
  if(type == "suce_dashboard"){
    
    theQuestions = c("Service", "Promoter", "Positive", "Respect", "Privacy")
  }
  
  if(type == "carer_dashboard"){
    
    theQuestions = c("InvCare", "Listening", "Communication", "SupportServices")
  }
  
  if(type == "suce_report"){
    
    theQuestions <- c("Service", "InvCare", "Positive", "Promoter", "Listening", 
                      "Communication", "Respect")
  }
  
  if(type == "carer_report"){
    
    theQuestions <- c("SupportServices", "Promoter", "Listening", 
                      "Communication", "Respect", "Privacy")
  }
  
  if(type == "patient_voices"){
    
    theQuestions = c("Service", "Promoter", "Positive")
  }
  
  # useful test- poor data
  # trend_data <- trustData %>% filter(TeamC %in% 505, Date > Sys.Date() - 365 * 2)
  # no data
  # trend_data <- trustData %>% filter(TeamC %in% 104, Date > Sys.Date() - 365 * 2, formtype == "carer")
  
  sample_data <- trend_data
  
  sample_data$Quarter = yq(paste0(year(sample_data$Date), ": Q", quarter(sample_data$Date)))
  
  mean_score <- sample_data %>% 
    select(c("Quarter", theQuestions)) %>% 
    group_by(Quarter) %>% 
    summarise_if(is.numeric, function(x) mean(x, na.rm = TRUE) * 20)
  
  minimum_value = mean_score %>% 
    select(theQuestions) %>% 
    min(na.rm = TRUE) %>% 
    `-`(10)
  
  number_scores <- sample_data %>% 
    select(c("Quarter", theQuestions)) %>% 
    group_by(Quarter) %>% 
    summarise_all(function(x) length(x[!is.na(x)]))
  
  mean_score[number_scores < 3] = NA
  
  data_for_graph <- mean_score %>% 
    gather(Question, value, -Quarter) %>% 
    filter(!is.na(value)) %>% 
    left_join(select(questionFrame, code, value), by = c("Question" = "code"))
  
  if(length(unique(data_for_graph$Quarter)) < 2){
    
    return(NULL)
  }
  
  if(type == "patient_voices"){
    
    data_for_graph %>%   
      ggplot(aes(x = Quarter, y = value.x, group = value.y)) +
      geom_line(aes(linetype = value.y)) +
      geom_point(aes(shape = value.y), size = 3) +
      ylab("%") + theme(legend.title=element_blank()) +
      ylim(minimum_value, 100) 
  } else {
    
    data_for_graph %>%   
      ggplot(aes(x = Quarter, y = value.x, group = value.y, colour = value.y)) +
      geom_line() +  geom_point() +
      ylab("%") + theme(legend.title=element_blank()) +
      ylim(minimum_value, 100) 
  }
}

# this function returns all comments sorted by category/ criticality & could improve/ do well

allComments <- function(comment_data, category_criticality, 
                        improve_do_well, category_table){
  
  df = comment_data
  
  if(improve_do_well == "Improve"){
    
    improve_well_code <- "Imp1"
    improve_well_crit <- "ImpCrit"
    
    df <- df %>% 
      filter(!Imp1 %in% c("4444", "XN", "XX"))
    
  } else {
    
    improve_well_code <- "Best1"
    improve_well_crit <- "BestCrit"
    
  }
  
  if(category_criticality == "Category"){
    
    # going to left join the improve codes to the category table to produce
    # a fully labelled dataframe with all comments, sub and super categories
    
    df = df %>% 
      filter(!is.na(!!(sym(improve_do_well)))) %>% 
      left_join(category_table, by = setNames("Number", improve_well_code)) %>%  
      select(key : Time, Improve : CommentCoderBest, 
             Location, Division, Directorate, Division2 : type) %>% 
      mutate(Super = replace_na(Super, "Uncategorised")) %>% 
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

returnTopComments <- function(the_data, nth_row, type, category_table){
  
  check_improve <- rbind(
    the_data %>% 
      filter(!is.na(Imp1)) %>% 
      mutate(Number = Imp1) %>% 
      select(-c(Imp1, Imp2, Best1, Best2)),
    
    the_data %>% 
      filter(!is.na(Imp2)) %>% 
      mutate(Number = Imp2) %>% 
      select(-c(Imp1, Imp2, Best1, Best2))
  )
  
  check_best <- rbind(
    the_data %>% 
      filter(!is.na(Best1)) %>% 
      mutate(Number = Best1) %>% 
      select(-c(Imp1, Imp2, Best1, Best2)),
    
    the_data %>% 
      filter(!is.na(Best2)) %>% 
      mutate(Number = Best2) %>% 
      select(-c(Imp1, Imp2, Best1, Best2))
  )
  
  if(type == "Improve"){
    
    check_final <- check_improve
  }
  
  if(type == "Best"){
    
    check_final <- check_best
  }
  
  if(type == "Both"){
    
    check_final <- rbind(check_improve, check_best)
  }
  
  check_final <- check_final %>% 
    filter(!Number %in% c("XN", "XX"))
  
  if(nrow(check_final) < 10){
    
    return(NULL)
  }
  
  count_table <- check_final %>% 
    filter(!is.na(Number)) %>% 
    group_by(Number) %>% 
    count() %>% 
    ungroup()
  
  return_table <- count_table %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(-percent) %>% 
    slice(nth_row) %>% 
    left_join(category_table)
  
  # return three comments that are exemplars of that comment
  # the "both" method returns rubbish because it doesn't work and 
  # isn't supposed to
  
  if(type == "Improve"){
    
    return_comments <- the_data %>% 
      filter(Imp1 %in% return_table$Number |
               Imp2 %in% return_table$Number) %>% 
      filter(!is.na(Improve)) %>% 
      sample_n(ifelse(nrow(.) >= 3, 3, nrow(.))) %>%  
      pull(Improve) %>% 
      paste0("<p>", 1 : length(.), ": ", ., "</p>", collapse = "")
  }
  
  if(type == "Best"){
    
    return_comments <- the_data %>% 
      filter(Best1 %in% return_table$Number |
               Best2 %in% return_table$Number) %>% 
      filter(!is.na(Best)) %>% 
      sample_n(ifelse(nrow(.) >= 3, 3, nrow(.))) %>%  
      pull(Best) %>% 
      paste0("<p>", 1 : length(.), ": ", ., "</p>", collapse = "")
  }
  
  if(type == "Both"){
    
    return_comments = NULL
  }
  
  return(list("return_table" = return_table, 
              "return_comments" = return_comments))
}

# function to return text that has been filtered by topic, criticality, and string

returnSearchText <- function(text_data, type = "Improve", 
                             filterCommentsBy,
                             searchTextInclude, textSearchExclude,
                             criticalityLevels, topSixThemes){
  
  # remove trailing punctuation from both input strings
  
  searchTextInclude <- sub("[[:punct:]]$", "", trimws(searchTextInclude))
  
  textSearchExclude <- sub("[[:punct:]]$", "", trimws(textSearchExclude))
  
  # give a set of variable names for each
  
  if(type == "Improve"){
    
    variable_names <- c("Improve", "Imp1", "Imp2", "ImpCrit")
  } else {
    
    variable_names <- c("Best", "Best1", "Best2", "BestCrit")
  }
  
  if("Text search" %in% filterCommentsBy){
    
    if(isTruthy(searchTextInclude)){ # or overwrite if search string exists
      
      text_data <- text_data %>%
        filter(grepl(paste(
          trimws(unlist(strsplit(searchTextInclude, ","))), 
          collapse = "|"), .data[[variable_names[1]]]))
    }
    
    if(isTruthy(textSearchExclude)){
      
      text_data <- text_data %>%
        filter(!grepl(paste(
          trimws(unlist(strsplit(textSearchExclude, ","))), 
          collapse = "|"), .data[[variable_names[1]]]))
    }
  }
  
  if(type == "Improve"){
    
    text_data <- text_data %>%
      mutate(ImpCrit = -ImpCrit)
  }
  
  if("Criticality" %in% filterCommentsBy){
    
    text_data <- text_data %>%
      filter(.data[[variable_names[4]]] %in% criticalityLevels)
  }
  
  if("Themes" %in% filterCommentsBy){
    
    text_data <- text_data %>%
      filter(.data[[variable_names[2]]] %in% topSixThemes |
               .data[[variable_names[3]]] %in% topSixThemes)
  }
  
  text_data <- text_data %>% 
    filter(!is.na(.data[[variable_names[1]]]))
  
  return(text_data)
}

