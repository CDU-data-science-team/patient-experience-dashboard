
count_type <- function(type, data){
  
  if(type == "online"){
    
    filter_data <- data %>% 
      filter(addedby %in% c("jisc", "survey.monkey"))
  }
  
  if(type == "SMS"){
    
    filter_data <- data %>% 
      filter(addedby == "SMS")
  }
  
  if(type == "paper"){
    
    filter_data <- data %>% 
      filter(grepl(".", addedby, fixed = TRUE), addedby != "survey.monkey")
  }
  
  if(type == "other"){
    
    filter_data <- data %>% 
      filter(addedby %in% c("SPSS", "NMHS", "IAPT"))
  }
  
  filter_data %>% 
    tally()
}
