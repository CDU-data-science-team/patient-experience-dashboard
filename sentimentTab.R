
output$sentimentComments <- renderText({
  
  use_data = passData()[["suce"]] %>% 
    filter(!is.na(Improve))
  
  use_data <- use_data %>% 
    mutate(Improve = str_to_lower(Improve)) %>% 
    mutate(Improve = gsub("[[:punct:]]", "", Improve)) %>% 
    mutate(Improve = str_squish(Improve))
  
  # remove the single word comments
  
  use_data <- use_data %>% 
    filter(!Improve %in% c("none", "nothing", "declined", "no", "blank")) %>% 
    filter(!Improve %in% c("", "0", "00", "100", "1010", "4444", "6", "6666", "7", "711", 
                           "9", "a", "a ok", "ai", "all", "dont", "idk",  
                           "n a", "n0", "na", "na9", "ni", "nil", "nine", 
                           "nne", "non", "nonw", "nope", "note", "nout", "nowt", "nr", "ok", 
                           "okay", "sw", "wa", "x", "x 56", "yes", "zero"))

  # add a comment number 
  
  use_data <- use_data %>% 
    filter(!is.na(Improve)) %>% 
    mutate(comment = row_number()) %>% 
    mutate(word_count = stri_count_words(Improve))

  tidy_words <- use_data %>%
    unnest_tokens(word, Improve, token = "words") %>% 
    filter(!is.na(word))
  
  nrc <- tidy_words %>% 
    inner_join(get_sentiments("nrc")) %>%
    group_by(comment) %>% 
    count(sentiment) %>%
    spread(sentiment, n, fill = 0) %>% 
    ungroup()
  
  angry_words <- get_sentiments("nrc") %>% 
    filter(sentiment == input$emotion) %>% 
    pull(word)
  
  angry_comments <- nrc %>% 
    left_join(use_data, by = c("comment" = "comment")) %>% 
    mutate(anger = anger / log(word_count)) %>% 
    top_n(10, !!(sym(input$emotion))) %>% 
    pull(Improve)
  
  for(i in angry_words){
    
    for(j in 1 : length(angry_comments)){
      
      angry_comments[j] = gsub(paste0("\\b", i, "\\b"), 
                               paste0('<span style="color:red">', i, "</span>"), 
                               angry_comments[j],
                               perl = TRUE)
    }
  }
  
  return(
    HTML(
      paste0('<p style="font-size:18px">', angry_comments, "</p>")
    )
  )
  
})