
### text analysis file

# return the data for the text bigrams- this needs to be cached for the plot click

bigram_words_data <- reactive({
  
  use_data = passData()[["suce"]] %>% 
    filter(!is.na(Improve))
  
  bigrams <- use_data %>% 
    select(Improve, Division2) %>% 
    unnest_tokens(bigram, Improve, token = "ngrams", n = 2)
  
  bigrams_separated <- bigrams %>% 
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>% 
    filter(!word1 %in% stop_words$word) %>% 
    filter(!word2 %in% stop_words$word) %>% 
    filter(!grepl('^\\d+$', word1)) %>% 
    filter(!grepl('^\\d+$', word2))
  
  bigram_counts <- bigrams_filtered %>% 
    count(word1, word2, sort = TRUE)
  
  bigram_graph <- bigram_counts %>% 
    arrange(-n) %>% 
    head(input$bigramSlider) %>% 
    graph_from_data_frame()
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), arrow = arrow()) + 
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)
})

output$bigram_plot <- renderPlot({
  
  bigram_words_data()
})

output$sentimentPlot <- renderPlot({
  
  use_data = passData()[["suce"]] %>% 
    filter(!is.na(Improve))
  
  tidy_words <- use_data %>%
    unnest_tokens(word, Improve, token = "words") %>% 
    filter(!is.na(word)) %>% 
    filter(Date > as.Date("2013-01-01")) %>% 
    mutate(yearmonth = as.numeric(paste0(year(Date), sprintf("%02d", month(Date))))) %>% 
    select(word, yearmonth, Date)
  
  sentiment <- tidy_words %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(Date = floor_date(Date, unit = "month")) %>% 
    count(index = Date, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>% 
    group_by(index) %>% 
    summarise(sentiment = mean(sentiment))
  
  ggplot(sentiment, aes(index, sentiment)) +
    geom_line(show.legend = FALSE) + geom_smooth()
    # facet_wrap(~Division2, ncol = 2, scales = "free")

})

output$tagBigrams <- renderPlot({
  
  use_data <- passData()[["suce"]] %>% 
    filter(!is.na(Improve))
  
  use_data2 <- use_data %>% 
    left_join(categoriesTable, by = c("Imp1" = "Number"))
  
  use_data2 <- use_data2 %>% 
    left_join(categoriesTable, by = c("Imp2" = "Number"))
  
  use_data2 <- use_data2 %>% 
    filter(!is.na(Category.x), !is.na(Category.y))
  
  use_data2 <- use_data2 %>% 
    mutate(Category.x = paste(Super.x, Category.x)) %>% 
    mutate(Category.y = paste(Super.y, Category.y))
  
  bigram_counts <- use_data2 %>% 
    count(Category.x, Category.y, sort = TRUE)
  
  bigrams_united <- use_data2 %>% 
    mutate(bigram = paste(Category.x, " : ", Category.y))
  
  bigram_graph <- bigram_counts %>% 
    top_n(40, n) %>% 
    graph_from_data_frame()
  
  ggraph(bigram_graph, layout = "fr") +
    geom_edge_link(aes(edge_alpha = n, edge_width = n)) + 
    geom_node_point() +
    geom_node_text(aes(label = name, color = "red"), repel = TRUE)
  
})

output$plotClickInformation <- renderText({
  
  validate(
    need(input$bigram_click, 'Please click the plot on the left to see example comments')
  )
  
  p <- bigram_words_data()
  
  pg <- ggplot_build(p)
  
  check <- pg[[1]][[3]]
  
  label <- nearPoints(check, input$bigram_click, threshold = 10, maxpoints = 1)$label
  
  passData()[["suce"]] %>% 
    filter(grepl(label, Improve, ignore.case = TRUE)) %>% 
    sample_n(ifelse(nrow(.) > 50, 50, nrow(.))) %>% # if more than 50 rows, sample 50. Else, sample number of rows
    mutate(improve_paste = paste("<p>", Improve, "</p>")) %>% 
    pull(improve_paste) %>% 
    HTML()
})