
# report page control defined here----

output$summaryPage <- renderUI({
  
  tagList(
    column(8, uiOutput("summaryOutputs")),
    column(4, h2("Report builder"),
           
           # write the stuff in here
           
           # I've had a good idea where the report updates the numbers
           # of responses, service areas, that kind of thing, as you go
           
           # Quarterly
           # Access to services, Pharmacy
           # Diversity
           # Exception based reporting
           # Show zero returning teams more easily (maybe outside the feedback tracker)
           
           selectInput("reportTime", "Report type (more reports TBA)", 
                       choices = c("Quarterly" = "quarterly"),
                       selected = "quarterly"),
           
           selectInput("serviceArea", "Service area", 
                       choices = c("Division", "Directorate", "Team")),
           
           uiOutput("reportCustomAreaSelector"),
           
           downloadButton("downloadDoc", "Download report")
    )
  )
})

# main value box output interface defined here----

output$summaryOutputs = renderUI({
  
  tagList(
    htmlOutput("summary_text"),
    
    fluidRow(
      
      valueBoxOutput("sqBox", width = 3), # service quality
      
      valueBoxOutput("fftBox", width = 3), # fft
      
      valueBoxOutput("numberResponsesBox", width = 3), # number respones
      
      valueBoxOutput("zeroRespondingTeams", width = 3) # breakdown of teams responding (zero responding teams, <3 responding teams)
      
    ),
    
    fluidRow(
      
      valueBoxOutput("impCritOneBox", width = 3), # minmally critical
      
      valueBoxOutput("impCritTwoBox", width = 3), # mildy critical
      
      valueBoxOutput("impCritThreeBox", width = 3), # highly critical
      
      valueBoxOutput("changeInCriticality", width = 3) # trend in criticality
    ),
    
    fluidRow(
      
      valueBoxOutput("topCompliment1", width = 3),
      
      valueBoxOutput("topCompliment2", width = 3),
      
      valueBoxOutput("topCompliment3", width = 3),
      
      valueBoxOutput("changeCompliment", width = 3)
    ),
    
    fluidRow(
      
      valueBoxOutput("topCriticism1", width = 3),
      
      valueBoxOutput("topCriticism2", width = 3),
      
      valueBoxOutput("topCriticism3", width = 3),
      
      valueBoxOutput("changeCriticism", width = 3)
    ),
    
    fluidRow(
      
      valueBoxOutput("topScore", width = 3), # best score
      
      valueBoxOutput("lowestScore", width = 3), # lowest score
      
      valueBoxOutput("biggestIncrease", width = 3), # lowest score
      
      valueBoxOutput("biggestDecrease", width = 3) # lowest score
    )
  )
})

# this is the reactive interface referred to in the first bit of the code

output$reportCustomAreaSelector <- renderUI({
  
  if(input$serviceArea == "Division"){
    
    selectInput("report_division", "Division (defaults to whole Trust)", 
                list("Local Partnerships- Mental Healthcare" = 0,
                     "Local Partnerships- Community Healthcare" = 2, "Forensic" = 1),
                multiple = TRUE)
    
  } else if(input$serviceArea == "Directorate"){
    
    finalTable = dirTable %>%
      filter(!DirC %in% c(0, 40))
    
    # finally pull the directorates and names
    
    directorates = finalTable %>%
      pull(DirC)
    
    names(directorates) = finalTable %>%
      pull(DirT)
    
    selectInput("report_directorate", "Choose directorate(s)",
                directorates, multiple = TRUE)
    
  } else if(input$serviceArea == "Team"){
    
    teams <- trustData %>% 
      filter(Date > Sys.Date() - 180) %>% 
      distinct(TeamC) %>% 
      inner_join(
        counts %>% 
          group_by(TeamC) %>%
          slice(which.max(as.Date(date_from)))
      ) %>% 
      left_join(dirTable, c("Directorate" = "DirC")) %>% 
      filter(!is.na(TeamN))
    
    team_names <- map(unique(teams$DirT), function(x) {
      
      team_name <- teams %>% 
        filter(DirT == x)
      
      numbers <- team_name$TeamC
      
      set_names(numbers, team_name$TeamN)
    })
    
    names(team_names) <- unique(teams$DirT)
    
    tagList(p("Please note that there are many teams listed within 
              this control. You can type to search or you may prefer to use 
              the custom selector"),
            selectInput("report_team", "Choose team(s)",
                        team_names, multiple = TRUE, selected = "All")
    )
  } else if(input$serviceArea == "Custom"){
    
    p("This feature is not yet implemented")
  }
})

# report page outputs defined here----

output$downloadDoc <- downloadHandler(
  filename = "CustomReport.docx",
  content = function(file){
    
    # determine which report we're rendering
    
    report_name = input$reportTime
    
    # Set up parameters to pass to Rmd document
    
    # if they're on the division selector
    
    if(input$serviceArea == "Division"){
      
      if(isTruthy(input$report_division)){
        
        area_name <- c("Local Partnerships- Mental Healthcare", 
                       "Forensic Services", 
                       "Local Partnerships- Community Healthcare")[as.numeric(input$report_division) + 1]
        
        params <- list(division = input$report_division,
                       carerSU = input$carerSU,
                       area_name = area_name)
      } else {
        
        area_name <- "the whole Trust"
        
        params <- list(division = "NA",
                       carerSU = input$carerSU,
                       area_name = area_name)
      }
    }
    
    if(input$serviceArea == "Directorate"){
      
      if(isTruthy(input$report_directorate)){
        
        area_name <- dirTable %>% 
          filter(DirC %in% input$report_directorate) %>% 
          pull(DirT) %>% 
          paste(collapse = ", ")
        
        params <- list(directorate = input$report_directorate,
                       carerSU = input$carerSU,
                       area_name = area_name)
      } else {
        
        showModal(
          modalDialog(
            title = "Error!",
            HTML("Please select a directorate"),
            easyClose = TRUE
          )
        )
        
        return()
      }
    }
      
      if(input$serviceArea == "Team"){
        
        if(isTruthy(input$report_team)){
        
        area_name_team <- counts %>% 
          filter(TeamC %in% input$report_team) %>% 
          pull(TeamN) %>% 
          unique() %>% 
          paste(collapse = ", ")
        
        params <- list(team = input$report_team,
                       carerSU = input$carerSU,
                       area_name = area_name_team)
        } else {
        
          showModal(
            modalDialog(
              title = "Error!",
              HTML("Please select a team"),
              easyClose = TRUE
            )
          )
          
          return()
        }
      }
    
    if(input$serviceArea == "Team"){
      
      render(paste0("reports/team_quarterly.Rmd"), output_format = "word_document",
             quiet = TRUE, params = params,
             envir = new.env(parent = globalenv()))
      
      # copy docx to 'file'
      file.copy(paste0("reports/team_quarterly.docx"), file, overwrite = TRUE)

    } else {
      
      render(paste0("reports/", report_name, ".Rmd"), output_format = "word_document",
             quiet = TRUE, params = params,
             envir = new.env(parent = globalenv()))
      
      # copy docx to 'file'
      file.copy(paste0("reports/", report_name, ".docx"), file, overwrite = TRUE)
    }
  }
)

# reactive function to produce data summary to pass to value boxes and to report

dataSummary <- reactive({
  
  suceData = passData()[["currentData"]]
  
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
    
    # name of the area
    
    # if(is.null(input$selTeam)){
    #   
    theArea = "the selected area"
    #   
    # } else {
    #   
    #   ### look up team names
    #   
    #   teams <- as.tibble(input$selTeam) %>%  
    #     inner_join(
    #       counts %>% 
    #         group_by(TeamC) %>%
    #         slice(which.max(as.Date(date_from)))
    #     )
    #   
    #   team_names <- teams %>% pull(TeamN)
    #   
    # }
    
    return(
      list("theArea" = theArea, "NR" = NR, "IC" = IC, "BC" = BC, "NFFT" = NFFT,
           "FFT" = FFT, "NSQ" = NSQ, "SQ" = SQ, "complaint_numbers" = complaint_numbers,
           "improve_numbers" = improve_numbers, "best_numbers" = best_numbers)
    )
  }
})

# 1st row----

output$sqBox <- renderValueBox({
  
  valueBox(value = paste0(dataSummary()[["SQ"]], "%"), 
           subtitle = HTML(paste0("Service quality<br>(", dataSummary()[["NSQ"]], " responses)")),
           icon = icon("thumbs-up")
  )
})

output$fftBox <- renderValueBox({
  
  valueBox(value = paste0(dataSummary()[["FFT"]], "%"), 
           subtitle = HTML(paste0("Would you recommend?<br>(", dataSummary()[["NFFT"]], " responses)")),
           icon = icon("smile")
  )
})

output$numberResponsesBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["NR"]], 
           subtitle = HTML("Number of<br>responses"),
           icon = icon("book-open")
  )
})

output$zeroRespondingTeams <- renderValueBox({
  
  # need to filter counts according to division/ directorate/ team
  
  all_teams = counts %>% # if the whole Trust is selected do this
    filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
    pull(TeamC) %>% 
    unique()
  
  if(!is.null(input$Division)){ 
    
    all_teams = counts %>% 
      filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
      filter(Division %in% input$Division) %>% 
      pull(TeamC) %>% 
      unique()
  } 
  
  if(!is.null(input$selDirect)){ # otherwise look at the directorate code
    
    all_teams = counts %>% 
      filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
      filter(Directorate %in% input$selDirect) %>% 
      pull(TeamC) %>% 
      unique()
  }
  
  if(!is.null(input$selTeam)){
    
    all_teams = counts %>% 
      filter(date_from >= input$dateRange[1], date_from < input$dateRange[2]) %>% 
      filter(TeamC %in% input$TeamC) %>% 
      pull(TeamC) %>% 
      unique()
  }
  
  df <- passData()[["currentData"]] %>% 
    mutate(quarter_date = floor_date(Date, "quarter")) %>% 
    mutate(TeamC = factor(TeamC, levels = all_teams)) %>% 
    group_by(TeamC, quarter_date) %>% 
    summarise(n = n()) %>%
    ungroup() %>% 
    complete(TeamC, quarter_date) %>%
    arrange(TeamC, quarter_date)
  
  zero_teams <- df %>% filter(!is.na(TeamC)) %>% 
    fill(TeamC) %>% 
    spread(quarter_date, n, fill = 0) %>%
    mutate(row_total = rowSums(select(., -1))) %>% 
    filter(row_total == 0) %>% 
    nrow()
  
  valueBox(value = zero_teams,
           subtitle = HTML("Zero responding<br>teams")
  )
})

# second row----

output$impCritOneBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["improve_numbers"]][1], 
           subtitle = HTML("Minimally critical<br>comments"),
           color = "green"
  )
})

output$impCritTwoBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["improve_numbers"]][2], 
           subtitle = HTML("Moderately critical<br>comments"),
           color = "orange"
  )
})

output$impCritThreeBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["improve_numbers"]][3], 
           subtitle = HTML("Highly critical<br>comments"),
           color = "red"
  )
})

output$changeInCriticality <- renderValueBox({
  
  req(passData()[["comparisonData"]])
  
  # take the current period and compare with three months previously
  
  current_criticality <- passData()[["currentData"]] %>% 
    filter(ImpCrit %in% 1:3) %>% 
    pull(ImpCrit) %>% 
    mean()
  
  previous_criticality <- passData()[["comparisonData"]] %>% 
    filter(ImpCrit %in% 1:3) %>% 
    pull(ImpCrit) %>% 
    mean()
  
  change <- round((current_criticality - previous_criticality) / current_criticality * 100, 1)
  
  req(change)
  
  valueBox(value = paste0(change, "%"), 
           subtitle = HTML("Change in<br>criticality"),
           color = ifelse(change >= 0, "green", "red")
  )
})

# third row----

# write a function to return the nth element of improve/ best thing table

returnTopComments <- function(nth_row, type){
  
  if(type == "Improve"){
    
    check1 <- passData()[["currentData"]] %>% 
      filter(!is.na(Imp1)) %>% 
      left_join(categoriesTable, by = c("Imp1" = "Number")) %>% 
      select(Category, Super)
    
    check2 <- passData()[["currentData"]] %>% 
      filter(!is.na(Imp2)) %>% 
      left_join(categoriesTable, by = c("Imp2" = "Number")) %>% 
      select(Category, Super)
  }
  
  if(type == "Best"){
    
    check1 <- passData()[["currentData"]] %>% 
      filter(!is.na(Best1)) %>% 
      left_join(categoriesTable, by = c("Best1" = "Number")) %>% 
      select(Category, Super)
    
    check2 <- passData()[["currentData"]] %>% 
      filter(!is.na(Best2)) %>% 
      left_join(categoriesTable, by = c("Best2" = "Number")) %>% 
      select(Category, Super)
  }
  
  check_final <- rbind(check1, check2)
  
  count_table <- check_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup()
  
  count_sum <- sum(count_table$n)
  
  count_table %>% 
    mutate(percent = round(n / count_sum * 100, 1)) %>% 
    arrange(-percent) %>% 
    slice(nth_row)
}

output$topCompliment1 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(1, "Best")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("smile"))
})

output$topCompliment2 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(2, "Best")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("smile"))
})

output$topCompliment3 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(3, "Best")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("smile"))
})

output$changeCompliment <- renderValueBox({
  
  current1 <- passData()[["currentData"]] %>% 
    filter(!is.na(Best1)) %>% 
    left_join(categoriesTable, by = c("Best1" = "Number")) %>% 
    select(Category, Super)
  
  current2 <- passData()[["currentData"]] %>% 
    filter(!is.na(Best2)) %>% 
    left_join(categoriesTable, by = c("Best2" = "Number")) %>% 
    select(Category, Super)
  
  previous1 <- passData()[["comparisonData"]] %>% 
    filter(!is.na(Best1)) %>% 
    left_join(categoriesTable, by = c("Best1" = "Number")) %>% 
    select(Category, Super)
  
  previous2 <- passData()[["comparisonData"]] %>% 
    filter(!is.na(Best2)) %>% 
    left_join(categoriesTable, by = c("Best2" = "Number")) %>% 
    select(Category, Super)
  
  current_final <- rbind(current1, current2)
  
  previous_final <- rbind(previous1, previous2)
  
  current_table <- current_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(percent) %>% 
    select(-n)
  
  previous_table <- previous_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(percent) %>% 
    select(-n)
  
  difference_table <- merge(current_table, previous_table, 
                            by = c("Category", "Super"), 
                            all = TRUE) %>% 
    mutate(difference = percent.x - percent.y) %>% 
    arrange(-abs(difference)) %>% 
    slice(1)
  
  biggest_difference <- difference_table$difference
  
  if(biggest_difference > 0){
    
    biggest_difference <- paste0("+", biggest_difference)
  }
  
    box1 <- valueBox(value = paste0(biggest_difference, "%"), 
             subtitle = HTML(paste0("<p title = 'largest change in category (green increase, red decrease)'>", 
                                    difference_table$Super, ":<br>", difference_table$Category, "</p>")),
             color = ifelse(difference_table$difference >= 0, "green", "red"),
             icon = icon("smile"),
             # href = actionLink("button", label = ""))
             href = '<a class="action-button">An action link</a>')
    
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_box_01"
    return(box1)
})

observeEvent(input$button_box_01, {
  
  showModal(
    modalDialog(
      title = "Hello!",
      HTML("Hello!"),
      easyClose = TRUE
    )
  )
})


# fourth row----

output$topCriticism1 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(1, "Improve")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("frown"))
})

output$topCriticism2 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(2, "Improve")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("frown"))
})

output$topCriticism3 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(3, "Improve")
  
  valueBox(value = paste0(count_table$percent, "%"), 
           subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
           icon = icon("frown"))
})

# change in criticism

output$changeCriticism <- renderValueBox({
  
  current1 <- passData()[["currentData"]] %>% 
    filter(!is.na(Imp1)) %>% 
    left_join(categoriesTable, by = c("Imp1" = "Number")) %>% 
    select(Category, Super)
  
  current2 <- passData()[["currentData"]] %>% 
    filter(!is.na(Imp2)) %>% 
    left_join(categoriesTable, by = c("Imp2" = "Number")) %>% 
    select(Category, Super)
  
  previous1 <- passData()[["comparisonData"]] %>% 
    filter(!is.na(Imp1)) %>% 
    left_join(categoriesTable, by = c("Imp1" = "Number")) %>% 
    select(Category, Super)
  
  previous2 <- passData()[["comparisonData"]] %>% 
    filter(!is.na(Imp2)) %>% 
    left_join(categoriesTable, by = c("Imp2" = "Number")) %>% 
    select(Category, Super)
  
  current_final <- rbind(current1, current2)
  
  previous_final <- rbind(previous1, previous2)
  
  current_table <- current_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(percent) %>% 
    select(-n)
  
  previous_table <- previous_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category, Super) %>% 
    count() %>% 
    ungroup() %>% 
    mutate(percent = round(n / sum(n) * 100, 1)) %>% 
    arrange(percent) %>% 
    select(-n)
  
  difference_table <- merge(current_table, previous_table, 
                            by = c("Category", "Super"), 
                            all = TRUE) %>% 
    mutate(difference = percent.x - percent.y) %>% 
    arrange(-abs(difference)) %>% 
    slice(1)
  
  biggest_difference <- difference_table$difference
  
  if(biggest_difference > 0){
    
    biggest_difference <- paste0("+", biggest_difference)
  }
  
  valueBox(value = paste0(biggest_difference, "%"), 
           subtitle = HTML(paste0(difference_table$Super, ":<br>", difference_table$Category)),
           color = ifelse(difference_table$difference >= 0, "green", "red"),
           icon = icon("frown"))
})

# fifth row----

# this is a reactive which returns the top and bottom score, and the biggest change score

highLowScoreChange <- reactive({
  
  all_data <- rbind(
    current_data <- passData()[["currentData"]] %>% 
      select(c("Service", "Promoter", "Listening", "Communication", "Respect", "Positive")) %>% 
      mutate(time = "current"),
    
    previous_data <- passData()[["comparisonData"]] %>% 
      select(c("Service", "Promoter", "Listening", "Communication", "Respect", "Positive")) %>% 
      mutate(time = "previous")
  )
  
  summary_data <- all_data %>% 
    group_by(time) %>% 
    summarise_all(function(x) mean(x, na.rm = TRUE) * 20) %>%
    ungroup() %>%  
    select(-time)
  
  summary_data <- rbind(summary_data, summary_data[1, ] - summary_data[2, ])
  
  names(summary_data) <- c("Service", "Likely to recommend", "Listening", "Communication", "Respect", "Positive difference")
  
  low_score_index <- summary_data %>% 
    slice(1) %>% 
    apply(2, min) %>% 
    which.min()
  
  high_score_index <- summary_data %>% 
    slice(1) %>% 
    apply(2, max) %>% 
    which.max()
  
  increase_index <- summary_data %>% 
    slice(3) %>% 
    apply(2, max) %>% 
    which.max()
  
  decrease_index <- summary_data %>% 
    slice(3) %>% 
    apply(2, min) %>% 
    which.min()
  
  low_score <- summary_data %>% 
    select(low_score_index) %>% 
    slice(1)
  
  high_score <- summary_data %>% 
    select(high_score_index) %>% 
    slice(1)
  
  biggest_increase <- summary_data %>% 
    select(increase_index) %>% 
    slice(3)
  
  biggest_decrease <- summary_data %>% 
    select(decrease_index) %>% 
    slice(3)
  
  return(
    list("low_score" = low_score, "high_score" = high_score,
         "biggest_increase" = biggest_increase,
         "biggest_decrease" = biggest_decrease)
  )
  
})

output$topScore <- renderValueBox({
  
  top_score = round(as.numeric(highLowScoreChange()[["high_score"]]), 1)
  
  top_score_name = names(highLowScoreChange()[["high_score"]])
  
  valueBox(value = paste0(top_score, "%"),
           subtitle = top_score_name,
           color = "green"
  )
})

output$lowestScore <- renderValueBox({
  
  low_score = round(as.numeric(highLowScoreChange()[["low_score"]]), 1)
  
  low_score_name = names(highLowScoreChange()[["low_score"]])
  
  valueBox(value = paste0(low_score, "%"),
           subtitle = low_score_name,
           color = "red"
  )
})

output$biggestIncrease <- renderValueBox({
  
  biggest_increase = round(as.numeric(highLowScoreChange()[["biggest_increase"]]), 1)
  
  if(biggest_increase > 0){
    
    biggest_increase <- paste0("+", biggest_increase)
  }
  
  biggest_increase_name = names(highLowScoreChange()[["biggest_increase"]])
  
  valueBox(value = paste0(biggest_increase, "%"),
           subtitle = biggest_increase_name,
           color = "green"
  )
})

output$biggestDecrease <- renderValueBox({
  
  biggest_decrease = round(as.numeric(highLowScoreChange()[["biggest_decrease"]]), 1)
  
  if(biggest_decrease > 0){
    
    biggest_decrease <- paste0("+", biggest_decrease)
  }
  
  biggest_decrease_name = names(highLowScoreChange()[["biggest_decrease"]])
  
  valueBox(value = paste0(biggest_decrease, "%"),
           subtitle = biggest_decrease_name,
           color = "red"
  )
})

# valueBoxOutput("impCritPercentageBox", width = 3), # number comments
# 
# valueBoxOutput("numberImproveBox", width = 3), # number comments
#
# valueBoxOutput("numberBestBox", width = 3), # number comments
#
# valueBoxOutput("criticalBox"), # criticality summary
# 
# valueBoxOutput("teamsRespondingBox"), # breakdown of teams responding (zero responding teams, <3 responding teams)
# 
# valueBoxOutput("bestScoreBox"), # best score
# 
# valueBoxOutput("worstScoreBox") # worst score

# this is the function that draws the parameterized rmarkdown

observe({
  
  searchString <- parseQueryString(session$clientData$url_search)
  
  cat(searchString[["division"]])
  
  # # update inputs according to query string
  # if(length(searchString) > 0){ # if the searchString exists
  #   # deal with first query which indicates the audience
  #   if(searchString[[1]] == "nhs"){ # for NHS users do the following
  #     updateCheckboxGroupInput(session, "domainShow",
  #                              choices = list("NHS users" = "nhs.uk",
  #                                             "Other" = "Other"), selected = c("nhs.uk"))
  #   }
  #   # do they want a smooth?
  #   if(searchString[[2]] == "yes"){
  #     updateTabsetPanel(session, "theTabs", selected = "trend")
  #     updateCheckboxInput(session, inputId = "smooth",
  #                         value = TRUE)
  #   }
  # }
  # 
  # input$Division
  # 
  # input$selDirect
  # 
  # input$selTeam
  
})


