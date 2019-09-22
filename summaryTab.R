
# report page control defined here----

output$summaryPage <- renderUI({
  
  # give the current quarter here and write it in the quarterly control
  
  today = Sys.Date()
  
  previous_quarter <- (quarter(today)) - 1 %% 4
  previous_year <- year(today)
  
  if(previous_quarter == 0){
    
    previous_quarter <- 4
    previous_year <- previous_year - 1
  }
  
  first_date <- yq(paste0(previous_year, ": Q", previous_quarter))
  
  end_date <- yq(paste0(year(today), ": Q", quarter(today))) - 1
  
  # render the whole control as a list
  
  choice_text <- list("quarterly")
  
  names(choice_text) = c(paste0("Quarterly (", 
                                substr(first_date, 1, 7), " to ", 
                                substr(end_date, 1, 7), ")"))
  
  tagList(
    column(width = 12, h2("Report builder")),
    column(4, 
           box(width = 12, 
               h4("Current selected time and area"),
               hr(),
               selectInput("commentSummary", "Comment summary type",
                           choices = c("Written summary" = "textSummary",
                                       "Verbatim comments" = "verbatimComments")),
               downloadButton("downloadCurrent", "Download report")
           ),
           box(width = 12,
               h4("Pre defined template reports"),
               
               selectInput("reportTime", "Report type (more reports TBA)", 
                           choices = choice_text,
                           selected = "quarterly"),
               
               selectInput("serviceArea", "Service area", 
                           choices = c("Division", "Directorate", "Team")),
               
               uiOutput("reportCustomAreaSelector"),
               
               downloadButton("downloadDoc", "Download report")
           )
    ),
    column(8, uiOutput("summaryOutputs"))
  )
})

# main value box output interface defined here----

output$summaryOutputs = renderUI({
  
  tagList(
    
    div("Some boxes will give more information if you hover pointer over the text."),
    hr(),
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
    
    h4("What did we do well?"),
    
    fluidRow(
      
      valueBoxOutput("topCompliment1", width = 3),
      
      valueBoxOutput("topCompliment2", width = 3),
      
      valueBoxOutput("topCompliment3", width = 3),
      
      valueBoxOutput("changeCompliment", width = 3)
    ),
    
    h4("What could be improved?"),
    
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
    
    selectInput("report_division", "Division (leave blank for whole Trust)", 
                list("Local Partnerships- Mental Healthcare" = 0,
                     "Local Partnerships- General Healthcare" = 2, "Forensic" = 1),
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
    
    # date
    
    today = Sys.Date()
    
    previous_quarter <- (quarter(today)) - 1 %% 4
    previous_year <- year(today)
    
    if(previous_quarter == 0){
      
      previous_quarter <- 4
      previous_year <- previous_year - 1
    }
    
    first_date <- yq(paste0(previous_year, ": Q", previous_quarter))
    
    end_date <- yq(paste0(year(today), ": Q", quarter(today))) - 1
    
    # if they're on the division selector
    
    if(input$serviceArea == "Division"){
      
      if(isTruthy(input$report_division)){
        
        area_name <- c("Local Partnerships- Mental Healthcare", 
                       "Forensic Services", 
                       "Local Partnerships- General Healthcare")[as.numeric(input$report_division) + 1]
        
        params <- list(division = input$report_division,
                       carerSU = input$carerSU,
                       area_name = area_name,
                       date_from = first_date,
                       date_to = end_date,
                       comment_summary = "textSummary")
        
      } else {
        
        area_name <- "the whole Trust"
        
        params <- list(division = "NA",
                       carerSU = input$carerSU,
                       area_name = area_name,
                       date_from = first_date,
                       date_to = end_date,
                       comment_summary = "textSummary")
      }
    }
    
    if(input$serviceArea == "Directorate"){
      
      if(isTruthy(input$report_directorate)){
        
        number_rows = trustData %>%
          filter(Directorate %in% input$report_directorate) %>% 
          filter(Date >= first_date, Date <= end_date) %>% 
          nrow()
        
        if(number_rows >= 10){
          
          area_name <- dirTable %>% 
            filter(DirC %in% input$report_directorate) %>% 
            pull(DirT) %>% 
            paste(collapse = ", ")
          
          params <- list(directorate = input$report_directorate,
                         carerSU = input$carerSU,
                         area_name = area_name,
                         date_from = first_date,
                         date_to = end_date,
                         comment_summary = "verbatimComments")
          
        } else {
          
          showModal(
            modalDialog(
              title = "Error!",
              HTML("Too few responses in the last quarter"),
              easyClose = TRUE
            )
          )
          
          return()
        }
        
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
                       area_name = area_name_team,
                       date_from = first_date,
                       date_to = end_date,
                       comment_summary = "verbatimComments")
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
    
    render(paste0("reports/oneRmarkdownToRuleThemAll.Rmd"), output_format = "word_document",
           output_file = "quarterly_report.docx", quiet = TRUE, params = params,
           envir = new.env(parent = globalenv()))
    
    # copy docx to 'file'
    file.copy(paste0("reports/quarterly_report.docx"), file, overwrite = TRUE)
  }
)

output$downloadCurrent <- downloadHandler(
  filename = "CustomReport.docx",
  content = function(file){
    
    params <- generate_rmd_parameters()
      
      # check number of rows for directorate
      
      if(is.null(params)){

        showModal(
          modalDialog(
            title = "Error!",
            HTML("Too few responses in the last quarter"),
            easyClose = TRUE
          )
        )
      }
    
    render(paste0("reports/oneRmarkdownToRuleThemAll.Rmd"), output_format = "word_document",
           output_file = "custom_report.docx", quiet = TRUE, params = params,
           envir = new.env(parent = globalenv()))
    
    # copy docx to 'file'
    file.copy(paste0("reports/custom_report.docx"), file, overwrite = TRUE)
  }
)

# reactive function to produce data summary to pass to value boxes and to report

dataSummary <- reactive({
  
  reportFunction(passData()[["currentData"]])
})

# 1st row----

output$sqBox <- renderValueBox({
  
  if(is.null(dataSummary()[["SQ"]])){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(dataSummary()[["SQ"]], "%"), 
             subtitle = HTML(paste0("Service quality<br>(", dataSummary()[["NSQ"]], " responses)")),
             icon = icon("thumbs-up"))
  }
})

output$fftBox <- renderValueBox({
  
  if(is.null(dataSummary()[["FFT"]])){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(dataSummary()[["FFT"]], "%"), 
             subtitle = HTML(paste0("Would you recommend?<br>(", dataSummary()[["NFFT"]], " responses)")),
             icon = icon("smile"))
  }
})

output$numberResponsesBox <- renderValueBox({
  
  valueBox(value = dataSummary()[["NR"]], 
           subtitle = HTML("<p title = 'Click to show response numbers for individual teams'>
                             Number of<br>responses</p>"),
           icon = icon("book-open"),
           href = "/apps/feedback_tracker"
  )
})

# produce a reactive object to hold the zero responding teams

zeroRespondingData <- reactive({
  
  # need to filter counts according to division/ directorate/ team
  
  sub_counts = counts %>% # if the whole Trust is selected do this
    filter(date_from >= floor_date(input$dateRange[1], "quarter"), 
           date_from < ceiling_date(input$dateRange[2], "quarter"))

  if(!is.null(input$Division)){ 
    
    sub_counts = counts %>% 
      filter(date_from >= floor_date(input$dateRange[1], "quarter"), 
             date_from < ceiling_date(input$dateRange[2], "quarter")) %>% 
      filter(Division %in% input$Division)
  } 
  
  if(!is.null(input$selDirect)){ # otherwise look at the directorate code
    
    sub_counts = counts %>% 
      filter(date_from >= floor_date(input$dateRange[1], "quarter"), 
             date_from < ceiling_date(input$dateRange[2], "quarter")) %>% 
      filter(Directorate %in% input$selDirect)
  }
  
  if(!is.null(input$selTeam)){
    
    sub_counts = counts %>% 
      filter(date_from >= floor_date(input$dateRange[1], "quarter"), 
             date_from < ceiling_date(input$dateRange[2], "quarter")) %>% 
      filter(TeamC %in% input$TeamC) 
  }
  
  all_teams <- sub_counts %>%
    pull(TeamC) %>% 
    unique()
  
  df <- passData()[["currentData"]] %>% 
    mutate(quarter_date = floor_date(Date, "quarter")) %>% 
    mutate(TeamC = factor(TeamC, levels = all_teams)) %>% 
    group_by(TeamC, quarter_date) %>% 
    summarise(n = n()) %>%
    ungroup() %>% 
    complete(TeamC, quarter_date) %>%
    arrange(TeamC, quarter_date)
  
  # return the actual zero teams
  
  zero_table <- df %>% filter(!is.na(TeamC)) %>% 
    fill(TeamC) %>% 
    spread(quarter_date, n, fill = 0) %>%
    mutate(row_total = rowSums(select(., -1))) %>% 
    filter(row_total == 0)
  
  # return the subset of counts that you were using to join for TeamN in the modal
  
  return(list("zero_table" = zero_table, "sub_counts" = sub_counts))
})

output$zeroRespondingTeams <- renderValueBox({
  
  zero_teams <- zeroRespondingData()[["zero_table"]] %>% 
    nrow()
  
  zerobox <- valueBox(value = zero_teams,
                      subtitle = HTML("<p title = 'Click to show response numbers for individual teams'>
                             Zero responding<br>teams</p>"),
                      href = 'Link to team names'
  )
  
  zerobox$children[[1]]$attribs$class<-"action-button"
  zerobox$children[[1]]$attribs$id<-"zerobox"
  
  return(zerobox)
})

observeEvent(input$zerobox, {
  
  showModal(
    modalDialog(
      a(href = "/apps/feedback_tracker", "Link to directorate response reports"),
      htmlOutput("zeroTeamsText"),
      size = "l", easyClose = TRUE)
  )
})

output$zeroTeamsText <- renderText({
  
  list_of_teams <- zeroRespondingData()[["zero_table"]] %>% 
    mutate(TeamC = as.numeric(as.character(TeamC))) %>% 
    left_join(zeroRespondingData()[["sub_counts"]], by = "TeamC") %>% 
    distinct(TeamC, .keep_all = TRUE) %>% 
    pull(TeamN) 
  
  HTML(paste0("<h2>Zero responding teams</h2><p>(click 
        anywhere to dismiss this dialogue)</p>", 
              paste0("<p>", list_of_teams, "</p>", collapse = "")))
  
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
           subtitle = HTML("<p title = 'Green less critical, red more critical'>Change in<br>criticality</p>"),
           color = ifelse(change < 0, "green", "red")
  )
})

# third row----

output$topCompliment1 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(passData()[["currentData"]], 1, "Best")[["return_table"]]
  
  if(is.null(count_table)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(count_table$percent, "%"), 
             subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
             icon = icon("smile"))
  }
})

output$topCompliment2 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(passData()[["currentData"]], 2, "Best")[["return_table"]]
  
  if(is.null(count_table)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(count_table$percent, "%"), 
             subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
             icon = icon("smile"))
  }
})

output$topCompliment3 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(passData()[["currentData"]], 3, "Best")[["return_table"]]
  
  if(is.null(count_table)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(count_table$percent, "%"), 
             subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
             icon = icon("smile"))
  }
})

output$changeCompliment <- renderValueBox({
  
  req(passData()[["comparisonData"]])
  
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
  
  if(is.na(difference_table$difference)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    biggest_difference <- difference_table$difference
    
    if(biggest_difference > 0){
      
      biggest_difference <- paste0("+", biggest_difference)
    }
    
    box1 <- valueBox(value = paste0(biggest_difference, "%"), 
                     subtitle = HTML(paste0("<p title = 'Largest change in category (green increase, red decrease)'>", 
                                            difference_table$Super, ":<br>", difference_table$Category, "</p>")),
                     color = ifelse(difference_table$difference >= 0, "green", "red"),
                     icon = icon("smile"),
                     # href = '<a class="action-button">An action link</a>')
                     href = 'Link to comments')
    
    box1$children[[1]]$attribs$class<-"action-button"
    box1$children[[1]]$attribs$id<-"button_box_01"
    return(box1)
  }
})

observeEvent(input$button_box_01, {
  
  updateTabItems(session, "tabs", "comments")
})

# fourth row----

output$topCriticism1 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(passData()[["currentData"]], 1, "Improve")[["return_table"]]
  
  if(is.null(count_table)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(count_table$percent, "%"), 
             subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
             icon = icon("frown"))
  }
})

output$topCriticism2 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(passData()[["currentData"]], 2, "Improve")[["return_table"]]
  
  if(is.null(count_table)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(count_table$percent, "%"), 
             subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
             icon = icon("frown"))
  }
})

output$topCriticism3 <- renderValueBox({
  
  # fetch from function
  
  count_table = returnTopComments(passData()[["currentData"]], 3, "Improve")[["return_table"]]
  
  if(is.null(count_table)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    valueBox(value = paste0(count_table$percent, "%"), 
             subtitle = HTML(paste0(count_table$Super, ":<br>", count_table$Category)),
             icon = icon("frown"))
  }
})

# change in criticism

output$changeCriticism <- renderValueBox({
  
  req(passData()[["comparisonData"]])
  
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
  
  if(nrow(difference_table) == 0 | is.na(difference_table$difference)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    biggest_difference <- difference_table$difference
    
    if(biggest_difference > 0){
      
      biggest_difference <- paste0("+", biggest_difference)
    }
    
    valueBox(value = paste0(biggest_difference, "%"), 
             subtitle = HTML(paste0("<p title = 'Largest change in category (green increase, red decrease)'>", 
                                    difference_table$Super, ":<br>", difference_table$Category, "</p>")),
             color = ifelse(difference_table$difference >= 0, "green", "red"),
             icon = icon("frown"))
  }
})

# fifth row----

# this is a reactive which returns the top and bottom score, and the biggest change score

highLowScoreChange <- reactive({
  
  req(passData()[["comparisonData"]])
  
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
  
  req(highLowScoreChange()[["high_score"]])
  
  top_score <- round(as.numeric(highLowScoreChange()[["high_score"]]), 1)
  
  top_score_name = names(highLowScoreChange()[["high_score"]])
  
  valueBox(value = paste0(top_score, "%"),
           subtitle = HTML(paste0("<p title = 'Highest score'>", 
                                  top_score_name, "</p>")),
           color = "green")
})

output$lowestScore <- renderValueBox({
  
  req(highLowScoreChange()[["low_score"]])
  
  low_score <- round(as.numeric(highLowScoreChange()[["low_score"]]), 1)
  
  low_score_name = names(highLowScoreChange()[["low_score"]])
  
  valueBox(value = paste0(low_score, "%"),
           subtitle = HTML(paste0("<p title = 'Lowest score'>", 
                                  low_score_name, "</p>")),
           color = "red")
})

output$biggestIncrease <- renderValueBox({
  
  req(highLowScoreChange()[["biggest_increase"]])
  
  biggest_increase = round(as.numeric(highLowScoreChange()[["biggest_increase"]]), 1)
  
  if(is.na(biggest_increase)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    if(biggest_increase > 0){
      
      biggest_increase <- paste0("+", biggest_increase)
    }
    
    biggest_increase_name = names(highLowScoreChange()[["biggest_increase"]])
    
    valueBox(value = paste0(biggest_increase, "%"),
             subtitle = HTML(paste0("<p title = 'Largest increase (or smallest decrease)'>", 
                                    biggest_increase_name, "</p>")),
             color = "green")
  }
})

output$biggestDecrease <- renderValueBox({
  
  req(highLowScoreChange()[["biggest_decrease"]])
  
  biggest_decrease = round(as.numeric(highLowScoreChange()[["biggest_decrease"]]), 1)
  
  if(is.na(biggest_decrease)){
    
    valueBox(value = "Error", 
             subtitle = HTML("Not enough<br>data"))
  } else {
    
    if(biggest_decrease > 0){
      
      biggest_decrease <- paste0("+", biggest_decrease)
    }
    
    biggest_decrease_name = names(highLowScoreChange()[["biggest_decrease"]])
    
    valueBox(value = paste0(biggest_decrease, "%"),
             subtitle = HTML(paste0("<p title = 'Largest decrease (or smallest increase)'>", 
                                    biggest_decrease_name, "</p>")),
             color = "red")
  }
})

