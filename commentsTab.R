
# criticality tables----

output$impCritTable = renderDT({
  
  impVariable = passData()$ImpCrit
  
  impVariable = factor(impVariable, levels = 1:3)
  
  impFrame = data.frame(table(impVariable))
  
  names(impFrame) = c("Criticality", "Frequency")
  
  impFrame$Criticality = c("Mildly critical", "Fairly Critical", "Highly Critical")
  
  impFrame},
  
  selection = 'single', rownames = FALSE, extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

output$bestCritTable = renderDT({
  
  bestVariable = passData()$BestCrit
  
  bestVariable = factor(bestVariable, levels = 1:3)
  
  bestFrame = data.frame(table(bestVariable))
  
  names(bestFrame) = c("Criticality", "Frequency")
  
  bestFrame$Criticality = c("Mildly Complimentary", "Fairly Complimentary", "Highly Complimentary")
  
  bestFrame},
  selection = 'single', rownames = FALSE, extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

# generate sub tables----

subCategories = function(y){ # this is a function that calculates
  # subcategories given a supercategory string
  
  if(input$commentsTab == "What could be improved?"){
    
    variableName = c("Imp1", "Imp2")
  }
  
  if(input$commentsTab == "Best thing"){
    
    variableName = c("Best1", "Best2")
  }
  
  tableData = passData() %>%
    filter(UQ(sym(variableName[1])) %in% categoriesTable$Number) %>%
    filter(UQ(sym(variableName[2])) %in% categoriesTable$Number)
  
  impCodes = c(
    tableData %>%
      filter(!is.na(UQ(sym(variableName[1]))) & UQ(sym(variableName[1])) < 4000) %>%
      pull(UQ(sym(variableName[1]))),
    tableData %>%
      filter(!is.na(UQ(sym(variableName[2]))) & UQ(sym(variableName[2])) < 4000) %>%
      pull(UQ(sym(variableName[2])))
  )
  
  data.frame(
    "Category" =
      staffCategories %>% # rownames made from
      filter(Super == y) %$% # the unique values below
      unique(Category),
    "Percent" =
      staffCategories %>% # for unique values of subcategory
      filter(Super == y) %$% # that match the supercategory
      unique(Category) %>% # given by string y
      
      map(function(x) {
        staffCategories %>% # for every category pull all the numbers
          filter(Category == x) %>% # related to it
          pull(Number)
      }) %>%
      
      map_dbl(function(x) {
        
        sum(impCodes %in% x) # then add up how many times
        # the codes feature those numbers
      })
  ) %>%
    mutate(Percent = # now we change the column to be a proportion
             round(Percent / sum(Percent) * 100, 1)) %>% # of the column sums
    arrange(desc(Percent))
}

# generate super tables----

superCategories = function(){
  
  # this runs three ways, one for each tab box
  # relevant variables are TeamDoBetter, TeamDoWell, Improvements
  
  # make the variable names
  
  if(input$commentsTab == "What could be improved?"){
    
    variableName = c("Imp1", "Imp2")
  }
  
  if(input$commentsTab == "Best thing"){
    
    variableName = c("Best1", "Best2")
  }
  
  # remove invalid categories
  
  tableData = passData() %>%
    filter(Imp1 %in% staffCategories$Number) %>%
    filter(Imp2 %in% staffCategories$Number)
  
  impCodes = c(
    tableData %>%
      filter(!is.na(UQ(sym(variableName[1]))) & UQ(sym(variableName[1])) < 4000) %>%
      pull(UQ(sym(variableName[1]))),
    tableData %>%
      filter(!is.na(UQ(sym(variableName[2]))) & UQ(sym(variableName[2])) < 4000) %>%
      pull(UQ(sym(variableName[2])))
  )
  
  data.frame(
    "Category" = unique(staffCategories$Super),
    "Percentage" = unique(staffCategories$Super) %>% # for unique values of supercategory
      map(function(x) {
        staffCategories %>%
          filter(Super == x) %>% # just pull the numbers for that supercategory
          pull(Number)
      }) %>%
      map_dbl(function(x) {
        
        round(sum(impCodes %in% x) %>% # find how many there are and divide by the total
                `/`(., length(impCodes)) * 100, 1)
      })
  ) %>%
    arrange(desc(Percentage))
}

# draw super/ sub improve/ best tables----

output$SuperTableImprove = renderDT({
  
  superCategories()
}, selection = 'single', rownames = FALSE, extensions = 'Buttons', 
options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20),
               dom = 'Blfrtip',
               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

output$SubTableImprove = renderDT({
  
  if(input$commentsTab == "What could be improved?"){
    
    req(!is.null(input$SuperTableImprove_rows_selected))
  }
  
  if(input$commentsTab == "Best thing"){
    
    req(!is.null(input$SuperTableBest_rows_selected))
  }
  
  rowIndices = ifelse(input$commentsTab == "What could be improved?",
                      input$SuperTableImprove_rows_selected,
                      input$SuperTableBest_rows_selected)
  
  subCategories(
    superCategories()$Category[rowIndices]
  )
  
}, selection = 'single', rownames = FALSE, extensions = 'Buttons', 
options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20),
               dom = 'Blfrtip',
               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

output$SuperTableBest = renderDT({
  
  superCategories()
}, selection = 'single', rownames = FALSE, extensions = 'Buttons', 
options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20),
               dom = 'Blfrtip',
               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

output$SubTableBest = renderDT({
  
  if(input$commentsTab == "What could be improved?"){
    
    req(!is.null(input$SuperTableImprove_rows_selected))
  }
  
  if(input$commentsTab == "Best thing"){
    
    req(!is.null(input$SuperTableBest_rows_selected))
  }
  
  rowIndices = ifelse(input$commentsTab == "What could be improved?",
                      input$SuperTableImprove_rows_selected,
                      input$SuperTableBest_rows_selected)
  
  subCategories(
    superCategories()$Category[rowIndices]
  )
  
}, selection = 'single', rownames = FALSE, extensions = 'Buttons', 
options = list(pageLength = 5, lengthMenu = c(5, 10, 15, 20),
               dom = 'Blfrtip',
               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))


# draw tables sorted on subcategory

output$subCategoryTableImprove <- renderDT({
  
  subCategoryTableFunction()
  
}, selection = 'single', rownames = FALSE, extensions = 'Buttons', 
options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
               dom = 'Blfrtip',
               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

output$subCategoryTableBest <- renderDT({
  
  subCategoryTableFunction()
  
}, selection = 'single', rownames = FALSE, extensions = 'Buttons', 
options = list(pageLength = 10, lengthMenu = c(5, 10, 15, 20),
               dom = 'Blfrtip',
               buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

subCategoryTableFunction <- reactive({
  
  if(input$commentsTab == "What could be improved?"){
    
    check1 <- passData() %>% 
      filter(!is.na(Imp1)) %>% 
      left_join(categoriesTable, by = c("Imp1" = "Number")) %>% 
      select(Category, Super, Imp1)
    
    check2 <- passData() %>% 
      filter(!is.na(Imp2)) %>% 
      left_join(categoriesTable, by = c("Imp2" = "Number")) %>% 
      select(Category, Super, Imp1)
  }
  
  if(input$commentsTab == "Best thing"){
    
    check1 <- passData() %>% 
      filter(!is.na(Best1)) %>% 
      left_join(categoriesTable, by = c("Best1" = "Number")) %>% 
      select(Category, Super, Imp1)
    
    check2 <- passData() %>% 
      filter(!is.na(Best2)) %>% 
      left_join(categoriesTable, by = c("Best2" = "Number")) %>% 
      select(Category, Super, Imp1)
  }

  check_final <- rbind(check1, check2)
  
  check_final <- check_final %>% 
    mutate(Category = paste0(Super, ": ", Category))
  
  count_table <- check_final %>% 
    filter(!is.na(Super), !is.na(Category)) %>% 
    group_by(Category) %>% 
    count()
  
  count_sum <- sum(count_table$n)
  
  return(
    count_table %>% 
      mutate(percent = round(n / count_sum * 100, 1)) %>% 
      arrange(-percent)
  )
})

# generate text----

output$filterText = renderText({ # this is for the category tables
  
  if(input$commentsTab == "What could be improved?"){
    
    req(!is.null(input$SubTableImprove_rows_selected))
  }
  
  if(input$commentsTab == "Best thing"){
    
    req(!is.null(input$SubTableBest_rows_selected))
  }
  
  superRow = ifelse(input$commentsTab == "What could be improved?",
                    input$SuperTableImprove_rows_selected,
                    input$SuperTableBest_rows_selected)
  
  superCategorySelected =
    as.character(
      superCategories()$Category[superRow]
    )
  
  wholeTable = subCategories(
    superCategorySelected
  )
  
  # now find the one they clicked
  
  subRow = ifelse(input$commentsTab == "What could be improved?",
                  input$SubTableImprove_rows_selected,
                  input$SubTableBest_rows_selected)
  
  theClick = as.character(wholeTable$Category[subRow])
  
  theNumbers = staffCategories %>% # for unique values of subcategory
    filter(Super == superCategorySelected) %>% # that match the supercategory
    filter(Category == theClick) %>% # that match the subcategory
    pull(Number)
  
  if(input$commentsTab == "What could be improved?"){
    
    variableName = c("Imp1", "Imp2")
    
    longName = "Improve"
  }
  
  if(input$commentsTab == "Best thing"){
    
    variableName = c("Best1", "Best2")
    
    longName = "Best"
  }
  
  theComments = filter(passData(),
                       UQ(sym(variableName[1])) %in% theNumbers |
                         UQ(sym(variableName[2])) %in% theNumbers) %>%
    filter(!is.na(UQ(sym(longName)))) %>% 
    pull(UQ(sym(longName)))
  
  HTML(
    paste(
      map_chr(theComments,
              function(x) paste0("<p>", x, "</p>")
      ), collapse = "")
  )
})

output$filterTextSubcategory <- renderText({
  
  if(input$commentsTab == "What could be improved?"){
    
    req(!is.null(input$subCategoryTableImprove_rows_selected))
    
    variableName = c("Imp1", "Imp2")
    
    longName = "Improve"
  }
  
  if(input$commentsTab == "Best thing"){
    
    req(!is.null(input$subCategoryTableBest_rows_selected))
    
    variableName = c("Best1", "Best2")
    
    longName = "Best"
  }
  
  wholeTable = subCategoryTableFunction()
  
  # now find the one they clicked
  
  subRow <- ifelse(input$commentsTab == "What could be improved?",
                  input$subCategoryTableImprove_rows_selected,
                  input$subCategoryTableBest_rows_selected)
  
  theClick  <- as.character(wholeTable$Category[subRow])
  
  theClick <- strsplit(theClick, ": ")[[1]]
  
  relevant_comment_codes <- categoriesTable %>% 
    filter(Super == theClick[1]) %>% 
    filter(Category == theClick[2]) %>% 
    pull(Number)
  
  theComments = filter(passData(),
                       UQ(sym(variableName[1])) %in% relevant_comment_codes |
                         UQ(sym(variableName[2])) %in% relevant_comment_codes) %>%
    filter(!is.na(UQ(sym(longName)))) %>% 
    pull(UQ(sym(longName)))
  
  HTML(
    paste(
      map_chr(theComments,
              function(x) paste0("<p>", x, "</p>")
      ), collapse = "")
  )
})

output$filterTextCrit = renderText({ # this is for the criticality tables
  
  if(input$commentsTab == "What could be improved?"){
    
    req(!is.null(input$impCritTable_rows_selected))
  }
  
  if(input$commentsTab == "Best thing"){
    
    req(!is.null(input$bestCritTable_rows_selected))
  }
  
  theClick = ifelse(input$commentsTab == "What could be improved?",
                    input$impCritTable_rows_selected,
                    input$bestCritTable_rows_selected)
  
  if(input$commentsTab == "What could be improved?"){
    
    variableName = c("ImpCrit")
    
    longName = "Improve"
  }
  
  if(input$commentsTab == "Best thing"){
    
    variableName = c("BestCrit")
    
    longName = "Best"
  }
  
  theComments = filter(passData(),
                       UQ(sym(variableName)) %in% theClick |
                         UQ(sym(variableName)) %in% theClick) %>%
    pull(UQ(sym(longName)))
  
  HTML(
    paste(
      map_chr(theComments,
              function(x) paste0("<p>", x, "</p>")
      ), collapse = "")
  )
})

