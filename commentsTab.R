### return list of PO and PALS stories

commentData = reactive({
  
  # fix the time variable
  
  if(is.null(input$Division)){ # if the whole Trust is selected do this
    
    passPO = PO[PO$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
    passPALS = PALS[PALS$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
    
  } else { # otherwise look at the directorate code
    
    if(is.null(input$selDirect)){ # if all directorates are selected do this
      
      passPO = PO[PO$Division %in% as.numeric(input$Division) &
                    PO$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      
      passPALS = PALS[PALS$Division %in% as.numeric(input$Division) &
                        PALS$Date %in%
                        seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      
    }
    
    if(!is.null(input$selTeam)){
      
      passPO = PO[PO$TeamC %in% as.numeric(input$selTeam) &
                    PO$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      passPALS = PALS[PALS$TeamC %in% as.numeric(input$selTeam) &
                        PALS$Date %in%
                        seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      
    }
    
    if(is.null(input$selTeam)){
      
      passPO = PO[PO$Directorate %in% as.numeric(input$selDirect) &
                    PO$Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      passPALS = PALS[PALS$Directorate %in% as.numeric(input$selDirect) &
                        PALS$Date %in%
                        seq.Date(input$dateRange[1], input$dateRange[2], by = "days"),]
      
    }
    
  }
  
  list(passPO, passPALS)
})

### criticality tables

output$impCritTable = renderDT({
  
  impVariable = passData()[["suce"]]$ImpCrit
  
  impVariable = factor(impVariable, levels = 1:3)
  
  impFrame = data.frame(table(impVariable))
  
  names(impFrame) = c("Criticality", "Frequency")
  
  impFrame$Criticality = c("Mildly critical", "Fairly Critical", "Highly Critical")
  
  impFrame},
  
  selection = 'single', rownames = FALSE, extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

output$bestCritTable = renderDT({
  
  bestVariable = passData()[["suce"]]$BestCrit
  
  bestVariable = factor(bestVariable, levels = 1:3)
  
  bestFrame = data.frame(table(bestVariable))
  
  names(bestFrame) = c("Criticality", "Frequency")
  
  bestFrame$Criticality = c("Mildly Complimentary", "Fairly Complimentary", "Highly Complimentary")
  
  bestFrame},
  selection = 'single', rownames = FALSE, extensions = 'Buttons', 
  options = list(dom = 'Blfrtip',
                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))

### subcategory tables

subCategories = function(y){ # this is a function that calculates
  # subcategories given a supercategory string
  
  if(input$commentsTab == "What could be improved?"){
    
    variableName = c("Imp1", "Imp2")
  }
  
  if(input$commentsTab == "Best thing"){
    
    variableName = c("Best1", "Best2")
  }
  
  tableData = passData()[["suce"]] %>%
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

# generate table - improve one thing

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
  
  tableData = passData()[["suce"]] %>%
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
  
  theComments = filter(passData()[["suce"]],
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
  
  # here
  
  theComments = filter(passData()[["suce"]],
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


# output for the text responses

output$TextResponses <- renderText({
  
  print(HTML(myComments()))
  
})

# return the contents of a custom text search

myComments = reactive({
  withProgress(
    message = 'Please wait',
    detail = 'Fetching comments...', value = 0, {
      
      # if they searched by demographic, scrub PO and PALS data
      
      if(input$sex != "All" | input$ethnic != "All" |
         input$disability != "All" | input$religion != "All" |
         input$sexuality != "All" | input$age != "All") {
        
        searchDemographic = TRUE
      } else {
        
        searchDemographic = FALSE
      }
      
      # make empty list and add all four comment types
      
      storyList = list()
      
      if(!is.null(input$criticality)){
        
        # if they select by criticality or demographic
        # we need only return SUCE comments
        
        impCriticality = input$criticality[input$criticality %in% 1:3]
        
        bestCriticality = input$criticality[input$criticality %in% 4:6]
        
        # we'll make a whole list about which to return and the values within each
        
        returnComments = list("Improve" = ifelse(length(impCriticality) > 0, TRUE, FALSE),
                              "Best" = ifelse(length(bestCriticality) > 0, TRUE, FALSE),
                              "Other" = FALSE)
        
      } else if(searchDemographic){
        
        returnComments = list("Improve" = TRUE, "Best" = TRUE, "Other" = FALSE)
        
        impCriticality = numeric(0)
        
        bestCriticality = numeric(0)
        
      } else {
        
        returnComments = list("Improve" = TRUE, "Best" = TRUE, "Other" = TRUE)
        
        impCriticality = numeric(0)
        
        bestCriticality = numeric(0)
      }
      
      # note that this is going to return a list of DATAFRAMES
      
      if("Improve" %in% input$stories & returnComments[["Improve"]]){
        
        # filter by criticality here
        
        toaddFrame = passData()[["suce"]][, c("Location", "Improve", "Imp1", "ImpCrit")]
        
        if(length(impCriticality) > 0){
          
          toaddFrame = subset(toaddFrame, ImpCrit %in% as.numeric(impCriticality))
          
        }
        
        storyList = lappend(storyList, toaddFrame)
        
      }
      
      if("Best" %in% input$stories & returnComments[["Best"]]){
        
        # filter by criticality here
        
        toaddFrame = passData()[["suce"]][, c("Location", "Best", "Best1", "BestCrit")]
        
        if(length(bestCriticality) > 0){
          
          toaddFrame = subset(toaddFrame, BestCrit %in% (as.numeric(bestCriticality) - 3))
          
        }
        
        storyList = lappend(storyList, toaddFrame)
        
      }
      
      if("PO" %in% input$stories & returnComments[["Other"]]){
        
        storyList = lappend(storyList, commentData()[[1]][, c("Location", "Title", "PO")])
      }
      
      if("PALS" %in% input$stories & returnComments[["Other"]]){
        
        storyList = lappend(storyList, commentData()[[2]][, c("Location", "PALS")])
      }
      
      theNames = c(ifelse(returnComments[["Improve"]] & "Improve" %in% input$stories, "Improve", NA),
                   ifelse(returnComments[["Best"]] & "Best" %in% input$stories, "Best", NA),
                   ifelse(returnComments[["Other"]] & "PO" %in% input$stories, "PO", NA),
                   ifelse(returnComments[["Other"]] & "PALS" %in% input$stories, "PALS", NA))
      
      names(storyList) = theNames[!is.na(theNames)]
      
      incProgress(1/3)
      
      # remove missing data
      
      storyList = lapply(storyList, function(x)
        x[!is.na(x[, names(x) %in% c("Improve", "Best", "PO", "PALS")]), ])
      
      if(length(unlist(storyList)) == 0){
        
        return("No comments within the chosen time and area, please broaden your search")
      }
      
      # find the "nothing could be improved" comments and remove
      
      if("Improve" %in% names(storyList)){
        
        if(nrow(storyList$Improve) > 0){
          
          countRecode = sum(storyList[["Improve"]][["Improve"]] %in% recodelist$Recode, na.rm = TRUE)
          
          countRecode = countRecode + sum(storyList[["Improve"]][["Imp1"]] == 4444, na.rm = TRUE)
          
        } else {
          countRecode = 0
        }
        
      } else {
        countRecode = 0
      }
      
      # this bit removes the "Nothing to improve" comments
      
      if("Improve" %in% unlist(input$stories)){
        
        storyList[["Improve"]] = storyList$Improve[!storyList$Improve$Improve %in%
                                                     c(recodelist$Recode, recodelist$Omit), ]
        
        storyList[["Improve"]] = storyList$Improve[!storyList$Improve$Imp1 == 4444 |
                                                     is.na(storyList$Improve$Imp1), ]
        
      }
      
      if("Best" %in% unlist(input$stories)){
        
        storyList[["Best"]] = storyList[["Best"]][!storyList[["Best"]] %in% c(recodelist$Omit)]
        
      }
      
      storyName = recode(names(storyList),
                         "Improve" = "Survey- What could we do better",
                         "Best" = "Survey- What did we do well",
                         "PO" = "Care Opinion")
      
      incProgress(1/3)
      
      if(length(unlist(storyList)) == 0){
        
        return("No comments in this time period!")
        
      }
      
      # we're going to kick them out with an error if they searched by demographic and there are <10 comments
      
      if(length(unlist(storyList)) < 30 & searchDemographic) {
        
        showModal(modalDialog(
          title = "Error",
          "Sorry, you searched on patient characteristics and there are
          fewer than 30 comments- individuals may be identifiable. Please
          broaden your search"
        ))
        
        return()
      }
      
      # set search to FALSE, we'll change later if they do search
      
      search = FALSE
      
      if(length(input$taxonomy) > 0){
        
        if(input$taxonomy != "None"){
          
          storyList = lapply(storyList, function(x)
            x[grep(
              paste0(
                paste0("\\b",
                       taxonomy[, input$taxonomy][!is.na(taxonomy[, input$taxonomy])],
                       collapse = "\\b|"), "\\b"),
              x[, names(x) %in% c("Improve", "Best", "PO", "PALS")],
              perl = TRUE, ignore.case = TRUE), ])
          
          # remember that we searched to remove "X number of comments that nothing to be improved"
          
          search = TRUE
          
        }
        
      }
      
      if(length(grep("[[:alpha:]]", input$keyword)) != 0){ # if keyword search is not blank then do this
        
        storyList = lapply(storyList, function(x)
          x[grep(input$keyword, x[, names(x) %in% c("Improve", "Best", "PO", "PALS")], ignore.case = TRUE), ])
        
        search = TRUE
        
      }
      
      # this tests how many comments are left at the end
      
      if(length(unlist(storyList)) > 6000){ # if too many results then do this
        
        return(as.character(div(HTML("<h3>Too many comments in this time and area,
                                   please narrow your search</h3>"))))
        
      }
      
      storyVec = unlist(lapply(storyList, nrow)) > 0
      
      toPrint = lapply((1 : length(storyList))[storyVec], function(x){
        
        if(names(storyList)[x] == "PO"){
          
          paste0("<h4>", storyList[[x]]$Title, "</h4>", storyList[[x]][, names(storyList)[x]],
                 " (", storyList[[x]]$Location, ")")
          
        } else {
          
          paste0(storyList[[x]][, names(storyList)[x]],
                 " (", storyList[[x]]$Location, ")")
          
        }
      })
      
      toPrint = lapply(toPrint, function(x) gsub("\\(NA)", "", x))
      
      names(toPrint) = storyName[storyVec]
      
      incProgress(1/3)
      
      return(
        unlist(
          lapply(
            names(toPrint), function(x)
              c(
                paste0(
                  "<h2>", x, "</h2>",
                  ifelse(x == "Survey- What could we do better" & countRecode > 0 & search == FALSE,
                         paste("<h4>We received ", countRecode,
                               " comments which stated that nothing could be improved.</h4>"),
                         "") # print nothing if recode value is 0
                ),
                paste0("<p>", toPrint[[x]], "</p>")
              )
          )
        )
      )
    })
  
})

