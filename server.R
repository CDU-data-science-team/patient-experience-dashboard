
library(shinydashboard)
library(scales)
library(knitr)
library(tidyverse)
library(rmarkdown)
library(DT)
library(lubridate)
library(pander)
library(magrittr)
library(tidytext)
library(igraph)
library(ggraph)
library(stringi)
library(stringr)

# lappend

lappend = function(lst, obj){
  
  lst[[length(lst) + 1]] <- obj
  
  return(lst)
}

# blank ggplot2 theme

new_theme_empty <- theme_bw()
new_theme_empty$line <- element_blank()
new_theme_empty$rect <- element_blank()
new_theme_empty$strip.text <- element_blank()
new_theme_empty$axis.text <- element_blank()
new_theme_empty$plot.title <- element_blank()
new_theme_empty$axis.title <- element_blank()
new_theme_empty$plot.margin <- structure(c(0, 0, -1, -1), unit = "lines", valid.unit = 3L, 
                                         class = "unit")

### Define server logic

function(input, output, session){
  
  source("summaryTab.R", local = TRUE)
  source("scoresTab.R", local = TRUE)
  source("commentsTab.R", local = TRUE)
  source("allCommentsTab.R", local = TRUE)
  source("textAnalysis.R", local = TRUE)
  source("sentimentTab.R", local = TRUE)
  
  # handle reactive UI from division selection
  
  output$divControls <- renderUI({
    
    # if directorate is empty return all directorates
    
    if(is.null(input$Division)){
      
      finalTable = dirTable
      
    } else {
      
      finalTable = dirTable %>%
        filter(Division %in% input$Division)
    }
    
    # get rid of corporate and unknown
    
    finalTable = finalTable %>%
      filter(!DirC %in% c(0, 40))
    
    # finally pull the directorates and names
    
    directorates = finalTable %>%
      pull(DirC)
    
    names(directorates) = finalTable %>%
      pull(DirT)
    
    selectInput("selDirect", "Choose directorate(s)",
                directorates, multiple = TRUE)
    
  })
  
  # handle reactive UI from directorate selection
  
  output$dirControls <- renderUI({
    
    if(is.null(input$selDirect) & !input$showTeams) return()
    
    if(input$showTeams){
      
      team_join <- trustData %>% 
        filter(Date > input$dateRange[1], Date < input$dateRange[2]) %>% 
        distinct(TeamC)
      
    } else {
      
      team_join <- trustData %>% 
        filter(Directorate %in% input$selDirect) %>% 
        filter(Date > input$dateRange[1], Date < input$dateRange[2]) %>% 
        distinct(TeamC)
    }
    
    teams <- team_join %>% 
      inner_join(
        counts %>% 
          group_by(TeamC) %>%
          slice(which.max(as.Date(date_from)))
      )
    
    if(nrow(teams) < 1) return()
    
    ### removing all missing names and sort
    
    teams = teams %>% 
      filter(!is.na(TeamN)) %>% 
      arrange(TeamN)
    
    team_numbers <- teams %>% pull(TeamC)
    
    names(team_numbers) <- teams %>% pull(TeamN)
    
    selectInput("selTeam", "Choose team(s)",
                team_numbers, multiple = TRUE, selected = "All")
  })
  
  # handle reactive list containing variable and column names
  # ready to pass to UI and graphics commands
  
  myQuestions = reactive({
    
    if(is.null(input$Division)){
      
      questions = as.list(questionFrame[questionFrame$trust == 1, "code"])
      
      names(questions) = questionFrame[questionFrame$trust == 1, "value"]
      
      return(questions)
    }
    
    # create an empty list
    
    questions = list()
    
    if(0 %in% input$Division | 1 %in% input$Division){
      
      addQuestions = as.list(questionFrame[questionFrame$local == 1, "code"])
      
      names(addQuestions) = questionFrame[questionFrame$local == 1, "value"]
      
      questions = c(questions, addQuestion)
    }
    
    if(2 %in% input$Division){
      
      addMoreQuestions = as.list(questionFrame[questionFrame$hp == 1, "code"])
      
      names(addMoreQuestions) = questionFrame[questionFrame$hp == 1, "value"]
      
      questions = c(questions, addMoreQuestions)
    } 
    
    return(questions)
  })
  
  # handle reactive UI from taxonomy selector
  
  output$serverTaxonomy = renderUI({
    
    selectInput("taxonomy", "Select comment theme",  c("None", names(taxonomy)))
  })
  
  # generate the dataframe based on location, time, and demographics
  
  passData = reactive({
    
    # select carer, service user, or both
    
    finalData = trustData
    
    # deal with demographics
    
    if(as.numeric(input$responder) != 9){
      
      finalData = finalData[finalData$SU %in% as.numeric(input$responder), ]
    }
    
    if(input$sex != "All") finalData = finalData[finalData$Gender %in% input$sex, ]
    
    if(input$commInp == "community") finalData = finalData[finalData$TeamC %in% communityTeams, ]
    
    if(input$commInp == "inpatient") finalData = finalData[finalData$TeamC %in% inpatientTeams, ]
    
    if(input$ethnic != "All") finalData = finalData[finalData$Ethnic %in% input$ethnic, ]
    
    if(input$disability != "All") finalData = finalData[grep(input$disability, finalData$Disability), ]
    
    if(input$religion != "All") finalData = finalData[finalData$Religion %in% input$religion, ]
    
    if(input$sexuality != "All") finalData = finalData[finalData$Sexuality %in% input$sexuality, ]
    
    if(input$age != "All") finalData = finalData[finalData$Age %in% input$age, ]
    
    # deal with the date
    
    finalData = finalData[finalData$Date %in% seq.Date(input$dateRange[1],
                                                       input$dateRange[2], by = "days"), ]
    
    # now filter for every available filter
    
    if(!is.null(input$Division)){ # if the whole Trust is selected do this
      
      finalData = finalData[!is.na(finalData$Division) &
                              finalData$Division %in% input$Division &
                              finalData$Date %in% seq.Date(input$dateRange[1],
                                                           input$dateRange[2], by = "days"), ]
    }
    
    if(!is.null(input$selDirect)){ # otherwise look at the directorate code
      
      finalData = finalData[!is.na(finalData$Directorate) &
                              finalData$Directorate %in% as.numeric(input$selDirect) &
                              finalData$Date %in% seq.Date(input$dateRange[1],
                                                           input$dateRange[2], by = "days"), ]
      
    }
    if(!is.null(input$selTeam)){
      
      finalData = finalData[!is.na(finalData$TeamC) &
                              finalData$TeamC %in% as.numeric(input$selTeam) &
                              finalData$Date %in% seq.Date(input$dateRange[1],
                                                           input$dateRange[2], by = "days"), ]
    }
    
    if(input$carerSU == "SU"){
      
      finalData = finalData %>%
        filter(is.na(formtype) | formtype != "SUCE")
      
    } else if(input$carerSU == "carer"){
      
      finalData = finalData %>%
        filter(formtype == "carer")
      
    } else if(input$carerSU == "bothCarerSU"){
      
      # nothing!
    }

    return(finalData)
  })
  
  
  # download the numbers from the stacked barchart
  # note this function returns a list- the output of prop.table and table
  
  myStackTable = reactive({
    
    if(is.null(passData())){
      
      mygraph = data.frame("Not", "Enough", "Data", "!")
      
    } else {
      
      if(input$carerSU == "carer" & !input$custom){
        
        theQuestions = as.list(questionFrame[questionFrame$carers == 1, "code"])
        
        names(theQuestions) = questionFrame[questionFrame$carers == 1, "value"]
        
      } else {
        
        theQuestions = input$selQuestions
      }
      
      # remove decimals from historic data
      
      fixedData = data.frame(apply(passData()[, unlist(theQuestions)], 1:2,
                                   function(x) round(x + .01)))
      
      # count the missing responses
      
      missnum = apply(fixedData, 2, function(x) sum(!is.na(x)))
      
      # this is the table() bit
      
      mytable = data.frame(
        lapply(names(missnum[missnum > 2]), function(x)
          table(factor(fixedData[, x], levels = 1:5)))
      )[, seq(2, length(missnum[missnum > 2]) * 2, 2)]
      
      names(mytable) = names(myQuestions()[which(unlist(myQuestions()) %in%
                                                   names(missnum[missnum > 2]))])
      
      rownames(mytable) = rev(c("Excellent", "Good", "Fair", "Poor", "Very poor"))
      
      # this is the prop.table() bit
      
      mygraph = data.frame(
        lapply(names(missnum[missnum > 2]), function(x)
          round(prop.table(table(factor(fixedData[, x], levels = 1:5)))*100, 1))
      )[, seq(2, length(missnum[missnum > 2]) * 2, 2)]
      
      names(mygraph) = names(myQuestions()[which(unlist(myQuestions()) %in%
                                                   names(missnum[missnum > 2]))])
      
      rownames(mygraph) = rev(c("Excellent", "Good", "Fair", "Poor", "Very poor"))
      
      # put them in a list
      
      theOutput = list("rawNumbers" = mytable, "proportions" = mygraph)
      
    }
    
    theOutput
  })
  
  # download table from stacked chart
  
  output$downloadData.stackTable = downloadHandler(
    filename <- function(){
      paste('StackTable', Sys.Date(), '.doc', sep='')},
    
    content <- function(file){
      cat("Raw numbers\n", file = file)
      print(xtable(myStackTable()[["rawNumbers"]]),
            type="html", file = file, include.rownames = TRUE, append = TRUE)
      cat("Proportions\n", file = file, append = TRUE)
      print(xtable(myStackTable()[["proportions"]]),
            type="html", file = file, include.rownames = TRUE, append = TRUE)
      
    }
  )
  
  # download graphs buttons
  
  output$downloadData.stack <- downloadHandler(
    filename <- function() {
      paste('Stack_plot', Sys.Date(),'.png',sep='') },
    content <- function(file) {
      png(file, width = 980, height = 400, units = "px", pointsize = 12,
          bg = "white", res = NA)
      stack.plot <- myStack()
      print(stack.plot)
      dev.off()},
    contentType = 'image/png'
  )
  
  # download trend graph
  
  output$downloadData.trend <- downloadHandler(
    filename <- function() {
      paste('Trend_plot', Sys.Date(),'.png',sep='') },
    content <- function(file) {
      png(file, width = 980, height = 400, units = "px", pointsize = 12,
          bg = "white", res = NA)
      trend.plot <- myTrend()
      print(trend.plot)
      dev.off()},
    contentType = 'image/png'
  )
  
  # download comments
  
  output$downloadData.comments <- downloadHandler(
    filename <- function(){
      paste('Comments', Sys.Date(), '.doc', sep='') },
    content <- function(file){
      
      comments <- myComments()
      sink(file)
      cat(gsub("<(.|\n)*?>", "\r\n", comments))
      sink()
      
    })
  
  # download improve table
  
  output$downloadData.imptable <- downloadHandler(
    filename <- function(){
      paste('ImproveTable', Sys.Date(), '.doc', sep='')},
    
    content <- function(file){
      print(xtable(myImprove()), type="html", file = file, include.rownames = FALSE)
      
    })
  
  # download best table
  
  output$downloadData.besttable <- downloadHandler(
    
    filename <- function(){
      paste('BestTable', Sys.Date(), '.doc', sep='')},
    
    content <- function(file){
      print(xtable(myBest()), type = "html", file = file, include.rownames = FALSE)
      
    })
  
  ###  download data
  
  output$downloadData <-
    downloadHandler(filename = "data.csv",
                    content = function(file){
                      
                      if(nrow(passData()) > 10000){
                        
                        createAlert(session, anchorId = "downloadDataAlert", title = "Error",
                                    content = "Too much data, please reduce the size of your request.", append = FALSE)
                        
                      } else {
                        
                        # find non empty columns
                        
                        theColumns = apply(passData(), 2, function(x) sum(!is.na(x))) > 0
                        
                        writeData = passData()[, theColumns]
                        
                        # remove data columns
                        
                        dataColumns = names(writeData) %in%
                          c("key", "paperindex", "addedby", "refused",
                            "Doctor", "Nurse", "SW", "Psycho",
                            "Therapist", "OtherName", "OtherValue",
                            "Optout", "date_from", "date_from",
                            "date_to", "Contacts", "Gender",
                            "Ethnic", "Disability", "Religion",
                            "Sexuality", "Age", "Relationship",
                            "Pregnant", "Baby")
                        
                        writeData = writeData[, -which(dataColumns)]
                        
                        write.table(writeData, file = "alldata.csv", row.names = FALSE,
                                    col.names = TRUE, sep = ",")
                        
                        # copy csv to 'file'
                        
                        file.copy("alldata.csv", file, overwrite = TRUE)
                        
                      }
                      
                      
                    }
    )
  
  ### feedback tracker - three for three divisions
  
  output$downloadFeedbackTrackerLocal <- # local division
    downloadHandler(filename = "feedbackTrackerDiv.docx",
                    content = function(file){
                      
                      passDivision = "Local Partnerships - Mental Healthcare"
                      
                      render("feedbackTrackerDiv.Rmd", output_format = "word_document",
                             output_file = file,
                             quiet = TRUE, envir = environment())
                      
                      # copy docx to 'file'
                      file.copy("feedbackTrackerDiv.docx", file, overwrite = TRUE)
                      
                    }
    )
  
  output$downloadFeedbackTrackerHealthPartnerships <- # HP
    downloadHandler(filename = "feedbackTrackerDiv.docx",
                    content = function(file){
                      
                      passDivision = "Local Partnerships - Community Healthcare"
                      
                      render("feedbackTrackerDiv.Rmd", output_format = "word_document",
                             output_file = file,
                             quiet = TRUE, envir = environment())
                      
                      # copy docx to 'file'
                      file.copy("feedbackTrackerDiv.docx", file, overwrite = TRUE)
                      
                    }
    )
  
  output$downloadFeedbackTrackerForensic <- # forensic division
    downloadHandler(filename = "feedbackTrackerDiv.docx",
                    content = function(file){
                      
                      passDivision = "Forensic division"
                      
                      render("feedbackTrackerDiv.Rmd", output_format = "word_document",
                             output_file = file,
                             quiet = TRUE, envir = environment())
                      
                      # copy docx to 'file'
                      file.copy("feedbackTrackerDiv.docx", file, overwrite = TRUE)
                      
                    }
    )
  
  # show modal with warning if they click "show all teams"
  
  observeEvent(input$showTeams, {
    
    showModal(
      modalDialog(
        title = "Warning!",
        HTML("There are a lot of teams. Search by typing or deselect this control 
              and narrow your search by directorate<br>
             (click anywhere to dismiss this message)."),
        easyClose = TRUE
      )
    )
  }, ignoreInit = TRUE, once = TRUE)
  
}