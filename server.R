
library(shiny)
library(ggplot2)
library(reshape)
library(scales)
library(knitr)
library(xtable)
library(tidyverse)
library(rmarkdown)
library(shinyBS)
library(DT)
library(lubridate)
library(pander)

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

shinyServer(function(input, output, session){
  
  # PO$Title = enc2utf8(PO$Title)
  # 
  # PO$PO = enc2utf8(PO$PO)
  # 
  # PO$Location = enc2utf8(PO$Location)
  
  # handle reactive UI from division selection
  
  output$divControls <- renderUI({
    
    if(is.null(input$Division)) return()
    
    directorates = unlist(directDiv[(as.numeric(input$Division) + 1)])
    
    if(is.null(directorates)) return()
    
    selectInput("selDirect", "Choose directorate(s)",
                directorates, multiple = TRUE)
    
  })
  
  # handle reactive UI from directorate selection
  
  output$dirControls <- renderUI({
    
    if(is.null(input$selDirect)) return()
    
    if(length(input$selDirect) == 1 & input$selDirect == 34){
      
      selectInput("selDistrict", "Choose district(s)",
                  list("Ashfield", "Bassetlaw", "Broxtowe", "Gedling", "Mansfield", 
                       "Newark and Sherwood", "Rushcliffe", "YOT"), multiple = TRUE, selected = "All")
    } else {
      
      teams = names(table(subset(trustData, Directorate %in% input$selDirect &
                                   Date %in% seq.Date(input$dateRange[1],
                                                      input$dateRange[2], by = "days"))$TeamC))
      
      if(is.null(teams)) return()
      
      teams = teams[!teams == 1700]
      
      nameteams = lapply(teams, function(x) tail(trustData$TeamN[which(trustData$TeamC == x)], 1))
      
      names(teams) = nameteams
      
      ### removing all missing names
      
      teams = teams[!is.na(names(teams))]
      
      # if(is.null(teams)) return()
      
      selectInput("selTeam", "Choose team(s)",
                  unlist(list(teams[order(names(teams))])), multiple = TRUE, selected = "All")
    }
    
  })
  
  output$distControls <- renderUI({
    
    # if(is.null(input$Division)) return()
    
    subDistricts = counts %>%
      filter(District %in% input$selDistrict) %>%
      select(SubDistrict) %>%
      distinct()
    
    if(is.null(subDistricts)) return()
    
    selectInput("selSubDistrict", "Choose subdistricts",
                subDistricts, multiple = TRUE)
    
  })
  
  output$subDistrictTeams = renderUI({
    
    teams = trustData %>%
      filter(SubDistrict %in% input$selSubDistrict, 
             Date %in% seq.Date(input$dateRange[1],
                                input$dateRange[2], by = "days")) %>%
      group_by(TeamC) %>%
      summarise(n()) %>%
      select(TeamC) %>%
      unlist(use.names = FALSE)
    
    # teams = names(table(teamsData$TeamC))
    
    if(is.null(teams)) return()
    
    teams = teams[!teams == 1700]
    
    nameteams = lapply(teams, function(x) tail(trustData$TeamN[which(trustData$TeamC == x)], 1))
    
    names(teams) = nameteams
    
    ### removing all missing names
    
    teams = teams[!is.na(names(teams))]
    
    # if(is.null(teams)) return()
    
    # selectInput("selSubDistrictTeam", "Choose team(s)",
    #             unlist(list(teams[order(names(teams))])), multiple = TRUE, selected = "All")
    
    selectInput("selSubDistrictTeam", "Choose team(s)",
                teams, multiple = TRUE, selected = "All")
  })
  
  # handle reactive list containing variable and column names 
  # ready to pass to UI and graphics commands
  
  myQuestions = reactive({
    
    if(is.null(input$Division)) return()
    
    if(input$carerSU == "carer"){
      
      questions = as.list(questionFrame[questionFrame$carers == 1, "code"])
      
      names(questions) = questionFrame[questionFrame$carers == 1, "value"]
      
    } else if(as.numeric(input$Division) == 9){
      
      questions = as.list(questionFrame[questionFrame$trust == 1, "code"])
      
      names(questions) = questionFrame[questionFrame$trust == 1, "value"]
      
    } else if(as.numeric(input$Division) == 0){
      
      questions = as.list(questionFrame[questionFrame$local == 1, "code"])
      
      names(questions) = questionFrame[questionFrame$local == 1, "value"]
      
    } else if(as.numeric(input$Division) == 2){
      
      questions = as.list(questionFrame[questionFrame$hp == 1, "code"])
      
      names(questions) = questionFrame[questionFrame$hp == 1, "value"]
      
    } else if(as.numeric(input$Division) == 1){
      
      # if there's only one value and it's 16 then HMP
      
      if(length(input$selDirect) == 1){
        
        if(input$selDirect == 16){
          
          questions = as.list(questionFrame[questionFrame$hmp == 1, "code"])
          
          names(questions) = questionFrame[questionFrame$hmp == 1, "value"]
          
        } else if(is.null(input$selDirect)){ # all directorates selected
          
          questions = as.list(questionFrame[questionFrame$trust == 1, "code"])
          
          names(questions) = questionFrame[questionFrame$trust == 1, "value"]
          
        } else {
          
          questions = as.list(questionFrame[questionFrame$forensic == 1, "code"])
          
          names(questions) = questionFrame[questionFrame$forensic == 1, "value"]
          
        }
        
      } else { # if there's multiple values:
        
        if(16 %in% input$selDirect){
          
          questions = as.list(questionFrame[questionFrame$trust == 1, "code"])
          
          names(questions) = questionFrame[questionFrame$trust == 1, "value"]
          
        } else {
          
          questions = as.list(questionFrame[questionFrame$forensic == 1, "code"])
          
          names(questions) = questionFrame[questionFrame$forensic == 1, "value"]
          
        }
      }
    }
    
    if(input$carerSU == "SU"){
      
      # do nothing!
      
    }
    
    if(input$carerSU == "bothCarerSU"){
      
      carerQuestions = as.list(questionFrame[questionFrame$carers == 1, "code"])
      
      names(carerQuestions) = questionFrame[questionFrame$carers == 1, "value"]
      
      combinedQuestions = c(questions, carerQuestions)
      
      questions = combinedQuestions[!duplicated(combinedQuestions)]
    }
    
    return(questions)
    
  })
  
  # handle reactive UI from question selection
  
  output$selectQuestions <- renderUI({
    
    checkboxGroupInput("selQuestions", "Choose questions", myQuestions(),
                       selected = myQuestions()[1:7])
    
  })
  
  # handle reactive UI from taxonomy selector
  
  output$serverTaxonomy = renderUI({
    
    selectInput("taxonomy", "Select comment theme",  c("None", names(taxonomy)))
  })
  
  # control the service user selection when it's just carers
  
  observe({
    
    if(input$carerSU == "carer"){
      
      updateSelectInput(session, "responder", "Responder type", 
                        list("Carer" = 9)
      )
      
    } else if(input$carerSU == "SU"){
      
      updateSelectInput(session, "responder", "Responder type", 
                        list("All" = 9, "Service user" = 0, "Carer" = 1)
      )
      
    } else if(input$carerSU == "bothCarerSU"){
      
      updateSelectInput(session, "responder", "Responder type", list("Carers and service users" = 9)
      )
      
    }
  })
  
  # generate the dataframe based on location, time, and demographics
  
  passData = reactive({
    
    # select carer, service user, or both
    
    if(input$carerSU == "SU"){
      
      finaldata = trustData[is.na(trustData$formtype) | trustData$formtype != "carer", ]
    } else if(input$carerSU == "carer"){
      
      finaldata = trustData[!is.na(trustData$formtype) & trustData$formtype == "carer", ]
    } else if(input$carerSU == "bothCarerSU"){
      
      finaldata = trustData
    }
    
    # deal with demographics
    
    if(as.numeric(input$responder) != 9){
      
      finaldata = finaldata[finaldata$SU %in% as.numeric(input$responder),]
    }
    
    if(input$sex != "All") finaldata = finaldata[finaldata$Gender %in% input$sex,]
    
    if(input$commInp == "community") finaldata = finaldata[finaldata$TeamC %in% communityTeams, ]
    
    if(input$commInp == "inpatient") finaldata = finaldata[finaldata$TeamC %in% inpatientTeams, ]
    
    if(input$ethnic != "All") finaldata = finaldata[finaldata$Ethnic %in% input$ethnic, ]
    
    if(input$disability != "All") finaldata = finaldata[grep(input$disability, finaldata$Disability), ]
    
    if(input$religion != "All") finaldata = finaldata[finaldata$Religion %in% input$religion, ]
    
    if(input$sexuality != "All") finaldata = finaldata[finaldata$Sexuality %in% input$sexuality, ]
    
    if(input$age != "All") finaldata = finaldata[finaldata$Age %in% input$age, ]
    
    # does the carertype input exist?
    
    if(!is.null(input$carertype)){
      
      if(input$carerSU == "carer"){
        
        # check whether each individual string in the carertype variable is in the carertype data row
        # combine with apply so if there are any matches the data is included
        
        finaldata = finaldata[apply(
          sapply(unlist(input$carertype), function(x) grepl(x, finaldata$carertype)), 1, 
          function(y) sum(y) > 0), ]
        
      }
      
    }
    
    # selecting Trust/ directorate/ team
    
    if(input$Division == 9){ # if the whole Trust is selected do this
      
      finaldata = finaldata[finaldata$Date %in% seq.Date(input$dateRange[1],
                                                         input$dateRange[2], by = "days"), ]
      
      return(finaldata)
      
    } else { # otherwise look at the directorate code
      
      if(is.null(input$selDirect)){ # if all directorates are selected do this
        
        finaldata = finaldata[!is.na(finaldata$Division) &
                                finaldata$Division %in% as.numeric(input$Division) &
                                finaldata$Date %in% seq.Date(input$dateRange[1],
                                                             input$dateRange[2], by = "days"), ]
        
        return(finaldata)
        
      }
      
      if(!is.null(input$selDirect)){ # if one or more directorates are selected do this
        
        if(input$selDirect[1] == 34 & length(input$selDirect) == 1){ # the special case of when just CYP is selected
          
          if(is.null(input$selDistrict)){ # if they haven't selected a district just show CYP results
            
            finaldata = filter(trustData, Directorate == 34, 
                               Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"))
            
            return(finaldata)
            
          } else {
            
            if(is.null(input$selSubDistrict)){ # if they haven't selected a sub district show the district results
              
              finaldata = filter(trustData, District %in% input$selDistrict,
                                 Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"))
              
              return(finaldata)
              
            } else { # if they have selected a subdistrict slice by that
              
              if(is.null(input$selSubDistrictTeam)){ # if they haven't selected individual teams slice by subdistrict
                
                finaldata = filter(trustData, SubDistrict %in% input$selSubDistrict,
                                   Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"))
                
                return(finaldata)
                
              } else { # else they did select subdistrict teams, slice by those
                
                finaldata = filter(trustData, TeamC %in% input$selSubDistrictTeam,
                                   Date %in% seq.Date(input$dateRange[1], input$dateRange[2], by = "days"))
                
                return(finaldata)
                
              }
              
            }
            
          }
          
        } else {
          
          # if(is.null(input$selTeam)) return()
          
          if(!is.null(input$selTeam)){
            
            finaldata = finaldata[!is.na(finaldata$TeamC) &
                                    finaldata$TeamC %in% as.numeric(input$selTeam) &
                                    finaldata$Date %in% seq.Date(input$dateRange[1],
                                                                 input$dateRange[2], by = "days"), ]
            
            return(finaldata)
            
          }
          
          if(is.null(input$selTeam)){
            
            finaldata = finaldata[!is.na(finaldata$Directorate) &
                                    finaldata$Directorate %in% as.numeric(input$selDirect) &
                                    finaldata$Date %in% seq.Date(input$dateRange[1],
                                                                 input$dateRange[2], by = "days"), ]
            
            return(finaldata)
            
          }
          
        }
        
      }
      
    }
    
  })
  
  # generate the comparison dataframe based on location, time, and demographics
  
  compData = reactive({
    
    # select carer, service user, or both
    
    if(input$carerSU == "SU"){
      
      finalComp = trustData[is.na(trustData$formtype) | trustData$formtype != "carer", ]
    } else if(input$carerSU == "carer"){
      
      finalComp = trustData[!is.na(trustData$formtype) & trustData$formtype == "carer", ]
    } else if(input$carerSU == "bothCarerSU"){
      
      finalComp = trustData
    }
    
    if(as.numeric(input$selDirect) %in% c(1, 2, 4, 6, 7, 13, 14)){ # local directorates
      
      finalComp = subset(finalComp, Division == 0 & Date %in% seq.Date(input$dateRange[1],
                                                                       input$dateRange[2], by = "days"))
      
    }
    
    if(as.numeric(input$selDirect) %in% c(3, 5, 8:12, 15, 16)){ # forensic directorates
      
      finalComp = subset(finalComp, Division == 1 & Date %in% seq.Date(input$dateRange[1],
                                                                       input$dateRange[2], by = "days"))
      
    }
    
    if(as.numeric(input$selDirect) > 20){ # if HP directorates
      
      finalComp = subset(finalComp, Division == 2 & Date %in% seq.Date(input$dateRange[1],
                                                                       input$dateRange[2], by = "days"))
      
    }
    
    if(is.null(input$selDirect)){ # divisions
      
      finalComp = finalComp[finalComp$Date %in% seq.Date(input$dateRange[1],
                                                         input$dateRange[2], by = "days"),]
      
    }
    
    # deal with demographics
    
    if(as.numeric(input$responder) != 9) finalComp=finalComp[finalComp$SU %in% as.numeric(input$responder),]
    
    if(input$sex != "All") finalComp=finalComp[finalComp$Gender %in% input$sex,]
    
    if(input$ethnic != "All") finalComp = finalComp[finalComp$Ethnic1 %in% input$ethnic,]
    
    if(input$disability != "All") finalComp = finalComp[grep(input$disability, finalComp$Disability),]
    
    if(input$religion != "All") finalComp = finalComp[finalComp$Religion %in% input$religion,]
    
    if(input$sexuality != "All") finalComp = finalComp[finalComp$Sexuality %in% input$sexuality,]
    
    if(input$age != "All") finalComp = finalComp[finalComp$Age %in% input$age,]
    
    if(as.numeric(input$Division) == 2) {
      
      finalComp[, c(which(names(finalComp) == "InvMed") : which(names(finalComp) == "OtherValue"),
                    which(names(finalComp) == "Staff"))] = NA
      
    }
    
    finalComp   
    
  })
  
  ### return list of PO and PALS stories
  
  commentData = reactive({
    
    # fix the time variable
    
    if(input$Division == 9){ # if the whole Trust is selected do this
      
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
  
  # textual summary
  
  output$SummaryOutput = renderText({
    
    if(is.null(passData())){
      
      myString = "Within the selected time and area there were no responses"
      
    } else {
      
      # FFT score
      
      promoterScores = passData()[, "Promoter2"]
      
      if(length(promoterScores[!is.na(promoterScores)]) > 0) {
        
        FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                      sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
        
      }
      
      # Quality score
      
      SQ = round(mean(passData()[, "Service"], na.rm = TRUE) * 20, 0)
      
      # Number of responses
      
      NR = nrow(passData())
      
      NSQ = length(passData()$Service[!is.na(passData()$Service)])
      
      NFFT = length(passData()$Promoter2[!is.na(passData()$Promoter2)])
      
      # number of comments
      
      IC = length(passData()[, "Improve"][!is.na(passData()[, "Improve"])])
      BC = length(passData()[, "Best"][!is.na(passData()[, "Best"])])
      
      # name of the area
      
      if(input$Division == 9){
        
        theArea = "the selected area"
        
      } else if(is.null(input$selDirect)){
        
        theArea = "the selected area"
        
      } else if(is.null(input$selTeam)){
        
        theArea = "the selected area"
        
      } else if(input$selTeam == 99){
        
        theArea = "the selected area"
        
      } else {
        
        ### look up team names
        
        teams = input$selTeam
        
        nameteams = lapply(teams, function(x) tail(trustData$TeamN[which(trustData$TeamC == x)], 1))
        
        theArea = paste(nameteams, collapse = ", ")
        
      }
      
      myString = paste0("<p>Within ", theArea, " in the selected time there were ", NR, 
                        " responses.</p><br>",
                        "<p>There were ", IC, " 'What could we do better' responses and ", BC,
                        " 'What did we do well' responses</p><br>",
                        ifelse(NFFT > 9,
                               paste0("<p>The Friends and Family Test Score is the proportion of patients
        who are extremely likely or likely to recommend a service. In the selected period of time it was ",
                                      FFT, "% (based on ", NFFT, " responses.)", "</p><br>"), ""),
                        ifelse(NSQ > 9,
                               paste0("<p>Service quality rating was ", SQ, "% (based on ", NSQ,
                                      " responses.)</p>"), "")
      )
      
      HTML(myString)
      
    }
    
  })
  
  # Generate stacked plot of the requested variable 
  
  myStack <- reactive({
    
    if(is.null(passData())){
      
      df <- data.frame()
      
      b = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
      
      b = b + annotate("text", label = "Not enough data in the specified time and",
                       x = 2, y = 2, size = 7, colour = "red") +
        annotate("text", label = "location, please broaden your search terms",
                 x = 2, y = 1, size = 7, colour = "red")
      b = b + options(theme = new_theme_empty)
      
      return(b)
      
    }
    
    # different questions needed for carers' survey when Advanced controls not selected
    
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
    
    if(length(names(missnum[missnum > 2])) > 2){
      
      mygraph = melt(lapply(names(missnum[missnum > 2]), function(x)
        prop.table(table(fixedData[, which(names(fixedData) == x)])) * 100))
      
      mygraph$L1 = factor(mygraph$L1, labels = c(names(missnum[missnum > 2]))) 
      
      mylabels = names(myQuestions()[which(unlist(myQuestions()) %in% names(missnum[missnum > 2]))])
      
      b = ggplot(mygraph, aes(L1, value, fill = factor(Var.1), order = -Var.1)) +
        geom_bar(position = "fill", stat = "identity") + ylab("Proportion responding") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Question") +
        scale_fill_manual(values = rainbow(5), "Response", limits = c(1:5), breaks = c(5:1),
                          labels = c("Excellent", "Good", "Fair", "Poor", "Very poor")) +
        scale_y_continuous(labels = percent_format()) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_x_discrete(labels = mylabels)
      
    } else {
      
      df <- data.frame()
      
      b = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
      
      b = b +  annotate("text", label = "Not enough data in the specified time and",
                        x = 2, y = 2, size = 7, colour = "red") +
        annotate("text", label = "location, please broaden your search terms",
                 x = 2, y = 1, size = 7, colour = "red")
      b = b + options(theme = new_theme_empty)
      
    }
    
  })
  
  output$StackPlot <- renderPlot({
    
    print(myStack())
    
  })
  
  # generate scores to put underneath the stacked plot
  
  output$scaleScores = renderText({
    
    if(is.null(passData())){
      
      return("")
      
    }
    
    # different questions needed for carers' survey when Advanced controls not selected
    
    if(input$carerSU == "carer" & !input$custom){
      
      theQuestions = as.list(questionFrame[questionFrame$carers == 1, "code"])
      
      names(theQuestions) = questionFrame[questionFrame$carers == 1, "value"]
      
    } else {
      
      theQuestions = input$selQuestions
    }
    
    # remove decimals from historic data
    
    fixedData = data.frame(apply(passData()[, unlist(theQuestions)], 1:2,
                                 function(x) round(x + .01)))
    
    # FFT first
    
    promoterScores = passData()[, "Promoter2"]
    
    if(length(promoterScores[!is.na(promoterScores)]) > 0) {
      
      FFT = round(sum(promoterScores %in% 4:5, na.rm = TRUE) /
                    sum(promoterScores %in% 0:5, na.rm = TRUE) * 100, 0)
      
    } else {
      
      FFT = FALSE
    }
    
    # count the missing responses
    
    missnum = apply(fixedData, 2, function(x) sum(!is.na(x)))
    
    theVariables = names(missnum[missnum > 2])
    
    theVariables <- theVariables[! "Promoter" == theVariables]
    
    cat(theVariables)
    
    if(length(theVariables) > 2){
      
      mygraph = melt(lapply(theVariables, function(x)
        prop.table(table(fixedData[, which(names(fixedData) == x)])) * 100))
      
      mygraph$L1 = factor(mygraph$L1, labels = theVariables) 
      
      # Quality score
      
      theScores = melt(lapply(theVariables, function(x)
        round(mean(fixedData[, which(names(fixedData) == x)], na.rm= TRUE) * 20, 0)))
      
      theScores$L1 = names(myQuestions()[which(unlist(myQuestions()) %in% theVariables)])
      
      # print FFT if it's there
      
      theString = ifelse(FFT, paste0("<p>FFT: ", FFT, "%</p>"), "")
      
      theString = c(theString, paste0("<p>", theScores$L1, ": ", theScores$value, "%</p>"))
      
      HTML(theString)
      
    } else {
      
      return()
    }
    
  })
  
  # Generate line plot for trend 
  
  myTrend = reactive({
    
    if(min(passData()$Time, na.rm = TRUE) == Inf){
      
      df <- data.frame()
      
      p = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
      
      p = p + annotate("text", label = "Not enough data in the specified time and",
                       x = 2, y = 2, size = 7, colour = "red") +
        annotate("text", label = "location, please broaden your search terms",
                 x = 2, y = 1, size = 7, colour = "red")
      
      p = p + options(theme = new_theme_empty)
      
      return(p)
    }
    
    mytime = min(passData()$Time, na.rm=TRUE) : max(passData()$Time, na.rm=TRUE)
    
    # different questions needed for carers' survey when Advanced controls not selected
    
    if(input$carerSU == "carer" & !input$custom){
      
      theQuestions = as.list(questionFrame[questionFrame$carers == 1, "code"])
      
      names(theQuestions) = questionFrame[questionFrame$carers == 1, "value"]
      
    } else {
      
      theQuestions = input$selQuestions
    }
    
    # if there's enough data
    
    if(nrow(passData()) > 10 & length(unlist(theQuestions) > 0) & max(mytime) - min(mytime) > 1){
      
      ### main data first
      
      helpTrend = function(x) { # x is a dataframe
        
        # prepare
        
        md = melt(x, measure.vars = unlist(theQuestions),
                  id.vars = c("Time"))
        
        md$variable = factor(md$variable, 
                             labels = names(myQuestions()[which(unlist(myQuestions()) %in% 
                                                                  unlist(theQuestions))]))
        
        # generate sums and lengths of variables
        
        mycast = data.frame(cast(md, variable + Time ~., sum, na.rm=TRUE), 
                            cast(md, variable + Time ~., function(x) sum(!is.na(x)))[,3])
        
        # remove missing Time rows
        
        names(mycast) = c("variable", "Time", "sum", "length")
        
        mycast2 = data.frame(cast(md, variable + Time ~., mean, na.rm=TRUE))
        
        names(mycast2) = c("variable", "Time", "value")
        
        # merge with an empty frame to fill in the blanks
        
        mymerge = data.frame("variable" = as.vector(sapply(unlist(theQuestions), 
                                                           function(x) rep(x, length(mytime)))),
                             "Time" = rep(mytime, length(unlist(theQuestions))),
                             "sum" = NA,
                             "length" = NA)
        
        theMerge = merge(mycast, mymerge, all=TRUE, by=c("variable", "Time"))[,1:4]
        
        theMerge = theMerge[!is.na(theMerge$Time),]
        
        # find all the cells that are below 5 responses and merge with the next cell along
        
        while(sum(theMerge$length.x < 5, na.rm=TRUE) > 0){
          
          myloc = which(theMerge$length.x == min(theMerge$length.x, na.rm = TRUE))[1]
          
          if(theMerge$Time[myloc] != max(theMerge$Time, na.rm = TRUE)){ # merge up if it's not the end of the time series
            
            theMerge[(myloc + 1), 3] = sum(theMerge[myloc, 3], theMerge[(myloc + 1), 3])
            
            theMerge[(myloc + 1), 4] = sum(theMerge[myloc, 4], theMerge[(myloc + 1), 4])
            
            theMerge[myloc, 3:4] = NA
            
          } else {
            
            theMerge[myloc, 3] = sum(theMerge[myloc, 3], theMerge[(myloc - 1), 3])
            
            theMerge[myloc, 4] = sum(theMerge[myloc, 4], theMerge[(myloc - 1), 4])
            
            theMerge[(myloc - 1), 3:4] = NA
            
          }
          
        } # end of while
        
        # calculate the mean response and fix the table ready for plotting
        
        theMerge$value = theMerge$sum.x / theMerge$length.x
        
        theMerge = theMerge[order(as.numeric(theMerge$Time)), ]
        
        theMerge$Time = factor(theMerge$Time, levels = unique(as.numeric(theMerge$Time)), 
                               labels = names(timelabels[unique(as.numeric(theMerge$Time))]))
        
        return(theMerge)
        
      }
      
      myfinal = helpTrend(passData())
      
      ### now comparison data
      
      if(input$comparison == TRUE & input$Division != 9 & 
         length(unique(myfinal$Time[!is.na(myfinal$value)])) > 1 &
         !(as.numeric(input$Division) == 2 &
           sum(unlist(theQuestions) %in% 
               c("Service", "Listening", "Communication", "Respect", "InvCare")) == 0)){
        
        mycomp = helpTrend(compData())
        
        changeloc = cumsum(!is.na(myfinal$value)) != 0
        
        myfinal = myfinal[changeloc,]
        
        mycomp = subset(mycomp, Time %in% myfinal$Time)
        
        p = ggplot(myfinal, aes(x = Time, y = value * 20)) + geom_blank() +
          geom_line(aes(group = variable, colour = variable),
                    linetype = 1, data = na.omit(myfinal)) +
          geom_point(aes(group = variable, colour = variable), data = na.omit(myfinal)) +
          annotate("text", x = myfinal$Time[!duplicated(myfinal$Time)],
                   y = floor(min(myfinal$value * 20, na.rm=TRUE)*.8),
                   label = myfinal$length[!duplicated(myfinal$Time)], angle = 90) +
          geom_line(aes(group = variable, colour = variable), linetype = 2,
                    data = na.omit(mycomp)) +
          geom_point(aes(group = variable, colour = variable), data = na.omit(mycomp)) +
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("% score") +
          labs(colour = "Question")
        
      } else {
        
        if(length(unique(myfinal$Time[!is.na(myfinal$value)])) > 1){
          
          changeloc = cumsum(!is.na(myfinal$value)) != 0
          
          myfinal = myfinal[changeloc, ]
          
          p = ggplot(myfinal, aes(x = Time, y = value * 20)) + geom_blank() +
            geom_line(aes(group = variable, colour = variable), linetype = 1,
                      data = na.omit(myfinal)) +
            geom_point(aes(group = variable, colour = variable), data = na.omit(myfinal)) +
            annotate("text", x = myfinal$Time[!duplicated(myfinal$Time)],
                     y = floor(min(myfinal$value * 20, na.rm=TRUE)*.8),
                     label = myfinal$length[!duplicated(myfinal$Time)], angle = 90) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ylab("% score") +
            labs(colour = "Question") 
          
        } else {
          
          df <- data.frame()
          
          p = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
          
          p = p + annotate("text", label = "Not enough data in the specified time and",
                           x = 3, y = 2, size = 7, colour = "red") +
            annotate("text", label = "location, please broaden your search terms",
                     x = 3, y = 1, size = 7, colour = "red")
          
          p = p + options(theme = new_theme_empty)
          
        }
        
        
      }
      
    } else { # end of if(nrow(passData()) > 10)
      
      df <- data.frame()
      
      p = ggplot(df) + geom_point() + xlim(0, 5) + ylim(0, 3)
      
      p = p + annotate("text", label = "Not enough data in the specified time and",
                       x = 2, y = 2, size = 7, colour = "red") +
        annotate("text", label = "location, please broaden your search terms",
                 x = 2, y = 1, size = 7, colour = "red")
      
      p = p + options(theme = new_theme_empty)
      
    }
    
  })
  
  output$TrendPlot <- renderPlot({
    
    print(myTrend())
    
  })
  
  # generate table - improve one thing
  
  myImprove = reactive({
    
    #    if(is.null(passData())) return()
    
    theCodes = c(passData()$Imp1, passData()$Imp2)
    
    # current sub themes
    
    subcommentList = lapply(SubList, function(x)
      theCodes[theCodes %in% unlist(x)])
    
    subfinalList = subcommentList[order(unlist(lapply(subcommentList, length)), decreasing = TRUE)]
    
    subtheComments = sapply(1:length(names(subfinalList)),
                            function(x) c(names(subfinalList)[x],
                                          round(length(subfinalList[[x]]) /
                                                  length(unlist(subcommentList)) * 100, 1)))
    
    # split the super from sub themes
    
    splitThemes = strsplit(as.character(subtheComments[1,]), " - ")
    
    ### put it all together
    
    finalFrame = data.frame("Category" = unlist(lapply(splitThemes, "[", 1)),
                            "Subcategory" = unlist(lapply(splitThemes, "[", 2)),
                            "Percentage subcategory" = as.numeric(subtheComments[2,]))
    
    # add up super themes
    
    finalFrame = finalFrame %>%
      group_by(Category) %>%
      mutate(PercentageCategory = sum(Percentage.subcategory))
    
    # ddply(finalFrame, "Category", mutate,
    #                  PercentageCategory = sum(Percentage.subcategory))
    
    ### get rid of missing data
    
    finalFrame = within(finalFrame, {
      
      Percentage.subcategory[is.nan(Percentage.subcategory)] = 0
      PercentageCategory[is.nan(PercentageCategory)] = 0
    })
    
    finalFrame = finalFrame[finalFrame$Percentage.subcategory > 0, ]
    
    if(nrow(finalFrame) < 2){
      
      finalFrame = data.frame("Not", "Enough", "Data", "!")
      
      names(finalFrame) = c("Category", "% category", "Subcategory", "Subcategory %")
      
    } else {
      
      finalFrame = finalFrame[order(finalFrame$Percentage.subcategory, decreasing = TRUE),]
      
      names(finalFrame) = c("Category", "Subcategory", "% subcategory", "% category")
      
    }
    
    finalFrame
    
  })
  
  output$TableImprove = DT::renderDataTable({
    
    datatable(myImprove(), rownames = FALSE)
    
  })
  
  # output table - best thing
  
  output$TableBest = DT::renderDataTable({
    
    datatable(myBest(), rownames = FALSE)
    
  })
  
  # generate table - best thing
  
  myBest = reactive({
    
    if(is.null(passData())) return()
    
    theCodes = c(passData()$Best1, passData()$Best2)
    
    # current sub themes
    
    subcommentList = lapply(SubList, function(x)
      theCodes[theCodes %in% unlist(x)])
    
    subfinalList = subcommentList[order(unlist(lapply(subcommentList, length)), decreasing = TRUE)]
    
    subtheComments = sapply(1:length(names(subfinalList)),
                            function(x) c(names(subfinalList)[x],
                                          round(length(subfinalList[[x]]) /
                                                  length(unlist(subcommentList)) * 100, 1)))
    
    # split the super from sub themes
    
    splitThemes = strsplit(as.character(subtheComments[1,]), " - ")
    
    ### put it all together
    
    finalFrame = data.frame("Category" = unlist(lapply(splitThemes, "[", 1)),
                            "Subcategory" = unlist(lapply(splitThemes, "[", 2)),
                            "Percentage subcategory" = as.numeric(subtheComments[2,]))
    
    # add up super themes
    
    finalFrame = finalFrame %>%
      group_by(Category) %>%
      mutate(PercentageCategory = sum(Percentage.subcategory))
    
    ### get rid of missing data
    
    finalFrame = within(finalFrame, {
      
      Percentage.subcategory[is.nan(Percentage.subcategory)] = 0
      PercentageCategory[is.nan(PercentageCategory)] = 0
    })
    
    finalFrame = finalFrame[finalFrame$Percentage.subcategory > 0, ]
    
    if(nrow(finalFrame) < 2){
      
      finalFrame = data.frame("Not", "Enough", "Data", "!")
      
      names(finalFrame)=c("Category", "% category", "Subcategory", "Subcategory %")
      
    } else {
      
      finalFrame = finalFrame[order(finalFrame$Percentage.subcategory, decreasing = TRUE),]
      
      names(finalFrame) = c("Category", "Subcategory", "% subcategory", "% category")
      
    }
    
    finalFrame
    
  })
  
  # output for the text responses
  
  output$TextResponses <- renderText({
    
    print(myComments())
    
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
          
          toaddFrame = passData()[, c("Location", "Improve", "Imp1", "ImpCrit")]
          
          if(length(impCriticality) > 0){
            
            toaddFrame = subset(toaddFrame, ImpCrit %in% as.numeric(impCriticality))
            
          }
          
          storyList = lappend(storyList, toaddFrame)
          
        }
        
        if("Best" %in% input$stories & returnComments[["Best"]]){
          
          # filter by criticality here
          
          toaddFrame = passData()[, c("Location", "Best", "Best1", "BestCrit")]
          
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
        
        if(length(unlist(storyList)) < 10 & searchDemographic) {
          
          showModal(modalDialog(
            title = "Error",
            "Sorry, you searched on patient characteristics and there are 
            fewer than 10 comments- individuals may be identifiable. Please 
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
  
  ###  pharmacy report
  
  output$downloadPharmacy <-
    downloadHandler(filename = "pharmacy.csv",
                    content = function(file){
                      
                      improveIndex = sapply(c("Medicine", "Medication", "Drug", "Pharmacy", "Pharmacist", "Chemist", "Prescribing",
                                              "Dispensing", "Dispenser", "Prescriber", "Drug Information", "Medicines Information",
                                              "Medicines Advice", "Dose", "Drug Error", "Incident", "Prescription", "Side-effect",
                                              "Injection"), function(x) grep(x, passData()$Improve, ignore.case = TRUE))
                      
                      improveData = passData()[unique(unlist(improveIndex)), c("Date", "TeamN", "Directorate2", "Division2", "Improve")]
                      
                      bestIndex = sapply(c("Medicine", "Medication", "Drug", "Pharmacy", "Pharmacist", "Chemist", "Prescribing",
                                           "Dispensing", "Dispenser", "Prescriber", "Drug Information", "Medicines Information",
                                           "Medicines Advice", "Dose", "Drug Error", "Incident", "Prescription", "Side-effect",
                                           "Injection"), function(x) grep(x, passData()$Best, ignore.case = TRUE))
                      
                      bestData = passData()[unique(unlist(bestIndex)), 
                                            c("Date", "TeamN", "Directorate2", "Division2", "Best")]
                      
                      write.table(improveData, file = "pharmacy.csv", 
                                  row.names = FALSE, col.names = TRUE, sep = ",")
                      
                      write.table(bestData, file="pharmacy.csv", 
                                  append = TRUE, row.names = FALSE, col.names = TRUE,  sep = ",")
                      
                      # copy csv to 'file'
                      
                      file.copy("pharmacy.csv", file, overwrite = TRUE)
                      
                    }
    )
  
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
  
  ### custom report docx 
  
  output$downloadDoc <-
    downloadHandler(filename = "CustomReport.docx",
                    content = function(file){
                      
                      render("CustomReport.Rmd", output_format = "word_document",
                             quiet = TRUE, envir = environment())
                      
                      # copy docx to 'file'
                      file.copy("CustomReport.docx", file, overwrite = TRUE)
                      
                    }
    )
  
  ###  HTML report
  
  output$downloadCustomReport <-
    downloadHandler(filename = "CustomReport.html",
                    content = function(file){
                      
                      render("CustomReport.Rmd", output_format = "html_document", # output_file = file,
                             quiet = TRUE, envir = environment())
                      
                      # copy HTML to 'file'
                      file.copy("CustomReport.html", file, overwrite = TRUE)
                      
                    }
                    
    )
  
  ### access to services report 
  
  output$downloadAccess <-
    downloadHandler(filename = "accessToServices.docx",
                    content = function(file){
                      
                      render("accessToServices.Rmd", output_format = "word_document", 
                             output_file = file,
                             quiet = TRUE, envir = environment())
                      
                      # copy docx to 'file'
                      file.copy("accessToServices.docx", file, overwrite = TRUE)
                      
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
  
  ## send message to wordcloud script- long string with all comments
  
  observe({
    
    if(input$theTabs == "wordCloud"){
      
      # form reactivity with word selector
      
      test = input$noWords
      
      theComments = passData()$Improve[!is.na(passData()$Improve)]
      
      theText = paste(theComments, collapse = " ")
      
      session$sendCustomMessage(type = 'sendMessage',
                                message = theText)
    }
  })
  
})