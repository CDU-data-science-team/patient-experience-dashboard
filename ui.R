
library(shinydashboard)
library(DT)

function(request) {
  
  dashboardPage(
    
    dashboardHeader(title = "Survey summary",
                    dropdownMenu(type = "notifications",
                                 notificationItem(
                                   text = HTML("Warning: Feedback from over 
                                   three<br> years ago has been removed. Please
                                   contact<br> the Experience team if required"),
                                   icon("exclamation-triangle")
                                   ),
                                 notificationItem(
                                   text = HTML("Warning: Criticality has been
                                   recoded.<br> Comparisons with old data may be<br>
                                   unreliable"),
                                   icon("exclamation-triangle")
                                 ))),
    
    # dashboard siderbar----
    
    dashboardSidebar(
      
      sidebarMenuOutput("sidebarMenu"),
      
      div(class = "shiny-input-container", 
          p(paste0("Data updated: ", date_update))
      ),
      
      # date range
      
      dateRangeInput("dateRange", label = "Date range",
                     start = as.Date(paste0(year(today()) - 3, "-",
                                            month(today()), "-",
                                            "01")),
                     end = Sys.Date(), startview = "year"),
      
      # Note that this goes team, directorate, division so it appears nicer
      # on the page
      
      
      # first set up All/ Division results
      
      selectInput("Division", 
                  HTML("Select Care Group<br/> (defaults to whole Trust)"),
                  divisions_labels,
                  multiple = TRUE),
      
      uiOutput("divControls"),
      
      # this panel appears if a particular directorate is selected
      
      conditionalPanel(
        condition = "input.selDirect != 99",
        uiOutput("dirControls")
      ),
      
      checkboxInput("showTeams", "Show all teams"),

      # select between service user/ carer data
      
      selectInput("carerSU", "Survey type",
                  list("Service user survey" = "SU",
                       "Carer survey" = "carer",
                       "Data from both surveys" = "bothCarerSU")),
      
      bookmarkButton()
      
    ),
    
    # dashboard body ----
    
    dashboardBody(

      tags$head(
        tags$style(
          HTML(".shiny-notification {
             position:fixed;
             top: calc(90%);
             left: calc(15%);
             opacity: 0.95;
             }"))
        ),
      
      tabItems(
        tabItem(tabName = "summary",
                fluidRow(
                  uiOutput("summaryPage")
                )
        ),
        tabItem(tabName = "scores",
                fluidRow(
                  box(width = 6, "Click plot to see figures", 
                      plotOutput("StackPlot", click = "stacked_suce_click")),
                  box(width = 6, "Trend", plotOutput("trendPlot"))
                ),
                fluidRow(
                  box(width = 6, "Click plot to see figures", 
                      plotOutput("carersPlot", click = "stacked_carer_click")),
                  box(width = 6, "Trend", plotOutput("carerTrendPlot"))
                )
        ),
        tabItem(tabName = "comments",
                radioButtons("categoryCriticality", 
                             "Query by:",
                             choices = c("Category", "Sub category", "Criticality")),
                
                fluidRow(
                  tabBox(
                    title = "SUCE comments",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "commentsTab", 
                    tabPanel(
                      "What could be improved?",
                      conditionalPanel(
                        condition = "input.categoryCriticality == 'Category'",
                        DTOutput("SuperTableImprove"),
                        DTOutput("SubTableImprove")
                      ),
                      conditionalPanel(
                        condition = "input.categoryCriticality == 'Criticality'",
                        DTOutput("impCritTable")
                      ),
                      conditionalPanel(
                        condition = "input.categoryCriticality == 'Sub category'",
                        DTOutput("subCategoryTableImprove")
                      )
                    ),
                    
                    tabPanel(
                      "Best thing",
                      conditionalPanel(
                        condition = "input.categoryCriticality == 'Category'",
                        DTOutput("SuperTableBest"),
                        DTOutput("SubTableBest")
                      ),
                      conditionalPanel(
                        condition = "input.categoryCriticality == 'Criticality'",
                        DTOutput("bestCritTable")
                      ),
                      conditionalPanel(
                        condition = "input.categoryCriticality == 'Sub category'",
                        DTOutput("subCategoryTableBest")
                      )
                    )
                  ),
                  column(6, # each of these is shown only when the relevant control is selected
                         conditionalPanel(
                           condition = "input.categoryCriticality == 'Category'",
                           htmlOutput("filterText")
                         ),
                         conditionalPanel(
                           condition = "input.categoryCriticality == 'Criticality'",
                           htmlOutput("filterTextCrit")
                         ),
                         conditionalPanel(
                           condition = "input.categoryCriticality == 'Sub category'",
                           htmlOutput("filterTextSubcategory")
                         )
                  )
                )
        ),
        tabItem(tabName = "allComments",
                fluidRow(
                  column(6, radioButtons("sortCategoryCriticality", 
                                         "Sort by category or Criticality?",
                                         choices = c("Category", "Criticality"))),
                  column(6, downloadButton("downloadAllComments", "Download all comments"))
                ),
                fluidRow(
                  column(6, h2("What could we do better?"), htmlOutput("allImproveComments")),
                  column(6, h2("What did we do well?"), htmlOutput("allBestComments"))
                )
        ),
        tabItem(tabName = "commentSearch",
                fluidRow(
                  uiOutput("commentSearchOutput")
                )
        ),
        tabItem(tabName = "demographics",
                fluidRow(
                  uiOutput("demographics")
                )
        ),
        tabItem(tabName = "patientVoices",
                uiOutput("patientVoicesOutput")
        ),
        tabItem(tabName = "textAnalysis",
                fluidRow(
                  column(7,
                         box(width = 12, "Co-occuring words. Click a word to see example comments", 
                             plotOutput("bigram_plot", click = "bigram_click"),
                             sliderInput("bigramSlider", "Number of terms", 20, 160, 100, step = 10)),
                         box(width = 12, "Co-occuring tags", plotOutput("tagBigrams"), 
                             sliderInput("tagBigramSlider", "Number of terms", 20, 160, 100, step = 10))
                  ),
                  column(5, h2("Example comments"), htmlOutput("plotClickInformation")
                  )
                )
        ),
        tabItem(tabName = "sentimentAnalysis",
                fluidRow(
                  column(2, selectInput("emotion", "Emotion", 
                                        c("anger", "anticipation", "disgust", "fear", "joy", "negative", 
                                          "positive", "sadness", "surprise", "trust"))),
                  column(10, h2("Top comments"), htmlOutput("sentimentComments"))
                )
        )
      )
    )
  )}    
