
library(shinydashboard)
library(DT)

function(request) {
  
  dashboardPage(
    dashboardHeader(title = "Survey summary"),
    
    # dashboard siderbar----
    
    dashboardSidebar(
      
      sidebarMenu(
        id = "tabs",
        menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
        menuItem("Scores", tabName = "scores", icon = icon("bar-chart")),
        menuItem("Comments", tabName = "comments", icon = icon("comment")),
        menuItem("All comments", tabName = "allComments", icon = icon("comment"))
        # menuItem("Text analysis", tabName = "textAnalysis", icon = icon("font")),
        # menuItem("Sentiment analysis", tabName = "sentimentAnalysis", icon = icon("font"))
      ),
      
      bookmarkButton(),
      
      # date range
      
      dateRangeInput("dateRange", label = "Date range",
                     start = Sys.Date() - 365,
                     end = Sys.Date(), startview = "year"),
      
      # select between service user/ carer data
      
      selectInput("carerSU", "Survey type",
                  list("Service user survey" = "SU",
                       "Carer survey" = "carer",
                       "Data from both surveys" = "bothCarerSU")),
      
      # first set up All/ Division results
      
      selectInput("Division", HTML("Select division<br/> (defaults to whole Trust)"),
                  list("Local Partnerships- Mental Healthcare" = 0,
                       "Local Partnerships- Community Healthcare" = 2, "Forensic" = 1),
                  multiple = TRUE),
      
      uiOutput("divControls"),
      
      # this panel appears if a particular directorate is selected
      
      conditionalPanel(
        condition = "input.selDirect != 99",
        uiOutput("dirControls")
      ),
      
      checkboxInput("showTeams", "Show all teams"),

      # toggle advanced controls
      
      # checkboxInput("custom", "Advanced controls", value = FALSE),
      
      # advanced controls follow
      
      conditionalPanel(
        condition = "input.custom == true",
        
        # community/ inpatient
        
        selectInput("commInp", "Community/ Inpatient",
                    list("All" = "all", "Community" = "community", "Inpatient" = "inpatient")),
        
        # service user/ carer
        
        selectInput("responder", "Responder type",
                    list("All" = 9, "Service user" = 0, "Carer" = 1)),
        
        # carer type
        
        conditionalPanel(
          condition = "input.carerSU == 'carer'",
          selectInput("carertype", "Carer of someone with...",
                      choices = list("Mental health" = "1", "Learning disability" = "2",
                                     "Physical health" = "3", "Dementia" = "4",
                                     "Substance misuse" = "5", "End of life" = "6",
                                     "With eating disorder" = "7", "Young carer" = "8"),
                      multiple = TRUE)
        ),
        
        # male/ female
        
        selectInput("sex", "Gender", list("All" = "All", "Men"= "M", "Women"= "F")),
        
        selectInput("ethnic", "Ethnicity",
                    list("All" = "All", "White British" = "WB", "White Irish" = "WI",
                         "White Other" = "WO", "Black Caribbean" = "BC",
                         "Black African" = "BA", "Other Black" = "BO",
                         "Asian Indian" = "AI", "Asian Pakistani" = "AP",
                         "Asian Bangladeshi" = "AB", "Asian Other" = "AO",
                         "Mixed white/ Black Caribbean" = "MC",
                         "Mixed white/ Black African" = "MA",
                         "White Asian" = "WA", "Other Mixed" = "MO",
                         "Chinese" = "CC", "Other" = "OO"),
                    selected = "All"),
        
        # disability
        
        selectInput("disability", "Do you have a disability?",
                    list("All" = "All", "Yes" = "Y", "No" = "N")),
        
        # religion
        
        selectInput("religion", "Religion",
                    list("All" = "All", "Christian" = "C", "Buddhist" = "B", "Hindu" = "H",
                         "Jewish" = "J", "Muslim" = "M", "Sikh" = "S", "Other" = "O",
                         "No religion" = "N")),
        
        # sexuality
        
        selectInput("sexuality", "Sexuality",
                    list("All" = "All", "Heterosexual/ straight" = "S", "Gay man" = "G",
                         "Lesbian/ gay woman" = "L", "Bisexual" = "B")),
        
        # age
        
        selectInput("age", "Age", list("All" = "All", "Under 12" = 1, "12- 17" = 2, "18-25" = 3,
                                       "26-39" = 4, "40-64" = 5, "65-79" = 6, "80+" = 7))
      )
    ),
    
    # dashboard body ----
    
    dashboardBody(
      
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
                  column(12, radioButtons("sortCategoryCriticality", 
                                          "Sort by category or Criticality?",
                                          choices = c("Category", "Criticality")))
                ),
                fluidRow(
                  column(6, h2("What could we do better?"), htmlOutput("allImproveComments")),
                  column(6, h2("What did we do well?"), htmlOutput("allBestComments"))
                )
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
    