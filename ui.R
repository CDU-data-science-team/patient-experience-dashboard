
library(shinyBS)
library(shinythemes)

# Define application

# shinyUI(

function(request) {
  
  
  fluidPage(
    titlePanel("Survey results- custom search"),
    
    sidebarLayout(sidebarPanel(
      
      # bookmark button
      
      bookmarkButton(),
      
      # date range
      
      dateRangeInput("dateRange", label = "Date range",
                     start = as.Date(paste0(as.numeric(format(Sys.Date(),'%Y')), "/01/01")),
                     end = Sys.Date(), startview = "year"),
      
      # first set up All/ Division results
      
      selectInput("Division", "Select division", 
                  list("Trust" = 9, "Local Partnerships- Mental Healthcare"= 0,
                       "Local Partnerships- Community Healthcare" = 2, "Forensic" = 1)),
      
      conditionalPanel(
        condition = "input.Division != 9",
        uiOutput("divControls"),
        
        # this panel appears if a particular directorate is selected
        
        conditionalPanel(
          condition = "input.selDirect != 99",
          uiOutput("dirControls")
        ),
        
        conditionalPanel(
          condition = "input.selDistrict != null && input.selDirect == '34'",
          uiOutput("distControls"),
          
          conditionalPanel(
            condition = "input.selSubDistrict != null",
            uiOutput("subDistrictTeams")
          )
        )
      ),
      
      # select between service user/ carer data
      
      selectInput("carerSU", "Survey type",
                  list("Service user experience survey" = "SU",
                       "Carer experience survey" = "carer",
                       "Data from both surveys" = "bothCarerSU")),
      
      # text search
      
      conditionalPanel(
        condition = "input.theTabs == 'commentsTab'",
        
        # search term
        
        textInput("keyword", "Keyword search: (e.g. food, staff)"),
        
        # criticality
        
        selectInput("criticality", "Criticality", 
                    c("Very critical" = 3, "Fairly critical" = 2, "Slightly critical" = 1,
                      "Slightly complimentary" = 4, "Fairly complimentary" = 5, "Very complimentary" = 6),
                    multiple = TRUE),
        
        # taxonomy
        
        uiOutput("serverTaxonomy")
      ),
      
      # text search
      
      conditionalPanel(
        condition = "input.theTabs == 'wordCloud'",
        
        # search term
        
        selectInput("noWords", "Number of words", list(10, 20, 30, 40, 50, 60), selected = 60)
      ),
      # download data
      
      downloadButton("downloadData", "Download data"),
      
      # check for amount of data to download
      
      bsAlert("downloadDataAlert"),
      
      # download custom report
      
      downloadButton("downloadDoc", "Download report"),
      
      # download custom report
      
      downloadButton("downloadCustomReport", "Printer friendly"),
      
      # toggle advanced controls
      
      checkboxInput("custom", "Advanced controls", value = FALSE),
      
      # advanced controls follow
      
      conditionalPanel(
        condition = "input.custom == true",
        
        # community/ inpatient
        
        selectInput("commInp", "Community/ Inpatient",
                    list("All" = "all", "Community" = "community", "Inpatient" = "inpatient")),
        
        # question
        
        uiOutput("selectQuestions"),
        
        # comparison
        
        conditionalPanel(
          condition = "input.theTabs == 'trendTab' && input.Division != 9 && input.carerSU != 'carer'",
          checkboxInput("comparison", "Provide comparison?", value = FALSE)
        ),
        
        # positive/ negative stories
        
        checkboxGroupInput("stories", "Story type", 
                           list("Survey- What could we do better?" = "Improve",
                                "Survey- What did we do well?" = "Best",
                                "Care Opinion" = "PO",
                                "PALS" = "PALS"),
                           selected = c("Improve", "Best", "PO", "PALS")
        ),
        
        div( # pharmacy and access to services reports
          
          downloadButton("downloadPharmacy", "Download pharmacy report"),
          downloadButton("downloadAccess", "Download access to services report"),
          
          style='padding:10px;'),
        
        # download feedback tracker- each division
        wellPanel(
          h4("Download feedback tracker"),
          downloadButton("downloadFeedbackTrackerLocal", 
                         "LP- Mental Health"),
          downloadButton("downloadFeedbackTrackerHealthPartnerships", 
                         "LP- Community Health"),
          downloadButton("downloadFeedbackTrackerForensic", "Forensic")
        ),
        
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
    
    # tabbed output, stacked plot, trend plot, and responses
    
    mainPanel(
      h3(textOutput("Results")),
      tabsetPanel(id = "theTabs",
                  tabPanel("Summary", htmlOutput("SummaryOutput"), value = "summaryTab"),
                  tabPanel("Stacked plot", p("To download the percentages behind the graph please
                                 click 'Download percentages' below."),
                           p("To see data on all survey questions select 'Advanced controls' on the left."),
                           plotOutput("StackPlot"), htmlOutput("scaleScores"),
                           downloadButton('downloadData.stack','Download Graph'),
                           downloadButton('downloadData.stackTable', 'Download percentages')),
                  tabPanel("Trend", p("To see data on all survey questions select 
                          'Advanced controls' on the left."),
                           plotOutput("TrendPlot"),
                           downloadButton('downloadData.trend','Download Graph'),
                           value = "trendTab"), 
                  tabPanel("What could we do better", value = "Improve",
                           tableOutput("impCritTable"),
                           DT::dataTableOutput("SuperTableImprove"), 
                           DT::dataTableOutput("SubTableImprove"),
                           htmlOutput("improveText")),
                  tabPanel("What did we do well", value = "Best",
                           tableOutput("bestCritTable"),
                           DT::dataTableOutput("SuperTableBest"), 
                           DT::dataTableOutput("SubTableBest"),
                           htmlOutput("bestText")),
                  tabPanel("Comments", downloadButton('downloadData.comments','Download Comments'),
                           htmlOutput("TextResponses"), value = "commentsTab")#,
                  # tabPanel("Word cloud",
                  #          includeHTML("wordCloud.js"), # include JS file
                  #          tags$canvas(id = "cloudCanvas", width="500", height="500"),
                  #          tags$footer(id = "foot"), 
                  #          tags$div(id = "foot2"), value = "wordCloud")
                  
      ))
    
    )
  )
}
# )
