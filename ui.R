
library(shinyBS)
library(shinythemes)

# Define application

# shinyUI(

function(request) {
  fluidPage(
    # theme = shinytheme("united"),
    
    div(id = "wrapper", style = "width: 940px; margin: auto; overflow: hidden;",
        
        tags$head(
          tags$link(rel = 'shortcut icon', 
                    href = "http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/favicon.ico"),
          tags$link(rel="apple-touch-icon", sizes="144x144", 
                    href="http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/apple-touch-icon-144x144.png"),
          tags$link(rel="apple-touch-icon", sizes="114x114", 
                    href="http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/apple-touch-icon-114x114.png"),
          tags$link(rel="apple-touch-icon", sizes="72x72",
                    href="http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/apple-touch-icon-72x72.png"),
          tags$link(rel="apple-touch-icon", href="http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/apple-touch-icon.png")#,
          # tags$style(type = "text/css"),
          # tags$style('@import url("http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/css/style.css?nyzhd3");')
        ),
        
        div(id = "header", style = "margin: 0 0 30px;",
            a(style = "margin: 30px;", href = "http://feedback.nottinghamshirehealthcare.nhs.uk", 
              img(border = 0, src = "http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/img/logo.png")),
              a(style = "float: right; margin: 30px;", href = "http://nottinghamshirehealthcare.nhs.uk", 
                img(border = 0, src = "http://feedback.nottinghamshirehealthcare.nhs.uk/sites/all/themes/notts/img/nhs.png"))
        ),
        
        div(id = "menu", style = 	"height: 55px; display: inline; background: #f3f3f3; margin-bottom: 30px; overflow: hidden;",
            tags$nav(role = "navigation",
                     tags$ul(class = "menu",
                             tags$li(class = "first leaf home", style = "width: 124px; display: inline; padding: 15px; background: #f3f3f3;
                                     border-bottom: 5px solid #30a900; font-size: 1.5em;",
                                     tags$a(href = "http://feedback.nottinghamshirehealthcare.nhs.uk/", title="Home page", class="active", "Home")),
                             tags$li(class="leaf leave-feedback", style = "width: 191px; height: 55px; display: inline; background: #f3f3f3; 
                                     padding: 15px; overflow: hidden; border-bottom: 5px solid #015cb7; font-size: 1.5em;",
                                     tags$a(href="http://feedback.nottinghamshirehealthcare.nhs.uk/leave-feedback", 
                                            title="Tell us what you think about our services", "Leave Feedback")),
                             tags$li(class = "leaf view-reports", style = "width: 164px; border-bottom: 5px solid #ff8500; 
                                     display: inline; padding: 15px; background: #f3f3f3; font-size: 1.5em;",
                                     tags$a(href="http://feedback.nottinghamshirehealthcare.nhs.uk/content/what-are-people-saying-about-our-services",
                                            title="See reports of the feedback we receive", "View reports")),
                             tags$li(class = "leaf news", style = "width: 104px; border-bottom: 5px solid #ccc; 
                                     display: inline; padding: 15px; background: #f3f3f3; font-size: 1.5em;",
                                     tags$a(href="http://feedback.nottinghamshirehealthcare.nhs.uk/latest-news", title = "", "News")),
                             tags$li(class = "leaf what-we-039-ve-done", style = "width: 203px; border-bottom: 5px solid #c80331; 
                                     display: inline; padding: 15px; background: #f3f3f3; font-size: 1.5em;",
                                     tags$a(href = "http://feedback.nottinghamshirehealthcare.nhs.uk/service/what-weve-done",
                                            title = "Changes made due to your feedback", "What we've done")),
                             tags$li(class = "last leaf learn-more", style = "width: 154px; border-bottom: 5px solid #00adb4; 
                                     display: inline; padding: 15px; background: #f3f3f3; font-size: 1.5em;",
                                     tags$a(href = "http://feedback.nottinghamshirehealthcare.nhs.uk/content/learning-development-and-get-involved", 
                                            title = "Learning, Development and Get Involved", "Learn more"))
                     )
            )
        ),
        
        div(style = "margin-bottom: 60px"),
        
        # Application title
        # titlePanel("Survey results- custom search"),
        
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
                               list("Survey- Improve one thing" = "Improve",
                                    "Survey- Best thing" = "Best",
                                    "Patient Opinion" = "PO",
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
                      tabPanel("Improve one thing", downloadButton('downloadData.imptable','Download Table'), 
                               DT::dataTableOutput("TableImprove"), value = "improveTab"),
                      tabPanel("Best thing", downloadButton('downloadData.besttable','Download Table'),
                               DT::dataTableOutput("TableBest"), value = "bestTab"),
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
  )
}
# )
