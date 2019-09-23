
output$patientVoicesOutput <- renderMenu({
  
  fluidRow(
    column(width = 6, plotOutput("patientVoicesTrend")),
    column(width = 6, 
           valueBoxOutput("patientVoicesSQ"),
           valueBoxOutput("patientVoicesFFT"),
           valueBoxOutput("patientVoicesPositive"),
           valueBoxOutput("patientVoicesNoImpComments"),
           valueBoxOutput("patientVoicesNoBestComments")
    )
  )
})

output$patientVoicesTrend <- renderPlot({
  
  trend_function(passData()[["trendData"]], type = "patient_voices")
})

output$patientVoicesSQ <- renderValueBox({
  
  valueBox(value = paste0(dataSummary()[["SQ"]], "%"), 
           subtitle = HTML(paste0("Service quality<br>(", dataSummary()[["NSQ"]], " responses)")),
           icon = icon("thumbs-up"))
})

output$patientVoicesFFT <- renderValueBox({
  
  valueBox(value = paste0(dataSummary()[["FFT"]], "%"), 
           subtitle = HTML(paste0("Would you recommend?<br>(", dataSummary()[["NFFT"]], " responses)")),
           icon = icon("smile"))
})

output$patientPositive <- renderValueBox({
  
  valueBox(value = "to",
           subtitle = "complete",
           icon = icon("book-open"))
  
  # valueBox(value = paste0(dataSummary()[["FFT"]], "%"),
  #          subtitle = HTML(paste0("Would you recommend?<br>(", dataSummary()[["NFFT"]], " responses)")),
  #          icon = icon("smile"))
})

output$patientVoicesNoImpComments <- renderValueBox({
  
  valueBox(value = dataSummary()[["IC"]], 
           subtitle = "Number of IOT comments",
           icon = icon("book-open")
  )
})

output$patientVoicesNoBestComments <- renderValueBox({
  
  valueBox(value = dataSummary()[["BC"]], 
           subtitle = "Number of Best comments"
           # icon = icon("smile")
  )
})


