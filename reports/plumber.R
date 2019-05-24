
library(rmarkdown)
library(tidyverse)
library(scales)
library(lubridate)

#* @serializer contentType list(type="application/vnd.openxmlformats-officedocument.wordprocessingml.document")
#* @param team select the team for the report
#* @get /word
function(team, date_from, date_to, carer_su, area_name, res){
  
  res$setHeader("Content-Disposition", "attachment; filename=report.docx")
  
  tmp <- tempfile()
  
  render("oneRmarkdownToRuleThemAll.Rmd", tmp, output_format = "word_document",
         params = list(team = team, date_from = date_from, date_to = date_to,
                       area_name = URLdecode(area_name), carerSU = carer_su))
  
  readBin(tmp, "raw", n=file.info(tmp)$size)
}

#* @serializer contentType list(type="text/html; charset=utf-8")
#* @param team select the team for the report
#* @get /html
function(team){
  
  tmp <- tempfile()
  
  render("oneRmarkdownToRuleThemAll.Rmd", tmp, output_format = "html_document",
         params = list(team = team, carerSU = carer_su, date_from = date_from, date_to = date_to,
                       area_name = URLdecode(area_name)))
  
  readBin(tmp, "raw", n=file.info(tmp)$size)
}
