
library(rmarkdown)
#library(tidyverse)
library(scales)
library(lubridate)
library(urltools)

#* @serializer contentType list(type="application/vnd.openxmlformats-officedocument.wordprocessingml.document")
#* @param team select the team for the report
#* @get /word
function(team, date_from, date_to, carer_su, area_name, res){
  
  res$setHeader("Content-Disposition", "attachment; filename=report.docx")
  
  tmp <- tempfile()
  
  render("oneRmarkdownToRuleThemAll.Rmd", tmp, output_format = "word_document",
         params = list(team = team, date_from = date_from, date_to = date_to,
                       area_name = url_decode(area_name), carerSU = carer_su))
  
  file_name = paste0(tmp, ".docx")
  
  readBin(file_name, "raw", n=file.info(file_name)$size)
}

#* @serializer contentType list(type="text/html; charset=utf-8")
#* @param team select the team for the report
#* @get /html
function(team, date_from, date_to, carer_su, area_name, res){
  
  tmp <- tempfile()
  
  render("oneRmarkdownToRuleThemAll.Rmd", tmp, output_format = "html_document",
         params = list(team = team, date_from = date_from, date_to = date_to,
                       area_name = url_decode(area_name), carerSU = carer_su))
  
  readBin(tmp, "raw", n=file.info(tmp)$size)
}
