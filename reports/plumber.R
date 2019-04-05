
library(rmarkdown)

load("../shiny.Rdata")

#* @serializer contentType list(type="application/vnd.openxmlformats-officedocument.wordprocessingml.document")
#* @param team select the team for the report
#* @get /word
function(team, res){
  
  res$setHeader("Content-Disposition", "attachment; filename=report.docx")
  
  tmp <- tempfile()
  
  render("team_quarterly_api.Rmd", tmp, output_format = "word_document",
         params = list(team = team))

  readBin(tmp, "raw", n=file.info(tmp)$size)
}

#* @serializer contentType list(type="text/html; charset=utf-8")
#* @param team select the team for the report
#* @get /html
function(team){
  
  tmp <- tempfile()
  
  render("team_quarterly_api.Rmd", tmp, output_format = "html_document",
         params = list(team = team))
  
  readBin(tmp, "raw", n=file.info(tmp)$size)
}
