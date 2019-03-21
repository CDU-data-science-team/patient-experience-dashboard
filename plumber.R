
#* @serializer contentType list(type="application/vnd.openxmlformats-officedocument.wordprocessingml.document")
#* @get /team
function(team){
  tmp <- tempfile()
  
  area_name_team <- counts %>% 
    filter(TeamC %in% team) %>% 
    pull(TeamN) %>% 
    unique() %>% 
    paste(collapse = ", ")
  
  render("reports/team_quarterly.Rmd", tmp, output_format = "word_document",
         params = list(team = team, area_name = area_name_team))

  readBin(tmp, "raw", n=file.info(tmp)$size)
}

# okay, what's the plan here?
# let's set up two /URLS
# one for a team quarterly report at /team
# another one at /team-html for HTML response
# one at /custom which accepts other parameters

# this returns a word document with a team report in

# # this returns an html report with a team report in
# #* @serializer contentType list(type="text/html; charset=utf-8")
# #* @get /team-html
# function(team){
#   tmp <- tempfile()
#   
#   render("test_report.Rmd", tmp, output_format = "html_document",
#          params = list(team = team))
#   
#   readBin(tmp, "raw", n=file.info(tmp)$size)
# }
# 
# # this returns a custom report just for kicks
# #* @serializer contentType list(type="application/html")
# #* @get /custom
# function(res){
#   
#   include_rmd("test.Rmd", res)
# }
