
## global.R ##

if(Sys.info()["nodename"] == "otis"){
  
  load("/opt/shiny-server/apps/SUCE/shiny.Rdata")
  
} else {
  
  load("~/shiny.Rdata")
}

# filter out the staff teams from the counts object

counts <- counts %>% 
  filter(Division < 3)

enableBookmarking(store = "server")