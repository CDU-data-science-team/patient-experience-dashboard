
## global.R ##

if(Sys.info()["nodename"] == "otis"){
  
  load("/opt/shiny-server/apps/SUCE/shiny.Rdata")
  
} else {
  
  load("shiny.Rdata")
}

enableBookmarking(store = "server")