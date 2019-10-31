
library(shinydashboard)
library(scales)
library(knitr)
library(tidyverse)
library(rmarkdown)
library(DT)
library(lubridate)
library(magrittr)
library(tidytext)
library(igraph)
library(ggraph)
library(stringi)
library(stringr)
library(shinyWidgets)
library(urltools)

if(Sys.info()["nodename"] == "otis"){
  
  load("/opt/shiny-server/apps/SUCE/shiny.Rdata")
  
  date_update <- file.info("/opt/shiny-server/apps/SUCE/shiny.Rdata")
  
  date_update <- format(date_update, "%d/%m/%Y")
  
} else {
  
  load("~/shiny.Rdata")
  
  date_update <- as.Date(file.info("~/shiny.Rdata")$mtime)
  
  date_update <- format(date_update, "%d/%m/%Y")
}

# filter out the staff teams from the counts object

counts <- counts %>% 
  filter(Division < 3)

enableBookmarking(store = "server")