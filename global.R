
library(shinydashboard)
library(scales)
library(knitr)
library(tidyverse)
library(rmarkdown)
library(DT)
library(lubridate)
library(pander)
library(magrittr)
library(tidytext)
library(igraph)
library(ggraph)
library(stringi)
library(stringr)
library(shinyWidgets)

if(Sys.info()["nodename"] == "otis"){
  
  load("/opt/shiny-server/apps/SUCE/shiny.Rdata")
  
} else {
  
  load("~/shiny.Rdata")
}

# filter out the staff teams from the counts object

counts <- counts %>% 
  filter(Division < 3)

enableBookmarking(store = "server")