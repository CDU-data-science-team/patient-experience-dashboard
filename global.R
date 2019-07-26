
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
library(urltools)
library(shinyTree)

if(Sys.info()["nodename"] == "otis"){
  
  load("/opt/shiny-server/apps/SUCE/shiny.Rdata")
  
} else {
  
  load("~/shiny.Rdata")
}

# filter out the staff teams from the counts object

counts <- counts %>% 
  filter(Division < 3)

enableBookmarking(store = "server")

# get rid of corporate and unknown from dirTable

dirTable = dirTable %>%
  filter(!DirC %in% c(0, 40))

