
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

team_tree <- map(c(0, 2, 1), function(division_code){
  
  dir_codes <- dirTable %>% 
    filter(Division == division_code, !DirC %in% c(17, 21)) %>% 
    pull(DirC) %>% 
    unique()
  
  all_directorates <- map(dir_codes, function(dir_code) {
    
    to_build <- trustData %>%
      filter(Directorate %in% dir_code) %>%
      select(Date, Division, Directorate, TeamC) %>% 
      left_join(counts, by = "TeamC") %>% 
      group_by(TeamC) %>% 
      arrange(Date) %>% 
      slice(1)
    
    built_list <- as.list(to_build$TeamC)
    
    names(built_list) <- to_build$TeamN
    
    for(i in 1 : length(built_list)){
      
      attr(built_list[[i]], "stid") <- built_list[[i]]
    }
    
    return(built_list)
  })
  
  dir_names <- dirTable %>% 
    filter(Division == division_code, !DirC %in% c(17, 21)) %>% 
    pull(DirT) %>% 
    unique()
  
  names(all_directorates) <- dir_names
  
  return(all_directorates)
})

names(team_tree) <- c("Local MH", "Local GH", "Forensic")
