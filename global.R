
############################################
# this is the old version of the dashboard #
############################################

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
library(pins)
library(plumber)

board_register_rsconnect("SPACED",
                         server = "https://involve.nottshc.nhs.uk:8443",
                         key = Sys.getenv("CONNECT_API_KEY"))

trustData <- pin_get("trustData", board = "SPACED")

categoriesTable <- pin_get("categoriesTable", board = "SPACED")

questionFrame <- pin_get("questionFrame", board = "SPACED")

counts <- pin_get("counts", board = "SPACED")

dirTable <- pin_get("dirTable", board = "SPACED")

date_update <- max(trustData$Date)

date_update <- format(date_update, "%d/%m/%Y")

# recode new criticality

trustData <- trustData %>% 
  mutate(ImpCrit = case_when(
    Date >= "2020-10-01" & ImpCrit %in% 0:1 ~ 1,
    Date >= "2020-10-01" & ImpCrit %in% 2:3 ~ 2,
    Date >= "2020-10-01" & ImpCrit %in% 4:5 ~ 3
  ))

# filter out the staff teams from the counts object

counts <- counts %>% 
  filter(Division < 3)

enableBookmarking(store = "server")
