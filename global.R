
library(shinydashboard)
library(scales)
library(knitr)
library(dplyr)
library(ggplot2)
library(purrr)
library(rmarkdown)
library(DT)
library(lubridate)
library(magrittr)
library(tidytext)
library(stringi)
library(stringr)
library(shinyWidgets)
library(urltools)
library(pins)
library(plumber)

board_register_rsconnect("SPACED",
                         server = "https://involve.nottshc.nhs.uk/rsconnect",
                         key = Sys.getenv("CONNECT_API_KEY"))

trustData <- pin_get("chrisbeeley/trustData", board = "SPACED") %>% 
  mutate(across(all_of(c("Imp1", "Imp2", "Best1", "Best2")), as.character))

questionFrame <- pin_get("chrisbeeley/questionFrame", board = "SPACED")

counts <- pin_get("chrisbeeley/counts", board = "SPACED")

dirTable <- pin_get("chrisbeeley/dirTable", board = "SPACED")

date_update <- max(trustData$Date)

date_update <- format(date_update, "%d/%m/%Y")

divisions_labels <- list("Local Partnerships- Mental Healthcare" = 0,
                         "Forensic" = 1,
                         "Community health services" = 2)
  
# recode new criticality

trustData <- trustData %>% 
  mutate(ImpCrit = case_when(
    Date >= "2020-10-01" & ImpCrit %in% 1 ~ 1L,
    Date >= "2020-10-01" & ImpCrit %in% 2:3 ~ 2L,
    Date >= "2020-10-01" & ImpCrit %in% 4:5 ~ 3L,
    TRUE ~ ImpCrit
  ))

trustData <- trustData %>% 
  mutate(BestCrit = case_when(
    Date >= "2020-10-01" & BestCrit %in% 1 ~ 1L,
    Date >= "2020-10-01" & BestCrit %in% 2:3 ~ 2L,
    Date >= "2020-10-01" & BestCrit %in% 4:5 ~ 3L,
    TRUE ~ BestCrit
  ))

# add the new codes to the bottom of the old codes

trustData <- trustData %>% 
  mutate(Imp1 = case_when(
    Date >= "2020-10-01" ~ Imp_N1,
    TRUE ~ Imp1
  )) %>% 
  mutate(Imp2 = case_when(
    Date >= "2020-10-01" ~ Imp_N2,
    TRUE ~ Imp2
  ))

trustData <- trustData %>% 
  mutate(Best1 = case_when(
    Date >= "2020-10-01" ~ Best_N1,
    TRUE ~ Best1
  )) %>% 
  mutate(Best2 = case_when(
    Date >= "2020-10-01" ~ Best_N2,
    TRUE ~ Best2
  ))


# filter out the staff teams from the counts object

counts <- counts %>% 
  filter(Division < 3)

enableBookmarking(store = "server")
