
library(shinydashboard)
library(scales)
library(knitr)
library(dplyr)
library(ggplot2)
library(tidyr)
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
library(shiny)
library(shinyjs)
library(SQLRtools)


board <- board_connect(name = "SPACED",
                       key =  get_env_var("CONNECT_API_KEY"),
                       server = "https://feedbackmatters.uk/rsconnect")

trustData <- pin_read(board, "chrisbeeley/trustData") %>% 
  mutate(across(all_of(c("Imp1", "Imp2", "Best1", "Best2")), as.character))

questionFrame <- pin_read(board, "chrisbeeley/questionFrame")

counts <- pin_read(board, "chrisbeeley/counts")


dirTable <- pin_read(board, "chrisbeeley/dirTable")

date_update <- max(trustData$Date)

date_update <- format(date_update, "%d/%m/%Y")

divisions_labels <- list("Mental health services" = 0,
                         "Forensic services" = 1,
                         "Community health services" = 2)

categorise_table <- pin_read(board, "chrisbeeley/newCategories") %>%
  set_names(c("Super", "Number", "Category")) %>% 
  mutate(type = "both")
  
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
