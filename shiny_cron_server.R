
### cron job to produce shiny.Rdata

library(httr)
library(RMySQL)
library(gtools)
library(car)
library(RCurl)
library(XML)

# setwd("~/Dropbox/R-files/SUCE_DATA")

setwd("/opt/shiny-server/apps/SUCE/")

# list of all objects and functions to generate them

### fetch from database

theDriver <- dbDriver("MySQL")

mydb = dbConnect(theDriver, user = "chrisbeeley", password = "kermitthefrog",
                 dbname = "SUCE", host = "localhost") # local
                 # dbname = "SUCE", host = "109.74.194.173") # local

dbSendQuery(mydb, "SET NAMES utf8")

# main dataset

localForensic = dbGetQuery(mydb, paste0("SELECT * FROM Local INNER JOIN Teams WHERE Local.TeamC = Teams.TeamC
                                        AND Local.Date >= Teams.date_from AND Local.Date <= Teams.date_to"))

hp = dbGetQuery(mydb, paste0("SELECT * FROM HealthPartnerships INNER JOIN Teams 
                              WHERE HealthPartnerships.TeamC = Teams.TeamC
                              AND HealthPartnerships.Date >= Teams.date_from 
                              AND HealthPartnerships.Date <= Teams.date_to"))

iapt = dbGetQuery(mydb, paste0("SELECT * FROM IAPT INNER JOIN Teams WHERE IAPT.TeamC = Teams.TeamC AND
                               IAPT.Date >= Teams.date_from AND IAPT.Date <= Teams.date_to"))

### taxonomy and inpatient information

inpatient = read.csv("inpatient.csv", stringsAsFactors = FALSE, na.strings = "")

communityTeams = subset(inpatient, Inpatient == 0)$TeamC

inpatientTeams = subset(inpatient, Inpatient == 1)$TeamC

taxonomy = read.csv("taxonomy.csv", stringsAsFactors = FALSE, na.strings = "")

### directorates

dirTable = dbGetQuery(mydb, paste0("SELECT * FROM Directorates"))

dirTable = dirTable[!duplicated(dirTable$DirC, fromLast = TRUE),]

directorate = as.character(sapply(1 : 34, function(x) dirTable$DirT[dirTable$DirC == x]))

directorate[directorate == "character(0)"] = NA

# team information

counts = dbGetQuery(mydb, paste0("SELECT * from Teams"))

# need to coerce TeamC to float so the hmp codes survive

hp$TeamC = as.numeric(hp$TeamC)

localForensic$TeamC = as.numeric(localForensic$TeamC)

iapt$TeamC = as.numeric(iapt$TeamC)

# deal with missing data codes etc.

localForensic$InvFam[localForensic$InvFam == 0] = NA

trustData = smartbind(localForensic, hp, iapt)

# create special promoter to deal with the Promoter calculation

trustData$Promoter2 = trustData$Promoter

trustData$Promoter[trustData$Promoter == 0] = NA

# which are the ones it's safe to remove 8s and 9s from?

scaleNames = names(trustData[, -which(names(trustData) %in% 
                                        c("Time", "key", "Directorate", "paperindex"))])

trustData[, scaleNames][trustData[, scaleNames] == 9] = NA
trustData[, scaleNames][trustData[, scaleNames] == 8] = NA

# also remove "NA"s

trustData[, scaleNames][trustData[, scaleNames] == "NA"] = NA

trustData$Date = as.Date(trustData$Date)

# remove the Optout comments

removeComments = which(trustData$Optout == "Yes")

trustData$Improve[removeComments] = NA
trustData$Best[removeComments] = NA

#### make a generic location variable to put in brackets after the comments

# first remove the 0 directorates

trustData$Directorate[trustData$Directorate == 0] = NA

trustData$Directorate2 = directorate[trustData$Directorate]

trustData$Division2 = recode(trustData$Division,
                             '0 = "Local services"; 1 = "Forensic services"; 2 = "Health Partnerships"')

trustData$Location = trustData$TeamN

trustData$Location[is.na(trustData$Location)] = trustData$Directorate2[is.na(trustData$Location)]

trustData$Location[is.na(trustData$Location)] = trustData$Division2[is.na(trustData$Location)]

trustData$Location[is.na(trustData$Location)] = "Trust"

###################################
######## Patient Opinion ##########
###################################

# lappend

lappend <- function(lst, obj) {  
  
  lst[[length(lst)+1]] <- obj
  
  return(lst)
}

apiKey = "SUBSCRIPTION_KEY erh7pre3y6untfcx8k3zbzvtca6sgphnyyw36rxy"

dateQuery = as.Date(dbGetQuery(mydb, "SELECT MAX(Date) FROM POFinal")[, 1]) + 1

### just new ones

# submittedonafter=01%2F08%2F2016

dateFinal = paste0(substr(dateQuery, 6, 7), "%2F", substr(dateQuery, 9, 10), "%2F", substr(dateQuery, 1, 4))

# dateFinal = "01%2F08%2F2005"

# produce empty list to lappend

opinionList = list()

# set skip at 0 and give opinions length value > 0

skip = 0

continue = TRUE

while(continue){
  
  opinions = GET(paste0("https://www.patientopinion.org.uk/api/v2/opinions?take=100&skip=",
                        skip, "&submittedonafter=", dateFinal),
                 add_headers(Authorization = apiKey))
  
  if(length(content(opinions)) == 0){
    
    continue = FALSE
  }
  
  opinionList = c(opinionList, content(opinions))
  
  skip = skip + 100
  
}

# if there are no new stories just miss this entire bit out

if(length(opinionList) > 0){
  
  keyID = lapply(opinionList, "[[", "id")
  title = lapply(opinionList, "[[", "title")
  story = lapply(opinionList, "[[", "body")
  date = lapply(opinionList, "[[", "dateOfPublication")
  criticality = lapply(opinionList, "[[", "criticality")
  
  # location is inside $links
  
  linkList = lapply(opinionList, "[[", "links")
  location = lapply(linkList, function(x) x[[5]][['id']])

  # there are null values in some of these, need converting to NA
  # before they're put in the dataframe
  
  date[sapply(date, is.null)] = NA
  criticality[sapply(criticality, is.null)] = NA
  
  finalData = data.frame("keyID" = unlist(keyID),
                         "Title" = unlist(title),
                         "PO" = unlist(story),
                         "Date" = as.Date(substr(unlist(date), 1, 10)),
                         "criticality" = unlist(criticality),
                         "location" = unlist(location),
                         stringsAsFactors = FALSE
  )
  
  ### match up NACS codes
  
  POLookup = read.csv("poLookup.csv", stringsAsFactors = FALSE)
  
  ### some of the cases are inconsistent, make them all lower case
  
  finalData$location = tolower(finalData$location)
  
  POLookup$NACS = tolower(POLookup$NACS)
  
  PO1 = merge(finalData, POLookup, by.x = "location", by.y = "NACS", all.x = TRUE)
  
  ### clean the data
  
  PO1$PO = gsub("<(.|\n)*?>", "", PO1$PO)
  
  PO1$Date = as.Date(PO1$Date)
  
  poQuarters = as.numeric(substr(quarters(PO1$Date), 2, 2))
  
  poYears = as.numeric(format(PO1$Date, "%Y"))
  
  PO1$Time = poQuarters + (poYears - 2009) * 4 - 1
  
  ### write PO to database
  
  # all team codes must be present
  
  PO1 = PO1[!is.na(PO1$TeamC), ]
  
  # strip out line returns
  
  PO1$PO = gsub(pattern = "\r", replacement = "", x = PO1$PO)
  
  # strip out the UTF-8 smileys
  
  PO1$PO = iconv(PO1$PO, "ASCII", "UTF-8", sub = "")
  
  # write to database
  
  dbWriteTable(mydb, "POFinal", PO1[, c("keyID", "Title", "PO", "Date", "Time", "TeamC", "criticality")], 
               append = TRUE, row.names = FALSE)
}

PO = dbGetQuery(mydb, 
                "SELECT POFinal.*, Teams.TeamN, Teams.TeamC, Teams.Division, Teams.Directorate FROM 
                                        POFinal 
                                          INNER JOIN Teams WHERE POFinal.TeamC = 
                                          Teams.TeamC AND POFinal.Date >= Teams.date_from 
                                          AND POFinal.Date <= Teams.date_to")

# remove the 0 directorates

PO$Directorate[PO$Directorate ==0] = NA

PO$Directorate2 = directorate[PO$Directorate]

PO$Division2 = recode(PO$Division,
                      '0 = "Local services"; 1 = "Forensic services"; 
                      2 = "Health Partnerships"')

PO$Location = PO$TeamN

PO$Location[is.na(PO$Location)] = PO$Directorate2[is.na(PO$Location)]

PO$Location[is.na(PO$Location)] = PO$Division2[is.na(PO$Location)]

PO$Date = as.Date(PO$Date)

# PALS

PALS = dbGetQuery(mydb, paste0("SELECT * FROM PALS INNER JOIN Teams WHERE PALS.TeamC = 
                               Teams.TeamC AND PALS.Date >= Teams.date_from AND PALS.Date <=
                               Teams.date_to"))

PALS$Date = as.Date(PALS$Date)

# remove the 0 directorates

PALS$Directorate[PALS$Directorate ==0] = NA

PALS$Directorate2 = directorate[PALS$Directorate]

PALS$Division2 = recode(PALS$Division,
                        '0 = "Local services"; 1 = "Forensic services"; 2 = "Health Partnerships"')

PALS$Location = PALS$TeamN

PALS$Location[is.na(PALS$Location)] = PALS$Directorate2[is.na(PALS$Location)]

PALS$Location[is.na(PALS$Location)] = PALS$Division2[is.na(PALS$Location)]

# get comment categories make into lists for use later

categoriesTable = dbGetQuery(mydb, "SELECT * from Categories")

SuperList = with(categoriesTable, sapply(unique(Super), function(x) Number[which(Super == x)]))

SubList = with(categoriesTable, sapply(unique(paste(Super, "-", Category)),
                                       function(x) Number[which(paste(Super, "-", Category) == x)]))

# get question lookup

questionFrame = dbGetQuery(mydb, "SELECT * from Questions")

dbDisconnect(mydb)

### produce time labels for graphs

timelabels = as.list(1 : ((as.numeric(format(Sys.Date(), "%Y")) - 2009) * 4 + 3))

names(timelabels) = c(paste(c("Apr - Jun", "Jul - Sep", "Oct - Dec"), "2009"),
                      as.vector(sapply(2010 : as.numeric(format(Sys.Date(), "%Y")),
                                       function(x) paste(c("Jan - Mar", "Apr - Jun",
                                                           "Jul - Sep", "Oct - Dec"), x))))

# list the directorates in each division

directDiv = list(list("Adult mental health" = 2, "CAMHS" = 4,
                      "Intell. and devel. disability" = 6, "MHSOP" = 7,
                      "Substance misuse" = 13, "Prescribed services" = 14, "IAPT" = 24),
                 list("Arnold lodge" = 3, "Low secure and Comm. forensic"= 5,
                      "High secure LD" = 8, "High secure mental health" = 9,
                      "High secure PD pathway" = 10, "Peaks" = 11, "High secure women's" = 12,
                      "Wathwood" = 15, "Offender health" = 16),
                 list("Bassetlaw" = 25, "Mansfield and Ashfield" = 26,
                      "Newark and Sherwood" = 27, "Nottingham North and East" = 28,
                      "Nottingham West" = 29, "Rushcliffe" = 30, "Specialist services" = 31,
                      "Sure Start" = 33, "CYP" = 34))

# which comments to omit

recodelist = read.csv("recodelist.csv", stringsAsFactors = FALSE)

omitlist = read.csv("omit.txt", stringsAsFactors = FALSE)

trustData[trustData$Improve %in% omitlist[, 1], "Improve"] = NA

trustData[trustData$Best %in% omitlist[, 1], "Best"] = NA

### save all objects

save("directDiv", "trustData", "PALS", "PO", "questionFrame", "SubList", "directorate",
     "SuperList", "timelabels", "recodelist", "counts", "communityTeams",
     "inpatientTeams", "taxonomy", file = "shiny.Rdata")

