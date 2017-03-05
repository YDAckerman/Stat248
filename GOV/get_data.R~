library(RCurl)
library(plyr)

govID <- read.csv("~/Documents/Stat248/government_ids.csv")
itemCode <- read.csv("~/Documents/Stat248/itemcodes.csv")

years <- c(96:99, "00", "01", "02", "03", "04",
           "05", "06", "07", "08", "09", 10:14)

bData <- ldply(years, function(yr){
    url <- paste0("https://www2.census.gov/govs/state/", yr, "state35.txt")
    read.csv(text = getURL(url), header = FALSE)
})

url <- paste0("https://raw.githubusercontent.com/",
              "jasonong/List-of-US-States/master/states.csv")
states <- read.csv(text = getURL(url), header = TRUE)

crimeData <- readLines("~/Documents/Stat248/CrimeStatebyState.csv")
cData <- ldply(states$State, function(state){
    i <- which(grepl(state, crimeData))[1]
    stateDat <- read.csv("~/Documents/Stat248/CrimeStatebyState.csv",
                         skip = i + 5, nrow = 25)
    stateDat$state <- state
    stateDat
})

pData <- read.csv(paste0("~/Documents/Stat248/dataverse_files",
                               "/Partisan_Balance_For_Use2011_06_09b.csv"))

save(pData, cData, bData, govID, itemCode, file = "all_data.rda")
