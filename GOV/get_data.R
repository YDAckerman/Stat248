library(RCurl)
library(plyr)

govID <- read.csv("~/Documents/Stat248/GOV/government_ids.csv")
itemCode <- read.csv("~/Documents/Stat248/GOV/itemcodes.csv")

years <- c(96:99, "00", "01", "02", "03", "04",
           "05", "06", "07", "08", "09", 10:14)

## get budget data (no longer using)
bData <- ldply(years, function(yr){
    url <- paste0("https://www2.census.gov/govs/state/", yr, "state35.txt")
    read.csv(text = getURL(url), header = FALSE)
})

url <- paste0("https://raw.githubusercontent.com/",
              "jasonong/List-of-US-States/master/states.csv")
states <- read.csv(text = getURL(url), header = TRUE)

## get crime data (no longer using...)
crimeData <- readLines("~/Documents/Stat248/GOV/CrimeStatebyState.csv")
cData <- ldply(states$State, function(state){
    i <- which(grepl(state, crimeData))[1]
    stateDat <- read.csv("~/Documents/Stat248/GOV/CrimeStatebyState.csv",
                         skip = i + 5, nrow = 25)
    stateDat$state <- state
    stateDat
})

## get political data
pData <- read.csv(paste0("~/Documents/Stat248/GOV/dataverse_files",
                               "/Partisan_Balance_For_Use2011_06_09b.csv"))

## get district data
dPopData <- ldply(78:105, function(cong){
    url <- paste0("https://sites.google.com/a/colorado.edu/adler-scott/fin",
                  cong, ".csv?attredirects=0&d=1")
    dat <- read.csv(url, header = TRUE)
    dat$CONGRESS <- cong
    dat
})

save(pData, cData, bData, govID, dData, itemCode, dPopData,
     file = "all_data.rda")

