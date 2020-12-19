### ronaVote.r
###
### Regression analysis of 2020 election, mashed with COVID outcomes
###
### Functions:
### - cleanData
### - naive

library(data.table)
setwd("~/git/bhack/nlyz/ronaVote/wrkdir")

# -----------------------------------------------------------------------------
cleanRona <- function(){
  
  #' Read csv corona data to data table: 

  pathRona <- "~/git/bhack/data/rona/NYT/us-counties.csv"
  ronaDays <- fread(pathRona,
                       select = grep("date|fips|cases", 
                       names(fread(pathRona, nrow = 0L))))
  
  zeroMonday <- as.IDate("2019-12-29")
  ronaDays$week <- as.numeric(ronaDays$date - zeroMonday) %/% 7
  ronaWeeks <- ronaDays[!duplicated(ronaDays[,c('fips','week')]),]
  return(ronaWeeks)
} # ---------------------------------------------------------------------------

cleanVote <- function(){
  
  #' Read csv to data table
  #' Positive margin2020 corresponds to Trump being ahead
  
  pathVote <- "~/git/bhack/data/vote/NYT/presidential.csv"
  vote <- fread(pathVote, 
                select = grep("fips|name|margin2020", 
                names(fread(pathVote, nrow = 0L))))
  return(vote)
} # ---------------------------------------------------------------------------

vote <- cleanVote()
rona <- cleanRona()



