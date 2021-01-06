### ronaVote.r
### Regression analysis of 2020 election, mashed with COVID outcomes

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
setwd("~/git/bhack/nlyz/ronaVote/wrkdir")

# -----------------------------------------------------------------------------
getData <- function(){
  pathRona <- "~/git/bhack/data/rona/NYT/us-counties.csv"
  pathArea <- "~/git/bhack/data/rona/CountiesLandArea/LND01.csv"
  pathPop <- "~/git/bhack/data/rona/USDA_PopulationData/PopEst2019.csv"
  pathVote <- "~/git/bhack/data/vote/NYT/presidential.csv"
  
  ronaDays <- fread(pathRona, select = grep("date|fips|cases", names(fread(pathRona, nrow = 0L))))
  area <- fread(pathArea, select = grep("fips|mi2", names(fread(pathArea, nrow = 0L))))
  popu <- fread(pathPop, select = grep("fips|state|area_name|pop2019", names(fread(pathPop, nrow = 0L))))
  vote <- fread(pathVote, select = grep("fips|margin2020", names(fread(pathVote, nrow = 0L))))

  return(list(ronaDays=ronaDays, area=area, popu=popu, vote=vote))
}
rawData <- getData()
ronaDays <- rawData[["ronaDays"]]
area <- rawData[["area"]]
popu <- rawData[["popu"]]
vote <- rawData[["vote"]]
rm(rawData)

days2Weeks <- function(ronaDays) {
  zeroMonday <- as.IDate("2019-12-29")
  ronaDays$weekNum <- as.numeric(ronaDays$date - zeroMonday) %/% 7
  ronaWeeks <- ronaDays[!duplicated(ronaDays[,c('fips','weekNum')]),]
  ronaWeeks$weekDate <- as.Date(ronaWeeks$weekNum * 7, origin="2019-12-29")
  ronaWeeks$week_Date <- paste("w", gsub("-", "_", ronaWeeks$weekDate), sep="")
  return(ronaWeeks)
} 

ronaWeeks <- days2Weeks(ronaDays)
rm(ronaDays)

weekDateVec <- ronaWeeks[!duplicated(ronaWeeks[,"weekDate"]), weekDate, week_Date]

# ronaTall <- ronaWeeks
tidyIt <- function(ronaTall, area, popu, vote) {
  ronaTall <- merge(ronaTall, popu, by="fips") #pop2019, area_name, state
  ronaTall$casesByPop <- (ronaTall$cases / ronaTall$pop2019) * 100000
  ronaWide <- pivot_wider(data = ronaTall, id_cols = fips, names_from = week_Date, values_from = casesByPop)

  ronaWide <- merge(ronaWide, popu, by="fips")
  ronaWide <- merge(ronaWide, vote, by="fips")
  ronaWide <- merge(ronaWide, area, by="fips") #mi2
  return(ronaWide)
}
rona <- tidyIt(ronaWeeks, area, popu, vote)
rm(ronaWeeks, area, popu, vote)

constructVars <- function(rona) {
  rona$mi2[rona$mi2 == 0.0] <- NA
  rona$popMi2 <- (rona$pop2019 / rona$mi2)
  
  stateDummies <- fastDummies::dummy_cols(rona$state)
  stateDummies$fips <- rona$fips
  rona <- merge(rona, stateDummies, by="fips")
  return(rona)
}
rona <- constructVars(rona)

# ---------------------------------------------------------------------------
ggplot(rona, aes(x=margin2020, y=w2020_03_01)) + geom_point() + scale_y_log10()
ggplot(rona, aes(x=margin2020, y=w2020_05_10)) + geom_point() + scale_y_log10()
ggplot(rona, aes(x=margin2020, y=w2020_07_19)) + geom_point() + scale_y_log10()
ggplot(rona, aes(x=margin2020, y=w2020_09_27)) + geom_point() + scale_y_log10()
ggplot(rona, aes(x=margin2020, y=w2020_12_06)) + geom_point() + scale_y_log10()

naive <- lm(w2020_12_06 ~ margin2020, data=rona)
w2020_12_06_margin2020_popMi2 <- lm(w2020_12_06 ~ margin2020 + popMi2, data=rona)
#w49_margin2020_stateDummies <- lm(w49 ~ margin2020 + .data_AK + .data_AL + .data_AR + .data_AZ + .data_CA + .data_CO + .data_CT + .data_DC + .data_DE + .data_FL + .data_GA + .data_HI + .data_IA + .data_ID + .data_IL + .data_IN + .data_KS + .data_KY + .data_LA + .data_MA + .data_MD + .data_ME + .data_MI + .data_MN + .data_MO + .data_MS + .data_MT + .data_NC + .data_ND + .data_NE + .data_NH + .data_NJ + .data_NM + .data_NV + .data_NY + .data_OH + .data_OK + .data_OR + .data_PA + .data_PR + .data_RI + .data_SC + .data_SD + .data_TN + .data_TX + .data_UT + .data_VA + .data_VT + .data_WA + .data_WI + .data_WV + .data_WY, data=rona)





