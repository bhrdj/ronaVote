### ronaVote.r
###
### Regression analysis of 2020 election, mashed with COVID outcomes
###
### Functions:
### - cleanData
### - naive

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
setwd("~/git/bhack/nlyz/ronaVote/wrkdir")

# -----------------------------------------------------------------------------
getData <- function(){
  #' Read csv corona data to data table
  #' Return cases-per-100,000-population, population-per-mi2, 
  #' Time in weeks from approx. the beginning of COVID: 2019-12-29
  pathRona <- "~/git/bhack/data/rona/NYT/us-counties.csv"
  pathArea <- "~/git/bhack/data/rona/CountiesLandArea/LND01.csv"
  pathPop <- "~/git/bhack/data/rona/USDA_PopulationData/PopEst2019.csv"
  ronaDays <- fread(pathRona, select = grep("date|fips|cases", names(fread(pathRona, nrow = 0L))))
  area <- fread(pathArea, select = grep("fips|mi2", names(fread(pathArea, nrow = 0L))))
  popu <- fread(pathPop, select = grep("fips|state|area_name|pop2019", names(fread(pathPop, nrow = 0L))))
  
  zeroMonday <- as.IDate("2019-12-29")
  weekNum <- as.numeric(ronaDays$date - zeroMonday) %/% 7
  ronaDays$week <- paste("w", weekNum, sep = "")
  ronaWeeks <- ronaDays[!duplicated(ronaDays[,c('fips','week')]),]

  area$mi2[area$mi2 == 0.0] <- NA
  ronaWeeks <- merge(ronaWeeks, area, by="fips") #mi2
  ronaWeeks <- merge(ronaWeeks, popu, by="fips") #pop2019, area_name, state
  
  ronaWeeks$casesByPop <- (ronaWeeks$cases / ronaWeeks$pop2019) * 100000
  ronaWeeks$popMi2 <- (ronaWeeks$pop2019 / ronaWeeks$mi2)
  
  ronaTidy <- pivot_wider(data = ronaWeeks, id_cols = fips, names_from = week, values_from = casesByPop)
  ronaTidy <- merge(ronaTidy, popu, by="fips")
  
  stateDummies <- fastDummies::dummy_cols(ronaTidy$state)
  stateDummies$fips <- ronaTidy$fips
  ronaTidy <- merge(ronaTidy, stateDummies, by="fips")
  
  return(ronaTidy)
} # ---------------------------------------------------------------------------

cleanVote <- function(){
  
  #' Read csv to data table
  #' Positive margin2020 corresponds to Trump being ahead
  
  pathVote <- "~/git/bhack/data/vote/NYT/presidential.csv"
  vote <- fread(pathVote, 
                select = grep("fips|margin2020", 
                names(fread(pathVote, nrow = 0L))))
  return(vote)
} # ---------------------------------------------------------------------------

vote <- cleanVote()
rona <- cleanRona()
ronaVote <- merge(rona, vote, by="fips")

ggplot(ronaVote, aes(x=margin2020, y=w49)) + geom_point() + scale_y_log10()
w49_naive <- lm(w49 ~ margin2020, data=ronaVote)
#w49_popmi2 <- lm(w49 ~ margin2020 + density, data=ronaVote)
w49_stateDummies <- lm(w49 ~ margin2020 + .data_AK + .data_AL + .data_AR + .data_AZ + .data_CA + .data_CO + .data_CT + .data_DC + .data_DE + .data_FL + .data_GA + .data_HI + .data_IA + .data_ID + .data_IL + .data_IN + .data_KS + .data_KY + .data_LA + .data_MA + .data_MD + .data_ME + .data_MI + .data_MN + .data_MO + .data_MS + .data_MT + .data_NC + .data_ND + .data_NE + .data_NH + .data_NJ + .data_NM + .data_NV + .data_NY + .data_OH + .data_OK + .data_OR + .data_PA + .data_PR + .data_RI + .data_SC + .data_SD + .data_TN + .data_TX + .data_UT + .data_VA + .data_VT + .data_WA + .data_WI + .data_WV + .data_WY, data=ronaVote)





