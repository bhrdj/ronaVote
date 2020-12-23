# ronaVote
# Regressing panel data on COVID outcomes on 2020 election data

library(data.table)
library(dplyr)
library(tibble)
library(tidyr)
library(purrr)
library(broom)
library(ggplot2)
setwd("~/git/bhack/nlyz/ronaVote/wrkdir")

# GET DATA ---------------------------------------------------------------------
getData <- function(){
  pathRona <- "~/git/bhack/data/rona/NYT/us-counties.csv"
  pathArea <- "~/git/bhack/data/rona/CountiesLandArea/LND01.csv"
  pathPop <-  "~/git/bhack/data/rona/USDA_PopulationData/PopEst2019.csv"
  pathVote <- "~/git/bhack/data/vote/NYT/presidential.csv"
  
  ronaDays <- fread(pathRona, select = grep(        "fips|date|cases", 
                    names(fread(pathRona, nrow = 0L))))
  ronaDays <- rename(ronaDays, casesT = cases)                                  # renamed "cases" to "casesT"
  area <- fread(pathArea, select = grep(            "fips|mi2",
                    names(fread(pathArea, nrow = 0L))))
  popu <- fread(pathPop, select = grep(             "fips|state|area_name|pop2019",
                    names(fread(pathPop, nrow = 0L))))
  vote <- fread(pathVote, select = grep(            "fips|margin2020|votes$",
                    names(fread(pathVote, nrow = 0L))))
  return(list(ronaDays=ronaDays, area=area, popu=popu, vote=vote))
}
rawData <- getData()

# REMOVE OBSERVATIONS WITH UNKNOWN COUNTY FIPS
rawData <- rawData %>%
  map(function(x) { filter(x, !is.na(fips)) })

# WRITE RAW DATA AS SEPARATE VARIABLES
ronaDays <- rawData[["ronaDays"]]
area <- rawData[["area"]]
popu <- rawData[["popu"]]
vote <- rawData[["vote"]]
rm(rawData)


# DAYS TO WEEKS ----------------------------------------------------------------
zeroMonday <- as.IDate("2019-12-29")                                            # Base date for counting weeks
zeroMondayInt <- as.numeric(zeroMonday)
ronaTall <- ronaDays                                                        %>%
mutate(weekNum = (as.numeric(date) - as.numeric(zeroMondayInt)) %/% 7)      %>% # Number the weeks from zeroMonday
  distinct(fips, weekNum, .keep_all = TRUE)                                 %>% # Remove extra rows after first row/week/county
  mutate(weekDate = as.IDate(weekNum * 7, origin=zeroMonday))               %>% # Reconstruct uniform dates for all rows in each week
  mutate(week_DateT = paste("t", gsub("-", "_", weekDate), sep=""))         %>% # weekDate without "-", with "t" for total cases
  mutate(week_DateW = paste("w", gsub("-", "_", weekDate), sep=""))             # weekDate without "-", with "w" for a week of cases
rm(ronaDays)
#weekDates <- distinct(select(ronaTall, weekDate, week_DateT, week_DateW))      # Put aside weekDates in advance of spread()

# TIDY IT ----------------------------------------------------------------------
ronaWide <- ronaTall                                                        %>%
  left_join(popu, by="fips")                                                %>% # pop2019, area_name, state
  mutate(casesByPopT = (casesT / pop2019) * 100000)                         %>% # Calculate total cases / 100,000 population
  pivot_wider(id_cols = fips, names_from = week_DateT, 
              values_from = casesByPopT)                                    %>% # Only 1 row/county; 1 col/week. Total cases at each week
  left_join(popu, by="fips")                                                %>%
  left_join(area, by="fips")                                                %>%
  left_join(vote, by="fips")                                                %>%
  mutate(fips2 = fips)                                                      %>%
  column_to_rownames(var="fips2")


#remove_rownames %>% 
ronaWideW <- select(ronaWideT, starts_with("w") )                              # NEED TO DIFF AND LOCF THESE COLUMNS Weekly new cases
                       ronaWide <- merge(ronaWideW, ronaWideT, by="fips")# casesByPopT, casesByPopW  # Merge variables into the tidy data frame
                       ronaWide <- merge(ronaWide, popu, by="fips")    # margin2020, votes           
                       ronaWide <- merge(ronaWide, vote, by="fips")    # state, area_name, pop2019
                       ronaWide <- merge(ronaWide, area, by="fips")    # mi2
                       return(ronaWide)
                       


