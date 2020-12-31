# ronaVote
# Regressing panel data on COVID outcomes on 2020 election data

library(data.table)
library(broom)
library(tidyverse)

setwd("~/git/bhack/nlyz/ronaVote/wrkdir")

# GET DATA, REMOVE ROWS W/ UNKNOWN FIPS, CONVERT>WEEKLY OBSERVATIONS --------  later: pad fips with zeroes 
# GET DATA
getData <- function(){
  pathRona <- "~/git/bhack/data/rona/NYT/us-counties.csv"
  pathArea <- "~/git/bhack/data/rona/CountiesLandArea/LND01.csv"
  pathPop <-  "~/git/bhack/data/rona/USDA_PopulationData/PopEst2019.csv"
  pathVote <- "~/git/bhack/data/vote/NYT/presidential.csv"
  
  ronaDays <- fread(pathRona, select = grep(        "fips|date|cases", 
                    names(fread(pathRona, nrow = 0L))))
  ronaDays <- rename(ronaDays, casesTot = cases)    # renamed "cases" to "casesTot"
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

# DAYS TO WEEKS
zeroMonday <- as.IDate("2019-12-29")                                            # Base date for counting weeks
zeroMondayInt <- as.numeric(zeroMonday)

ronaTall <- rawData[["ronaDays"]] %>%
mutate(weekNum = (as.numeric(date) - as.numeric(zeroMondayInt)) %/% 7) %>%      # Number the weeks from zeroMonday
  distinct(fips, weekNum, .keep_all = TRUE) %>%                                 # Remove extra rows after first row/week/county
  mutate(weekDate = as.IDate(weekNum * 7, origin=zeroMonday)) %>%               # Reconstruct uniform dates for all rows in each week
  mutate(week_Date = paste("w", gsub("-", "_", weekDate), sep="")) %>%          # weekDate for row/colnames: with _ instead of -, +prefix "w" for "week number"
  mutate(fips2 = paste("x", fips, sep = ""))                                    # for col or row names
  
# later: pad fips with zeroes 
  
# TIDY casesTot: WIDEN, NA'S-TO-ZEROES --------------------------------------------
ronaSectionsTot <- ronaTall  %>%                                                # WIDEN FOR COLUMNS NAMED BY WEEKDATES
  pivot_wider(id_cols = fips2, names_from = week_Date, values_from = casesTot) %>% # Only 1 row/county; 1 col/week. Total cases at each week
  mutate(across(starts_with("w"), ~replace_na(., 0)))                           # REPLACE casesTot NA'S WITH ZEROES

ronaSeriesesTot <- ronaTall  %>%                                                    # WIDEN FOR COLUMNS NAMED BY WEEKDATES
  pivot_wider(id_cols = fips, names_from = week_Date, values_from = casesTot) %>% # Only 1 row/county; 1 col/week. Total cases at each week
  mutate(across(starts_with("w"), ~replace_na(., 0)))                           # REPLACE casesTot NA'S WITH ZEROES

# ===============================================================================================
# CURRENT FRONTIER
# ===============================================================================================

# DIFF SERIES, TRANSPOSE TO SECTIONS, JOIN -------------------------------------
# TRANSPOSE
r <- ronaWideTot %>%
  mutate(fips2 = paste("x", fips, sep = ""))                                %>% # in case I want to save the fips
  pivot_longer(-fips) %>% 
  pivot_wider(names_from=fips2, values_from=value) %>%
  column_to_rownames(var="name")





# JOIN DESCRIPTIVE DATA      ---------------------------------------------------
  left_join(popu, by="fips")                                                %>%
  left_join(area, by="fips")                                                %>%
  left_join(vote, by="fips")                                                %>%
  
  mutate(fips3 = paste("x", fips, sep = ""))                                %>%
  column_to_rownames(var="fips3")



  # CHECK FOR: NEGATIVE DIFFERENCES FROM WEEK TO WEEK
  # https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

rm(area, popu, vote, ronaTall)


#  END OF CURRENT CODE =========================================================


diff_df <- function(df, exceptCol = "name", newPrefix = "D") {
  df %>%
    mutate(. - lag(.)) %>%
    rename_with(function(x) paste0({{newPrefix}}, x)) %>%
    return(.)
}

checkit <- function(countyNum) {cbind(r = r[,countyNum], d =  d[,countyNum])}

ronaWideTot_t <- ronaWideTot %>%
  select(fips2, starts_with("t")) %>%
  pivot_longer(-fips2) %>% 
  pivot_wider(names_from=fips2, values_from=value)


x <- left_join(name = ronaWideWk$name, ronaWideWk2)

# TEMPLATE CODE PRACTICE -------------------------------------------------------

df <- tibble(x = c(NA, 1,3,4,5,7))
df2 = df - lag(df)
replace_na(df2, list(x = 0))

xy <- data.frame(v1 = c(NA,2,3), v2 = c(11,12,13)) %>%
  mutate(rn = c("r1", "r2", "r3")) %>%
  column_to_rownames(var = "rn") %>%
  mutate(across(everything(), function(x) {x+1})) %>%   # replace_na(0)
  print()

# ITEMS TO INCLUDE IN CODE LATER, MAYBE ----------------------------------------

# crossing() instead of join when no key column
# left_join(popu, by="fips")                                                %>% # pop2019, area_name, state
# mutate(casesByPopTot = (casesTot / pop2019) * 100000)                     %>% # Calculate total cases / 100,000 population
  