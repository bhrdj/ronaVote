### ronaVote.r
### Regression analysis of 2020 election, mashed with COVID outcomes

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
ronaDays <- rawData[["ronaDays"]]
area <- rawData[["area"]]
popu <- rawData[["popu"]]
vote <- rawData[["vote"]]
rm(rawData)

# FILL IN THE NA's (LAST OBSERVATION CARRIED FORWARD) --------------------------
ronaDays_S <- ronaDays %>% pivot_wider(names_from = fips, values_from = casesT)
ronaDays_S$casesT <- zoo::na.locf(ronaDays$casesT)

# DAYS TO WEEKS ----------------------------------------------------------------
days2Weeks <- function(ronaDays) {
  zeroMonday <- as.IDate("2019-12-29")                                          # Base date for counting weeks
  ronaDays$weekNum <- as.numeric(ronaDays$date - zeroMonday) %/% 7              # Number the weeks from zeroMonday
  ronaWeeks <- ronaDays[!duplicated(ronaDays[,c('fips','weekNum')]),]           # Remove extra rows after first row/week/county
  ronaWeeks$weekDate <- as.Date(ronaWeeks$weekNum * 7, origin="2019-12-29")     # Reconstruct uniform dates for all rows in each week
  ronaWeeks$week_DateT <- paste("t", gsub("-", "_", ronaWeeks$weekDate), sep="") # weekDate without "-", with "t" for total cases
  ronaWeeks$week_DateW <- paste("w", gsub("-", "_", ronaWeeks$weekDate), sep="") # weekDate without "-", with "w" for a week of cases
  return(ronaWeeks)
} 
ronaWeeks <- days2Weeks(ronaDays)
rm(ronaDays)
weekDates <- distinct(select(ronaWeeks, weekDate, week_DateT, week_DateW))      # Put aside weekDates in advance of spread()


# TIDY IT ----------------------------------------------------------------------
# ronaTall <- ronaWeeks
tidyIt <- function(ronaTall, area, popu, vote) {
  ronaTall <- merge(ronaTall, popu, by="fips")    #pop2019, area_name, state
  ronaTall$casesByPopT <- (ronaTall$casesT / ronaTall$pop2019) * 100000         # Calculate total cases / 100,000 population
  ronaWideT <- pivot_wider(data = ronaTall, id_cols = fips, 
                          names_from = week_DateT, values_from = casesByPopT)   # Only 1 row/county; 1 col/week. Total cases at each week
*  ronaWideW <- select(ronaWideT, starts_with("w")                               # NEED TO DIFF AND LOCF THESE COLUMNS Weekly new cases
  ronaWide <- merge(ronaWideW, ronaWideT, by="fips")# casesByPopT, casesByPopW  # Merge variables into the tidy data frame
  ronaWide <- merge(ronaWide, popu, by="fips")    # margin2020, votes           
  ronaWide <- merge(ronaWide, vote, by="fips")    # state, area_name, pop2019
  ronaWide <- merge(ronaWide, area, by="fips")    # mi2
  return(ronaWide)
}
rona <- tidyIt(ronaWeeks, area, popu, vote)
rm(ronaWeeks, area, popu, vote)

# CONSTRUCT CALCULATED VARIABLES -----------------------------------------------
constructVars <- function(rona) {
  rona$mi2[rona$mi2 == 0.0] <- NA                           # area in sq. mi.   # ?? WHAT ARE THE ZERO-AREA COUNTIES ??
  rona$popMi2 <- (rona$pop2019 / rona$mi2)                  # pop / sq. mi/     # Calculate population density of counties
  
  stateDummies <- fastDummies::dummy_cols(rona$state)                           # make the dummy variables for state fixed effects
  stateDummies$fips <- rona$fips                                                # give dummy variable data frame fips id's by county
  rona <- merge(rona, stateDummies, by="fips")                                  # merge state dummies into the rona data frame
  return(rona)
}
rona <- constructVars(rona)
rona <- rona[,colSums(is.na(rona))<nrow(rona)]                                  # remove any all-NA columns
# REGRESS ----------------------------------------------------------------------
#yvars <- select(rona, starts_with("w"))                                         
lm_allT <- map(select(rona, starts_with("t")),
            function(yvar) {
              return( lm(yvar ~ margin2020, rona) )
            }
)

lm_allW <- map(select(rona, starts_with("w")),
               function(yvar) {
                 return( lm(yvar ~ margin2020, rona) )
               }
)

# GET REGRESSION OUTPUT --------------------------------------------------------
lm_outT <- map(lm_allT, function(an_lm) {
                c( tidy(an_lm)$estimate[2],
                   glance(an_lm)$r.squared,
                   glance(an_lm)$p.value )
              }
)

lm_outW <- map(lm_allW,
               function(an_lm) {
                 c( tidy(an_lm)$estimate[2],
                    glance(an_lm)$r.squared,
                    glance(an_lm)$p.value )
               }
)

# PROCESS REGRESSION OUTPUT ----------------------------------------------------
lm_dfT = as.data.frame(do.call(rbind, lm_outT))
colnames(lm_dfT) <- c("TrumpCountiesMoreCovid", "RSquared", "PValue")
lm_dfT2 = rownames_to_column(lm_dfT, var = "week_DateT")
lm_dfT2 <- left_join(lm_dfT2, weekDates, by = "week_DateT")

lm_dfW = as.data.frame(do.call(rbind, lm_outW))
colnames(lm_dfW) <- c("TrumpCountiesMoreCovid", "RSquared", "PValue")
lm_dfW2 = rownames_to_column(lm_dfW, var = "week_DateW")
lm_dfW2 <- left_join(lm_dfW2, weekDates, by = "week_DateW")


rona %>% ggplot(aes(x=margin2020, y=t2020_08_16)) + scale_y_log10() + geom_point()
rona %>% ggplot(aes(x=margin2020, y=t2020_12_06)) + scale_y_log10() + geom_point()
rona %>% ggplot(aes(x=margin2020, y=w2020_08_16)) + scale_y_log10() + geom_point()
rona %>% ggplot(aes(x=margin2020, y=w2020_12_06)) + scale_y_log10() + geom_point()
lm_dfT2 <- filter(lm_df, weekDate > as.Date("2020-04-12"))
lm_dfT2 %>% ggplot(aes(x=weekDate, y=RSquared)) + geom_point()
lm_dfT2 %>% ggplot(aes(x=weekDate, y=PValue)) + geom_point()
lm_dfT2 %>% ggplot(aes(x=weekDate, y=TrumpCountiesMoreCovid)) + geom_point()



# 17.5 million cases in the USA total accumulated by Dec 19 2020 (NYT)
# 330 million total population in the USA in Dec 2020 (census.gov/popclock)
# -> 5.3 % of Americans have been counted as a COVID case by Dec 2020
# A regression coefficient of 10 on the chart implies this non-causal correlation:
# -> For each additional percentage point that a county leaned Trump, the county
#    now has 10 extra cases. That is, 1 case per 10k population per %-point:
#    10 extra cases / 100,000 population / 1 %-point (Trump vote% - Biden vote%)
# -> In summary, an average county today that had voted 60% for Trump and 40% for
#    Biden (+20% margin), compared to a 60%-40% Biden county (-20% margin), has
#    experienced (40 * 10 / 100k) = 0.4% more covid cases. And as you can see,
#    this trend continues to exacerbate today, despite the fact that the virus
#    itself spreads in a non-partisan way.
#    

#PLAN
# A - FINISH CURRENT STATUS
# 1. Make a chart of regression coeff and R2 for naive and state dummies over 49 wks
# 2. Discussion of interpretation of the magnitude of the regression coeff (not causal)
# B - SURVEY OF DATA IN PREPARATION FOR FIXED EFFECTS
# 3. Make & browse charts of new cases/wk for subsets of counties:
#     - High- & low-pop counties by quartile
#     - High- & low-pop-density counties by quartile
#     - Counties with top international airports versus the average
# 4. Refine plan for criteria for designating fixed effects based on magnitude of "first wave"
#     - Find counties that surpassed a per-cap floor before a threshold date
#     - Sort cases/week county time series into 2 baskets
#     - Review tiled charts, and a vector of first-peaks to pick a threshold
#     - Regress covid current outcomes on vote spread, using basket fixed effects
# 5. Refine?
#     - Use weekly data only at first, then later refine with daily-rolling weekly averages
#     ? Counties that surpassed a per-cap floor before a threshold date and missed the next ceiling?
#     ? Counties that surpassed floor by a date, and then decreased below a ceiling by a date?
#     ? Counties that never reached the floor by a date?



na.locf <- function(x) {                                                        # Last observation carried forward
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}


ronaWeeks$casesW <- append(diff(ronaWeeks$casesT), NA, 0)                       # Calculate the delta each week in total cases
ronaTall$casesByPopW <- (ronaTall$casesW / ronaTall$pop2019) * 100000         # Calculate total cases / 100,000 population
