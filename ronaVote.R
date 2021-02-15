# ronaVote
# Regressing panel data on COVID outcomes on 2020 election data

library(data.table)
library(broom)
library(tidyverse)

setwd("~/git/ronaVote")

# GET DATA, REMOVE ROWS W/ UNKNOWN FIPS, CONVERT>WEEKLY OBSERVATIONS --------  later: pad fips with zeroes 
# GET DATA
getData <- function(){
  pathRona <- "~/git/ronaVote/data/rona/NYT/us-counties_2021-02-15.csv"
  pathArea <- "~/git/ronaVote/data/rona/CountiesLandArea/LND01.csv"
  pathPop <-  "~/git/ronaVote/data/rona/USDA_PopulationData/PopEst2019.csv"
  pathVote <- "~/git/ronaVote/data/vote/NYT/presidential.csv"
  
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
  mutate(weekNum = (as.numeric(date) - as.numeric(zeroMondayInt)) %/% 7) %>%    # Number the weeks from zeroMonday
  distinct(fips, weekNum, .keep_all = TRUE) %>%                                 # Remove extra rows after first row/week/county
  mutate(weekDate = as.IDate(weekNum * 7, origin=zeroMonday)) %>%               # Reconstruct uniform dates for all rows in each week
  mutate(week_DateT = paste("T", gsub("-", "_", weekDate), sep="")) %>%         # for casesDiff row/colnames: with _ instead of -, +prefix "w" for "week number"
  mutate(week_DateW = paste("W", gsub("-", "_", weekDate), sep="")) %>%         # for casesTot row/colnames: with _ instead of -, +prefix "w" for "week number"
  mutate(fips2 = paste("x", fips, sep = ""))                                    # for col or row names

weekDates <- distinct(ronaTall, week_DateT, week_DateW, weekDate) #THIS IS NEW
fips_fips2 <- distinct(ronaTall, fips, fips2, .keep_all = FALSE)                # keep fips fips2 dictionary
  
# TIDY casesTot: WIDEN, NA'S-TO-ZEROES -----------------------------------------
ronaSectionsTot <- ronaTall  %>%                                                # WIDEN FOR COLUMNS NAMED BY WEEKDATES
  pivot_wider(id_cols = fips2, 
              names_from = week_DateT, 
              values_from = casesTot) %>%                                       # Only 1 row/county; 1 col/week. Total cases at each week
  mutate(across(starts_with("T"), ~replace_na(., 0)))                           # REPLACE casesTot NA'S WITH ZEROES

ronaSeriesesTot <- ronaTall  %>%                                                # WIDEN FOR COLUMNS NAMED BY WEEKDATES
  pivot_wider(id_cols = week_DateW, 
              names_from = fips2, 
              values_from = casesTot) %>%                                       # Only 1 row/county; 1 col/week. Total cases at each week
  mutate(across(starts_with("x"), ~replace_na(., 0)))                           # REPLACE casesTot NA'S WITH ZEROES

# DIFF SERIES, TRANSPOSE TO SECTIONS, JOIN -------------------------------------
# DIFF SERIES
diffit <- function(x) { {{x}} - lag({{x}}) } 
ronaDiff <- ronaSeriesesTot %>%
  mutate(across(starts_with("x"), ~diffit(.) ))

# TRANSPOSE ronaDiff TO SECTIONS, JOIN TO MAKE rona
ronaAllSections <- ronaDiff %>%
  pivot_longer(cols = starts_with("x"), names_to = "fips2") %>%
  pivot_wider(id_cols = fips2, 
              names_from = week_DateW, 
              values_from = "value") %>%
  left_join(ronaSectionsTot, by="fips2") %>%
  left_join(fips_fips2, by = "fips2") %>%
  column_to_rownames(var = "fips2") %>%
  left_join(rawData[["popu"]], by="fips") %>%
  left_join(rawData[["area"]], by="fips") %>%
  left_join(rawData[["vote"]], by="fips") # %>%

rm(fips_fips2, rawData, ronaSectionsTot, ronaSeriesesTot, ronaTall) #, ronaDiff)

# REMOVE OBSERVATIONS WITH pop2019 == NA
ronaAllSections <- ronaAllSections %>% filter(!is.na(pop2019))

# CONSTRUCT CALCULATED VARIABLES INCLUDING CASES-PER-CAPITA --------------------
tP = sum(ronaAllSections$pop2019)                                     # total Population
rona <- ronaAllSections %>%
  mutate(mi2 = ifelse(mi2 == 0, NA, mi2)) %>%               # area in sq. mi.   # ?? WHAT ARE THE ZERO-AREA COUNTIES ??
  mutate(popMi2 = (pop2019 / mi2)) %>%                      # pop / sq. mi/     # Calculate population density of counties
  mutate(across(starts_with("T"), function(x) {x/pop2019/100000} )) %>%         # Make total cases / 100,000 population variables
  mutate(across(starts_with("W"), function(x) {x/pop2019/100000} )) %>%         # "    (weekly new cases)  "     "      "
  arrange(margin2020) %>%                                  # put counties in order of vote margin
  mutate(cP = cumsum(pop2019)) %>%                         # new column = cumulative Sum of Population in order of vote margin
  mutate(margin_qtile = ifelse(cP>tP*.75,4, ifelse(cP>tP*.5,3, ifelse(cP>tP*.25,2, 1)))) # label counties by their vote margin quartile (pop-weighted)

# ===============================================================================================
# CURRENT FRONTIER
# ===============================================================================================


ronaQuartiles <- rona %>%
  group_by(margin_qtile) %>%
  slice(1) %>%
  ungroup %>%
  print

rona2 %>% 
  ggplot(aes(x = margin2020, y = ..density.., weight = pop2019)) + geom_histogram() + 
  geom_vline(xintercept={{ronaQuartiles$margin2020[2]}}, size=.3) +
  geom_vline(xintercept={{ronaQuartiles$margin2020[3]}}, size=.3) + 
  geom_vline(xintercept={{ronaQuartiles$margin2020[4]}}, size=.3)

rona %>%
  filter()


# y = ..density.., weight = pop2019

 #%>%   # number quartiles
  # number population-weighted quartiles







# REGRESS ----------------------------------------------------------------------

lm_allT <- map(select(rona, starts_with("T")),
               function(yvar) {
                 return( lm(yvar ~ margin2020, rona) )
               })

l <- length(rona)
lm_allW <- map(select(rona[2:l], starts_with("W")),
               function(yvar) {
                 return( lm(yvar ~ margin2020, rona) )
               })

# GET REGRESSION OUTPUT --------------------------------------------------------
lm_outT <- map(lm_allT, 
               function(an_lm) {
                 c( tidy(an_lm)$estimate[2], 
                    glance(an_lm)$r.squared,
                    glance(an_lm)$p.value )
                 })

lm_outW <- map(lm_allW,
               function(an_lm) {
                 c( tidy(an_lm)$estimate[2],
                    glance(an_lm)$r.squared,
                    glance(an_lm)$p.value )
                 })




# PROCESS REGRESSION OUTPUT ----------------------------------------------------
lm_dfT = as.data.frame(do.call(rbind, lm_outT))                                 # Data frame of Total Cases 
colnames(lm_dfT) <- c("TrumpCountiesMoreCovid", "RSquared", "PValue")
lm_dfT2 = rownames_to_column(lm_dfT, var = "week_DateT")                        
lm_dfT2 <- left_join(lm_dfT2, weekDates, by = "week_DateT")

lm_dfW = as.data.frame(do.call(rbind, lm_outW))
colnames(lm_dfW) <- c("TrumpCountiesMoreCovid", "RSquared", "PValue")
lm_dfW2 = rownames_to_column(lm_dfW, var = "week_DateW")
lm_dfW2 <- left_join(lm_dfW2, weekDates, by = "week_DateW")

lm_dfT2 <- filter(lm_dfT2, weekDate > as.Date("2020-04-12"))
lm_dfW2 <- filter(lm_dfW2, weekDate > as.Date("2020-04-12"))

rona %>% ggplot(aes(x=margin2020, y=T2020_08_16)) + scale_y_log10() + geom_point()
rona %>% ggplot(aes(x=margin2020, y=T2020_12_27)) + scale_y_log10() + geom_point()
rona %>% ggplot(aes(x=margin2020, y=W2020_08_16)) + scale_y_log10() + geom_point()
rona %>% ggplot(aes(x=margin2020, y=W2020_12_27)) + scale_y_log10() + geom_point()
lm_dfT2 %>% ggplot(aes(x=weekDate, y=RSquared)) + geom_point()
lm_dfT2 %>% ggplot(aes(x=weekDate, y=PValue)) + geom_point()
lm_dfT2 %>% ggplot(aes(x=weekDate, y=TrumpCountiesMoreCovid)) + geom_point()
lm_dfW2 %>% ggplot(aes(x=weekDate, y=RSquared)) + geom_point()
lm_dfW2 %>% ggplot(aes(x=weekDate, y=PValue)) + geom_point()
lm_dfW2 %>% ggplot(aes(x=weekDate, y=TrumpCountiesMoreCovid)) + geom_point()



# ===============================================================================================
# SOME SANDBOX
# ===============================================================================================



xy <- data.frame(v1 = c(NA,2,3), v2 = c(11,12,13)) %>%
  mutate(rn = c("r1", "r2", "r3")) %>%
  column_to_rownames(var = "rn") %>%
  mutate(across(everything(), function(x) {x+1})) %>%   # replace_na(0)
  print()



na.locf <- function(x) {                                                        # Last observation carried forward
  v <- !is.na(x)
  c(NA, x[v])[cumsum(v)+1]
}

  
# ===============================================================================================
# SOME IDEAS
# ===============================================================================================

# Make state dummies
stateDummies <- fastDummies::dummy_cols(rona$state)                           # make the dummy variables for state fixed effects
stateDummies$fips <- rona$fips                                                # give dummy variable data frame fips id's by county
rona <- merge(rona, stateDummies, by="fips")                                  # merge state dummies into the rona data frame



# CHECK FOR: NEGATIVE DIFFERENCES FROM WEEK TO WEEK
# https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value

# ITEMS TO INCLUDE IN CODE LATER, MAYBE ----------------------------------------
# later: pad fips with zeroes 
# crossing() instead of join when no key column
# left_join(popu, by="fips")                                                %>% # pop2019, area_name, state
# mutate(casesByPopTot = (casesTot / pop2019) * 100000)                     %>% # Calculate total cases / 100,000 population
