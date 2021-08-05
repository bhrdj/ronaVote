# ronaVoteShiny.R
# Ongoing COVID and the 2020 USA Election: Time Trends in a County Correlation

# CODE AND INFO FOR LATER ------------------------------------------------------
# pathRona <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
# pathVote <- "https://raw.githubusercontent.com/bhrdj/ronaVote/main/data/vote/Harvard/countypres_2000-2020.csv"
# pathPop <- "https://raw.githubusercontent.com/bhrdj/ronaVote/main/data/countyData/USDA_PopulationData/PopEst2019.csv"

# library(rsconnect)
# rsconnect::deployApp('~/git/RmdSandbox/')
# https://bhrdj.shinyapps.io/rmdsandbox/

# https://www.shinyapps.io/admin/#/dashboard

# RUN THESE TO CLEAR LOCAL DATA BUILDING UP IN R WORKSPACE
# rm(list = ls())
# .rs.restartR()

# IMPORTING LIBRARIES ----------------------------------------------------------
library(data.table)
library(broom)
library(tidyverse)
library(shiny)
library(bslib)
library(thematic)

# IMPORTING DATA ---------------------------------------------------------------
pathRona <- "./data/rona/NYT/us-counties_panel_2021-08-02.csv"
pathVote <- "./data/vote/Harvard/countypres_2000-2020.csv"
pathPop <- "./data/countyData/USDA_PopulationData/PopEst2019.csv"

ronaDays <- fread(pathRona, select = grep(  "fips|date|cases",
                                            names(fread(pathRona, nrow = 0L)))) %>%
                                            rename(casesTot = cases)
popu <- fread(pathPop, select = grep(       "fips|state|area_name|pop2019",
                                            names(fread(pathPop, nrow = 0L))))
vote <- fread(pathVote, select = grep(      "year|party|mode|candidate|county_name|county_fips|candidatevotes|totalvotes",
                                            names(fread(pathVote, nrow = 0L)))) %>%
                                            rename(fips = county_fips)

rawData <- list(ronaDays=ronaDays, popu=popu, vote=vote) %>%                    # Clean up the workspace
    map(function(x) { filter(x, !is.na(fips)) })
rm(pathRona, pathVote, pathPop, ronaDays, popu, vote)

# ORGANIZE AND CLEAN TALL (MELTED) DATA ----------------------------------------
zeroMonday <- as.IDate("2019-12-29")                                            # Base date for counting weeks
zeroMondayInt <- as.numeric(zeroMonday)

ronaTall <- rawData[["ronaDays"]] %>%
    mutate(weekNum = (as.numeric(date) - as.numeric(zeroMondayInt)) %/% 7) %>%  # Number the weeks from zeroMonday
    distinct(fips, weekNum, .keep_all = TRUE) %>%                               # Remove extra rows after first row/week/county
    mutate(weekDate = as.IDate(weekNum * 7, origin=zeroMonday)) %>%             # Reconstruct uniform dates for all rows in each week
    mutate(week_DateT = paste("T", gsub("-", "_", weekDate), sep="")) %>%       # for casesDiff row/colnames: with _ instead of -, +prefix "w" for "week number"
    mutate(fipsText = paste("x", fips, sep = ""))                                  # for col or row names

weekDates <- distinct(ronaTall, week_DateT, weekDate) #THIS IS NEW
fips_fipsText <- distinct(ronaTall, fips, fipsText, .keep_all = FALSE)                # keep fips fipsText dictionary

# TIDY vote: WIDEN BY candidate ------------------------------------------------
vote_wide <- rawData[["vote"]] %>%
    filter(year == 2020) %>%
    select(-c(year)) %>%
    filter((candidate != "JO JORGENSEN") & (candidate != "OTHER")) %>% 
    pivot_wider(names_from = party, 
                values_from = candidatevotes,
                id_cols = -c(party, candidatevotes, candidate))  %>%
    group_by(fips) %>%
    summarize(DEMOCRAT = sum(DEMOCRAT),
              REPUBLICAN = sum(REPUBLICAN),
              county_name = unique(county_name), 
              totalvotes = unique(totalvotes)) %>%
    mutate(DJT_Margin = (REPUBLICAN - DEMOCRAT) / totalvotes)

# TIDY casesTot: WIDEN, NA'S-TO-ZEROES, JOIN WITH popu AND vote_wide------------
ronaSectionsTot <- ronaTall  %>%                                                # WIDEN rona FOR TOTAL CASES COLUMNS NAMED BY WEEKDATES
    pivot_wider(id_cols = fipsText,                                             # ONE ROW FOR EACH FIPS COUNTY
                names_from = week_DateT, 
                values_from = casesTot) %>%                                     # Only 1 row/county; 1 col/week. Total cases at each week
    mutate(across(starts_with("T"), ~replace_na(., 0))) %>%                     # REPLACE casesTot NA'S WITH ZEROES
    left_join(fips_fipsText, by = "fipsText") %>%
    column_to_rownames(var = "fipsText") %>%
    left_join(rawData[["popu"]], by="fips") %>%
    left_join(vote_wide, by="fips")  %>%
    filter(!is.na(pop2019))

# ADD COLUMN CASES-PER-CAPITA -------------
# REMOVE OTHER VARS
rona <- ronaSectionsTot %>%
    mutate(across(starts_with("T"), function(x) {x/pop2019} ))

rm(fips_fipsText, rawData, ronaTall) #,  ronaSectionsTot)

# RUN REGRESSIONS --------------------------------------------------------------
lm_allT <-  map(select(rona, starts_with("T")), 
                       function(yvar) {lm(yvar ~ DJT_Margin, rona)} )

# GET REGRESSION OUTPUT --------------------------------------------------------
# UNITS OF ESTIMATE: 
#   Y: vote margin (decimal)   X: cases/population 
lm_outT <- map(lm_allT, 
               function(an_lm) {
                   c( tidy(an_lm)$estimate[2], 
                      glance(an_lm)$r.squared,
                      glance(an_lm)$p.value )
               })

# PROCESS REGRESSION OUTPUT ----------------------------------------------------
lm_dfT <- as.data.frame(do.call(rbind, lm_outT))                                 # Data frame of Total Cases 
colnames(lm_dfT) <- c("Correlation_MoreTrumpMargin_MoreCovid", "RSquared", "PValue")
lm_dfT2 <- lm_dfT %>% 
    rownames_to_column(var = "week_DateT") %>%
    left_join(weekDates, by = "week_DateT") %>%
    filter(weekDate > as.Date("2020-04-12"))

# PLOT -------------------------------------------------------------------------
thematic_shiny()

# Define UI for application that draws a histogram
ui <- fluidPage(
    # theme = shinytheme("slate"),
    theme = bs_theme(version = 4, bootswatch = "darkly"),
    titlePanel("Trend in Correlation Over Time"),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot1")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot1 <- renderPlot({
        lm_dfT2 %>% ggplot(aes(x=weekDate, y=Correlation_MoreTrumpMargin_MoreCovid)) + geom_point()
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
