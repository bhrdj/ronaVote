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


library(shiny)
library(scales)
library(data.table)
library(broom)
library(tidyverse)

pathRona <- "/home/bhrdwj/git/ronaVote/data/rona/NYT/us-counties_panel_2021-07-17.csv"
pathVote <- "/home/bhrdwj/git/ronaVote/data/vote/Harvard/countypres_2000-2020.csv"
pathPop <- "/home/bhrdwj/git/ronaVote/data/countyData/USDA_PopulationData/PopEst2019.csv"

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

zeroMonday <- as.IDate("2019-12-29")                                            # Base date for counting weeks
zeroMondayInt <- as.numeric(zeroMonday)

ronaTall <- rawData[["ronaDays"]] %>%
    mutate(weekNum = (as.numeric(date) - as.numeric(zeroMondayInt)) %/% 7) %>%  # Number the weeks from zeroMonday
    distinct(fips, weekNum, .keep_all = TRUE) %>%                               # Remove extra rows after first row/week/county
    mutate(weekDate = as.IDate(weekNum * 7, origin=zeroMonday)) %>%             # Reconstruct uniform dates for all rows in each week
    mutate(week_DateT = paste("T", gsub("-", "_", weekDate), sep="")) %>%       # for casesDiff row/colnames: with _ instead of -, +prefix "w" for "week number"
    mutate(week_DateW = paste("W", gsub("-", "_", weekDate), sep="")) %>%       # for casesTot row/colnames: with _ instead of -, +prefix "w" for "week number"
    mutate(fips2 = paste("x", fips, sep = ""))                                  # for col or row names

weekDates <- distinct(ronaTall, week_DateT, week_DateW, weekDate) #THIS IS NEW
fips_fips2 <- distinct(ronaTall, fips, fips2, .keep_all = FALSE)                # keep fips fips2 dictionary


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
    mutate(DEM_Margin = (DEMOCRAT - REPUBLICAN) / totalvotes)



# TIDY casesTot: WIDEN, NA'S-TO-ZEROES -----------------------------------------
ronaSectionsTot <- ronaTall  %>%                                                # WIDEN rona FOR TOTAL CASES COLUMNS NAMED BY WEEKDATES
    pivot_wider(id_cols = fips2,                                                # ONE ROW FOR EACH FIPS COUNTY
                names_from = week_DateT, 
                values_from = casesTot) %>%                                       # Only 1 row/county; 1 col/week. Total cases at each week
    mutate(across(starts_with("T"), ~replace_na(., 0)))                           # REPLACE casesTot NA'S WITH ZEROES


# TIDY TO TIME SERIES TO CALCULATE WEEKLY NEW CASES FROM ACCUMULATED TOTAL CASES
ronaSeriesesTot <- ronaTall  %>%                                                # WIDEN TO ONE OBS PER COUNTY, ONE FIELD PER WEEK
    pivot_wider(id_cols = week_DateW,                                           # ROW F/EA WEEKDATE, COL F/EA FIPS
                names_from = fips2, 
                values_from = casesTot) %>%                                       # Only 1 row/county; 1 col/week. Total cases at each week
    mutate(across(starts_with("x"), ~replace_na(., 0)))                           # REPLACE casesTot NA'S WITH ZEROES




# DIFF SERIES, TRANSPOSE TO SECTIONS, JOIN -------------------------------------
# DIFF SERIES
diffit <- function(x) { {{x}} - lag({{x}}) } 
ronaDiff <- ronaSeriesesTot %>%
    mutate(across(starts_with("x"), ~diffit(.) ))

# TRANSPOSE ronaDiff TO SECTIONS
# JOIN WITH ronaSectionsTot TO MAKE ronaAllSections
ronaAllSections <- ronaDiff %>%
    pivot_longer(cols = starts_with("x"), names_to = "fips2") %>%
    pivot_wider(id_cols = fips2, 
                names_from = week_DateW, 
                values_from = "value") %>%
    left_join(ronaSectionsTot, by="fips2") %>%
    left_join(fips_fips2, by = "fips2") %>%
    column_to_rownames(var = "fips2") %>%
    left_join(rawData[["popu"]], by="fips") %>%
    left_join(vote_wide, by="fips")  %>%
    filter(!is.na(pop2019))

rm(fips_fips2, rawData, ronaSectionsTot, ronaSeriesesTot, ronaTall) #, ronaDiff)

# CONSTRUCT CALCULATED VARIABLES INCLUDING CASES-PER-CAPITA --------------------
tP = sum(ronaAllSections$pop2019)                                     # total Population
rona <- ronaAllSections %>%
    mutate(across(starts_with("T"), function(x) {x/pop2019/100000} )) %>%         # Make total cases / 100,000 population variables
    mutate(across(starts_with("W"), function(x) {x/pop2019/100000} )) %>%         # "    (weekly new cases)  "     "      "
    arrange(DEM_Margin) %>%                                  # put counties in order of vote margin
    mutate(cP = cumsum(pop2019)) %>%                         # new column = cumulative Sum of Population in order of vote margin
    mutate(margin_qtile = ifelse(cP>tP*.75,4, ifelse(cP>tP*.5,3, ifelse(cP>tP*.25,2, 1)))) # label counties by their vote margin quartile (pop-weighted)



lm_allT <- map(select(rona, starts_with("T")),
               function(yvar) {
                   return( lm(yvar ~ DEM_Margin, rona) )
               })

l <- length(rona)
lm_allW <- map(select(rona[2:l], starts_with("W")),
               function(yvar) {
                   return( lm(yvar ~ DEM_Margin, rona) )
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
colnames(lm_dfT) <- c("TrumpCountiesMoreCovidTotal", "RSquared", "PValue")
lm_dfT2 = rownames_to_column(lm_dfT, var = "week_DateT")                        
lm_dfT2 <- left_join(lm_dfT2, weekDates, by = "week_DateT")

lm_dfW = as.data.frame(do.call(rbind, lm_outW))
colnames(lm_dfW) <- c("TrumpCountiesMoreCovidWeekly", "RSquared", "PValue")
lm_dfW2 = rownames_to_column(lm_dfW, var = "week_DateW")
lm_dfW2 <- left_join(lm_dfW2, weekDates, by = "week_DateW")

lm_dfT2 <- filter(lm_dfT2, weekDate > as.Date("2020-04-12"))
lm_dfW2 <- filter(lm_dfW2, weekDate > as.Date("2020-04-12"))


# PLOT -------------------------------------------------------------------------




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ronaVote"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Week",
                        "Date:",
                        min = as.Date(weekDates$weekDate[1], "%Y-%m-%d"),
                        max = as.Date(weekDates$weekDate %>% .[length(.)], "%Y-%m-%d"),
                        value = as.Date(weekDates$weekDate %>% .[length(.)], "%Y-%m-%d"),
                        timeFormat = "%Y-%m-%d")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot1") ,
           plotOutput("distPlot2")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot1 <- renderPlot({
        # ggplot(cars, aes(x=speed, y=dist)) + geom_point()
        lm_dfT2 %>% ggplot(aes(x=weekDate, y=TrumpCountiesMoreCovidTotal)) + geom_point()  # Total Cases
        })
    
    output$distPlot2 <- renderPlot({
        lm_dfW2 %>% ggplot(aes(x=weekDate, y=TrumpCountiesMoreCovidWeekly)) + geom_point()  # Weekly New Cases
        })

    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- popu$log10_pop
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #     })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
