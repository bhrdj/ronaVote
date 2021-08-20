# ronaVote

Chart **raw correlation** over time between:
- County-level COVID outcomes
  - New York Times, "Coronavirus (Covid-19) Data in the United States"
  - https://github.com/nytimes/covid-19-data/blob/master/live/us-counties.csv
- County-level 2020 Presidential electoral margins
  - MIT Election Data and Science Lab, "County Presidential Election Returns 2000-2020"
  - https://doi.org/10.7910/DVN/VOQCHQ
- Population data from USDA
  - USDA Population estimates for the U.S., States, and counties, 2010-19. 
  - https://www.ers.usda.gov/data-products/county-level-data-sets/download-data.aspx

Calculations done with R Markdown:
- All processing code displayed in R Markdown code chunks.
  - Code processing begins with raw data as-downloaded.
  - No manual data cleaning.
