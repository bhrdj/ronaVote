# rona_prez-naive.py
#
#
# SUMMARY OF RELEVANT FIELDS
#
#	NYT: presidential.csv:
#		vCheck: FIPS,	ElcnDistName,	Reporting,	Precincts,	Margin2020, Margin2016, State
#		vRaw:	FIPS,	ElcnDistName,	Votes,		Trump,		Biden
#
#	NYT COVID data: us-counties.csv
#		cCheck:	FIPS,	CountyName,	
#		cRaw:	FIPS,	Date,			Cases,		Deaths
#
#	USDA Population Data: PopulationEstimates.xls
#		pRaw:	FIPS,	PopEst2019,		Deaths2019
#
#	Final Variables: 
#		c:		FIPS,	Date,			Cases/Pop	ExcessDeaths/Deaths2019
#		v:		FIPS,	T-B_margin,		
#
#
# Pseudocode:
# 1. Read in data by FIPS code
# 2. Calculate v from vRaw:
#		T_B_margin = (Trump - Biden)/Votes
#
# 3. Regress c on v (using v to predict c)
