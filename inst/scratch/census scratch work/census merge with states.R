library(plyr)
library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(expss)
library(tibble)

setwd("/Users/ishaandave/Desktop/CDC-Leidos/Data/AIDS Vu State New Diagnoses/")
dataFiles <- lapply(Sys.glob("AIDSVu_State_New-DX-Data-Sets_*.xlsx"), read_xlsx)
dataFiles

allStateData <- do.call("rbind", dataFiles)


census = fread("/Users/ishaandave/Desktop/CDC-Leidos/Data/Census/cc-est2017-alldata.csv")
#want only measurements made on July 1, 2010 and later
# First 3 columns are useless
census2 = census[census$YEAR>2, c(-1, -2, -3)]

census2$YEAR_new = census2$YEAR + 2007

names(census2)[names(census2) == "YEAR_new"] = "Year"
names(census2)[names(census2) == "STNAME"] = "State"


census2$adjPop = ifelse(census2$AGEGRP == 3, 
                        0.2*census2$TOT_POP, 
                        census2$TOT_POP)

censusAgeSub = subset(census2, (AGEGRP>2))

censusAgeSub2 = censusAgeSub %>% 
  group_by(State, CTYNAME, Year) %>%
  summarise(pop=round(sum(adjPop)))

censusAgeSub3 = censusAgeSub2 %>% 
  group_by(State, Year) %>%
  summarise(pop = round(sum(pop)))


censusNo2017 = censusAgeSub3[censusAgeSub3$Year != 2017,]
censusNo2017$State = ifelse(censusNo2017$State == "District of Columbia", "Washington, D.C.", censusNo2017$State)

## At this point, census data is population in each 2010 - 2017 for each state 
## State data from AIDS Vu has Puerto Rico, census doesn't, so we remove it 

allStateData_noPR = allStateData[allStateData$State != "Puerto Rico",]

mergedCensusState = merge(allStateData_noPR, censusNo2017, by = c("State", "Year"))

mergedCensusState2 = mergedCensusState[, c(1,2,3,ncol(mergedCensusState), 5:ncol(mergedCensusState)-1)]


error = -100*(mergedCensusState2$`New Diagnoses State Cases`/mergedCensusState2$pop*100000 - mergedCensusState2$`New Diagnoses State Rate`)/mergedCensusState2$`New Diagnoses State Rate`
# mean(calc) ~ 17% difference between calculated and actual

plot(density(error), main = "", xlab = "% Difference")

# pretty normally distributed
## Calculated rate is, on average, 17% different from given rate
