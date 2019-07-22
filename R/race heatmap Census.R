
### Takes census data and allows you to map racial distributions across the US (according to HHS boundaries)
### Will allow user to make a map whatever race/ethnicity they want to input

## eventually want to make it so, so we can create racial distributions across the 10 HHS divisions and use that
## to sample into our new dataset

census = data.frame(read.csv("C:/Users/yhd8/Desktop/Data/Census/cc-est2017-alldata.csv"))
census2 = census[census$YEAR == 10 &  census$AGEGRP == 0, ]
#want only measurements in 2017 to see if anything works

census2$YEAR_new = census2$YEAR + 2007

names(census2)[names(census2) == "YEAR_new"] = "Year"
names(census2)[names(census2) == "STNAME"] = "state"


hhsRegions = readxl::read_xlsx("C:/Users/yhd8/Desktop/Data/Atlas/HHS Regions.xlsx")
names(hhsRegions) = c("state", "division")
hhsRegions$state = tolower(hhsRegions$state)



library(mapproj)
library(maps)
library(usmap)
library(dplyr)
library(ggplot2)
## create function to plot whatever race/ethnicity across the US

raceDistribution = function (race) {

  if (!is.character(race)) {
    print("INPUT MUST BE CHARACTER VALUE")
  }

  us_state_map = map_data("state");


  #load us state map data
  us_state_map = map_data("state");

  us_state_map.mod = merge(us_state_map, hhsRegions, by.x = "region", by.y = "state")
  us_state_map.mod = arrange(us_state_map.mod, division, order)
  us_state_map.mod$division = as.numeric(as.character((us_state_map.mod$division)))

  race = as.character(race)
  female = paste0(toupper(race), "_FEMALE")
  male = paste0(toupper(race), "_MALE")

  census2[, paste0("TOT_", toupper(race))] = census2[, female] + census2[, male]

  totals = aggregate(census2[,c("TOT_POP",  paste0("TOT_", toupper(race)))], by = list(census2$state), sum)
  names(totals)[1] = "state"
  totals$state = tolower(totals$state)
  names(totals) = c("state", "totpop", paste0("n", race))
  totals$prop = totals[, paste0("n", race)] / totals$totpop

  totalsAndHHS = join(totals, hhsRegions, by = "state")

  aggDivision = aggregate(totalsAndHHS[, c("totpop", paste0("n", race))], by = list(totalsAndHHS$division), sum)
  aggDivision$overallRate = aggDivision[, paste0("n", race)] / aggDivision$totpop
  names(aggDivision)[1] = "division"
  us_state_map.mod$rate = aggDivision$overallRate[match(us_state_map.mod$division, aggDivision$division)]

  map <- ggplot()
  map = map + geom_polygon(data=us_state_map.mod, aes(x=long, y=lat, group=group, fill = rate))
  map = map + scale_fill_gradient(low = "thistle2", high = "darkred")
  map = map + ggtitle(paste0("Distribution of ", toupper(race), " Across America"))
  map

}









######################################################################################
# SCRATCH WORK TO MAKE SURE ABOVE FUNCTION WORKS IF WE JUST INPUT THE VARIABLE NAME
######################################################################################

library(ggplot2)
library(maps)
library(plyr)
library(grid)

#load us state map data
us_state_map = map_data("state");

us_state_map.mod = merge(us_state_map, hhsRegions, by.x = "region", by.y = "state")
us_state_map.mod = arrange(us_state_map.mod, division, order)
us_state_map.mod$division = as.numeric(as.character((us_state_map.mod$division)))

female = paste0(toupper(race), "_FEMALE")
male = paste0(toupper(race), "_MALE")

census2[, paste0("TOT_", toupper(race))] = census2[, female] + census2[, male]

totals = aggregate(census2[,c("TOT_POP",  paste0("TOT_", toupper(race)))], by = list(census2$state), sum)
names(totals)[1] = "state"
totals$state = tolower(totals$state)
names(totals) = c("state", "totpop", paste0("n", race))
totals$prop = totals[, paste0("n", race)] / totals$totpop

totalsAndHHS = join(totals, hhsRegions, by = "state")

aggDivision = aggregate(totalsAndHHS[, c("totpop", paste0("n", race))], by = list(totalsAndHHS$division), sum)
aggDivision$overallRate = aggDivision[, paste0("n", race)] / aggDivision$totpop
names(aggDivision)[1] = "division"
us_state_map.mod$rate = aggDivision$overallRate[match(us_state_map.mod$division, aggDivision$division)]

map <- ggplot()
map = map + geom_polygon(data=us_state_map.mod, aes(x=long, y=lat, group=group, fill = rate))
map = map + scale_fill_gradient(low = "lightblue", high = "darkblue")
map


us_state_map$division[us_state_map$region %in% c("connecticut", "maine", "massachusetts", "new hampshire", "rhode island", "vermont")] <- 1
us_state_map$division[us_state_map$region %in% c("new jersey","new york","puerto rico","virgin islands")] <- 2
us_state_map$division[us_state_map$region %in% c("delaware","district of columbia","maryland","pennsylvania","virginia","west virginia")] <- 3
us_state_map$division[us_state_map$region %in% c("alabama","florida","georgia","kentucky","mississippi","north carolina","south carolina","tennessee")] <- 4
us_state_map$division[us_state_map$region %in% c("illinois","indiana","michigan","minnesota","ohio","wisconsin")] <- 5
us_state_map$division[us_state_map$region %in% c("arkansas","louisiana","new mexico","oklahoma","texas")] <- 6
us_state_map$division[us_state_map$region %in% c("iowa","kansas","missouri","nebraska")] <- 7
us_state_map$division[us_state_map$region %in% c("colorado","montana","north dakota","south dakota","utah","wyoming")] <- 8
us_state_map$division[us_state_map$region %in% c("arizona","california","hawaii","nevada")] <- 9
us_state_map$division[us_state_map$region %in% c("alaska","idaho","oregon","washington")] <- 10



