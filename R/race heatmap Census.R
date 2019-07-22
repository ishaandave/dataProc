
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

library(mapproj)
library(maps)
library(usmap)
library(dplyr)
library(ggplot2)
## create function to plot whatever race/ethnicity across the US

raceDistribution = function (race) {


  female = paste0(toupper(race), "_FEMALE")
  male = paste0(toupper(race), "_MALE")

  census2[, paste0("TOT_", toupper(race))] = census2[, female] + census2[, male]

  totals = aggregate(census2[,c("TOT_POP",  paste0("TOT_", toupper(race)))], by = list(census2$state), sum)
  names(totals)[1] = "state"
  totals$state = tolower(totals$state)
  names(totals) = c("state", "totpop", paste0("n", race))
  totals$prop = totals[, paste0("n", race)] / totals$totpop

  totalsAndHHS = merge(totals, hhs, by.x = "state", by.y = "region")
  # aggDivision = aggregate(totalsAndHHS[, c("totpop", paste0("n", race))], by = list(totalsAndHHS$division), sum)
  # aggDivision$overallRate = aggDivision[, paste0("n", race)] / aggDivision$totpop
  names(aggDivision)[1] = "division"

  us_state_map.mod <- merge(x =hhsRegions, y = aggDivision, all.x=TRUE, by = "division")
  us_state_map.mod = us_state_map.mod[order(us_state_map.mod$division), ]

  us_state_map.mod$division = as.factor(us_state_map.mod$division)
  usLatLong = merge(us_state_map.mod, map_data("state"), by = "region")
  usLatLong = usLatLong[order(c(usLatLong$long), usLatLong$lat),]


  mergeHHSUS = merge(map_data("state"), hhs, by = "region")



  #plot a map of each division
  map <- ggplot()
  map = map + geom_polygon(data=map_data("state"), aes(x=long, y=lat, group=division, fill=overallRate))
  map = map + scale_fill_gradient(low = "lightblue", high = "darkblue")
  map
}




#plot a map of each division
map <- ggplot()
map = map + geom_map(data=mergeHHSUS, map = us, aes(x=long, y=lat, map_id = region))
map = map + geom_map(data = aggDivision, map = us,  group=aggDivision$division, fill=aggDivision$overallRate,
                     aes(map_id = aggDivision$division))
map = map + scale_fill_gradient(low = "lightblue", high = "darkblue")
map


























































### Takes census data and allows you to map racial distributions across the US (according to HHS boundaries)
### Will allow user to make a map whatever race/ethnicity they want to input

## eventually want to make it so, so we can create racial distributions across the 10 HHS divisions and use that
## to sample into our new dataset

census = data.frame(read.csv("C:/Users/yhd8/Desktop/Data/Census/cc-est2017-alldata.csv"))
census2 = census[census$YEAR == 10 &  census$AGEGRP == 0,]

#want only measurements in 2017 to see if anything works

census2$YEAR_new = census2$YEAR + 2007

names(census2)[names(census2) == "YEAR_new"] = "Year"
names(census2)[names(census2) == "STNAME"] = "state"

library(mapproj)
library(maps)
library(usmap)
library(dplyr)
library(ggplot2)
## create function to plot whatever race/ethnicity across the US
us_state_map = map_data("state")
raceDistribution = function (race) {


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

  us_state_map.mod <- right_join(x=hhsRegions, y=aggDivision, all.x=TRUE, by = "division")
  us_state_map.mod$division = as.numeric(us_state_map.mod$division)
  us_state_map.mod = us_state_map.mod[order(us_state_map.mod$division),]
  state_boundaries = plyr::join(us_state_map.mod, us_state_map, by = "division")

  #plot a map of each division
  map <- ggplot()
  map = map + geom_polygon(data=us_state_map, aes(x=long, y=lat,group = division))
  map = map + scale_fill_gradient(low = "lightblue", high = "darkblue")
  map

  map <- ggplot()
  map = map + geom_polygon(data=state_boundaries, aes(x=long, y=lat, group = region, fill = division))
  map
}












library(ggplot2)
library(maps)
library(plyr)
library(grid)

#load us state map data
us_state_map = map_data("state");

#map each state to a division
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


#create a dummy variable that counts the number of states in each division
divisions.subtotal <- ddply(us_state_map, .(division), summarize, NumberOfStates=length(unique(region)))

#merge our dummy data back into the map data table
us_state_map.mod <- merge(x=us_state_map, y=divisions.subtotal, all.x=TRUE, by.x="division", by.y="division")
us_state_map.mod = arrange(us_state_map.mod, division, order);
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


