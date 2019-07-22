
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

totalsAndHHS = merge(totals, hhsRegions, by.x = "state", by.y = "region")
aggDivision = aggregate(totalsAndHHS[, c("totpop", paste0("n", race))], by = list(totalsAndHHS$division), sum)
aggDivision$overallRate = aggDivision[, paste0("n", race)] / aggDivision$totpop
names(aggDivision)[1] = "division"

us_state_map.mod <- merge(x=us_state_map, y=aggDivision, all.x=TRUE, by.x="division", by.y="division")
us_state_map.mod = arrange(us_state_map.mod, order);
us_state_map.mod$division = as.factor(us_state_map.mod$division)

#plot a map of each division
map <- ggplot()
map = map + geom_polygon(data=us_state_map.mod, aes(x=long, y=lat, group=group, fill=overallRate))
map = map + scale_fill_gradient(low = "lightblue", high = "darkblue")
map
}

