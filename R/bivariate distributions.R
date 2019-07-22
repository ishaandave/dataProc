# atl = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Atlas/AtlasPlusTableData-nygafl16.csv")
all = (read.csv("C:/Users/yhd8/Desktop/Data/Atlas/AtlasPlusTableData-allstates2016.csv", stringsAsFactors=FALSE))
names(all)[1] = "state"
all$Cases = as.numeric(gsub(",","", all$Cases))

hhsRegions = readxl::read_xlsx("C:/Users/yhd8/Desktop/Data/Atlas/HHS Regions.xlsx")
names(hhsRegions) = c("state", "division")
hhsRegions$state = tolower(hhsRegions$state)


# atl$Cases = as.numeric(gsub(",", "", atl$Cases))

#
# num = as.numeric(gsub(",","",atl$Cases))
# atl[,'Cases'] = num
#
# atl = atl[atl$Cases > 0 , ]
# atlState = aggregate(atl$Cases, by = list(atl$Geography), FUN =  sum)
#
#
#
#
# atlExpand <- atl[rep(row.names(atl), atl$Cases), 1:4]
#
#
#

# allState = aggregate(all$Cases, by = list(all$Geography), FUN =  sum)
all2 = all[all$Cases != "0" & !is.na(all$Age.Group) & !is.na(all$Cases) , ]

all2$stateAbb =  state.abb[match(as.character(all2$state),state.name)]
all2$division = hhsRegions$division[match(toupper(all2$state), toupper(hhsRegions$state))]

 expand <- all2[rep(row.names(all2), all2$Cases), c(1:4,7)]


# fun = aggregate(expand, by = list(expand$Geography, expand$Race.Ethnicity), FUN = length, drop = T)

# fun$stateAbb = state.abb[match(as.character(fun$Group.1),state.name)]



library(ggplot2)
library(dplyr)
 library(plyr)

us_state_map = map_data("state")


HIVdists = function (race) {

#load us state map data
us_state_map = map_data("state");

us_state_map.mod = merge(us_state_map, hhsRegions, by.x = "region", by.y = "state")
us_state_map.mod = arrange(us_state_map.mod, division, order)
us_state_map.mod$division = as.numeric(as.character((us_state_map.mod$division)))

#map each state to a division
all2$state = tolower(all2$state)

us_state_map = us_state_map[, -c(3, 4, 6)]
f = merge(all2, hhsRegions, by = "state", all.x = T)

names(f)[1] = "state"

f$state = as.character(f$state)

latLong = data.frame(state = as.character(state.name),
                     long = state.center$x,
                     lat = state.center$y) %>%
 right_join(f, by = "state")



new = aggregate(Cases ~ state, latLong, sum)
new$state = tolower(new$state)

race = as.character(race)
female = paste0(toupper(race), "_FEMALE")
male = paste0(toupper(race), "_MALE")

census2[, paste0("TOT_", toupper(race))] = census2[, female] + census2[, male]
totals = aggregate(census2[,c("TOT_POP",  paste0("TOT_", toupper(race)))], by = list(census2$state), sum)
  names(totals)[1] = "state"
  totals$state = tolower(totals$state)
    names(totals) = c("state", "totpop", paste0("n", race))

rates = merge(new, totals, by = "state")
  rates$rate = rates$Cases / rates[, paste0("n", race)]

allhhs = merge(rates, hhsRegions, by = "state")


aggDivision = aggregate(allhhs[, c("totpop", paste0("n", race), "Cases")], by = list(allhhs$division), sum)
  aggDivision[, paste0("rateAmong", toupper(race))] = aggDivision[, "Cases"] / aggDivision[, paste0("n", race)]
  names(aggDivision)[1] = "division"

us_state_map.mod$rate = aggDivision[, paste0("rateAmong", toupper(race))][match(us_state_map.mod$division, aggDivision$division)]

map <- ggplot()
map = map + geom_polygon(data=us_state_map.mod, aes(x=long, y=lat, group=group, fill = rate))
map = map + scale_fill_gradient(low = "thistle2", high = "darkred")
map = map + ggtitle(paste0("Distribution of ", toupper(race), " Across America"))
map



}




































  ##################################################################################
  ##################################################################################
  ##################################################################################

  ########################## USE THIS CODE FOR MAP ALL STATES#######################

  ##################################################################################
  ##################################################################################
  ##################################################################################


  library(maps)
  library(ggthemes)
  names(rates)[1] = "region"
  new$region = tolower(rates$region)
  states_map <- map_data("state", region = rates$region)
  new_map <- merge(states_map, rates, by = "region")
  newHHS = merge(new_map, hhsRegions, by = "region"  )
  new_map <- arrange(new_map, group, order) # to sort polygons in right order

  ggplot(new_map, aes(x = long, y = lat, group = group, fill = rate)) +
    geom_polygon(color = "black") +
    coord_map("polyconic") + theme_tufte() + labs(x = "", y = "") +
    scale_fill_gradient2(low = "red", mid = "white",
                         high = "blue", midpoint = 0, space = "Lab",
                         na.value = "grey50", guide = "colourbar", aesthetics = "fill")







#
# blacks = expand[expand$Race.Ethnicity == "Black/African American",]
#
# allState$region = tolower(allState$Group.1)
# allState$prop = allState$x/sum(allState$x)
#
#
# allState$stateAbb = state.abb[match(allState$Group.1,state.name)]
#
