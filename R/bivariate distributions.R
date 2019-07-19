# atl = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Atlas/AtlasPlusTableData-nygafl16.csv")
all = (read.csv("C:/Users/yhd8/Desktop/Data/Atlas/AtlasPlusTableData-allstates2016.csv", stringsAsFactors=FALSE))
names(all)[1] = "state"
all$Cases = as.numeric(all$Cases)


hhsRegions = readxl::read_xlsx("C:/Users/yhd8/Desktop/Data/Atlas/HHS Regions.xlsx")
names(hhsRegions) = c("region", "division")

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
all2 = all[all$Cases != "0" & !is.na(all$Age.Group) , ]


all2$Cases = as.numeric(gsub(",","", all2$Cases))
all3 = all2[!is.na(all2$Cases),]
all3$stateAbb =  state.abb[match(as.character(all3$state),state.name)]

# expand <- all3[rep(row.names(all3), all3$Cases), 1:4]


# fun = aggregate(expand, by = list(expand$Geography, expand$Race.Ethnicity), FUN = length, drop = T)

# fun$stateAbb = state.abb[match(as.character(fun$Group.1),state.name)]



library(ggplot2)
library(dplyr)

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

all3$state = tolower(all3$state)
hhsRegions$region = tolower(hhsRegions$region)
names(hhsRegions)[1] = "state"
us_state_map = us_state_map[, -c(3, 4, 6)]
f = merge(all3, hhsRegions, by = "state", all.x = T)
names(f)[1] = "state"
latLong = data.frame(state = (state.name),
                     long = state.center$x,
                     lat = state.center$y) %>%
 right_join(f, by = "state")


new = aggregate(Cases ~ state, latLong, sum)













































  ##################################################################################
  ##################################################################################
  ##################################################################################

  ########################## USE THIS CODE FOR MAP OMG #############################

  ##################################################################################
  ##################################################################################
  ##################################################################################


  library(maps)
  library(ggthemes)
  names(new)[1] = "region"
  new$region = tolower(new$region)
  states_map <- map_data("state", region = new$region)
  new_map <- merge(states_map, new, by = "region")
  newHHS = merge(new_map, hhsRegions, by = "region"  )
  new_map <- arrange(new_map, group, order) # to sort polygons in right order

  ggplot(new_map, aes(x = long, y = lat, group = group, fill = Cases)) +
    geom_polygon(color = "black") +
    coord_map("polyconic") + theme_tufte() + labs(x = "", y = "")







#
# blacks = expand[expand$Race.Ethnicity == "Black/African American",]
#
# allState$region = tolower(allState$Group.1)
# allState$prop = allState$x/sum(allState$x)
#
#
# allState$stateAbb = state.abb[match(allState$Group.1,state.name)]
#
