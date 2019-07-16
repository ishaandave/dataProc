atl = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Atlas/AtlasPlusTableData-nygafl16.csv")
all = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Atlas/AtlasPlusTableData-allstates2016.csv")


# HELLO
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


all2 = all[all$Cases != "0" & !is.na(all$Age.Group) , ]


all2$Cases = as.numeric(gsub(",","", all2$Cases))
all3 = all2[!is.na(all2$Cases),]


expand <- all3[rep(row.names(all3), all3$Cases), 1:4]

fun = aggregate(expand, by = list(expand$Geography, expand$Race.Ethnicity), FUN = length, drop = T)



c1 = expand$Geography
c2 = expand$Race.Ethnicity
c = cbind.data.frame(c1, c2)
trueProbs = prop.table(table(c1, c2))
probmat = data.frame(matrix(prop.table(table(c1, c2)), ncol = nlevels(c2)))
# rownames(probmat) = unique(all2$Geography)
probmat2 = cbind(unique(all2$Geography), probmat)
names(probmat2) = c("State", levels(c2))





set.seed(1)
c1Sorted = sort(levels(c1))
c2Sorted = sort(levels(c2))
n1 <- length(c1Sorted)
n2 <- length(c2Sorted)

probs  <- c(trueProbs)
events <- as.matrix(expand.grid(state = c1Sorted, race = c2Sorted))
nSamp  <- 500000
samp   <- as.data.frame(events[sample.int(n1*n2, nSamp, prob=probs, replace=TRUE),])
head(samp)

predProbs = prop.table(table(samp$state, samp$race))
predProbMat = data.frame(matrix(predProbs, ncol = nlevels(samp$race)))

predProbMat2 = cbind(unique(samp$state), predProbMat)
names(predProbMat2) = c("State", levels(samp$race))
predProbMat3 = predProbMat2[order(predProbMat2$State),]

observedAndPredicted = left_join(x = probmat2, y = predProbMat3, by = "State")
observedAndPredicted[is.na(observedAndPredicted)] <- 0

true = observedAndPredicted %>%
  select(select_vars(names(observedAndPredicted), ends_with('.x', ignore.case = TRUE)))

expected = observedAndPredicted %>%
  select(select_vars(names(observedAndPredicted), ends_with('.y', ignore.case = TRUE)))

listTrue = c(as.matrix(true))
listExpected = c(as.matrix(expected))
plot(c(as.matrix(true)),
     c(as.matrix(expected)), ylim = c(0, 1*max(c(listTrue, listExpected))), xlim = c(0,1*max(c(listTrue, listExpected))))


fit = lm(log(listExpected)~log(listTrue))
summary(fit)


samp$stateAbb = state.abb[match(samp$state,state.name)]



library(ggplot2)
library(dplyr)

us <- map_data("state")


gg <- ggplot()
gg <- gg + geom_map(data=us, map=us,
                    aes(x=us$long, y=us$lat, map_id=us$region),
                    fill="#ffffff", color="#ffffff", size=0.15)
gg <- gg + geom_map(data=samp, map=us,
                    aes(fill=allState$prop, map_id=allState$region),
                    color="#ffffff", size=0.15)
gg <- gg + scale_fill_continuous(low='thistle2', high='darkred',
                                 guide='colorbar')
gg <- gg + labs(x=NULL, y=NULL)
gg <- gg + coord_map("albers", lat0 = 39, lat1 = 45)
gg <- gg + theme(panel.border = element_blank())
gg <- gg + theme(panel.background = element_blank())
gg <- gg + theme(axis.ticks = element_blank())
gg <- gg + theme(axis.text = element_blank())
gg


















blacks = expand[expand$Race.Ethnicity == "Black/African American",]

allState = aggregate(all$Cases, by = list(all$Geography), FUN =  sum)
allState$region = tolower(allState$Group.1)
allState$prop = allState$x/sum(allState$x)


allState$stateAbb = state.abb[match(allState$Group.1,state.name)]

