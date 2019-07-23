library(dplyr)
library(tidyverse)
library(data.table)
library(readxl)
library(expss)
library(tibble)



## Reading in county level data
setwd("/Users/ishaandave/Desktop/CDC-Leidos/Data/AIDS Vu County New Diagnoses/")
dataFiles <- lapply(Sys.glob("AIDSVu_County_New-DX-Data-Sets_*.xlsx"), read_xlsx)
dataFiles

allCountyData <- do.call("rbind", dataFiles)

### reading in census data

census = fread("/Users/ishaandave/Desktop/CDC-Leidos/Data/Census/cc-est2017-alldata.csv")

#want only measurements made on July 1, 2010 and later
# First 3 columns are useless
census2 = census[census$YEAR>2, c(-1, -2, -3)]

# combining age categories
# 1-5 is <24
# 6, 7 is 25-34
# 8, 9 is 35-44
# 10, 11 is 45 - 54
# 12+ is 55+

attach(census2)
census2$AGECAT = ifelse(AGEGRP == 0, 0,
                        ifelse(AGEGRP > 0 & AGEGRP <= 5, 1, 
                               ifelse(AGEGRP == 6 | AGEGRP == 7, 2,
                                      ifelse(AGEGRP == 8 | AGEGRP == 9, 3,
                                             ifelse(AGEGRP == 10 | AGEGRP == 11, 4, 55)
                                      )
                               )
                        )
)

census2$YEAR_new = census2$YEAR + 2007

## Getting total population in each year 
# at each group for every county in each state
attach(census2)
temp = census2 %>% 
  group_by(YEAR_new, AGECAT, CTYNAME, STNAME) %>%
  summarize_all(sum)


temp2  = temp[order(temp$AGECAT, temp$STNAME),]     

temp3 =subset(temp2, select =  -AGEGRP)

temp3$ageCategory = ifelse (temp3$AGECAT == 0, "Total",
                            ifelse(temp3$AGECAT == 1, "< 24",
                                   ifelse(temp3$AGECAT == 2, "25 - 34",
                                          ifelse(temp3$AGECAT == 3, "35 - 44",
                                                 ifelse(temp3$AGECAT == 4, "45 - 54", "55+"
                                                 )
                                          )
                                   )
                            )
)

## Deleting old variable
groupedCensus  = subset(temp3, select =  -c(AGECAT, YEAR) )

# reording variables so it makes more sense to look at 
groupedCensus2 = groupedCensus[, c(1, ncol(groupedCensus), 3:ncol(groupedCensus)-1)]

#Renaming variables in order to merge easily
names(groupedCensus2)[names(groupedCensus2) == "YEAR_new"] = "Year"
names(allCountyData)[names(allCountyData) == "County Name"] = "CTYNAME"
names(allCountyData)[names(allCountyData) == "State"] = "STNAME"



# Merging the county and census data
allData = merge(allCountyData, groupedCensus2, by = c("STNAME", "CTYNAME", "Year"))


# Removing unnecessary variables
allData2 = subset(allData, select = -c(`GEO ID`, `State Abbreviation`))

# Reordering variables
allData_reorder = allData2[, c(1,2,3,8,4:ncol(allData2))]


## deleting variables we don't need 
dat = allData_reorder[, -c(9, (25:34))]

##############################################################################
####### ONLY USING NY 2015 DATA TO HAVE SOMETHING SMALLER TO WORK WITH #######
##############################################################################

ny2015 = dat[dat$STNAME == "New York" & dat$Year == 2015,]
ny2015$TOT_HISP = ny2015$H_MALE + ny2015$H_FEMALE
ny2015$TOT_NONHISP = ny2015$NH_MALE + ny2015$NH_FEMALE

# "White", "Hispanic/Latinx", 
# "Black", "American Indian/Alaska Native", 
# "Asian", "Pacific Islander", "Multiple"

## GEtting total white population
ny2015$TOT_WHITE = ny2015$WA_MALE + ny2015$WA_FEMALE

## GEtting total black population
ny2015$TOT_BLACK = ny2015$BA_MALE + ny2015$BA_FEMALE

# Asian Population
ny2015$TOT_ASIAN = ny2015$AA_MALE + ny2015$AA_FEMALE

# Hispanic/Non-hispanic
ny2015$TOT_HISP = ny2015$H_MALE + ny2015$H_FEMALE
ny2015$TOT_NONHISP = ny2015$NH_MALE + ny2015$NH_FEMALE

# American Indian / Alaska Native
ny2015$TOT_IA = ny2015$IA_MALE + ny2015$IA_FEMALE

# Native American
ny2015$TOT_NA = ny2015$NA_MALE + ny2015$NA_FEMALE

ny2015$TOT_MULT = ny2015$TOM_MALE + ny2015$TOM_FEMALE

females = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_FEMALE ))
males = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_MALE ))

whites = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_WHITE ))
blacks = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_BLACK ))
asians = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_ASIAN ))
hisps = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_HISP ))
ias = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_IA ))
nas = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_NA ))
mults = with(ny2015, sum_col_if("Total", ageCategory, data = TOT_MULT ))

pop = sum(males, females)


### Creating data frame with available variables

n = 1000
set.seed(1234)

patients <- tibble( 
  `Sex` = sample(c("Male", "Female"), n, TRUE, 
                 prob = c(males, females)
  ),
  `Race/Ethnicity` = sample(c("White", "Black", "Asian",
                              "Hispanic/Latin", "American Indian/Alaska Native",
                              "Native American/Pacific Islander", "Multiple"), n, TRUE,
                            prob = c(whites, blacks, asians, hisps,
                                     ias, nas, mults)
  ),
  `Age Category` = sample(c("<24", "25-34", "35-44", "45-54", "55+"), n, TRUE,
                          prob = c(rep(0.2, 5))
  ),
  `State` = sample(c(state.name), n, TRUE,
                   prob = c(rep(.02, 50))
  )
)

#

## 

