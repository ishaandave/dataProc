# merging census data with new diagnoses
library(plyr)
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

census = data.table(read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Census/cc-est2017-alldata.csv"))
census2 = census[census$YEAR>2, c(-1, -2, -3)]

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


groupedCensus  = subset(temp3, select =  -c(AGECAT, YEAR) )

groupedCensus2 = groupedCensus[, c(1, ncol(groupedCensus), 3:ncol(groupedCensus)-1)]
names(groupedCensus2)[names(groupedCensus2) == "YEAR_new"] = "Year"




names(allCountyData)[names(allCountyData) == "County Name"] = "CTYNAME"

names(allCountyData)[names(allCountyData) == "State"] = "STNAME"
allData = merge(allCountyData, groupedCensus2, by = c("STNAME", "CTYNAME", "Year"))

allData2 = subset(allData, select = -c(`GEO ID`, `State Abbreviation`))
allData_reorder = allData2[, c(1,2,3,8,4:ncol(allData2))]


## deleting variables we don't need 
dat = allData_reorder[, -c(9, (25:34))]

##############################################################################
############ Generic Code to maybe creat simulated dataset here ##############

###### Don't actually run any of it until you fix dataset/variable names #####
##############################################################################

SUBSET_OF_WHATEVER_GROUP = dat[dat$STNAME == "XXXXX" & dat$Year == XXXX,]
TOT_HISP =  H_MALE +  H_FEMALE
TOT_NONHISP =   NH_MALE +   NH_FEMALE

# "White", "Hispanic/Latinx", 
# "Black", "American Indian/Alaska Native", 
# "Asian", "Pacific Islander", "Multiple"

## GEtting total white population
TOT_WHITE =   WA_MALE +   WA_FEMALE

## GEtting total black population
TOT_BLACK =   BA_MALE +   BA_FEMALE

# Asian Population
TOT_ASIAN =   AA_MALE +   AA_FEMALE

# Hispanic/Non-hispanic
TOT_HISP =   H_MALE +   H_FEMALE
TOT_NONHISP =   NH_MALE +   NH_FEMALE

# American Indian / Alaska Native
TOT_IA =   IA_MALE +   IA_FEMALE

# Native American
TOT_NA =   NA_MALE +   NA_FEMALE

TOT_MULT =   TOM_MALE +   TOM_FEMALE

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
                            prob = c(whites/pop, blacks/pop, asians/pop, hisps/pop,
                                     ias/pop, nas/pop, mults/pop)
  ),
  `Age Category` = sample(c("<24", "25-34", "35-44", "45-54", "55+"), n, TRUE,
                          prob = c(0.2, 0.2, 0.2, 0.2, 0.2)
  ),
  `State` = sample(c(state.name), n, TRUE,
                   prob = c(rep(.02, 50))
  )
)