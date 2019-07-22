library(foreign)
library(lubridate)
num.decimals <- function(x) {
  stopifnot(class(x)=="numeric")
  x <- nchar(sub("0+$","",sub("^.+[.]","",x)))
  x <- sub("^.+[.]","",x)
  nchar(x)
}
x <- 5.2300000
num.decimals(x)

5.1%%1
5%%1
5.12%%2

dat = data.frame(read.csv("/Users/ishaandave/Downloads/listings.csv"))
dat$last_review = as.character(dat$last_review)
123


# run this on actual data with actual people
# getting the bivariate distributions
#

## package for dates is lubridate


dat <- data.frame(
  Date=c("29/11/2012","30/12/2012"),
  AE=c(1211,100),
  Percent=c(0.03,0.43)
)

sapply(data, function(x) !all(is.na(as.Date(as.character(x),format="%d/%m/%Y"))))
sapply(data, function(x) !all(is.na(as.Date(as.character(x),format="%m/%d/%Y"))))
sapply(data, function(x) !all(is.na(as.Date(as.character(x)))))



standarDates <- function(string) {
  patterns = c('[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]',  ## YYYY/mm/dd
               '[0-9][0-9]/[0-9][0-9]/[0-9][0-9][0-9][0-9]',  ## dd/mm/YYYY
               '[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]',  ## mm/dd/YYYY
               )
  formatdates = c('%Y/%m/%d','%d/%m/%Y','%Y-%m-%d')
  standardformat='%d/%m/%Y'
  for(i in 1:3){
    if(grepl(patterns[i], string)){
      aux=as.Date(string,format=formatdates[i])
      if(!is.na(aux)){
        return(format(aux, standardformat))
      }
    }
  }
  return(FALSE)
}
standarDates(data$Date)







isDate <- function(date) {
  if (sapply(date, function(x)
    ! all(is.na(as.Date(
      as.character(x),
      format = c("%d/%m/%Y", '%m/%d/%Y', "%Y/%m/%d", "%Y/%d/%m", "%m/%Y/%d", "%d/%Y/%m",
                 "%d-%m-%Y", '%m-%d-%Y', "%YYYY-%mm-%dd", "%Y-%d-%m", "%m-%Y-%d", "%d-%Y-%m")
    ))))) {
    return(TRUE)
  } else{
    return(FALSE)
  }
}
#
# iso 8601
# YYYY-mm--dd



listings = read.csv("/Users/ishaandave/Desktop/CDC-Leidos/Data/Pretend/listings.csv")

listings = listings[c(1:200),]

month = cities <- gsub("/.*$", "", listings$last_review)

for (i in 1:ncol(listings))
 which(sapply(listings, function(x) !all(is.na(as.Date(as.character(x),format="%m/%d/%Y"))))==T)

dates = c("2012-05-23",
          "2012-02-29",
          "2013-07-20",
          "2013-04-04",
          "2012-02-28",
          "2013-01-11",
          "2013-01-02",
          "2013-10-22",
          "2013-04-02",
          "2012-11-17",
          "2012-01-08",
          "2012-06-19",
          "2013-11-30",
          "2012-01-30",
          "2013-02-29",
          "2012-03-24",
          "2012-11-18",
          "2013-01-30",
          "2013-06-15",
          "2013-02-05",
          "2012-02-11",
          "2013-01-21",
          "2012-05-07",
          "2012-11-19",
          "2013-11-01",
          "2013-11-07",
          "2012-09-26",
          "2013-01-16",
          "2013-06-03",
          "2013-03-28",
          "2013-05-01",
          "2012-05-26",
          "2013-07-07",
          "2013-10-16",
          "2013-01-07",
          "2012-03-27",
          "2013-05-29",
          "2013-06-23",
          "2013-02-22",
          "2012-11-05",
          "2013-08-13",
          "2013-04-30",
          "2013-10-13",
          "2012-08-08",
          "2012-07-21",
          "2013-10-12",
          "2012-03-07",
          "2012-07-09",
          "2013-03-13",
          "2012-06-27",
          "2012-10-14",
          "2012-06-28",
          "2013-07-16",
          "2013-06-15",
          "2013-11-01",
          "2013-12-09",
          "2013-02-16",
          "2012-03-24",
          "2013-02-28",
          "2012-12-01")

d = as.data.frame(dates)
d$e = c(1:60)


for (i in 1:nrow(d)){
  d$checkIfDate = "([0-9][0-9][0-9][0-9])[-]([0-1][0-9])[-]([0-9][0-9])"
}

try = str_detect(d$dates, d$checkIfDate)







