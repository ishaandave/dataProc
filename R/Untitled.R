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

dat = data.frame(read_xls("/Users/ishaandave/Desktop/SASUniversityEdition/myfolders/sasuser.v94/BIOS 8010/wcgs.xls"))
dat2 = dat[c(1:20),]
123

dates = as.data.frame(c(20141212,
 20160228,
  20161231,
  20160618,
  20170123,
  20151124,
 20141212,
 20160228,
 20161231,
 20160618,
 20170123,
 20151124,
 20141212,
 20160228,
 20161231,
 20160618,
 20170123,
 20151124,
 20170123,
 20151124))

try = cbind(dat2, dates)
names(try)[ncol(try)] = "dates"




# temp = try[c(1:6, 23),]
# temp = try[c(1:6), c(1:6,23)]


something = as.data.frame(cbind(try$arcus, try$chd69, try$smoke, try$typchd69))


a = try %>%
  select_if
