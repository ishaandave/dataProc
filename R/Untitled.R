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

123

for(n in 1:5) {
  if(n==3) next # skip 3rd iteration and go to next iteration
  cat(n)
}
