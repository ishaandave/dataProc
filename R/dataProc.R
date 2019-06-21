library(truncnorm)
library(lubridate)
library(stringr)
library(ggplot2)


dataProc = function (inputData, n, seed) {


  simData <- data.frame(matrix(nrow = n, ncol = ncol(inputData)))


  ## getting distribution of each variable and randomly sampling from that to get new dataset

  if (!missing(seed)) {
    set.seed(seed)
  }


  for (i in 1:ncol(inputData))  { # (1)


    datePattern = "([0-9][0-9][0-9][0-9])[-]([0-1][0-9])[-]([0-9][0-9])"


    if (any(str_detect(as.character(inputData[,i]), datePattern))) {

      dateFormatted = as.Date(as.character(inputData[,i]))


      dates2 = sample(seq(min(dateFormatted, na.rm = T),
                          max(dateFormatted, na.rm = T), by ="day"), n)
      simData[,i] = dates2
#
      ggplot() + aes(x = format(dates2, "%Y-%m")) +
        geom_bar() + labs(x = "Month")


    }

    else if (all(is.na(inputData[,i])) |
           (all(is.character(inputData[,i])) & length(unique(inputData[,i])) == nrow(inputData))) next

     else if (length(unique(inputData[,i])) < 6 | all(is.factor(inputData[,i]))) {

      simData[,i] = sample(c(as.character(as.data.frame(table(inputData[,i]))$Var1)), n, TRUE,
                           prob = c(as.data.frame(table(inputData[,i]))$Freq)
                          )
     }


    else if (mean(inputData[,i] %% 1, na.rm = T) == 0) {

      simData[,i] = round(rtruncnorm(n,
                                     a = min(inputData[,i], na.rm = T), mean = mean(inputData[,i], na.rm = T),
                                     sd = sd(inputData[,i], na.rm = T)))
      }

    else {
      simData[,i] = rtruncnorm(n,
                               a = min(inputData[,i], na.rm = T), mean = mean(inputData[,i], na.rm = T),
                               sd = sd(inputData[,i], na.rm = T))
    }

     for (j in 1:nrow(inputData)) { # (2)
    #
      if (is.na(inputData[j, i])) {
        simData[c(which(is.na(inputData[,i]))), i] = NA
      }
    # #
    #   else if (inputData[j, i]== "")  {
    #     simData[c(which(inputData[,i]=="")), i] = ""
    #   }
     } # (2)



  } #close big for loop (1)
  names(simData) = names(inputData)
  simData$fakeDates = dates2
  simData$originalFormattedDates = dateFormatted[c(1:n)]
    View(simData)

  return(data.frame(simData))
}


