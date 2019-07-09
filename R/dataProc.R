library(truncnorm)
library(lubridate)
library(stringr)
library(ggplot2)
library(MASS)

dataProc = function (inputData, n, seed) {


  simData = data.frame(matrix(nrow = n, ncol = ncol(inputData)))


  ## getting distribution of each variable and randomly sampling from that to get new dataset

  if (!missing(seed)) {
    set.seed(seed)
  }


  for (i in 1:ncol(inputData))  { # (1)


    # if (any(str_detect(complete.cases(as.character(inputData[,i])), datePattern))) {
    #
    #   dateFormatted = as.Date(as.character(inputData[,i]))
    #
    #
    #   dates2 = sample(seq(min(dateFormatted, na.rm = T),
    #                       max(dateFormatted, na.rm = T), by ="day"), n)
    #
    #   simData[,i] = dates2
    #
    # }

     if ((any(is.Date(as.Date(as.character(inputData[,i]), format = "%Y%m%d"))))
                   & nchar(inputData[min(which(!is.na(inputData[,i]))),i]) == 8) {

          dateFormatted2 = as.Date(as.character(inputData[,i]), format = "%Y%m%d")

          dates3 = sample(seq(min(dateFormatted2, na.rm = T),
                          max(dateFormatted2, na.rm = T), by ="day"), n)

          simData[,i] = dates3

    }


      else if (all(is.na(inputData[,i])) |
           (all(is.character(inputData[,i])) & length(unique(inputData[,i])) == nrow(inputData))) next

      else if (length(unique(inputData[,i])) < 6 | all(is.factor(inputData[,i]))) {

          simData[,i] = sample(c(as.character(as.data.frame(table(inputData[,i]))$Var1)), n, TRUE,
                               prob = c(as.data.frame(table(inputData[,i]))$Freq)
                              )
     }


    else if (any(na.omit(inputData[,i]) %% 1 == 0)) {

          simData[,i] = round(rtruncnorm(n,
                                     a = min(inputData[,i], na.rm = T), mean = mean(inputData[,i], na.rm = T),
                                     sd = sd(inputData[,i], na.rm = T)))
    }

    else {
          simData[,i] = rtruncnorm(n,
                               a = min(inputData[,i], na.rm = T), mean = mean(inputData[,i], na.rm = T),
                               sd = sd(inputData[,i], na.rm = T))
    }

  } #close big for loop (1)


  for (j in 1:nrow(inputData)) { # (2)

    if (is.na(inputData[j, i])) {

          simData[c(which(is.na(inputData[,i]))), i] = NA

    }


    else if (inputData[j, i]== "")  {

          simData[c(which(inputData[,i] == "")), i] = ""
    }

  } ## Close for loop (2)

        # ggplot() + aes(x = format(dates2, "%Y-%m")) +
        # geom_bar() + labs(x = "Month")

  # simData$fakeDates = dates2
  # simData$originalFormattedDates = dateFormatted[c(1:n)]
  #   View(simData)
   names(simData) = names(inputData)
   return(data.frame(simData))
}













categoricalPvals = function (indat) {

  pVals = matrix(nrow = ncol(indat)**2, ncol = 4)

  nrowsPval = 0

  for (i in 1:ncol(indat)) {

    if (length(unique(indat[,i])) <= 5 | all(is.factor(indat[,i]))) {

        for (j in 1:ncol(indat)) {

          if (i == j) next

            else if (length(unique(indat[,i])) == 1 | length(unique(indat[,j])) == 1) next

            else if (length(unique(indat[,j])) <= 5 | all(is.factor(indat[,j])))  {

                chi = chisq.test(indat[,i], indat[,j])

                nrowsPval = nrowsPval + 1

                pVals[nrowsPval, 1] = paste0(names(indat[i]), ", ", names(indat[j]))
                pVals[nrowsPval, 2] = round(chi$statistic, 3)
                pVals[nrowsPval, 3] = round(chi$parameter, 3)
                pVals[nrowsPval, 4] = round(chi$p.value, 3)

                pVals2 = as.data.frame(pVals[!duplicated(pVals[,c(2,3,4)]),])

                names(pVals2) = c( "catVarsTested", "chiSquare", "degreesOfFreedom", "pValue")

                pVals2 = subset(pVals2, is.na(pVals2$chiSquare) == F)

          }


        # else if (all(is.na(indat[,i])) |
        #          (all(is.character(indat[,i])) & length(unique(indat[,i])) == nrow(indat))) next
        #

      }  # close j for loop

    }  # close if statement checking if categorical

  }  # close i for loop

  return(pVals2)


} # close function






pairsCatVars = function (indat) {

  pVals = matrix(nrow = ncol(indat)**2, ncol = 4)

  nrowsPval = 0

  for (i in 1:ncol(indat)) {

    if (length(unique(indat[,i])) <= 5 | all(is.factor(indat[,i]))) {

      for (j in 1:ncol(indat)) {

        if (i == j) next

          else if (length(unique(indat[,i])) == 1 | length(unique(indat[,j])) == 1) next

          else if (length(unique(indat[,j])) <= 5 | all(is.factor(indat[,j])))  {

              chi = chisq.test(indat[,i], indat[,j])

              nrowsPval = nrowsPval + 1

              pVals[nrowsPval, 1] = paste0(names(indat[i]), ", ", names(indat[j]))
              pVals[nrowsPval, 2] = round(chi$statistic, 3)
              pVals[nrowsPval, 3] = round(chi$parameter, 3)
              pVals[nrowsPval, 4] = round(chi$p.value, 3)

              pVals2 = as.data.frame(pVals[!duplicated(pVals[,c(2,3,4)]),])

              names(pVals2) = c( "catVarsTested", "chiSquare", "degreesOfFreedom", "pValue")

              pVals2 = subset(pVals2, is.na(pVals2$chiSquare) == F)

              pairs = as.data.frame(pVals2$catVarsTested)
              names(pairs) = "catVarsTested"
        }


        # else if (all(is.na(indat[,i])) |
        #          (all(is.character(indat[,i])) & length(unique(indat[,i])) == nrow(indat))) next
        #

      }  # close j for loop

    }  # close if statement checking if categorical

  }  # close i for loop

  return(pairs)


} # close function
























correlationPvals = function (indat) {

  corPvals = matrix(nrow = ncol(indat)**2, ncol = 4)

  nrowsPval = 0

  for (i in 1:ncol(indat)) {

    for (j in 1:ncol(indat)) {

      if (i == j) next

      if ((is.numeric(indat[,i]) & length(unique(indat[,i])) > 5) &
          (is.numeric(indat[,j]) & length(unique(indat[,j])) > 5)) {

          correlation = cor.test(indat[,i], indat[,j])

          nrowsPval = nrowsPval + 1;

          corPvals[nrowsPval, 1] = round(correlation$estimate, 3)
          corPvals[nrowsPval, 2] = paste0("(",round(correlation$conf.int[1],3), ", ",
                                              round(correlation$conf.int[2],3), ")")

          corPvals[nrowsPval, 3] = round(correlation$p.value, 3)
          corPvals[nrowsPval, 4] = paste0(names(indat)[i], ",", names(indat)[j])


          corPvals2 = as.data.frame(corPvals[!duplicated(corPvals[,c(1,2,3)]),])

          names(corPvals2) = c("corrEstimate", "CI95", "pValue", "contVarsTested")

          corPvals2 = subset(corPvals2, is.na(corPvals2$corrEstimate) == F)


      } # close if statement looking for numeric/continuous data
    } # close j for loop
  } # close i for loop
  return(corPvals2)
} # close function
















pairsContVars = function (indat) {

  corPvals = matrix(nrow = ncol(indat)**2, ncol = 4)

  nrowsPval = 0

  for (i in 1:ncol(indat)) {

    for (j in 1:ncol(indat)) {

      if (i == j) next

      if ((is.numeric(indat[,i]) & length(unique(indat[,i])) > 5) &
          (is.numeric(indat[,j]) & length(unique(indat[,j])) > 5)) {

        correlation = cor.test(indat[,i], indat[,j])

        nrowsPval = nrowsPval + 1;


        corPvals[nrowsPval, 1] = paste0(names(indat)[i], ",", names(indat)[j])
        corPvals[nrowsPval, 2] = round(correlation$estimate, 3)
        corPvals[nrowsPval, 3] = paste0("(",round(correlation$conf.int[1],3), ", ",
                                        round(correlation$conf.int[2],3), ")")

        corPvals[nrowsPval, 4] = round(correlation$p.value, 3)


        corPvals2 = as.data.frame(corPvals[!duplicated(corPvals[,c(2,3,4)]),])

        names(corPvals2) = c( "contVarsTested", "corrEstimate", "CI95", "pValue")

        corPvals2 = subset(corPvals2, is.na(corPvals2$corrEstimate) == F)

        pairs = as.data.frame(corPvals2$contVarsTested)
        names(pairs) = "catVarsTested"

      } # close if statement looking for numeric/continuous data
    } # close j for loop
  } # close i for loop
  return(pairs)
} # close function






