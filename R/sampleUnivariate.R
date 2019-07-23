
#' Univariate sampling of variables through a dataset
#'
#' Takes any dataset, checks format of each variable
#' Based on distribution from original variable, randomly samples into a new dataset n times
#'
#' Returns distributionally similar dataset
#'
#' @param inputData dataset that you want to sample from
#' @param n number of rows in output/simulated dataset
#' @param dateFormat if date variable exists in dataset, it's the format the date must be read in
#'
#' @return simData -- distributionally similar to input dataset
#'
#'
#' @export
#'
#'
#' @examples
#' sampleUnivariate()

sampleUnivariate = function (inputData, n, dateFormat = "%Y%m%d") {

  simData = data.frame(matrix(nrow = n, ncol = ncol(inputData)))

  ## getting distribution of each variable and randomly sampling from that to get new dataset


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

     if (any(sapply(inputData, function(x) !all(is.na(as.Date(as.character(x), format= dateFormat)))))) {
                   # & nchar(inputData[min(which(!is.na(inputData[,i]))), i]) == 8) {

          possibleDates = which(sapply(inputData,
                                       function(x)
                                         !all(is.na(as.Date(as.character(x),
                                         format="%m/%d/%Y")))))

          dateFormatted2 = as.Date(as.character(inputData[, as.numeric(c(unname(possibleDates)))],
                                                format = dateFormat))

          dates3 = sample(seq(min(dateFormatted2, na.rm = T),
                          max(dateFormatted2, na.rm = T), by ="day"), n)

          simData[,i] = dates3


      } else if (all(is.na(inputData[,i])) |
           (all(is.character(inputData[,i])) & length(unique(inputData[,i])) == nrow(inputData))) next

            ### RETURNS NA's




      else if (length(unique(inputData[,i])) < 7 | all(is.factor(inputData[,i]))) {

          simData[,i] = sample(c(as.character(as.data.frame(table(inputData[,i]))$Var1)), n, TRUE,
                               prob = c(as.data.frame(table(inputData[,i]))$Freq)
                              )

    } else if (any(na.omit(inputData[,i]) %% 1 == 0)) {


      fitNormal  <- fitdist(inputData[,i], "norm", method = "mme")
      fitGamma   <- fitdist(inputData[,i], "gamma", method = "mme")
      # fitLogNorm <- fitdist(abs(inputData[,i]), "lnorm", method = "mme")
      # fitWeibull <- fitdist(inputData[,i], "weibull", method = "mge")


      listFits = list(fitNormal, fitGamma)#, fitWeibull)

      fits = gofstat(listFits, fitnames=c("norm", "gamma"))#, "weibull"))

      simData[,i] = round(eval(parse(text = paste0("r", names(which.min(fits$aic)), '(', 'n, ',
                                   listFits[[which.min(fits$aic)[[1]]]][[1]][[1]], ', ',
                                   listFits[[which.min(fits$aic)[[1]]]][[1]][[2]], ')'))))

    } else {

      simData[,i] = eval(parse(text = paste0("r", names(which.min(fits$aic)), '(', 'n, ',
                                   listFits[[which.min(fits$aic)[[1]]]][[1]][[1]], ', ',
                                   listFits[[which.min(fits$aic)[[1]]]][[1]][[2]], ')')))
    }

  } #close big for loop (1)


  for (j in 1:nrow(inputData)) { # (2)

    if (is.na(inputData[j, i])) {

          simData[c(which(is.na(inputData[,i]))), i] = NA

    }  else if (inputData[j, i]== "")  {

          simData[c(which(inputData[,i] == "")), i] = ""
    }

  } ## Close for loop (2)

   names(simData) = names(inputData)
   return(data.frame(simData))

}

