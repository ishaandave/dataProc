library(truncnorm)

dataProc = function (inputData, n, seed) {


  simData <- data.frame(matrix(nrow = n, ncol = ncol(inputData)))
  names(simData) <- paste0("var", c(1:ncol(inputData)))


  ## getting distribution of each variable and randomly sampling from that to get new dataset

  if (!missing(seed)) {
    set.seed(seed)
  }



  for (i in 1:ncol(inputData))  { # (1)

    if (all(is.na(inputData[,i])) |
           (all(is.character(inputData[,i])) & length(unique(inputData[,i])) == nrow(inputData))) next

     if (length(unique(inputData[,i])) < 6 | all(is.factor(inputData[,i]))){

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

    names(simData) = names(inputData)

  } #close big for loop (1)
    View(simData)

  return(data.frame(simData))
}

