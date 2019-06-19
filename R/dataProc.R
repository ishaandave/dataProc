
dataProc = function (inputData, n, seed) {
  # getting variable names and number of unique levels in each of them
  levelsEachVariable = data.frame(nLevels = double(),
                                  var = character())



  for (i in 1:ncol(inputData)){ #1
    levelsEachVariable[i,1] = length(unique(inputData[,i]))
  }                             #1

  levelsEachVariable$var = colnames(inputData)


  simData <- data.frame(matrix(nrow = n, ncol = ncol(inputData)))
  colnames(simData) <- paste0("var", c(1:ncol(inputData)))


  ## getting distribution of each variable and randomly sampling from that to get new dataset

  if (!missing(seed)) {
    set.seed(seed)
  }



  for (i in 1:ncol(inputData))  { # 2

    if (all(is.na(inputData[,i]))) next


     if (length(unique(inputData[,i])) < 5 | all(is.factor(inputData[,i]))){
      simData[,i] = sample(c(as.character(as.data.frame(table(inputData[,i]))$Var1)), n, TRUE,
                           prob = c(as.data.frame(table(inputData[,i]))$Freq)
      )
  }


    else if (mean(inputData[,i] %% 1, na.rm = T) != 0) {
      simData[,i] = rnorm(n, mean = mean(inputData[,i], na.rm = T), sd = sqrt(var(inputData[,i], na.rm = T)))
      }

    else {
      simData[,i] = round(rnorm(n, mean = mean(inputData[,i], na.rm = T), sd = sqrt(var(inputData[,i], na.rm = T))))
    }



    for (j in 1:nrow(inputData)) { # 3

      if (is.na(inputData[j, i])) {
        simData[j, i] = NA
      }

      else if (inputData[j, i]== "")  {
        simData[j, i] = ""
      }
    } # 3

    names(simData) = names(inputData)

  } #close big for loop #2

  return(data.frame(simData))
}


