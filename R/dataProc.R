
dataProc = function (inputData, n, seed) {
  # getting variable names and number of unique levels in each of them
  levelsEachVariable = data.frame(nLevels = double(),
                                  var = character())



  for (i in 1:ncol(inputData)){
    levelsEachVariable[i,1] = length(unique(inputData[,i]))
  }

  levelsEachVariable$var = colnames(inputData)


  simData <- data.frame(matrix( nrow = n, ncol = ncol(inputData)))
  colnames(simData) <- paste0("var", c(1:ncol(inputData)))


  ## getting distribution of each variable and randomly sampling from that to get new dataset

  if (!missing(seed)) {
    set.seed(seed)
  }

  for (i in 1:ncol(inputData))  {

    if (length(unique(inputData[,i])) < 5){
      simData[,i] = sample(c(as.character(as.data.frame(table(inputData[,i]))$Var1)), n, TRUE,
                           prob = c(as.data.frame(table(inputData[,i]))$Freq)
      )
    }
    else if (mean((inputData[,i]%% 1) != 0, na.rm = T)) {
      simData[,i] = rnorm(n, mean = mean(inputData[,i], na.rm = T), sd = sqrt(var(inputData[,i], na.rm = T)))
      }

    else {
      simData[,i] = round(rnorm(n, mean = mean(inputData[,i]), sd = sqrt(var(inputData[,i]))))
    }


    for (j in 1:nrow(inputData)){

      if (is.na(inputData[j, i])) {
        simData[j, i] = NA
      }

      else if (inputData[j, i]== "")  {
        simData[j, i] = ""
      }
    }

    names(simData) = names(inputData)

  } #close for loop

  return(data.frame(simData))
}


