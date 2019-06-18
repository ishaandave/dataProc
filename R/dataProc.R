
dataProc = function (data, n, seed) {
  # getting variable names and number of unique levels in each of them
  levelsEachVariable = data.frame(nLevels = double(),
                                  var = character())

  for (i in 1:ncol(data)){
    levelsEachVariable[i,1] = length(unique(data[,i]))
  }

  levelsEachVariable$var = colnames(data)



  simData <- data.frame(matrix(ncol = ncol(data), nrow = n))
  colnames(simData) <- paste0("var", c(1:ncol(data)))


  ## getting distribution of each variable and randomly sampling from that to get new dataset

  if (!missing(seed)) {
    set.seed(seed)
  }


  for (i in 1:ncol(data))
  {

    if (length(unique(data[,i])) < 5){
      simData[,i] = sample(c(as.character(as.data.frame(table(data[,i]))$Var1)), n, TRUE,
                           prob = c(as.data.frame(table(data[,i]))$Freq)
      )
    }
    else  {
      simData[,i] = rnorm(n, mean = mean(data[,i]), sd = sqrt(var(data[,i])) )

    }
    names(simData) = names(data)

  } #close for loop

  return(data.frame(simData))
}


