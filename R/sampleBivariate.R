

#' Title
#'
#' @param inputData
#' @param listVars
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#'
sampleBivariate = function(inputData, n, listVars, dateFormat = "%Y%m%d") {

  outputData = sampleUnivariate(inputData, n, dateFormat)


  for (i in 1:nrow(listVars)) {

    c1 = inputData[,(listVars[i,1])] ## creating 2 vectors from names of correlated variables
    c2 = inputData[,(listVars[i,2])]
    c = cbind(c1, c2)
    trueProbs = prop.table(table(c1, c2)) ## Getting "true" proportions from original dataset
    probmat = as.data.frame.matrix(trueProbs)
    names(probmat) = c(paste0("true", c(unique(c[,2]))))  ## Creating dataset of proportions from original datast


    c1Sorted = sort(levels(factor(c1))) ## sorting levels of the 2 vectors for convenience
    c2Sorted = sort(levels(factor(c2)))
    n1 <- length(c1Sorted)              ## Getting number of levels in each vector (used below)
    n2 <- length(c2Sorted)

    probs  <- c(trueProbs)              ## probability "weights" in a vector used for sampling
    events <- as.matrix(expand.grid(V1 = c1Sorted, V2 = c2Sorted)) ## events is a matrix of all combinations of 2 variables
    samp   <- as.data.frame(events[sample.int(n1*n2, n, prob=probs, replace=TRUE),]) ## sampling from combos with respective proportions
    # head(samp)

    predProbs = prop.table(table(samp[, 1], samp[, 2])) ## seeing new/sampled dataset proportions
    predProbMat = as.data.frame(matrix(predProbs, ncol = nlevels(samp[,2]))) ## making a dataframe of sample proportions
    names(predProbMat) = paste0("pred", c(levels(samp$V2))) ## changing column and row names for easy merging
    rownames(predProbMat) = c(c1Sorted)



    predProbMat2 = cbind(predProbMat, unique(samp$V1))
    names(predProbMat2) = c(paste0("pred", c(levels(samp$V2))), listVars[i,1])
    predProbMat3 = predProbMat2[order(predProbMat2[,ncol(predProbMat2)]),] ## ordering for merging

    observedAndPredicted = cbind(probmat, predProbMat3) ## creating output dataset with true and predicted proportions
                                                        ## to see how well the sampling procedure did

    observedAndPredicted[is.na(observedAndPredicted)] <- 0 ## input 0's if a level of a variable wasn't sampled



    outputData[, (listVars[i, 1])] = samp$V1
    outputData[, (listVars[i, 2])] = samp$V2


  }

  return(outputData)

}


