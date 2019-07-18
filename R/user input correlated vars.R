wcgs = data.frame(read_xls("C:/Users/yhd8/Desktop/Data/Pretend/wcgs.xls"))
wcgs = wcgs[1560:1620,]




corrVars = data.frame(read_xlsx("C:/Users/yhd8/Desktop/Data/Pretend/listVars.xlsx"))

# dat2$char
# char2 = paste0(dat2$char, ", ", letters[3:22])

# x =


bivariateDistsList = function(indat, listVars, n) {


  for (i in 1:nrow(listVars)) {



    c1 = indat[,(listVars[i,1])] ## creating 2 vectors from names of correlated variables
    c2 = indat[,(listVars[i,2])]
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
    head(samp)

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

   print(observedAndPredicted) ## printing to check

  }

  return(observedAndPredicted)



  simData[,indat[,(listVars[i,1])]] = samp$V1

}












########## SCRATCH WORK ###########################








c1 = wcgs$smoke
c2 = wcgs$agec
c = cbind.data.frame(c1, c2)
trueProbs = prop.table(table(c1, c2))
probmat = as.data.frame.matrix(prop.table(table(c1, c2)))
# rownames(probmat) = unique(all2$Geography)
# probmat2 = cbind(unique(all2$Geography), probmat)
names(probmat) = paste0("true", c(unique(c[,2])))





set.seed(1)
c1Sorted = sort(levels(factor(c1)))
c2Sorted = sort(levels(factor(c2)))
n1 <- length(c1Sorted)
n2 <- length(c2Sorted)

probs  <- c(trueProbs)
events <- as.matrix(expand.grid(v1 = c1Sorted, v2 = c2Sorted))
nSamp  <- 500000
samp   <- as.data.frame(events[sample.int(n1*n2, nSamp, prob=probs, replace=TRUE),])
head(samp)

predProbs = prop.table(table(samp$v1, samp$v2))
predProbMat = as.data.frame(matrix(predProbs, ncol = nlevels(c[,2])))
names(predProbMat) = paste0("pred", c(levels(samp$v2)))
rownames(predProbMat) = c(c1Sorted)



predProbMat2 = cbind(predProbMat, unique(samp$v1))
names(predProbMat2) = c(paste0("pred", c(levels(samp$v2))), "arcus")
predProbMat3 = predProbMat2[order(predProbMat2[,ncol(predProbMat2)]),]

observedAndPredicted = cbind(probmat, predProbMat3)
observedAndPredicted[is.na(observedAndPredicted)] <- 0

true = observedAndPredicted %>%
  select(select_vars(names(observedAndPredicted), starts_with('true', ignore.case = TRUE)))

expected = observedAndPredicted %>%
  select(select_vars(names(observedAndPredicted), starts_with('pred', ignore.case = TRUE)))

listTrue = c(as.matrix(true))
listExpected = c(as.matrix(expected))

plot(c(as.matrix(true)),
     c(as.matrix(expected)), ylim = c(0, 1*max(c(listTrue, listExpected))), xlim = c(0,1*max(c(listTrue, listExpected))),
     xlab = "True", ylab = "Expected")

