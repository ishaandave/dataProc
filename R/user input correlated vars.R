wcgs = read_xls("C:/Users/yhd8/Desktop/Data/Pretend/wcgs.xls")
wcgs = wcgs[1560:1620,]

dat2$char
char2 = paste0(dat2$char, ", ", letters[3:22])

x =


bivariateDistsList = function(listVars, var1, var2, n) {


  for (i in nrow(listVars)) {




    c1 = wcgs$arcus
    c2 = wcgs$behpat
    c = cbind.data.frame(c1, c2)
    trueProbs = prop.table(table(c1, c2))
    probmat = as.data.frame.matrix(prop.table(table(c1, c2)))
    # rownames(probmat) = unique(all2$Geography)
    # probmat2 = cbind(unique(all2$Geography), probmat)
    names(probmat) = paste0("true", c(levels(samp$behpat)))





    set.seed(1)
    c1Sorted = sort(levels(factor(c1)))
    c2Sorted = sort(levels(factor(c2)))
    n1 <- length(c1Sorted)
    n2 <- length(c2Sorted)

    probs  <- c(trueProbs)
    events <- as.matrix(expand.grid(arcus = c1Sorted, behpat = c2Sorted))
    nSamp  <- 50
    samp   <- as.data.frame(events[sample.int(n1*n2, nSamp, prob=probs, replace=TRUE),])
    head(samp)

    predProbs = prop.table(table(samp$arcus, samp$behpat))
    predProbMat = as.data.frame(matrix(predProbs, ncol = nlevels(samp$behpat)))
    names(predProbMat) = paste0("pred", c(levels(samp$behpat)))
    rownames(predProbMat) = levels(samp$arcus)



    predProbMat2 = cbind(predProbMat, unique(samp$arcus))
    names(predProbMat2) = c(paste0("pred", c(levels(samp$behpat))), "arcus")
    predProbMat3 = predProbMat2[order(predProbMat2$arcus),]

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



  }






} ### END FUNCTION ###



c1 = wcgs$arcus
c2 = wcgs$behpat
c = cbind.data.frame(c1, c2)
trueProbs = prop.table(table(c1, c2))
probmat = as.data.frame.matrix(prop.table(table(c1, c2)))
# rownames(probmat) = unique(all2$Geography)
# probmat2 = cbind(unique(all2$Geography), probmat)
names(probmat) = paste0("true", c(levels(samp$behpat)))





set.seed(1)
c1Sorted = sort(levels(factor(c1)))
c2Sorted = sort(levels(factor(c2)))
n1 <- length(c1Sorted)
n2 <- length(c2Sorted)

probs  <- c(trueProbs)
events <- as.matrix(expand.grid(arcus = c1Sorted, behpat = c2Sorted))
nSamp  <- 50
samp   <- as.data.frame(events[sample.int(n1*n2, nSamp, prob=probs, replace=TRUE),])
head(samp)

predProbs = prop.table(table(samp$arcus, samp$behpat))
predProbMat = as.data.frame(matrix(predProbs, ncol = nlevels(samp$behpat)))
names(predProbMat) = paste0("pred", c(levels(samp$behpat)))
rownames(predProbMat) = levels(samp$arcus)



predProbMat2 = cbind(predProbMat, unique(samp$arcus))
names(predProbMat2) = c(paste0("pred", c(levels(samp$behpat))), "arcus")
predProbMat3 = predProbMat2[order(predProbMat2$arcus),]

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

