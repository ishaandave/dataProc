
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
          
          pVals[nrowsPval, 1] = paste0(names(indat)[i], ", ", names(indat[j]))
          pVals[nrowsPval, 2] = round(chi$statistic, 3)
          pVals[nrowsPval, 3] = round(chi$parameter, 3)
          pVals[nrowsPval, 4] = round(chi$p.value, 3)
          
          pVals2 = as.data.frame(pVals[!duplicated(pVals[,c(2,3,4)]),])
          
          names(pVals2) = c( "catVarsTested", "chiSquare", "degreesOfFreedom", "pValue")
          
          pVals3 = subset(pVals2, is.na(pVals2$chiSquare) == F)
          
          pairs = as.data.frame(pVals3$catVarsTested)
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



