
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
        
        
        corPvals[nrowsPval, 1] = paste0(names(indat)[i], ", ", names(indat)[j])
        corPvals[nrowsPval, 2] = round(correlation$estimate, 3)
        corPvals[nrowsPval, 3] = paste0("(",round(correlation$conf.int[1],3), ", ",
                                        round(correlation$conf.int[2],3), ")")
        
        corPvals[nrowsPval, 4] = round(correlation$p.value, 3)
        
        
        corPvals2 = as.data.frame(corPvals[!duplicated(corPvals[,c(2,3,4)]),])
        
        names(corPvals2) = c( "contVarsTested", "corrEstimate", "CI95", "pValue")
        
        corPvals2 = subset(corPvals2, is.na(corPvals2$corrEstimate) == F)
        
        pairs = as.data.frame(corPvals2$contVarsTested)
        names(pairs) = "catVarsTested"
        
      } # close the if statement looking for numeric/continuous data
    } # close j for loop
  } # close i for loop
  return(pairs)
} # close function






