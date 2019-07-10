

continuousPvals = function (indat) {
  
  contPvals = matrix(nrow = ncol(indat)**2, ncol = 4)
  
  nrowsPval = 0
  
  for (i in 1:ncol(indat)) {
    
    for (j in 1:ncol(indat)) {
      
      if (i == j) next
      
      if ((is.numeric(indat[,i]) & length(unique(indat[,i])) > 5) &
          (is.numeric(indat[,j]) & length(unique(indat[,j])) > 5)) {
        
        correlation = cor.test(indat[,i], indat[,j])
        
        nrowsPval = nrowsPval + 1;
        
        contPvals[nrowsPval, 1] = round(correlation$estimate, 3)
        contPvals[nrowsPval, 2] = paste0("(",round(correlation$conf.int[1],3), ", ",
                                        round(correlation$conf.int[2],3), ")")
        
        contPvals[nrowsPval, 3] = round(correlation$p.value, 3)
        contPvals[nrowsPval, 4] = paste0(names(indat)[i], ",", names(indat)[j])
        
        
        contPvals2 = as.data.frame(contPvals[!duplicated(contPvals[,c(1,2,3)]),])
        
        names(contPvals2) = c("corrEstimate", "CI95", "pValue", "contVarsTested")
        
        contPvals2 = subset(contPvals2, is.na(contPvals2$corrEstimate) == F)
        
        
      } # close if statement looking for numeric/continuous data
    } # close j for loop
  } # close i for loop
  return(contPvals2)
} # close function






