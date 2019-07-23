#' Associations between each pair of categorical variables
#'
#' This gives associations (\Chi^2 values) and p values of all pairwise comparisons of categorical variables
#' User can input any dataset, and this will return only associations between categorical variables
#'
#' @param inputData dataset that you want to sample from
#'
#' @return associations p-values for each pair of categorical variables
#'
#'
#' @examples
#' categoricalPvals()

categoricalPvals = function (indat) {

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

          pVals[nrowsPval, 1] = paste0(names(indat[i]), ", ", names(indat[j]))
          pVals[nrowsPval, 2] = round(chi$statistic, 3)
          pVals[nrowsPval, 3] = round(chi$parameter, 3)
          pVals[nrowsPval, 4] = round(chi$p.value, 3)

          pVals2 = as.data.frame(pVals[!duplicated(pVals[,c(2,3,4)]),])

          names(pVals2) = c( "catVarsTested", "chiSquare", "degreesOfFreedom", "pValue")

          pVals2 = subset(pVals2, is.na(pVals2$chiSquare) == F)

        }


        # else if (all(is.na(indat[,i])) |
        #          (all(is.character(indat[,i])) & length(unique(indat[,i])) == nrow(indat))) next
        #

      }  # close j for loop

    }  # close if statement checking if categorical

  }  # close i for loop

  return(pVals2)


} # close function




