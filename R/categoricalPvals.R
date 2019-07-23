
#' Associations between each pair of categorical variables
#'
#' This gives associations (Chi-square values) and p values of all pairwise comparisons of categorical variables
#' User can input any dataset, and this will return only associations between categorical variables
#'
#' @param inputData "true" dataset that you want to sample from
#'
#' @return associations p-values for each pair of categorical variables
#'
#' @export
#'
#' @importFrom stats chisq.test na.omit
#'
#' @examples
#' categoricalPvals()

categoricalPvals = function (inputData) {

  pVals = matrix(nrow = ncol(inputData)**2, ncol = 4)

  nrowsPval = 0

  for (i in 1:ncol(inputData)) {

    if (length(unique(inputData[,i])) <= 5 | all(is.factor(inputData[,i]))) {

      for (j in 1:ncol(inputData)) {

        if (i == j) next

        else if (length(unique(inputData[,i])) == 1 | length(unique(inputData[,j])) == 1) next

        else if (length(unique(inputData[,j])) <= 5 | all(is.factor(inputData[,j])))  {

          chi = chisq.test(inputData[,i], inputData[,j])

          nrowsPval = nrowsPval + 1

          pVals[nrowsPval, 1] = paste0(names(inputData[i]), ", ", names(inputData[j]))
          pVals[nrowsPval, 2] = round(chi$statistic, 3)
          pVals[nrowsPval, 3] = round(chi$parameter, 3)
          pVals[nrowsPval, 4] = round(chi$p.value, 3)

          pVals2 = as.data.frame(pVals[!duplicated(pVals[,c(2,3,4)]),])

          names(pVals2) = c( "catVarsTested", "chiSquare", "degreesOfFreedom", "pValue")

          pVals2 = subset(pVals2, is.na(pVals2$chiSquare) == F)

        }


        # else if (all(is.na(inputData[,i])) |
        #          (all(is.character(inputData[,i])) & length(unique(inputData[,i])) == nrow(inputData))) next
        #

      }  # close j for loop

    }  # close if statement checking if categorical

  }  # close i for loop

  return(pVals2)


} # close function




