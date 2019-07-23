#' Correlations between each pair of continuous variables
#'
#' This gives correlations and p values of all pairwise comparisons of continuous variables
#' User can input any dataset, and this will return only associations between continuous variables
#'
#' @param inputData  "true" dataset that you want to sample from
#'
#' @return correlations p-values for each pair of continuous variables
#'
#' @export
#'
#' @examples
#' continuousPvals()

continuousPvals = function (inputData) {

  contPvals = matrix(nrow = ncol(inputData)**2, ncol = 4)

  nrowsPval = 0

  for (i in 1:ncol(inputData)) {

    for (j in 1:ncol(inputData)) {

      if (i == j) next

      if ((is.numeric(inputData[,i]) & length(unique(inputData[,i])) > 5) &
          (is.numeric(inputData[,j]) & length(unique(inputData[,j])) > 5)) {

        correlation = cor.test(inputData[,i], inputData[,j])

        nrowsPval = nrowsPval + 1;

        contPvals[nrowsPval, 1] = round(correlation$estimate, 3)
        contPvals[nrowsPval, 2] = paste0("(",round(correlation$conf.int[1],3), ", ",
                                        round(correlation$conf.int[2],3), ")")

        contPvals[nrowsPval, 3] = round(correlation$p.value, 3)
        contPvals[nrowsPval, 4] = paste0(names(inputData)[i], ",", names(inputData)[j])


        contPvals2 = as.data.frame(contPvals[!duplicated(contPvals[,c(1,2,3)]),])

        names(contPvals2) = c("corrEstimate", "CI95", "pValue", "contVarsTested")

        contPvals2 = subset(contPvals2, is.na(contPvals2$corrEstimate) == F)


      } # close if statement looking for numeric/continuous data
    } # close j for loop
  } # close i for loop
  return(contPvals2)
} # close function






