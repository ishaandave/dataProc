install.packages("fitdistrplus")
library(fitdistrplus)

install.packages("logspline")
library(logspline)

x <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)


descdist(x, discrete = FALSE)
descdist(x, discrete = F)$skewness

weib = fitdist(x, "lognorm")
weib
summary(weib)
########################################################################
categorical_example <- fabricate(
  N = 6,
  p1 = runif(N, 0, 1),
  p2 = runif(N, 0, 1),
  p3 = runif(N, 0, 1),
  cat = draw_categorical(N = N, prob = cbind(p1, p2, p3))
)




sum(prop.table(table(c1, c2)))


cat1 = fabricate(
  N = 100,
  category = draw_categorical(N = N, prob = c(prop.table(table(c1, c2))))
)

 xrespondents <- fabricate(
  N = 100,
  cat = draw_categorical(N = N, prob = cbind(.4, .5, .1)),
  cat2 = correlate(given = cat,
                                    rho = 0.5,
                                    draw_binomial,
                                    prob = 0.6,
                                    trials = 20),
  foreign_policy = correlate(given = conservative_values,
                             rho = 0.3,
                             draw_binomial,
                             prob = 0.4,
                             trials = 20)
)





 set.seed(2)
 c1 = sample(c("a", "b", "c", "d"), prob = c(0.25, 0.25, 0.25, 0.25), size = 250, replace = T)
 c2 = sample(c("n", "s"), prob = c(0.5, 0.5), size = 250, replace = T)
 c = cbind(c1, c2)
 prop.table(table(c1, c2))


 set.seed(1)
 c1Sorted = sort(unique(c1))
 c2Sorted = sort(unique(c2))
 n1 <- length(c1Sorted)
 n2 <- length(c2Sorted)
 probmat <- matrix(prop.table(table(c1, c2)), ncol = 2)
 probmat

 probs  <- c(probmat)
 events <- as.matrix(expand.grid(var1=c(c1Sorted), var2=c(c2Sorted)))
 nSamp  <- 250
 samp   <- as.data.frame(events[sample.int(n1*n2, nSamp, prob=probs, replace=TRUE),])
 head(samp)

predProbMat = prop.table(table(samp$var1, samp$var2))
probmat


  plot(c(probmat), c(predProbMat), ylim = c(0, 1.3*max(c(predProbMat, probmat))), xlim = c(0,1.3*max(c(probmat, predProbMat))))
  fit = lm(c(predProbMat) ~ c(probmat))
