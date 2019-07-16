
x<-c(1.89,5.73,5.27,6.77,1.08,7.5,4.24,1.12,2.68,1.29,2.68,1.26,1.1,1.19,4.16,
     1.71,5.7,2.88,5.43,12.88,1.35,1.2,1.07,1.03,1.39,1.72,4.76,1.45,1.37,5.91,9.59,
     4.36,5.15,1.85,2.82,8.58,1,6.93,1.48,6.23,12.42,1.22,7.02,2.73,7.11,1.93,
     5.68,1.03,1.26,1.07,1.22)

fn <- fitdist(x, "norm")
fg <- fitdist(x, "gamma")
fln <- fitdist(x, "lnorm")
fw <- fitdist(x, "weibull")

listfits = list(fn, fg, fln, fw)

fits = gofstat(list(fn, fg, fln, fw),fitnames=c("norm", "gamma","lnorm","weibull"))

print(which.min(fits$aic))

 a = eval(parse(text = paste0("r", names(which.min(fits$aic)), '(', '100, ',
                              listfits[[which.min(fits$aic)[[1]]]][[1]][[1]], ', ',
                              listfits[[which.min(fits$aic)[[1]]]][[1]][[2]], ')')))

plot(density(a))

