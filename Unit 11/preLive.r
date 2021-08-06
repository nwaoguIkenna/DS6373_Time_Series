library(tswge)
library(orcutt)

###### Sunspot data
sunspot = read.csv2("~/Documents/datascience/DS6373/Unit 11/SN_y_tot_V2.0.csv", header = F)

plotts.sample.wge(sunspot$V2)

sunspot.1 <- as.double(sunspot$V2)

aic5.wge(sunspot.1, p = 0:15, q = 0:2, type = 'bic')

# Box-Jenkins model
acf(sunspot.1)
pacf(sunspot.1)

# as stationary
s2 = est.ar.wge(sunspot.1, p = 9, type = 'mle')

# check residuals for white noise check
plotts.sample.wge(s2$res, arlimits = TRUE)
ljung.wge(s2$res, p = 9)
ljung.wge(s2$res, p = 9, K = 48)

# There is evidence that the residual is white noise

# ASE stationary
length(sunspot.1)
mlefc = fore.arma.wge(sunspot.1, phi = s2$phi, lastn = T, n.ahead = 15,limits = FALSE)
ASE = mean((mlefc$f - sunspot.1[306:320])^2)
ASE


# forecasts
# stationary model 
fore.arma.wge(sunspot.1, phi = s2$phi, n.ahead = 10, limits = FALSE)

# As Non -Stationary
sunspot.1.12 = artrans.wge(sunspot.1, phi.tr = c(rep(0,11),1))

aic5.wge(sunspot.1.12, p = 0:20, q = 0:2, type = 'bic')

s2.1 = est.ar.wge(sunspot.1.12, p = 14, type = 'mle')

# check residuals for white noise check
plotts.sample.wge(s2.1$res, arlimits = TRUE)
ljung.wge(s2.1$res, p = 14)
ljung.wge(s2.1$res, p =14, K = 48)

# There is evidence that the residual is not white noise

# ASE non-stationary
mlefc = fore.aruma.wge(sunspot.1, s=12, phi = s2.1$phi, lastn = T, n.ahead = 15,limits = FALSE)
ASE = mean((mlefc$f - sunspot.1[306:320])^2)
ASE


# forecasts
# stationary model 
fore.aruma.wge(sunspot.1,s = 12, phi = s2.1$phi, n.ahead = 10, limits = FALSE)

# As Non -Stationary
sunspot.1.12 = artrans.wge(sunspot.1, phi.tr = c(rep(0,11),1))


# Accuspike data live session

accuspike = read.csv("~/Documents/datascience/DS6373/Unit 11/Accuspike.csv", header = TRUE)
accuspike.1 = accuspike$Active.Users

plotts.sample.wge(accuspike.1)

# Box-Jenkins model
acf(accuspike.1)
pacf(accuspike.1)

factor.wge(accuspike.1)
aic5.wge(accuspike.1, p = 5:15, q = 0:2)


# as stationary
acc = est.ar.wge(sunspot.1, p = 12, type = 'mle')

# stationary model 
fore.arma.wge(accuspike.1, phi = acc$phi, n.ahead = 10, limits = FALSE)

# ASE stationary
length(accuspike.1)
mlefc = fore.arma.wge(accuspike.1, phi = acc$phi, lastn = T, n.ahead = 10,limits = FALSE)
ASE = mean((mlefc$f - accuspike.1[183:192])^2)
ASE

# check residuals for white noise check
plotts.sample.wge(acc$res, arlimits = TRUE)
ljung.wge(acc$res, p = 12)
ljung.wge(acc$res, p = 12, K = 48)
