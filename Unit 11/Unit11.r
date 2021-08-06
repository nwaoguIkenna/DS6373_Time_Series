library(tswge)
library(orcutt)

x = gen.arma.wge(n=100,phi = c(1.6,-.9), theta = .8, sn = 67)
x = x+10
plotts.sample.wge(x)

aic.wge(x, p = 0:8, q = 0:4)

x21 = est.arma.wge(x, p =2,q=1)
x21$phi
x21$theta
x21$avar

# Examine the residuals
plotts.sample.wge( x21$res)

x21ljungTest = ljung.wge(x21$res, p = 2, q = 1)  # default K value is 24
x21ljungTest$pval #this value is 0.46 is above 0.05 so we fail to reject the null hypothesis

x21ljungTest = ljung.wge(x21$res, p = 2, q = 1, K = 48)  # K value is 48
x21ljungTest$pval #this value is 0.48 is above 0.05 so we fail to reject the null hypothesis

### Aruma Model
x = gen.aruma.wge(n=200, s = 12, phi =c(1.5, -.8), sn = 87)
x = x+ 50
plotts.sample.wge(x, lag.max = 60)
factor.wge(x)
aci = aic.wge(x, p = 0:10, q = 0:4)
est.arma.wge(x, p = aci$p, q = aci$q)

# Transform the data
y = artrans.wge(x, phi.tr = c(0,0,0,0,0,0,0,0,0,0,0,1))

aic.wge(y, type = 'bic') # p = 2

est.y = est.ar.wge(y, p = 2)

plotts.sample.wge(est.y$res) # looks like white noise.
lj = ljung.wge(est.y$res, p = 2)
lj$pval # 0.52 so we failed to reject. we think it is a white noise residual

ljk = ljung.wge(est.y$res, p = 2, K = 48)
ljk$pval # 0.41 so we failed to reject. we think it is a white noise residual

# Dr woodward model for the airline data
dward = gen.aruma.wge(n = 200, s = 12, d = 1, phi = c (-.36,-.05,-.14,-.11,.04,.09,-.02,.02,.17,.03,-.1,-.38), sn = 61)
dward.1 = artrans.wge(dward, phi.tr = 1)
dward.1.12 = artrans.wge(dward.1, phi.tr = c(0,0,0,0,0,0,0,0,0,0,0,1))

est.dward.1.12 = est.arma.wge(dward.1.12, p =12)
res = est.dward.1.12$res
plotts.sample.wge(res)

lj.dward = ljung.wge(res, p = 12)
lj.dward$pval

##### Global Temperature data
data(hadley)
mean(hadley)
plotts.sample.wge(hadley)

# Model as stationary
aic5.wge(hadley, p = 0:6,q = 0:1)
# go with the p =3 and q = 1
had.est = est.arma.wge(hadley, p =3, q =1)

# plot the residuals with limits to check if it is consistent with white noise.
plotts.sample.wge(had.est$res, arlimits = TRUE)
ljung.wge(had.est$res, p =3, q = 1)
ljung.wge(had.est$res, p =3, q = 1, K = 48)

##### Model hadley as data non stationary
d1.temp = artrans.wge(hadley, phi.tr = 1)
plotts.sample.wge(d1.temp, arlimits = TRUE)    ## Differenced data plotted

# Model the differenced
aic5.wge(d1.temp,p = 0:6, q = 0:1)
d1.temp.est = est.arma.wge(d1.temp, p = 2, q = 1)

# check residuals for white noise check
plotts.sample.wge(d1.temp.est$res, arlimits = TRUE)
ljung.wge(d1.temp.est$res, p = 2, q = 1)
ljung.wge(d1.temp.est$res, p = 2, q = 1, K = 48)

d1.temp.est$avar

# forecasts
# stationary model 
fore.arma.wge(hadley, phi = c(1.27, -.47, .19), theta = .63, n.ahead = 50, limits = FALSE)

# Non stationary
fore.aruma.wge(hadley, d= 1, phi = c(.33,-.18), theta = .7, n.ahead = 50, limits = FALSE)


# Signal plus noise. X_t = a + bt + Zt
library(orcutt)
x = hadley
n = length(x)
t = 1:n
d = lm(x~t)
x.z = x - d$coefficients[1] - d$coefficients[2]*t     # x.z are the residuals from the regression line

ar.z = aic.wge(x.z, p = 0:6) # ar.z$p is the p (aic selects p = 4 here). ar.z$phi is vector of ar.z$p (4) estimated AR coefficients
y.trans = artrans.wge(hadley, phi.tr = ar.z$phi)

# Transform the independent variable
t.trans = artrans.wge(t, phi.tr = ar.z$phi)
plotts.sample.wge(t)

# regress Y hat t on T hat t using OLS
fitco = lm(y.trans~t.trans)
summary(fitco)

# evaluate the residuals( after Cochrane-Orcutt)

plotts.wge(fitco$residuals)
acf(fitco$residuals)
ljung.wge(fit$residuals)

# phis and white noise variance
x.z.est = est.arma.wge(x.z, p = 4) # sig plus noise phis
x.z.est$avar

d  # X_t = d$coefficients[1] + d$coefficients[2]*t + Z_t

# generate signal plus noise with this model
gen.sigplusnoise.wge(160, b0 = -.5257, b1 = .0044, phi = x.z.est$phi, vara = x.z.est$avar) # x.z.est$phi is same as ar.z$phi as well as other parameters that they have in common
# forecast 
fore.sigplusnoise.wge(hadley, max.p = 4, n.ahead = 50, limits = F)




###### Sunspot data
data("sunspot.classic")

plotts.wge(sunspot.classic)

# Box-Jenkins model
acf(sunspot.classic)
pacf(sunspot.classic)

aic5.wge(sunspot.classic)

# Estimate AR(2)
s2 = est.ar.wge(sunspot.classic, p = 2)
plotts.sample.wge(s2$res, arlimits = T)

# check residuals for white noise check
plotts.sample.wge(s2$res, arlimits = TRUE)
ljung.wge(s2$res, p = 2)
ljung.wge(s2$res, p = 2, K = 48)

aic5.wge(sunspot.classic, p = 0:10, q = 0:0)

plotts.sample.wge(sunspot.classic)
factor.wge(sunspot.classic)

