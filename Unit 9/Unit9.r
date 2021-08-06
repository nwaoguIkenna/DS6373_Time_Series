library(tswge)

x = gen.arma.wge(200, phi = c(.3,-.7),theta = .4, vara = 4, sn = 27)
x = x + 37
est.arma.wge(x, p = 2, q = 1)
mean(x)

x = gen.arma.wge(n = 200, phi = c(1.6,-.9), vara = 2, sn = 33)
plotts.wge(x)

#Yule Walker Estimates
x.yw = est.ar.wge(x, p = 2, type = 'yw')
x.yw

#Burg Estimates
x.burg = est.ar.wge(x, p = 2, type = 'burg')
x.burg

# MLE Estimates
x.mle = est.ar.wge(x, p =2, type = 'mle')
x.mle

#ACF and Spectral Density
plotts.sample.wge(x)


# burg, mle, YW only works with AR model estimates.
x = gen.arma.wge(200, phi = c(.3,-.7), vara = 4, sn = 27)
x = x + 37
est.ar.wge(x, p = 2,type = 'burg')

# Generate AR(3)
x = gen.arma.wge(n = 100,phi=c(2.195,-1.994,.796), sn = 53)

#Find MLE Estimates
x.mle = est.ar.wge(x,p=3, type = 'mle')
# check out white noise variancee estimate avar
x.mle
# Note that the residuals are the a_hats found by backcasting
# avar is the mean of these squared residuals (the variance assuming zero mean)
mean(x.mle$res^2)
x.mle$avar

# in class exercise 1
noise = read.csv("~/Downloads/maybewhitenoise2.csv", header = TRUE)
plotts.sample.wge(noise$x)


x = gen.arma.wge(200, phi = c(.967),theta = .477, vara = 0.0139, sn = 27)
x = x + 2.2
est.ar.wge(x, p = 2,type = 'burg')

#AIC Demo: aic.wge
# fig3.16a is a realization from the AR(3) model
data(fig3.16a)
#plotts.sampple.wge provides a look at the data
plotts.sample.wge(fig3.16a)

#lets tswge find the model with the lowest aic
aic.wge(fig3.16a, p = 0:5, q = 0:2, type = 'aic')
mean(fig3.16a)

#Generate Data from AR(3)
x = gen.arma.wge(n = 100,phi = c(1.6,-.9), theta = .8, sn = 67)
x = x + 10
plotts.sample.wge(x)

aic.wge(x,p=0:8,q=0:4)
# picks ARMA(2,1)
est.arma.wge(x,p=2,q=1)
mean(x)

# Using AIC 5 which is the top 5 model.
# generate data
x31 = gen.arma.wge(n=75, phi= c(2.3,-1.92,.56), theta =-.8, sn = 61)
x31 = x31 + 30
plotts.sample.wge(x31)
aic5.wge(x31,p=0:8,q=0:2)
est.arma.wge(x31, p=6,q=1)
#yse AIC BIC type to get a lesser degree model
aic5.wge(x31,p=0:8,q=0:2, type = 'bic')
est.arma.wge(x31, p=3,q=1)
mean(x31)

# in class exercise 2
inf = read.csv("~/Downloads/inflation.csv", header = TRUE)
aic5.wge(inf$Inflation,p=0:8,q=0:2)
aic5.wge(inf$Inflation)
aic.wge(inf$Inflation, type = 'bic')

# Partial autocorrelation pacf
t = gen.arma.wge(1000, phi = c(.3,.5), sn =1)
pacf(t)
acf(t)

# in class exercise 2
pq = read.csv("~/Downloads/armawhatpq1.csv", header = TRUE)
pacf(pq$x)
acf(pq$x)
plotts.sample.wge(pq$x)
aic5.wge(pq$x)
aic.wge(pq$x, type = 'bic')

# Prelive Assignment
gas = read.csv("~/Downloads/texasgasprice.csv", header = TRUE)
length(gas$Price)

# MLE
x.mle = est.ar.wge(gas$Price, p =2, type = 'mle')
x.mle$aic
x.mle$bic

x = gen.arma.wge(205, phi = x.mle$phi, sn = 43)
plotts.sample.wge(gas$Price)
plotts.sample.wge(x)

## Burg
x.burg = est.ar.wge(gas$Price, p =2, type = 'burg')
x.burg$aic
x.burg$bic

x = gen.arma.wge(205, phi = x.burg$phi, sn = 43)
plotts.sample.wge(x)

# Forecast with the 2
mlefc = fore.arma.wge(gas$Price, phi = x.mle$phi, lastn = T, n.ahead = 24)
ASE = mean((mlefc$f - gas$Price[182:205])^2)
ASE

bgfc = fore.arma.wge(gas$Price, phi = x.burg$phi, lastn = T, n.ahead = 24)
ASE = mean((bgfc$f - gas$Price[182:205])^2)
ASE
  