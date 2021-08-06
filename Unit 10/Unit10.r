library(tseries)

unit9 = read.csv("~/Downloads/Unit9_2.csv", header = TRUE)
inclass = unit9$x

aic5.wge(inclass,p=0:5)
x.mle = est.ar.wge(inclass, p =1, type = 'mle')
est = est.arma.wge(inclass, p =4, q = 1)

p = pacf(inclass)

#(1-0.861B)Xt
length(inclass)


### Attempting to model SPG data

SPG = read.csv("~/Downloads/SPG.csv", header = TRUE)
close = SPG$Close
high = SPG$High
plotts.sample.wge(close)
plotts.sample.wge(high)

est.arma.wge(close)
aic5.wge(close,p=0:13,q=0:2)
aic5.wge(high,p=0:13,q=0:2)

x.mle = est.arma.wge(close, p =7,q=1)

fore.arma.wge(close, phi = x.mle$phi,theta = x.mle$theta, lastn = T, n.ahead = 3)

#tail(SPG,10)

close.diff1 = artrans.wge(close, phi.tr = 1)
aic5.wge(close.diff1,p=0:13,q=0:2)
x.mle = est.arma.wge(close.diff1, p =6,q=1)

fore.aruma.wge()

fore.aruma.wge(close, phi=x.mle$phi,theta = x.mle$theta,  d = 1, n.ahead = 4, lastn = T, limits = F)

## Exercise
Bond = read.csv("~/Downloads/10_year_bond_rate_2010-2015.csv", header = TRUE)
adjustClose = Bond$Adj.Close
plotts.sample.wge(adjustClose)

est.arma.wge(adjustClose)
est.ar.wge(adjustClose, p = 1, type = 'burg')

## Difference the data
adjustClose.diff1 = artrans.wge(adjustClose, phi.tr = 1)

##### estimate which model fits the differenced data using aic5
aic5.wge(adjustClose.diff1)  ### both p and q are 0 for top model which means that this is white noise
aic5.wge(adjustClose) 

est.ar.wge(close, p = 6, type = 'burg')


zero = read.csv("~/Downloads/zero_one_or_tworootsofone.csv", header = TRUE)

zero = zero$x

est.ar.wge(zero, p = 10, type = 'burg')

# Dickey-fuller test
x = gen.arma.wge(200,phi = c(.99999), sn = 5)
adf.test(x)
x = gen.signalplusnoise.wge(100, b0 = 0, b1= 0, phi= .95, sn = 28)
delay = read.csv("~/Downloads/swadelay.csv", header = TRUE)

factor.wge(delay$arr_delay)

est.ar.wge(delay$arr_delay, p = 15, type = 'mle')
plotts.sample.wge(delay$arr_delay)

# signal plus noise
x = gen.sigplusnoise.wge(100, b0 = 0, b1= 0, phi= .95, sn = 28)
t = seq(1,100,1)
df = data.frame(x = x, t= t)
fit = lm(x~t, data = df)
summary(fit)
