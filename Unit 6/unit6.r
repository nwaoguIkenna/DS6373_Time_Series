library(tswge)

# Randomly generating signal plus noise
gen.sigplusnoise.wge(100, b0 = 2, b1 = 4, vara = 100)
gen.sigplusnoise.wge(100, b0 = 0, b1 = 0, vara = 10)
gen.sigplusnoise.wge(100, b0 = 0, b1 = 0, phi = 0.975, vara = 10)

# periodic Signal
gen.sigplusnoise.wge(100, coef = c(5,0), freq = c(.1,0), psi = c(0,0.25),phi = 0.975, vara = 10)

#AR(4) from the slides
parms = mult.wge(c(.975), c(.2, -.45),c(-.53))
parms$model.coef
gen.arma.wge(160, phi = parms$model.coef, vara = 1)

# (1-B)X_t = a_t
x = gen.arima.wge(200, phi = 0, var = 1, d = 1, sn =31)
acf(x)

# The following command differences the data in x
y = artrans.wge(x, phi.tr = 1)

#This simply means that y(i) = x(i) -x(i-1)
# y has n-1 becuse x(1) has no x(0) before it.
# example

x = c(1,3,6,10,25)
y = artrans.wge(x, phi.tr = 1)
y # shows the 4 differences


# Arima with 2
data = gen.arima.wge(200, phi = c(1.5,-.8), d = 2, theta = c(-.8), sn = 31)

FirstDif = artrans.wge(data,phi.tr = 1)

SecondDif = artrans.wge(FirstDif, phi.tr = 1)

aic5.wge(SecondDif) 




### ARUMA Models (seasonal)
# (1-B^4) = a_t
x1 = gen.aruma.wge(n=80, s = 4, sn = 6)
plotts.sample.wge(x1)

#(1-B^4) with ARMA(2,1)
x2 = gen.aruma.wge(n = 80, phi =c(1,-.6), s = 4, theta = -.5, sn = 6, d = 1)
plotts.sample.wge(x2)

factor.wge(phi = c(1,-.6))
factor.wge(phi = -.5)

#(1-B^12) with ARMA(2,1)
x3 = gen.aruma.wge(n = 180, phi =c(1,-.6), s = 12, theta = -.5, sn = 6)
plotts.sample.wge(x3, lag.max = 48)


x2 = gen.aruma.wge(n = 180, phi =c(.6,-.94), s = 7, theta = -.3, sn = 19)
plotts.sample.wge(x2)

factor.wge(phi = c(.6,-.94))
factor.wge(phi = -.3)

### Stationarize the quaterly data (1-b^4)X_t = a_t
x = gen.aruma.wge(n=80, s=4, sn = 81)
dif = artrans.wge(x,c(0,0,0,1)) # Take out the (1-B^4)
aic5.wge(dif)

## (1-.4B - .6B^2 + .74B^3)(1-B^12)X_t = (1 + .7B)a_t
x = gen.aruma.wge(n = 80, phi = c(.4,.6,-.74), theta = -.7, s =12, sn = 31)
dif = artrans.wge(x,c(rep(0,11),1)) # take out the (1-B^12)
aic5.wge(dif)


x = gen.aruma.wge(n = 500, phi = c(.6,-.8), theta = c(.3,-.7), s =12, sn = 37)
dif = artrans.wge(x,c(rep(0,11),1)) # take out the (1-B^12)
dif = artrans.wge(dif,c(rep(0,11),1)) # take out the (1-B^12)
aic5.wge(dif)

#factor ARUMA models
#factor.wge(phi = c(0,0,0,1)) or factor.wge(phi = c(rep(0,11),1)) for (1-B^12)

# (.2B - .4B^2 -.49B^3 -B^12 -.2B^13 + .4B^14 + .49B^15)
factor.wge(c(-.2, .4, .49, rep(0,5), 1, .2, -.4, -.49))

factor.wge(c(-.5, .2, 0, -1, .5, -.2))

factor.wge(c(-.3, 1.2, .4,0,.5, rep(0,6), 1, .3, -1.2, -.4))

factor.wge(c(-.3, 1.2, .4,0,.5, rep(0,6), 1, .3, -1.2, -.4))




#Pre-live
energy = read.csv("~/Documents/datascience/DS6373/Dataset/Appliance_Energy/energydata_complete.csv", header = TRUE)
plotts.wge(energy$Appliances)

aic5.wge(energy$Appliances)



factor.wge(c(rep(0,11),1))

factor.wge(c(0.6996, 0.2599, 0.0079, -0.0646, 0.1381, -0.0953, 0.0235, -0.0969, 0.1770, -0.1191, 0.1030, 0.7754, -0.4590, -0.4099, 0.0501))








