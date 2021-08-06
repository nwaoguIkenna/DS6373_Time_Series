library(tswge)
library(tidyverse)
library("dplyr") 

# MA(1)
x = gen.arma.wge(100, theta = -0.99)
x

gen.arma.wge(100, theta = 0.99,sn=5)
plotts.true.wge(theta=c(.99))

# MA(2)
x = gen.arma.wge(200, theta = c(0.9, -0.4))
plotts.true.wge(theta=c(0.9,-.4))

plotts.true.wge(theta=c(-0.1,.3))


plotts.true.wge(phi = c(-.5,-.6))   # This is for AR model
plotts.true.wge(theta = c(-.5,-.6))

### Check if invertibility
#MA(2): X_t = a_t - 1.6a_t-1 + .9a_t-2       Factors work with both AR and MA
factor.wge(phi=c(1.6,-.9))
factor.wge(phi=c(1.6,.9))

factor.wge(phi=c(-.1,-.82,.16))


# ARMA Examples
# AR
plotts.true.wge(phi = c(.3,.9,.1,-.8075)) 
factor.wge(c(.3,.9,.1,-.8075))

# MA
plotts.true.wge(theta = c(-.9,-.8,-.72)) 
factor.wge(c(-.9,-.8,-.72))

#ARMA
plotts.true.wge(phi = c(.3,.9,.1,-.8075), theta = c(-.9,-.8,-.72)) 

# SWA Delay Analysis

SWA = read.csv(file.choose(),header =TRUE)
plotts.wge(SWA$arr_delay)
plotts.sample.wge(SWA$arr_delay)
aic.wge(SWA$arr_delay, p = 4, q = 1)$value
aic5.wge(SWA$arr_delay)

# psi-weights for simple MA(1) model X(t) = (1-.8B)a(t)
psi.weights.wge(theta = .8, lag.max = 5)
# psi-weights for simple AR(1) model (1-.8B)X(t)=a(t)
psi.weights.wge(phi = .8, lag.max = 5)  # note that psi(j) = .8j
# psi-weights for ARMA(2,1) model (1-1.2B=.6B2)X(t) = (1-.5B)a(t)
psi.weights.wge(phi = c(91.2,-.6), theta = c(.5),lag.max = 5)

psi.weights.wge(phi = c(1.95,-1.9), lag.max = 5) 


# Pre-Live session
energy = read.csv("~/Documents/datascience/DS6373/Dataset/Appliance_Energy/energydata_complete.csv", header = TRUE)
aic5.wge(energy$Appliances)

Walmart = read.csv('/home/ikenna/Documents/datascience/DS6373/Unit 3/Walmart.csv',header = TRUE)
aic5.wge(Walmart$sales)

SWA = read.csv(file.choose(),header =TRUE)
aic5.wge(SWA$arr_cancelled)

x = gen.arma.wge(200,phi = c(.3,.9,.1,-.8075), theta = c(-.9,-.8,-.72)) 
aic.wge(x, p = 4, q = 2)$value
aic5.wge(x)

aic5.wge(x, p = 0:5, q = 0:2, type = 'aic')