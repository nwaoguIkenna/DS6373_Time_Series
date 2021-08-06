library(tswge)
library(tidyverse)
library("dplyr") 

# Xt -0.2X_t-1 - 0.48X_t-2 one positive one negative root haave to solve with quadratic eqn
x = gen.arma.wge(200, phi = c(.2, .48))


# Xt - 1.4X_t-1 + .48X_t-2 two positive roots
x = gen.arma.wge(200, phi = c(1.4, -0.48))



# Xt + 1.4X_t-1 + .48X_t-2 two negative roots
x = gen.arma.wge(200, phi = c(-1.4, -0.48))

#X_t - 1.95X_t-1 + 1.85X_t-2 - 0.855X_t-3 = a_t
# factor Table
factor.wge(phi = c(1.95,-1.85,0.855))


#plotting a realization along with true autocorrelations and spectral density
plotts.true.wge(phi = c(1.95,-1.85,0.855))

factor.wge(phi = c(1.59,-0.544,-.511,0.222))
plotts.true.wge(phi = c(1.59,-0.544,-.511,0.222))


# pre-live
factor.wge(phi = c(-.5,-.6))
plotts.true.wge(phi = c(-.5,-.6))


Walmart = read.csv('/home/ikenna/Documents/datascience/DS6373/Unit 3/Walmart.csv',header = TRUE)

# Load the Data
Stor9Item50 = Walmart %>% filter(item == 50 & store == 9)
#Look at and Visualize the data
head(Stor9Item50)
plotts.wge(Stor9Item50$sales)

plotts.sample.wge(Stor9Item50$sales)

Bpart1Low <- butterworth.wge(Stor9Item50$sales,order = 3, type = 'low', cutoff = 0.28)
plotts.sample.wge(Bpart1Low$x.filt)

# 5 moving average smoother
ma = stats::filter(Stor9Item50$sales, rep(1,51))/51
plot(ma,type = 'l')
parzen.wge(ma[!is.na(ma)])

# Check each model to see which fits the data better
# first AR(1)
factor.wge(phi = c(.967))
plotts.true.wge(phi = c(.967))

# AR(6)
factor.wge(phi = c(1.452,-0.453,-0.294,0.175,0.237,-0.154))
plotts.true.wge(phi = c(1.452,-0.453,-0.294,0.175,0.237,-0.154))

#AR(8)
factor.wge(phi = c(1.445,-0.411,-0.038, 0.17, 0.362, -0.245,-0.177, 0.213))
plotts.true.wge(phi = c(1.445,-0.411,-0.038, 0.17, 0.362, -0.245,-0.177, 0.213))


#AR(9)
factor.wge(phi = c(1.384,-0.359,-0.309,.063,.317,-.140,-.0587,-0.199,0.2877))
plotts.true.wge(phi = c(1.384,-0.359,-0.309,.063,.317,-.140,-.0587,-0.199,0.2877))


factor.wge(phi = c(0.1516,))
plotts.true.wge(phi = c(1.384,-0.359,-0.309,.063,.317,-.140,-.0587,-0.199,0.2877))

