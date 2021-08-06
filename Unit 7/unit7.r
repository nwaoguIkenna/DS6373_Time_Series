library(tswge)

#AR(1) when phi is Positive
data(fig6.1nf)
plotts.wge(fig6.1nf)

fore.arma.wge(fig6.1nf, phi = .8, n.ahead = 20, plot = TRUE, limits = FALSE)

#Ar(1) with phi Negative
x1 = gen.arma.wge(100, phi = -.8)
fore.arma.wge(fig6.1nf, phi = -.8, n.ahead = 20, plot = TRUE, limits = FALSE)

#AR(2)
x2 = gen.arma.wge(n=75, phi = c(1.6,-.8),sn =24)
x2 = x2 + 25
plotts.wge(x2)

fore.arma.wge(x2, phi = c(1.6,-.8),n.ahead = 20, limits = FALSE)
fore.arma.wge(x2, phi = c(1.6,-.8),n.ahead = 40, limits = FALSE)

# Add the MA part for forecasting
## ARMA(2,1)
x2 = gen.arma.wge(n=75, phi = c(1.6,-.8),theta = -.9,sn =24)
fore.arma.wge(x2, phi = c(1.6,-.8),theta = -.9,n.ahead = 20, limits = FALSE)
x2 = gen.arma.wge(n=75, phi = c(1.6,-.8),sn =24)
fore.arma.wge(x2, phi = c(1.6,-.8),n.ahead = 20, limits = FALSE)
plotts.sample.wge(x2)
factor.wge(phi = c(1.6,-.8))
# wnv is the white noise variance

## ARMA(1,1)
fore.arma.wge(x2, phi = c(-.8),theta = -.9,n.ahead = 20, limits = FALSE)

# CAnadian Lynx Data

data(llynx)
plotts.wge(llynx)

# ARMA(4,0)
fore.arma.wge(llynx, phi = c(1.3,-.7,.1,-.2),n.ahead = 20, limits = FALSE)

# ARMA(4,1)
fore.arma.wge(llynx, phi = c(1.3,-.7,.1,-.2), theta = -.6, n.ahead = 20, limits = FALSE)

# ARMA(11,0)
fore.arma.wge(llynx, phi = c(1.167,-.5446,.2662,-.3094,0.154,-.146,.0569,-.0294,.1346,.2021,-.3394),n.ahead = 20, limits = FALSE)

# Generating psi weights..... psi weights are used to calculate the probability limits for the forecast prediction(confidence intervals)
psi.weights.wge(phi=c(1.2,-.6), theta=.5, lag.max = 5)



# Calculating Probability limits for ARMA forecast
# AR(1)
data(fig6.1nf)
fore.arma.wge(fig6.1nf, phi = 0.8, n.ahead = 20, plot = T, limits = F)
fore.arma.wge(fig6.1nf, phi = 0.8, n.ahead = 20, plot = T, limits = T)

#ARMA(2,1)
data(fig6.2nf)
fore.arma.wge(fig6.1nf, phi =c(1.2,-.6), theta = .5, n.ahead = 20, plot = T, limits = F)

fore.arma.wge(fig6.1nf, phi =c(1.2,-.6), theta = .5, n.ahead = 20, plot = T, limits = T)

#Canadian Lynx: AR(4) ASE
data(llynx)
plotts.wge(llynx)
lynxF_AR4 = fore.arma.wge(llynx, phi =c(1.3,-.7,0.1,-.2), n.ahead = 30, limits = F, lastn = T)
ASE = mean((lynxF_AR4$f - llynx[85:114])^2)
ASE

#Canadian Lynx: AR(11) ASE
lynxF_AR11 = fore.arma.wge(llynx, phi =c(1.17, -.54, .27, -.31, .15, -.15, .06, -.03,.13, .20, -.34), n.ahead = 30, limits = F, lastn = T)
ASE = mean((lynxF_AR11$f - llynx[85:114])^2)
ASE

# forcast simple ARIMA model
# ARIMA(0,1,0)
x = gen.aruma.wge(n=50, phi = 0.8, d = 1, sn = 15)
fore.aruma.wge(x, d = 1, n.ahead = 20, limits = F)

# ARIMA(1,1,0)
x = gen.aruma.wge(n=50, phi = 0.8, d = 1, sn = 15)
fore.aruma.wge(x,phi = 0.8,  d = 1, n.ahead = 20, limits = F)

# ARIMA(0,2,0)
x = gen.aruma.wge(n=50, phi = 0.8, d = 1, sn = 15)
fore.aruma.wge(x, d = 2, n.ahead = 20, limits = F)

# forecast simple ARUMA model
# (1-B^4)
x= gen.aruma.wge(n=20,s=4,sn=6)
fore.aruma.wge(x,s=4,n.ahead= 8, lastn = F, plot = T, limits = F)

x= gen.aruma.wge(n=20,s=4,sn=6)
fore.aruma.wge(x,s=4,n.ahead= 8, lastn = T, plot = T, limits = F)

# (1- .8B)(1-B^4)
x= gen.aruma.wge(n=20,phi = .8, s=4, sn=6)
fore.aruma.wge(x,s=4, phi= 0.8, n.ahead= 8, lastn = F, plot = T, limits = F)

# Signal plus noise
x = gen.sigplusnoise.wge(100, coef = c(5,0), freq = c(.1,0), psi = c(0,0.25),phi = 0.975, vara = 10)   # from uniit 6
x= gen.sigplusnoise.wge(50, b0 = 10, b1 = .2, phi = c(.8,-.6))
fore.sigplusnoise.wge(x, linear = T, freq = 0, max.p = 5, n.ahead = 10, lastn = F, plot = T, limits = F)


#Pre-live
energy = read.csv("~/Documents/datascience/DS6373/Dataset/Appliance_Energy/energydata_complete.csv", header = TRUE)

aic5.wge(energy$Appliances)
energy_Low <- butterworth.wge(energy$Appliances,order = 3, type = 'low', cutoff = .05)
fore.aruma.wge(energy$Appliances,s=4, d = 1, n.ahead= 6, limits = T)
fore.sigplusnoise.wge(c,linear = T, freq = 0, max.p = 10, n.ahead = 100, lastn = F, plot = T, limits = T)

# selecting a portion of the data
c = energy$Appliances[c(19000:19735)]
aic5.wge(c)
plotts.sample.wge(c)
fore.aruma.wge(c,s=12, d = 1, n.ahead= 6, limits = T)


psi.weights.wge(phi=c(1.7,-.72), lag.max = 5)
x = gen.aruma.wge(n=50, phi=c(1.7,-.72), sn = 15)
fore.arma.wge(x, phi=c(1.7,-.72),n.ahead = 3, limits = T)


lynxF_AR11 = fore.arma.wge(llynx, phi =c(1.17, -.54, .27, -.31, .15, -.15, .06, -.03,.13, .20, -.34), n.ahead = 30, limits = F, lastn = T)
ASE = mean((lynxF_AR11$f - llynx[85:114])^2)
ASE

amtrak = read.csv(file.choose(),header = TRUE)
Amt1 = fore.aruma.wge(amtrak$Ridership,phi = c(0.5511, 0.1680, -0.0145, 0.0651, 0.1388, -0.2966, 0.1539, 0.1270, -0.1815, 0.0364, 0.1456, 0.6287, -0.3832, -0.0199, -0.1679),theta = 0,s  = 0,d  = 0,n.ahead = 30)
ASE = mean((Amt1$f - amtrak$Ridership[150:159])^2)
ASE

length(amtrak$Ridership)

#Model 2  
Amt1 = fore.aruma.wge(amtrak$Ridership,phi = c(-0.02709541,  0.74213105),theta = c(-0.5844596,  0.3836931),s = 12,d = 0,n.ahead = 30)
ASE = mean((Amt1$f - amtrak$Ridership[150:159])^2)
ASE

#Model 3
Amt1 = fore.aruma.wge(amtrak$Ridership, phi = 0.306943,theta = 0.7431719,s = 12,d = 1,n.ahead = 30)
ASE = mean((Amt1$f - amtrak$Ridership[150:159])^2)
ASE


############################# Live Session code #################################
#(1-.9B)(1-.8B)Xt = at 
#(1-1.7B+.72B^2)Xt = at 
#Phi_1 = 1.7 Phi_2 = -.72 

s = c(5,8,9,8,7,6,4,3) 

#double check psi weights 
psi.weights.wge(phi = c(1.7, -.72), lag = 5) 

#AR(2) 

fit = fore.arma.wge(s,phi = c(1.7, -.72), n.ahead = 3) 

#forecasts for l = 1,2 and 3 
fit$f 

#Conf limits for l = 3 
# ll: 1.75415 - 1.96*sqrt(fit$wnv)*sqrt(1+1.7^2+ 2.17^2) 
# ul: 1.75415 + 1.96*sqrt(fit$wnv)*sqrt(1+1.7^2+ 2.17^2) 
fit$ll[3] 
fit$ul[3] 

#sigma_at_hat 
sqrt(fit$wnv) 

#Calc sigma_at_hat 
wnv = 1/(8-2) * sum(fit$resid[3:8]^2) 