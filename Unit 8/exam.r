library(tswge)
x = gen.sigplusnoise.wge(100, b0 = 2, b1 = 4, vara = 100)
y =gen.sigplusnoise.wge(100, coef = c(5,0), freq = c(.1,0), psi = c(0,0.25),phi = 0.975, vara = 10)
plotts.sample.wge(y)
factor.wge(x)
plotts.true.wge(n = 200, phi = -0.9, theta = 1.9)

d = plotts.true.wge(n = 200, theta = c(0.95,-.9,.855))

factor.wge( phi = c(-2,1.76,-1.6,0.77))

plotts.wge((d$data)+25)
d$acv

# Aruma
x = gen.aruma.wge(n=100,d=1, phi = 0, theta = 0, s = 0)
plotts.sample.wge(x)

fore.aruma.wge(x,d=1, phi = .6, theta = 1,n.ahead = 25, s=12)

fore.aruma.wge(x,d=1, phi = 2, theta = 0,n.ahead = 5, s=0)

# signal Plus Noise
x = gen.sigplusnoise.wge(100, b0 = 2, b1 = 4, vara = 100)
y =gen.sigplusnoise.wge(100, psi = c(0,0.25), vara = 10)
plotts.sample.wge(x)
plotts.sample.wge(y)
fore.sigplusnoise.wge(x,n.ahead = 5,last  = T, limits = F)
fore.sigplusnoise.wge(y,n.ahead = 50,last  = T, limits = F)


x = c(1,5,10,9,4,6,12)
var(x)*7/8
var(x)


psi.weights.wge(phi = c(.2,.08), lag.max = 5)

factor.wge(phi = c(.2,.08))


x = gen.arma.wge(100, phi = c(.2,.08))
plotts.true.wge(theta = c(.2,.12))
fore.sigplusnoise.wge(x,n.ahead = 10,last  = T, limits = F)

data("global.temp")
gt = global.temp[100:length(global.temp)]
plotts.sample.wge(gt)
fore.sigplusnoise.wge(gt,n.ahead = 10,last  = T, limits = F)
fore.arma.wge(gt, phi = 0, theta = 0,n.ahead = 15)

x = gen.sigplusnoise.wge(100, b0 = 0, b1 = 0, vara = 10)
fore.sigplusnoise.wge(x,n.ahead = 10,last  = T, limits = F)




#AR(2)
x2 = gen.arma.wge(200, phi = c(1.95,-1.85,0.855))
x = gen.arma.wge(200, phi = c(1.4, -0.48))

x = gen.arma.wge(200, phi = c(-1.4, -0.48))


fore.arma.wge(x2, phi = c(1.95,-1.85,0.855),n.ahead = 40, limits = FALSE)
fore.aruma.wge(x2, phi = c(1.4, -0.48),n.ahead = 40, limits = FALSE)

fore.aruma.wge(x2,n.ahead = 10, limits = FALSE,d=1)

factor.wge(phi = c(1.4, -0.48))


factor.wge(phi = c(.9,-.2,-.94,-.6))

factor.wge(phi = c(-.1))

gen.sigplusnoise.wge(200, phi = c(.9,-.2,-.94,-.6), theta = -.1)

psi.weights.wge(phi = c(.9,-.2,-.94,-.6), theta = -.1, lag.max = 5)




mid = read.csv("~/Documents/datascience/DS6373/Project/MidtermSummer2020InClass.csv", header = TRUE)
plotts.sample.wge(mid$x)
length(mid$x)
aic5.wge(mid$x)


data = gen.arima.wge(200, phi = c(-.8), d = ,  sn = 31)
x2 = gen.arma.wge(200, phi =c(1.17, -.54, .27, -.31, .15), theta = c(-0.5844596,  0.3836931))


length(mid$x)
midx_ARMA_5_2 = fore.arma.wge(mid$x, phi =c(1.17, -.54, .27, -.31, .15), theta = c(-0.58,  0.38), n.ahead = 30, theta = 0)
ASE = mean((mid$x_ARMA_5_2 - mid$x[488:500])^2)
ASE



length(mid$x)
midx_ARMA_5_2 = fore.arma.wge(mid$x, phi =c(1.17, -.54, .27, -.31, .15),theta = c(-0.58,  0.38), n.ahead = 24)
ASE = mean((midx_ARMA_5_2$f - mid$x[477:500])^2)
ASE

midx_ARMA_5_2 = fore.arma.wge(mid$x[450:500], phi =c(1.17, -.54, .27, -.31, .15),theta = c(-0.58,  0.38), n.ahead = 24,last = T)
ASE = mean((midx_ARMA_5_2$f - mid$x[477:500])^2)
ASE

midx_AR1 = fore.arma.wge(mid$x, phi = .8, n.ahead = 24)
ASE = mean((midx_AR1$f - mid$x[477:500])^2)
ASE

midx_AR1 = fore.arma.wge(mid$x[450:500], phi = .8, n.ahead = 24, last = T)
ASE = mean((midx_AR1$f - mid$x[477:500])^2)
ASE

x2 = gen.aruma.wge(200, phi =c(0.6996, 0.2599, 0.0079, -0.0646, 0.1381, -0.0953, 0.0235, -0.0969, 0.1770, -0.1191, 0.1030, 0.7754, -0.4590, -0.4099, 0.0501), theta = c(-0.5844596,  0.3836931, 0.8), s = 12, d =1)
plotts.sample.wge(x2, lag.max = 48)
