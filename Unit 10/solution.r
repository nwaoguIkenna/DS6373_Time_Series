# Time Series Unit 10 Live Session

plotts.wge(patemp)
parzen.wge(patemp)

fore.patemp.spn = fore.sigplusnoise.wge(patemp,freq = .083, max.p = 4, n.ahead = 24, linear = FALSE, lastn = TRUE)
ASE.spn = mean((patemp[(length(patemp)-23): length(patemp)]-fore.patemp.spn$f)^2)
ASE.spn

fore.patemp.B12 = fore.aruma.wge(patemp,s = 12, n.ahead = 24, lastn = TRUE)
ASE.B12 = mean((patemp[length(patemp)-23: length(patemp)]-fore.patemp.B12$f)^2)
ASE.B12


patemp_B12 = artrans.wge(patemp, phi.tr = c(rep(0,11),1))
aic5.wge(patemp_B12)
patemp.est.AR2 = est.arma.wge(patemp_B12,p = 2)
fore.patemp.B12.AR2 = fore.aruma.wge(patemp,s = 12, phi = patemp.est.AR2$phi, n.ahead = 24, lastn = TRUE)
ASE.B12.AR2 = mean((patemp[length(patemp)-23: length(patemp)]-fore.patemp.B12.AR2$f)^2)
ASE.B12.AR2


y.tr=artrans.wge(patemp,phi.tr=c(1.732,-1))
patemp.est.lamda.AR3 = est.ar.wge(y.tr,p=3)
fore.patemp.L2.AR3 = fore.aruma.wge(patemp,lambda = c(1.732, -1), phi = patemp.est.lamda.AR3$phi, n.ahead = 24, lastn = TRUE)
ASE.L2.AR3 = mean((patemp[length(patemp)-23: length(patemp)]-fore.patemp.L2.AR3$f)^2)
ASE.L2.AR3


fore.patemp.spn = fore.sigplusnoise.wge(patemp,freq = .083, max.p = 4, n.ahead = 24, linear = FALSE, lastn = TRUE)
ASE.spn = mean((patemp[(length(patemp)-23): length(patemp)]-fore.patemp.spn$f)^2)
ASE.spn


#Cochran Orcutt vs OLS vs fore.sigplusnoise.wge()

gen.sigplusnoise.wge(100,b0=3,b1 = 8,phi = .99, vara = 1000,sn = 1)
x = gen.sigplusnoise.wge(100,b0=3,b1 = 8,phi = .99, vara = 1000,sn = 1)
t = seq(1,100,1)
fit = lm(x~t)
summary(fit)
library(orcutt)
cochrane.orcutt(fit)
cfit = cochrane.orcutt(fit)
summary(cfit)
f = fore.sigplusnoise.wge(x,n.ahead = 50)
#compare
fit$coefficients #OLS
cfit$coefficients #CO
f$b0 #fore.sigplusnoise.wge()

