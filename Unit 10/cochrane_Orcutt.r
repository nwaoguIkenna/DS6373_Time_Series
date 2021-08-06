library(orcutt)

# signal plus noise
x = gen.sigplusnoise.wge(100, b0 = 0, b1= 0, phi= .99, sn = 28)
t = seq(1,100,1)
df = data.frame(x = x, t= t)
fit = lm(x~t, data = df)
summary(fit)

cfit = cochrane.orcutt(fit)
summary(cfit)


#Exercise

delay = read.csv("~/Downloads/swadelay.csv", header = TRUE)
delay = delay$arr_delay
t = seq(1,length(delay),1)
df = data.frame(x = delay, t= t)
fit = lm(x~t, data = df)
summary(fit)

cfit = cochrane.orcutt(fit)
summary(cfit)
