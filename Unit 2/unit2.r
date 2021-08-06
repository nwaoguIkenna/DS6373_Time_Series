library('tswge')
t = seq(1,100, length = 100)
y1 = sin(2*pi *0.025*t)
plot(t,y1,type = 'l')
y2 = sin(2*pi*.1*t+1)
plot(t,y2,type = 'l')
y3 = sin(2*pi*.15*t+2.5)
plot(t,y3,type = 'l')
ysum = y1 + y2 +y3
plot(t,ysum, type = 'l')

l1 = sin(2*pi*.8*t + 5)
plot(t,l1,type = 'l')
l2 = .3*t + 10
plot(t,l2,type = 'l')
l3 = sin(2*pi*(.08)*t + 5) + .3*t + 10
plot(t,l3,type = 'l')


gen.arma.wge(n = 1000, phi = .99, sn = 8)
gen.arma.wge(n = 1000, phi = 0, sn = 8)


# Load data from tswge
data(patemp)
plotts.dwt.wge(patemp)
parzen.wge(patemp)
data("airlog")
plotts.dwt.wge(airlog)
parzen.wge(airlog)
parzen.wge(airlog, trunc =  70)
data(bat)
plotts.dwt.wge(bat)
parzen.wge(bat)
data("sunspot.classic")

parzen.wge(sunspot.classic)
plotts.sample.wge(patemp)
plotts.sample.wge(airlog)
plotts.sample.wge(bat)
plotts.sample.wge(sunspot.classic)
plot(ysum,trunc = 70) # data generated from above
parzen.wge(ysum)

data("llynx")
parzen.wge(llynx)
plotts.sample.wge(llynx)



# Pre-Live session
energy = read.csv("~/Documents/datascience/DS6373/Dataset/Appliance_Energy/energydata_complete.csv", header = TRUE)
plotts.wge(energy$Appliances)
parzen.wge(energy$Appliances,trunc =  150)
plotts.sample.wge(energy$Appliances, trunc =  70)
