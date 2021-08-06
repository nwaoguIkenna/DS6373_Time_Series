library('tswge')

bondRate = read.csv("~/Documents/datascience/DS6373/10_year_bond_rate_2010-2015.csv", header = TRUE)

data("patemp")
plotts.wge(bondRate$Adj.Close)
plotts.wge(bondRate$Close)
plotts.wge(bondRate$Open)
plotts.wge(bondRate$High)
plotts.wge(bondRate$Low)

length(bondRate$Open)

acf(bondRate$Adj.Close[1:750])
acf(bondRate$Adj.Close[751:1509])

x = as.numeric(paste(bondRate$Adj.Close))
x = x[!is.na(x)]
n=length(x) #n = 1509
nlag=n-1 #n-1
m=mean(x)
v=var(x,na.rm = TRUE)
gamma0=var(x)*(n-1)/n
aut=acf(x,lag.max=nlag) #n-1
sum=0
for  (k in 1:nlag) {sum=sum+(1-k/n)*aut$acf[k+1]*gamma0}
vxbar=2*sum/n+gamma0/n   #note the mult of sum by 2 
vxbar  # estimate of the variance x-bar


# Pre-Live session
energy = read.csv("~/Downloads/energydata_complete.csv", header = TRUE)
plotts.wge(energy$Appliances)

main = acf(energy$Appliances, plot = FALSE)
first = acf(energy$Appliances[1:9867], plot = FALSE)
second = acf(energy$Appliances[9868:19735], plot = FALSE)

plot(main, main = "Kitchen Temperature Time Series ACF")
plot(first, main = ("First Half Kitchen Temperature Time Series ACF"))
plot(second, main = ("Second Half Kitchen Temperature Time Series ACF"))

  x = as.numeric(paste(energy$T1))
x = x[!is.na(x)]
n=length(x) #n = 1509
nlag=n-1 #n-1
m=mean(x)
v=var(x,na.rm = TRUE)
gamma0=var(x)*(n-1)/n
aut=acf(x,lag.max=nlag) #n-1
sum=0
for  (k in 1:nlag) {sum=sum+(1-k/n)*aut$acf[k+1]*gamma0}
vxbar=2*sum/n+gamma0/n   #note the mult of sum by 2 
vxbar  # estimate of the variance x-bar

par(mfrow=c(1,2))


num_realizations = 1000

a = gen.arma.wge(100,.9, plot = FALSE)
plot(seq(1,100,1),a, type = "l")
aa = gen.arma.wge(100,.9, plot = FALSE)
lines(aa)


realizationHolder = matrix(nrow = num_realizations, ncol = 100)

for( i in 1:num_realizations)
{
  a = gen.arma.wge(100,.9, plot = FALSE)
  lines(a)
  realizationHolder[i,]
}

means_t = colMeans(realizationHolder,na.rm = TRUE)
lines(means_t, col = "yellow",lwd = 4) 

x = c(7,8,10,11,9,7,6,4)
x = c(7,8,10,11,9,7,6,4) 
plotts.sample.wge(x)
tsstat = plotts.sample.wge(x) 
ac = acf(x)

parzen.wge(x)

