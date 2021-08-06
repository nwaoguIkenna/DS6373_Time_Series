library(tswge)
library(tidyverse)
library("dplyr") 

#gen.arma.wge() is used for only stationary models
gen.arma.wge(n=200)
gen.arma.wge(n=200, sn = 1)

gen.arma.wge(n=200, phi = 0.95)
gen.arma.wge(n=200, phi = 0.7)
gen.arma.wge(n=200, phi = -0.95)
gen.arma.wge(n=200, phi = -0.7)

plotts.true.wge(phi=.95)
plotts.true.wge(phi=-.95)

x = gen.arma.wge(n=200, phi = 0.95)
plotts.sample.wge(x)

# gen.arima.wge() is uded to generate non stationary arima models like phi = 1 case
gen.arma.wge(n=100, phi = c(.999999999)) #nearly nonstationary
gen.arma.wge(n=100,phi = 1) # error because this can only generate stationary models abs(phi) supposed to be less than 1

gen.arima.wge(n=100, d=1) # arima case similar to phi = 0.999999
gen.arima.wge(n=50, phi = 1.1) # error because this function only generates arima models with phi = 1

#nonstationary model with phi = 1.1
n = 50
x = rep(0,50)
a = rnorm(n)
x[1:50] = 0
for(k in 2:n){x[k] = 1.1*x[k-1]+a[k]} # AR(1) formula using random generated values from normal distribution with mean of 0 and std of 1
plotts.wge(x)

#prelive
#Walmart = read.csv(file.choose(),header = TRUE)
Walmart = read.csv('/home/ikenna/Documents/datascience/DS6373/Unit 3/Walmart.csv',header = TRUE)

summary(Walmart)
Stor8Item1 = Walmart %>% filter(item == 1 & store == 8)

#Use item 1. use the mean of all 10 stores
Item1 = Walmart %>% filter(item == 1)

newData <- Walmart %>% group_by(date)%>% summarise(meanSales = mean(sales))
plotts.wge(newData$meanSales)

par(mfrow=c(1,2))
ma5 = stats::filter(newData$meanSales, rep(1,5))/5
plot(ma5,type = 'l')
parzen.wge(ma5[!is.na(ma5)])

ma51 = stats::filter(newData$meanSales, rep(1,51))/51
plot(ma51,type = 'l')
parzen.wge(ma51[!is.na(ma51)])


#Trying to loop through the filter function.

n = length(newData$meanSales)
x = newData$meanSales
count = 1
ma1 = 5
ma2 = 51
x[-1][1]
x[1][1]
x = head(x,50)
n = length(x)




new = c()
# Point moving avg filter 
m1 = (ma1-1)/2
m2 = (ma2-1)/2
acc = c()

ma <- for(i in m1:n){ x = x[i] #+ sum(acc)
        for (j in count:i) { count = count + 1 
                    acc = x[1][i-j] }}
ma = filter(newData, rep(1,5))/5
#require(smooth)
#require(Mcomp)
#sma(x, h=18, silent=FALSE)

x <- tibble::rowid_to_column(newData, "ID")
x <- x %>% select('ID','meanSales')

x$ID <- as.factor(x$ID)
ma = stats::filter(x$meanSales, rep(1,5))/5
plot(ma,type = 'l')
parzen.wge(ma[!is.na(ma)])



plotts.true.wge(100, 0.7) # 0.7 is the phi value

# butterworth smoothing
Part1 = read.csv('/home/ikenna/Documents/datascience/DS6373/Unit 3/Unit3BOut1Part1.csv',header = TRUE)

plotts.sample.wge(Part1$x)

Bpart1Low <- butterworth.wge(Part1$x,order = 3, type = 'low', cutoff = 0.28)
plotts.sample.wge(Bpart1Low$x.filt)

Bpart1High <- butterworth.wge(Part1$x,order = 1, type = 'high', cutoff = 0.5)
plotts.sample.wge(Bpart1High$x.filt)

Bpart1Pass <- butterworth.wge(Part1$x,order = 1, type = 'pass', cutoff = c = (0.2,0.5))
plotts.sample.wge(Bpart1Pass$x.filt)

Part2 = read.csv('/home/ikenna/Documents/datascience/DS6373/Unit 3/Unit3BOut1Part2.csv',header = TRUE)

