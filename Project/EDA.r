library(dplyr)
library(tswge)
library(ggplot2)


usCovid = read.csv("~/Documents/datascience/DS6373/Project/states.csv", header = TRUE)
usCovid

#Convert all the Nas to 0
usCovidClean <- usCovid %>% replace(is.na(.), 0)
#usCovidClean %>% dplyr::filter(state == 'AK')

DataUsing <- usCovidClean %>% select(date,'positive',negative,hospitalizedCurrently,onVentilatorCurrently,recovered,death,deathConfirmed,death,totalTestResults, inIcuCurrently, pending)
summary(DataUsing)

# We ignored using only data quality that scored at C because we have 1100 non labelled which is a lot to ignore. We only group by date
dataGrouped <-DataUsing %>% dplyr::group_by(date) %>% dplyr::group_by(date) %>% summarise_each(tibble::lst(sum))
#ddply(usCovid, .(date), summarise, medABV=sum(positive))
dataGrouped<- dataGrouped %>% filter(date > 20200229)   # Start february 29

# select start date 
datalagged = as.data.frame(lapply(dataGrouped[,2:length(dataGrouped)], diff, lag=1))
datalagged <- datalagged %>% mutate(positive_perc = (positive_sum/totalTestResults_sum)*100)
datalagged <- datalagged %>% replace(is.na(.), 0)




dataGrouped<- as.data.frame(dataGrouped)
dataGrouped$date <- as.factor(dataGrouped$date)
dataGrouped %>% ggplot(aes( y=hospitalizedCurrently_sum, x=date,colour=hospitalizedCurrently_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
dataGrouped %>% ggplot(aes( y=onVentilatorCurrently_sum, x=date,colour=onVentilatorCurrently_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
dataGrouped %>% ggplot(aes( y=pending_sum, x=date,colour=pending_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
dataGrouped %>% ggplot(aes( y=pending_sum, x=date,colour=pending_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
dataGrouped %>% ggplot(aes( y=inIcuCurrently_sum, x=date,colour=inIcuCurrently_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
dataGrouped %>% ggplot(aes( y=onVentilatorCurrently_sum, x=date,colour=onVentilatorCurrently_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
#dataGrouped %>% ggplot(aes( y=recovered_sum, x=date,colour=recovered_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))


datalagged$date <- as.factor(dataGrouped$date[1:1-length(dataGrouped$date)])
datalagged<- as.data.frame(datalagged)
datalagged$date <- as.factor(dataGrouped$date[1:1-length(dataGrouped$date)])
datalagged %>% ggplot(aes( y=positive_sum, x=date,colour=positive_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
datalagged %>% ggplot(aes( y=negative_sum, x=date, colour=negative_sum)) + geom_bar(stat="identity") + scale_colour_gradientn(colours=rainbow(5))
datalagged %>% ggplot(aes( y=positive_perc, x=date,colour=positive_perc)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
datalagged %>% ggplot(aes( x=positive_sum, y=negative_sum, colour=positive_sum)) + geom_point() + scale_colour_gradientn(colours=rainbow(5))
datalagged %>% ggplot(aes( y=totalTestResults_sum, x=date, colour=totalTestResults_sum)) + geom_bar(stat = 'identity') + scale_colour_gradientn(colours=rainbow(5))
datalagged %>% ggplot(aes( y=recovered_sum, x=date, colour=recovered_sum)) + geom_bar(stat = 'identity') + scale_colour_gradientn(colours=rainbow(5))
datalagged %>% ggplot(aes( y=hospitalizedCurrently_sum, x=date,colour=hospitalizedCurrently_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
datalagged %>% ggplot(aes( y=onVentilatorCurrently_sum, x=date,colour=onVentilatorCurrently_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))
datalagged %>% ggplot(aes( y=death_sum, x=date,colour=death_sum)) + geom_bar(stat="identity") + guides(fill = T) + scale_colour_gradientn(colours=rainbow(10))


#geom_boxplot()
#geom_density()
#geom_histogram()
#geom_point()

# Time Series analysis
dailyRates <- datalagged$positive_sum
plotts.sample.wge(dailyRates)
# Looking at the plot it looks stationary because the realization is wandering. The spectral density has a peak at 0. The sample correlation is steadily fading/damping as the lag grows.
# There is also another small peak at around 0.14. We will overfit with aic to see what the p and q for the realization would be using aic with type bic. AIC picks an AR(10). We looked at 
# acquired factor table and we didn't see any factors that were consitent with aruma or arima models so we proceed with the AR(10) model. 

#factor.wge(dailyRates)

aic.dailyRates = aic.wge(dailyRates, p = 0:15, q = 0:1, type='bic')

est.dailyRates = est.arma.wge(dailyRates,p=aic.dailyRates$p)    # we conclude it is an AR model 10 with mle

# We conclude that the data is Stationary.

# Check residual is noice by using ljung-box test and acf
acf(est.dailyRates$res)  # This is consistent with white noise
lj = ljung.wge(est.dailyRates$res, p = 10)
lj$pval # 0.75 so we failed to reject. There is evidence that this might be white noise

ljk = ljung.wge(est.dailyRates$res, p = 10, K = 48)
ljk$pval # 0.96 so we failed to reject. There is evidence that this is white noise

# ASE
l = length(dailyRates)
f = l-10
foreDailyRates = fore.arma.wge(dailyRates, phi = est.dailyRates$phi, lastn = T, n.ahead = 10, limits = F)
ASE = mean((foreDailyRates$f - dailyRates[f:l])^2)
ASE

# Forecast 7 days
fore.arma.wge(dailyRates, phi = est.dailyRates$phi, n.ahead = 7, limits = T) # Did somewhat of a good job but knowing that AR and ARMA models would always trend towards the mean we expect 
# the trend to move towards the mean. We suspect that as the forecast moves away or oges further in the future it would fade into predicting the mean.

# Forecast 90 days
fore.arma.wge(dailyRates, phi = est.dailyRates$phi, n.ahead = 90, limits = T) 


# Signal plus noise
library(orcutt)
x = dailyRates
n = length(x)
t = 1:n
d = lm(x~t)
x.z = x - d$coefficients[1] - d$coefficients[2]*t


ar.z = aic.wge(x.z, p = 0:15) # ar.z$p is 10

# Tranform the dependent varible daily rates 
y.trans = artrans.wge(x, phi.tr = ar.z$phi)

# Transform the independent variable t (trend)
t.trans = artrans.wge(t, phi.tr = ar.z$phi)
plotts.sample.wge(t)

# regress y hat t on T hat t using OLS
fitDaily = lm(y.trans~t.trans)
summary(fitDaily)

# evaluate the residuals( after Cochrane-Orcutt)

plotts.wge(fitDaily$residuals)
acf(fitDaily$residuals)
ljung.wge(fitDaily$residuals) # There is evidence that this is white noise. Pval = 0.99. Fail to reject.

# phis and white noise variance
x.z.est = est.arma.wge(x.z, p = ar.z$p) # sig plus noise phis
x.z.est$avar  # the variance

# ASE for signal plus noise
l = length(dailyRates)
f = l-10
foreDailyRates = fore.sigplusnoise.wge(dailyRates, max.p = 10, lastn = T, n.ahead = 10, limits = F)
ASE = mean((foreDailyRates$f - dailyRates[f:l])^2)
ASE

# forecast short
fore.sigplusnoise.wge(dailyRates, max.p = 10, n.ahead = 7, limits = T)

# forecast long
fore.sigplusnoise.wge(dailyRates, max.p = 10, n.ahead = 90, limits = T)
  