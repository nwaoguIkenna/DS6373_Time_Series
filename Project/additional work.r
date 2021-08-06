dataGrouped$positive_perc
dataGrouped$hospitalizedCurrently
dataGrouped$totalTestResults
dataGrouped$negativeIncrease


ggpairs(dataGrouped %>% select(positiveIncrease_sum,negativeIncrease_sum, deathIncrease_sum,totalTestResults_sum,positive_perc,hospitalizedCurrently_sum))

################## Hospitalized Currrently ######################################################
plotts.sample.wge(dataGrouped$hospitalizedCurrently)

aic = aic.wge(dataGrouped$hospitalizedCurrently,p = 0:10, q = 0:2, type = 'bic')
hospital = est.arma.wge(dataGrouped$hospitalizedCurrently,p = aic$p, q = aic$q)
acf(hospital$res)
ljung.wge(hospital$res, p = 2, q = 1)
ljung.wge(hospital$res, p = 2, q = 1, K = 48)

hospitalFore7 = fore.arma.wge(dataGrouped$hospitalizedCurrently, phi = hospital$phi, theta = hospital$theta, n.ahead = 7)
hospitalFore90 = fore.arma.wge(dataGrouped$hospitalizedCurrently, phi = hospital$phi, theta = hospital$theta, n.ahead = 90)

hospitalFore7 = hospitalFore7$f
hospitalFore90 = hospitalFore90$f

####################### Positive Percent ################################
plotts.sample.wge(dataGrouped$positive_perc)
perc_07= artrans.wge(dataGrouped$positive_perc, c(rep(0,6),1))
aic = aic.wge(perc_07,p = 0:10, q = 0:2, type = 'bic')
#aic = aic.wge(dataGrouped$positive_perc,p = 0:10, q = 0:2, type = 'bic')
#perc = est.arma.wge(dataGrouped$positive_perc,p = aic$p, q = aic$q)
perc = est.arma.wge(perc_07,p = aic$p, q = aic$q)

acf(perc$res) 
ljung.wge(perc$res, p = 2, q = 1)
ljung.wge(perc$res, p = 2, q = 1, K = 48)

percFore7 = fore.aruma.wge(dataGrouped$positive_perc, phi = perc$phi, s = 7, theta = perc$theta, n.ahead = 7)
percFore90 = fore.aruma.wge(dataGrouped$positive_perc, phi = perc$phi, s = 7, theta = perc$theta, n.ahead = 90)


percFore7 = percFore7$f
percFore90 = percFore90$f


############################### Signal plus noise Total Results ######################################
library(orcutt)
x = dataGrouped$totalTestResults_sum
n = length(x)
t = 1:n
d = lm(x~t)
x.z = x - d$coefficients[1] - d$coefficients[2]*t


ar.z = aic.wge(x.z, p = 0:15, q = 0:2) # ar.z$p is 10

# Tranform the dependent varible daily rates 
y.trans = artrans.wge(x, phi.tr = ar.z$phi)

# Transform the independent variable t (trend)
t.trans = artrans.wge(t, phi.tr = ar.z$phi)
plotts.sample.wge(y.trans)    # looks like noise
plotts.sample.wge(t.trans)


# regress y hat t on T hat t using OLS
fitTotal = lm(y.trans~t.trans)

# evaluate the residuals( after Cochrane-Orcutt)

plotts.wge(fitTotal$residuals)
acf(fitTotal$residuals)
ljung.wge(fitTotal$residuals) # There is evidence that this is white noise. Pval = 0.99. Fail to reject.
ljung.wge(fitTotal$residuals, K = 48)

# phis and white noise variance
x.z.est = est.arma.wge(x.z, p = ar.z$p, q = ar.z$q) # sig plus noise phis

x.z.est$avar  # the variance

# ASE for signal plus noise
totalFore7 = fore.sigplusnoise.wge(dataGrouped$totalTestResults, max.p = 10, n.ahead = 7)
totalFore90 = fore.sigplusnoise.wge(dataGrouped$totalTestResults, max.p = 10, n.ahead = 90)

totalFore7 = totalFore7$f
totalFore90 = totalFore90$f


######################## Negative Increase Daily ###############################################
plotts.sample.wge(dataGrouped$negativeIncrease_sum)
negativeInc_01= artrans.wge(dataGrouped$negativeIncrease_sum, c(rep(0,0),1))
aic = aic.wge(dataGrouped$negativeIncrease_sum,p = 0:10, q = 0:2, type = 'bic')
negative = est.arma.wge(dataGrouped$negativeIncrease_sum,p = aic$p, q = aic$q)

acf(negative$res)
ljung.wge(negative$res, p = 2, q = 1)
ljung.wge(negative$res, p = 2, q = 1, K = 48)

negativeFore7 = fore.aruma.wge(dataGrouped$negativeIncrease_sum, phi = perc$phi, d = 1, theta = perc$theta, n.ahead = 7)
negativeFore90 = fore.aruma.wge(dataGrouped$negativeIncrease_sum, phi = perc$phi, d = 1, theta = perc$theta, n.ahead = 90)

negativeFore7 = negativeFore7$f
negativeFore90 = negativeFore90$f








smallData <- dataGrouped[1:(len-10),]
Corofit = lm(positiveIncrease_sum~negativeIncrease_sum + leadhosp + lagtest + leaddeath, data = smallData)
phi = aic.wge(Corofit$residuals)

DataForeDF = data.frame(lagtest= dataGrouped$totalTestResults_sum[(len-9):len],leadhosp = dataGrouped$hospitalizedCurrently[(len-9):len], negativeIncrease_sum = dataGrouped$negativeIncrease[(len-9):len], leaddeath = dataGrouped$positive_perc[(len-9):len])


resids = fore.arma.wge(Corofit$residuals,phi = phi$phi,n.ahead = horizon)


preds = predict(Corofit, newdata = DataForeDF)

predsFinal = preds + resids$f

predsFinal <- as.numeric(predsFinal)




plot(seq(len,(len+89),1), preds$fcst$y1[,1], type = "l", col = "red")


plot(seq(len,(len+89),1), preds$fcst$y1[,1], type = "l", col = "red",xlim = c(0,len+90), ylim=c(0,140000), ylab = "New Daily Cases", main = paste(as.character(90),"Days Forecast"))
line(seq(1,len,1), dataGrouped$positiveIncrease, type = "l")



plot(seq(1,len,1), dataGrouped$positiveIncrease, type = "l",xlim = c(0,len+90), ylim=c(0,140000),  ylab = "New Daily Cases", main = paste(as.character(90),"Days Forecast"))
lines(seq(len,(len+89),1), preds$fcst$y1[,1], type = "l", col = "red")x



?var

?fore.arma.wge()


ggpairs(dataGrouped %>% dplyr::select(positiveIncrease_sum,negativeIncrease_sum, deathIncrease_sum,totalTestResults_sum,positive_perc,hospitalizedCurrently_sum))
