ASEHolder = data.frame(seed = as.numeric(),reps = as.numeric(),ASE = as.double(),MAE = as.double())  #matrix(nrow = 15000, ncol = 3)
for( i in 1:20)
{
  set.seed(i)
  for (j in seq(from=10, to=100, by=10)){
    fit.mlp = mlp(ts(Class$Daily_New_Cases),reps = j,comb = "mean",xreg = CoroDF)
    fore.mlp = forecast(fit.mlp, h = lenPos,xreg = foreAllDF)
    ASE = mean((texasPos - fore.mlp$mean)^2)
    MAE = sqrt(ASE)
    
    
    ASEHolder <- rbind(ASEHolder, c(i,j,ASE,MAE))
    #ASEHolder[i,j] = CM$overall[1]
  }
  
}

ASEHolder%>% filter(X3221.09889998244<2000)

x = gen.arma.wge(100,phi = .99)
fore.arma.wge(x, n.ahead = 10, phi = -.99)



x = gen.arma.wge(100,phi = c(-.19,.792)) # stationary This is A
x = gen.arma.wge(100,phi = c(.9,-1.4)) # non- stationary This is B?


library(time)

Data1 = read.csv("~/Documents/datascience/DS6373/finaldata2.csv", header = TRUE)
aic = aic.wge(Data1$x,p = 0:10, q = 0:2, type = 'bic')
data1.x = est.arma.wge(Data1$x,p = aic$p, q = aic$q)
acf(data1.x$res)

data1.01 = artrans.wge(Data1$x, c(rep(0,0),1))
acf(data1.01)

aic = aic.wge(data1.01,p = 0:10, q = 0:2, type = 'bic')
data1.01.x = est.arma.wge(data1.01,p = aic$p, q = aic$q)
ljung.wge(data1.01.x$res, p = aic$p, q = aic$q)
ljung.wge(data1.01.x$res, p = aic$p, q = aic$q, K = 48)


aic = aic.wge(data1.01,p = 0:10, q = 0:2, type = 'bic')
data1.01.x = est.arma.wge(data1.01,p = aic$p, q = aic$q)


r = gen.arima.wge(200, phi = data1.01.x$phi, d = 1)
f = fore.aruma.wge(r,phi = data1.01.x$phi, d = 1, n.ahead = 30,lastn = T)

ASE = mean((r[171:200] - f$f)^2)
ASE



train = ts(data = r, start = 1, end = 170)
test = ts(data = r, start = 171, end = 200)

# Fit an MLP model
set.seed(2)
pos.mlp = mlp(train, reps = 50, comb = "mean")
pos.mlp
plot(pos.mlp)

# Forecast the model to find the ASE
pos.mlp.fore = forecast(pos.mlp, h = 30)
plot(pos.mlp.fore)

# Calculate the ASE
pos.mlp.ase = mean((test - pos.mlp.fore$mean)^2)
pos.mlp.ase




Data2 = read.csv("~/Documents/datascience/DS6373/finaldata2.csv", header = TRUE)
Var = VAR(cbind(Data2$X1,Data2$X2,Data2$X3), type = "both", lag.max = 5, ic='AIC')

preds = predict(Var,n.ahead = 5) 
plot(preds)

ASE = mean((dataGrouped$positiveIncrease[(len-9):len] - preds$fcst$y1[,1])^2)
ASE