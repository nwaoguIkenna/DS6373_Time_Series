library(tswge)
library(vars)
library(RColorBrewer)
#fanchart(preds, colors = brewer.pal(n=8, name = 'Blues")) This is to chancolor and add cooler coloring to the CI

# Exercise forthe cross correlation using the ccf function
  lag1 = read.csv(file.choose(), header = TRUE)
lag1
ccf(lag1$Y, lag1$X1)

# Business Data with vars package(VARselect and VAR)
BSales = read.csv(file.choose(), header = TRUE)

BSales

#VARSelect on Differenced Data chooses 2
VARselect(cbind(BSales$sales, BSales$ad_tv, BSales$ad_online),lag.max = 10, type = "both")
#VAR with p = 2
VARfit = VAR(cbind(BSales$sales, BSales$ad_tv, BSales$ad_online),type = "both",p = 2)
preds=predict(CMortDiffVAR,n.ahead=20)
