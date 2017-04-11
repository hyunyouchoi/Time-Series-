## LOAD LIBRARIES
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)

# IMPORT DATA AND CREATE OBJECTS
#  Load data with no variable names into the data frame "da"
myd=read.table("consump.csv", header=T, sep=',') 
x1=myd$consump
xts=ts(myd$consump,frequency=12,start=c(2000,1))

# APPLYING DIFFERENCING TO DATA
# compute regular differences
dx=diff(x1) 

#a.
#scatter plot
plot(myd[,c(2,4,5)])
#Correlation value
cor(myd[,c(2,4,5)])

# b.
# Fit Regression Model with three covariates (consump, pers_inc, unemp)
# and AR(2) error term as suggested by ACF/PACF analysis
m1=arima(xts, order=c(1,1,1), xreg=data.frame(myd$pers_inc, myd$unemp), method="ML")
coeftest(m1)


source("backtest.R")
backtest(m1, xts, xre=data.frame(myd$pers_inc, tslag(myd$unemp)), 150, h=1, inc.drift=F)

# updated model without non-significant variables
# Fit Regression Model with two covariates (consump, pers_inc)
# and AR(2) error term as suggested by ACF/PACF analysis
m2=arima(xts, order=c(1,1,1), xreg=data.frame(myd$pers_inc), method="ML")
coeftest(m2)

acf(coredata(m2$resid), lag=20)
Box.test(m2$resid, lag=5, fitdf=3)
Box.test(m2$resid, lag=10, fitdf=3)

qqnorm(m2$residuals)
qqline(m2$residuals)

source("backtest.R")
backtest(m2, xts, xre=data.frame(myd$pers_inc), 150, h=1, inc.drift=F)

#d.
# Fit Regression Model lagged 
# and AR(2) error term as suggested by ACF/PACF analysis
m3=arima(xts, order=c(1,1,1), xreg=data.frame(myd$pers_inc, myd$unemp, tslag(myd$pers_inc),tslag(myd$unemp)), method="ML")
coeftest(m3)

m4=arima(xts, order=c(1,1,1), xreg=data.frame(myd$pers_inc, myd$unemp,tslag(myd$unemp)), method="ML")
coeftest(m4)

acf(coredata(m4$resid), lag=20)
Box.test(m4$resid, lag=5, fitdf=3)
Box.test(m4$resid, lag=10, fitdf=3)

source("backtest.R")
backtest(m4, xts, xre=data.frame(myd$pers_inc), 150, h=1, inc.drift=F)
