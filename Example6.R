## LOAD LIBRARIES
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)

# IMPORT DATA AND CREATE OBJECTS
#  Load data with no variable names into the data frame "da"
myd1=read.table("consump.csv", header=T, sep=',') 

# creates time series object
x1=myd1$consump
xts1=ts(myd1$consump,frequency=12,start=c(2000,1))
head(xts1)

# create time plot
plot(xts1,type='l')

# ACF ANALYSIS

# ACF plot of consump
#time series to regular numerical vector
acf(xts1,lag.max=20, main="ACF of starts")

# APPLYING DIFFERENCING TO DATA
# compute regular differences
dx1=diff(x1) 
# create acf plot
acf(dx1,lag.max=20, main="ACF of DX1 ")

#Dickey Fuller test
library(fUnitRoots)
adfTest(xts1, type=c("c"))

adfTest(dx1, type=c("c"))

acf(dx1, lag=20)
pacf(dx1, lag=20)

# try automated order selection
m=auto.arima(xts1, trace=T,seasonal=T, ic="bic")
coeftest(m)

# Let's try an alternative ARIMA model.
# fit multiplicative seasonal model ARIMA(1,1,1)
m3<-Arima(xts1, order=c(1,1,1), include.drift=TRUE, method="ML") 
coeftest(m3)
acf(m3$resid)

# "approximated" ljung box test on residuals
Box.test(m3$residuals, 5, "Ljung-Box",fitdf=length(m3$coef) ) 
Box.test(m3$residuals, 8, "Ljung-Box", fitdf=length(m3$coef)) 

qqnorm(m3$residuals)
qqline(m3$residuals)

#Apply backtesting procedures
source("backtest.R")
backtest(m3, xts1, 150, h=1, inc.drift=T)

plot(forecast(m3, h=10))

