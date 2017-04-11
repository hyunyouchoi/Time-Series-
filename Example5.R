## LOAD LIBRARIES
library(tseries)
library(fBasics)
library(zoo)
library(lmtest)
library(forecast)

# IMPORT DATA AND CREATE OBJECTS
#  Load data with no variable names into the data frame "da"
myd=read.table("costco.csv", header=T, sep=',') 

# creates time series object
x=myd$eps
xts=ts(myd$eps,frequency=4,start=c(2002,3))
head(xts)

# create time plot
plot(xts,type='l')

# compute log transformation
lnxts=log(xts)
# create time plot
plot(lnxts,type='l')
# ACF ANALYSIS

# ACF plot for time series
# log transforms time series to regular numerical vector
acf(lnxts,plot= T,lag.max=30, main="ACF of Costco log (earnings) Yt")

# APPLYING DIFFERENCING TO DATA
# compute 1st differences
dx=diff(lnxts) 
# create acf plot
acf(dx,lag.max=30, main="ACF of first difference")

# try automated order selection
m1=auto.arima(lnxts,max.p=5, max.q=5, trace=T, ic="bic")


m2 <- Arima(lnxts, order = c(1,0,0), seasonal = list(order = c(1,1,0), period = 4), method = "ML", include.drift = TRUE)
coeftest(m2)

# One plot per page 
par(mfcol=c(1,1)) 
acf(m2$residuals)
# "approximated" ljung box test on residuals
Box.test(m2$residuals, 5, "Ljung-Box",fitdf=length(m2$coef) ) 
Box.test(m2$residuals, 8, "Ljung-Box", fitdf=length(m2$coef)) 

qqnorm(m2$residuals)
qqline(m2$residuals)

#e. Compute forecasts
f1=forecast(m2,h=4)
f1
#predictions in original scale;
exp(f1$mean)
#plot the forecast
plot(forecast(m2, h=10))

#f.compare models using backtesting
source("backtest.R")
backtest(m2,lnxts,50, h=1, inc.drift=T)

#g. 
f1=forecast(m1,h=4)
# create forecast plot for original variable
# join data with forecasts
s1=ts(c(xts,exp(f1$mean)), start=c(2002,1), freq=4)
# compute lower limit for 95% interval in original scale
lcl=ts(exp(f1$lower[,2]), start=c(2016,1), freq=4)
# compute upper limit for 95% interval in original scale
ucl=ts(exp(f1$upper[,2]), start=c(2016,1), freq=4)
# Forecast plot
plot(s1, ylim=c(0, 2.5))
lines(ts(c(exp(f1$fitted),exp(f1$mean)), start=c(2002,1),freq=4), col="blue")
lines(ucl,lty=2, col='red')
lines(lcl,lty=2, col='red')
