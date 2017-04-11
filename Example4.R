# LOAD LIBRARIES
library(tseries)
library(fBasics)
library(zoo)
library(forecast)
library(lmtest)

#a. IMPORT DATA
#  Load data with no variable names into the data frame 
myd=read.table("indpro.csv",header=T, sep=',') 
head(myd)

# CREATE TIME SERIEs using ts() function.
# ts(datavector, start=c(year, index), freq=nperiods)
myd1 = myd[,2]
mts = ts(myd1, start=c(1990,1), freq=12)


#b. CREATE TIME PLOT 
# use time series object to draw time plot indexed with time
# Time plot for rate
plot(mts, type="l", xlab="Time", ylab="rate", main = "Time series plot")


#c. Analysis of distribution
par(mfcol=c(1,1)) 
hist(myd1, xlab="Distribution of Rate", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(myd1),max(myd1),length=40)
yfit<-dnorm(xfit,mean=mean(myd1),sd=sd(myd1))
lines(xfit, yfit, col="blue", lwd=2) 

# CREATE NORMAL PROBABILITY PLOT
qqnorm(myd1)
qqline(myd1, col = 2)

# NORMALITY TESTS
# Perform Jarque-Bera normality test.
normalTest(myd1,method=c('jb')) 


#d. COMPUTE ACF, LJUNG-BOX TEST, PACF AND PLOT CORRELOGRAM
# NOTE: acf(xvar) will display lags in integers if xvar is a numeric vector
# Thus, we use coredata(tsobject) to retrieve numeric values of ts object.
#plots acf (correlogram)
acf(coredata(mts), plot=T, lag=15)

#COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 3
Box.test(mts,lag=3,type='Ljung')
# to Lag 6
Box.test(mts,lag=6, type='Ljung')
# to Lag 9
Box.test(mts,lag=9, type='Ljung')

#e. PACF plot
pacf(mts) 

#f. BIC criterion
# 
auto.arima(mts, ic =c("bic"), trace=TRUE, allowdrift=TRUE)

### Fit ARIMA(1,0,2) model
m1= Arima(mts, order=c(1,0,2), method='ML')
coeftest(m1)

# RESIDUAL ANALYSIS
acf(m1$residuals)

Box.test(m1$residuals,lag=3,type='Ljung-Box', fitdf=2)
Box.test(m1$residuals,lag=6,type='Ljung-Box', fitdf=2)

#Analysis of distribution
par(mfcol=c(1,1)) 
hist(m1$residuals, xlab="Distribution of residuals", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(m1$residuals),max(m1$residuals),length=40)
yfit<-dnorm(xfit,mean=mean(m1$residuals),sd=sd(m1$residuals))
lines(xfit, yfit, col="blue", lwd=2) 

# CREATE NORMAL PROBABILITY PLOT
qqnorm(m1$residuals)
qqline(m1$residuals, col = 2) 

# NORMALITY TESTS
# Perform Jarque-Bera normality test.
normalTest(m1$residuals,method=c('jb')) 


par(mfrow = c(1,1))
plot(m1)


###############################################################################
#g. FIT MA(4) MODEL (ORDER AUTOMATICALLY SELECTED)

#FIT AR MODEL OF GIVEN ORDER
#Another approach to fit MA(4) model of given order
m2=Arima(mts, c(0,0,4), method="ML")  
coeftest(m2)

# RESIDUAL ANALYSIS
acf(m2$residuals)

Box.test(m2$residuals,lag=5,type='Ljung-Box', fitdf=4)
Box.test(m2$residuals,lag=9,type='Ljung-Box', fitdf=4)

#Analysis of distribution
par(mfcol=c(1,1)) 
hist(m2$residuals, xlab="Distribution of residuals m2", prob=TRUE, main="Histogram")
# add approximating normal density curve
xfit<-seq(min(m2$residuals),max(m2$residuals),length=40)
yfit<-dnorm(xfit,mean=mean(m2$residuals),sd=sd(m2$residuals))
lines(xfit, yfit, col="blue", lwd=2) 

# CREATE NORMAL PROBABILITY PLOT
qqnorm(m2$residuals)
qqline(m2$residuals, col = 2) 

# NORMALITY TESTS
# Perform Jarque-Bera normality test.
normalTest(m2$residuals,method=c('jb')) 

par(mfrow = c(1,1))
plot(m2)

#compute roots of Polynomial
1 / polyroot(c(1,-m2$coef[1:3]))

# h. COMPUTE PREDICTIONS and compare their behavior m1 and m2
# Use functions in the forecast package 
# forecast.Arima(arima_model, h) produces forecasts and 80% and 95% prediction intervals
# where arima_model is an object created by arima() function and h= horizon
f1=forecast.Arima(m1, h=10)
f1
f2=forecast.Arima(m2, h=10)
f2

#PLOT PREDICTIONS FOR 10 STEPS AHEAD
# use plot in forecast package
# include = number of past observations to be plotted
plot(forecast.Arima(m1, h=10), include=200)
abline(h = mean(coredata(mts)), col = 2, lwd = 2)

plot(forecast.Arima(m2, h=10), include=200)
abline(h = mean(coredata(mts)), col = 2, lwd = 2)

