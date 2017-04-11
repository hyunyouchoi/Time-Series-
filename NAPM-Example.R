# LOAD LIBRARIES
library(tseries)
library(fBasics)
library(zoo)

#a. IMPORT DATA
#  Load data with no variable names into the data frame "da"
myd=read.table("NAPM.csv",header=T, sep=',') 
head(myd)

# CREATE TIME SERIEs using ts() function.
# ts(datavector, start=c(year, index), freq=nperiods)
myd1 = myd[,2]
mts = ts(myd1, start=c(1980,1), freq=12)

#b. CREATE TIME PLOT 
# use time series object to draw time plot indexed with time
# Time plot for index
plot(mts, type="l", xlab="Time", ylab="Index", main = "Time series plot")

#c. COMPUTE ACF, LJUNG-BOX TEST, PACF AND PLOT CORRELOGRAM
# NOTE: acf(xvar) will display lags in integers if xvar is a numeric vector
# Thus, we use coredata(tsobject) to retrieve numeric values of ts object.
#plots acf (correlogram)
acf(coredata(mts), plot=T, lag=20)


#COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION)
# to Lag 3
Box.test(mts,lag=3,type='Ljung')
# to Lag 6
Box.test(mts,lag=6, type='Ljung')
# to Lag 9
Box.test(mts,lag=9, type='Ljung')

#d. PACF plot
pacf(mts) #lines at lag 1,2,3 are above and below blue dotted line. Therefore, our p value is 3. 

#e. FIT AR(P) MODEL (ORDER AUTOMATICALLY SELECTED)

#FIT AR MODEL OF GIVEN ORDER
#Another approach to fit AR model of given order
library(forecast)
m2=Arima(mts, c(3,0,0), method="ML")  
library(lmtest)
coeftest(m2)

# residuals are saved in the resid variable and can be retrieved using 
# m1$resid 
# create residual plot
acf(m2$resid)

# Test if residuals are white noise using the Ljung-Box test
# Since L-B test for residuals has m-p degrees of freedom,
# where m=#lags and p = order of AR(p) model, we need to specify the order p
# using the fitdf option in Box.test() function.
Box.test(m2$resid, lag=4, type='Ljung', fitdf=3)
Box.test(m2$resid, lag=6, type='Ljung', fitdf=3)
Box.test(m2$resid, lag=9, type='Ljung', fitdf=3)

#PLOT ACF of RESIDUALS
acf(m2$resid)

par(mfrow = c(1,2))
hist(m2$resid, main ='residual histogram')
qqnorm(m2$resid)
qqline(m2$resid,col=2)

par(mfrow = c(1,1))
plot(m2)

#compute roots of Polynomial
1 / polyroot(c(1,-m2$coef[1:3]))

# h. COMPUTE PREDICTIONS
# Use functions in the forecast package 
# forecast.Arima(arima_model, h) produces forecasts and 80% and 95% prediction intervals
# where arima_model is an object created by arima() function and h= horizon
f=forecast.Arima(m2, h=5)
f

#i.PLOT PREDICTIONS FOR 10 STEPS AHEAD
# use plot in forecast package
# include = number of past observations to be plotted
plot(forecast.Arima(m2, h=10), include=200)
abline(h = mean(coredata(mts)), col = 2, lwd = 2)

