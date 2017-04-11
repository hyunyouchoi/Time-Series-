library(fBasics)
library(tseries)
library(rugarch)
library(zoo)
library(lmtest)
library(forecast)


# import data in R 
# import libraries for TS analysis
myd1= read.table('amazon_prices.csv', header=T, sep=',')
# create time series object ?
pricets = zoo(myd1$Price, as.Date(as.character(myd1$Date), format=c("%m/%d/%Y")))
#log return time series
rets = log(pricets/lag(pricets, -1))
head(rets)

#b.
# Plots ACF function of vector data
acf(coredata(rets), lag=20)
# Plot ACF of squared returns to check for ARCH effect 
acf(coredata(rets^2), lag=20)
# Plot ACF of absolute returns to check for ARCH effect 
acf(coredata(abs(rets)), lag=20)

# Computes Ljung-Box test on returns to test  independence 
Box.test(coredata(rets),lag=4,type='Ljung')
Box.test(coredata(rets),lag=6,type='Ljung')
# Computes Ljung-Box test on squared returns to test non-linear independence 
Box.test(coredata(rets^2),lag=4,type='Ljung')
Box.test(coredata(rets^2),lag=6,type='Ljung')
# Computes Ljung-Box test on absolute returns to test non-linear independence 
Box.test(abs(coredata(rets)),lag=4,type='Ljung')
Box.test(abs(coredata(rets)),lag=6,type='Ljung')


#specify model using functions in rugarch package
#Fit ARMA(0,0)-GARCH(1,1) model with t-distribution
garch11.t.spec=ugarchspec(variance.model=list(garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
garch11.t.fit=ugarchfit(spec=garch11.t.spec, data=rets)
garch11.t.fit
#plot of residuals
plot(garch11.t.fit)
persistence(garch11.t.fit)


#Fit ARMA(0,0)-eGARCH(1,1) model with t-distribution
egarch11.t.spec=ugarchspec(variance.model=list(model = "eGARCH", garchOrder=c(1,1)), mean.model=list(armaOrder=c(0,0)), distribution.model = "std")
#estimate model 
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=rets)
egarch11.t.fit
plot(egarch11.t.fit)

egarch11.fcst=ugarchforecast(egarch11.t.fit, n.ahead=5)
egarch11.fcst
plot(egarch11.fcst)

#rolling estimates
egarch11.t.fit=ugarchfit(spec=egarch11.t.spec, data=rets, out.sample=500)

egarch11.fcst=ugarchforecast(egarch11.t.fit, n.ahead=20, n.roll=499)
egarch11.fcst
plot(egarch11.fcst)
