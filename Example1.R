# LOAD LIBRARIES
# you may need to install packages, if this is the first time you use them. Select Packages > Install Packages in R/RStudio) 
library(tseries)
library(zoo)
library(fBasics)
#import dataset into a dataframe

crudeoil=read.table("crudeoil_w0416.csv",header=T, sep=',') 

# create time series for cisco prices
crudeoilts = zoo(crudeoil$price, as.Date(as.character(crudeoil$date), format = "%d-%b-%y"))
crudeoilts
head(crudeoilts)
plot(crudeoilts, xlab = "Time", ylab = "Price")


# sort data in chronological order
# set variable Date as time/date variable
#crudeoil$Date=as.Date(as.character(crudeoil$date), format = "%d-%b-%y")
#crudeoil=crudeoil[order(crudeoil$date),]

#Creating new variables 

# create lagged series using function lag(tsobject, k==1);
pricelag = lag(crudeoilts, k=-1);
head(pricelag)
# diff = p_t - p_(t-1);
pricedif = diff(crudeoilts);

#compute simple returns ret = (p_t-p_(t-1))/p_(t-1)
ret=(crudeoilts-pricelag)/pricelag
head(ret)
plot(ret, xlab = "Time", ylab = "Price")

#Example of data analysis for cisco dataset


# LOAD LIBRARIES
# Load fBasics packages into current session
# To install the package the first time, 
# select Tools from top Menu and select Install Packages 
library(fBasics)

# COMPUTE SUMMARY STATISTICS
basicStats(ret) 

# CREATE HISTOGRAM 
# OPTIONAL creates 2 by 2 display for 4 plots 
# par(mfcol=c(2,2)) 
hist(ret, xlab="crudeoil log returns", prob=TRUE, main="Histogram") 
# add approximating normal density curve 
xfit<-seq(min(ret),max(ret),length=40) 
yfit<-dnorm(xfit,mean=mean(ret),sd=sd(ret)) 
lines(xfit, yfit, col="blue", lwd=2) 

# CREATE NORMAL PROBABILITY PLOT 
qqnorm(ret) 
qqline(ret, col = 2) 

#Test on Kurtosis
K_stat= kurtosis(ret) / sqrt(24/length(ret))
K_stat

2*(1-pnorm(abs(K_stat)))

#Test on skewness
skew_test = skewness(ret)/sqrt(6/length(ret))
skew_test

2*(1-pnorm(abs(skew_test)))

# NORMALITY TESTS 
# Perform Jarque-Bera normality test. 
normalTest(ret,method=c("jb")) 


# COMPUTE ACF AND PLOT CORRELOGRAM 
#plot acf values on graph (correlogram) 
acf(ret, plot=T, lag = 15) 

# COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION) 
# to Lag 15 
Box.test(ret,lag=15,type='Ljung') 
