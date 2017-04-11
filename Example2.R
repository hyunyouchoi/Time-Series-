# LOAD LIBRARIES
# you may need to install packages, if this is the first time you use them. Select Packages > Install Packages in R/RStudio) 
library(tseries)
library(zoo)
library(fBasics)
#import dataset into a dataframe


groceries=read.table("groceries.csv",header=T, sep=',') 

# create time series for cisco prices
groceriests = zoo(groceries$ToothPaste, as.Date(as.character(groceries$Date), format = "%d%b%Y"))
head(groceriests)


plot(groceriests, xlab = "Time", ylab = "ToothPaste sales")

# CREATE HISTOGRAM 
# OPTIONAL creates 2 by 2 display for 4 plots 
# par(mfcol=c(2,2)) 
hist(groceriests, xlab="Toothpast sales", prob=TRUE, main="Histogram") 
# add approximating normal density curve 
xfit<-seq(min(groceriests),max(groceriests),length=40) 
yfit<-dnorm(xfit,mean=mean(groceriests),sd=sd(groceriests)) 
lines(xfit, yfit, col="blue", lwd=2) 

# CREATE NORMAL PROBABILITY PLOT 
qqnorm(groceriests) 
qqline(groceriests, col = 2) 

# NORMALITY TESTS 
# Perform Jarque-Bera normality test. 
normalTest(groceriests,method=c("jb")) 


# COMPUTE ACF AND PLOT CORRELOGRAM 
#plot acf values on graph (correlogram) 
acf(groceriests, plot=T, lag = 15) 

# COMPUTE LJUNG-BOX TEST FOR WHITE NOISE (NO AUTOCORRELATION) 
# to Lag 15 
Box.test(groceriests,lag=5,type='Ljung') 
Box.test(groceriests,lag=2,type='Ljung') 
Box.test(groceriests,lag=10,type='Ljung') 

