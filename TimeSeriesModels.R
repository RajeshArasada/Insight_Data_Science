
#------------------------------------------------------------------
# Load libraries
#------------------------------------------------------------------

library(forecast)
library(TTR)
library(tseries)
library(urca)

# Set up the working environment
#------------------------------------------------------------------
setwd("/InsightProject")

# Load in the data
#------------------------------------------------------------------
mydata <- read.csv("predictEating.csv")  # read in the csv file 

# Time Series for Modeling
#------------------------------------------------------------------
#
#myvector<-c(7,10,5,11,16,14,9,12,19,21,51)
# Create My Time Series Model
#------------------------------------------------------------------
myvector <- c(mydata[['X3']])
myvector <- as.numeric(myvector)
myts <- ts(myvector, start=c(2005, 1), end=c(2015, 1), frequency=1)

fit <- HoltWinters(myts, gamma=FALSE)
plot(fit)
# predict next two future values
forecastplot <- forecast(fit , n.ahead = 2, prediction.interval = T,)
forecast(fit, 2)
adf.test(myts)
# Save a numeric vector as a time series object
#------------------------------------------------------------------
myts <- ts(myvector, start=c(2005, 1), end=c(2014, 1), frequency=1)

forecast(ets(myts),h=2)
plot(forecast(ets(myts),h=2))
# Subset the time series (June 2014 to December 2014)
#------------------------------------------------------------------
myts2 <- window(myts, start=c(2011, 1), end=c(2014, 1))
## Test for stationary
#-----------------------------------------------------------------

adf.test(myts)
ndiffs(myts)
myts<-diff(log(myts), differences=1)
# Plot series
#------------------
plot(myts)
# Exponential Models
#------------------------------------------------------------------------
##HoltWinters Forecast
#---------------------------
# Simple Exponential - models level
#--------------------------------------------------------
fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
plot(fit)
# Double exponential - models level and trend
#--------------------------------------------------------
fit <- HoltWinters(myts, gamma=FALSE)
plot(fit)
# predict next two future values
forecastplot <- forecast(fit , n.ahead = 2, prediction.interval = T,)
forecast(fit, 2)
#plot(forecast(fit, 2))
plot(fit,forecastplot)


myvector <- c(mydata[['X15']],3)
myvector <- as.numeric(myvector)
myts <- ts(myvector, start=c(2005, 1), end=c(2015, 1), frequency=1)
adf.test(myts)
ndiffs(myts)
fit <- HoltWinters(myts, gamma=FALSE)
plot(fit)
# predict next two future values
forecastplot <- forecast(fit , n.ahead = 2, prediction.interval = T,)
forecast(fit, 2)




forecast(myts)
fit <- ets(myts)
plot(forecast(fit, 1))
forecast(fit, 1)
# Triple exponential - models level, trend, and seasonal components
#--------------------------------------------------------
#fit <- HoltWinters(myts)
#plot(fit)


##-------------------------------------------------------
plotgraph<-HoltWinters(myts,gamma = F)

# predictive accuracy
#--------------------------------------------------------
mytstrain <- ts(myvector, start=c(2005, 1), end=c(2015, 1), frequency=1)
mytstest <- window(mytstrain, start=c(2013, 1), end=c(2015, 1), frequency=1)
fittrain <- HoltWinters(mytstrain, beta=FALSE, gamma=FALSE)
fittrain <- HoltWinters(mytstrain, gamma=FALSE)

accuracy(fittrain,test=mytstest)


#
#
#
#ARIMA Models
#------------------------------------------------------------------------
myvector <- as.numeric(mydata[['X15']])

# Save a numeric vector as a time series object
#------------------------------------------------------------------
myts <- ts(myvector, start=c(2005, 1), end=c(2014, 1), frequency=1)
sample <- window(myts, start=c(2011, 1), end=c(2014, 1))
#------------------------------------------------------------------------
ndiffs(myts)
adf.test(myts, alternative = "stationary")
ndiffs(sample)
fit <- auto.arima(myts, seasonal = FALSE)
fit
forecast(fit,2)
plot(forecast(fit,2))



myvector <- c(mydata[['X15']])
myvector <- as.numeric(myvector)
myts <- ts(myvector, start=c(2005, 1), end=c(2014, 1), frequency=1)
#------------------------------------------------------------------------------
myvector <- c(mydata[['X15']],51)
myvector <- as.numeric(myvector)
myts <- ts(myvector, start=c(2005, 1), end=c(2015, 1), frequency=1)

adf.test(myts)
myts<-diff(myts, differences=4)
adf.test(myts)
library(forecast)
ndiffs(myts)
#Box.test(sample, lag = 1,type="Ljung-Box")
#acf(myts)
# pacf(myts)
# fit an ARIMA model of order P, D, Q
#--------------------------------------------------------
auto.arima(myts,ic='aic',trace = T)

fittwo <- arima(myts, order=c(0,1, 0))
# predictive accuracy
#--------------------------------------------------------
accuracy(fittwo)
# predict next 2 observations
#--------------------------------------------------------
forecast(fittwo, 2)
#predict(fittwo,2)
plot(forecast(fittwo, 2)) 

# Automated forecasting
#--------------------------------------------------------
#
# Automated forecasting using an exponential model
#--------------------------------------------------------
fit <- ets(myts, additive.only = TRUE)
fit
#--------------------------------------------------------
# Automated forecasting using an ARIMA model
#--------------------------------------------------------
fit <- auto.arima(myts)
fit
#--------------------------------------------------------
auto.arima(myts,ic='aic',trace = T)

myvector <- c(mydata[['X14']],9)
myvector <- as.numeric(myvector)
myts <- ts(myvector, start=c(2005, 1), end=c(2015, 1), frequency=1)


# predictive accuracy for Arima Model
#--------------------------------------------------------
data <- ts(myts, start=c(2005, 1), end=c(2015, 1), frequency=1)
train <- window(data, end=c(2013, 1), frequency=1)
test <- window(data,start=c(2014,1))
## fit a model on training data
aaFit <- auto.arima(train)

plot(aaFit)
## forecaset training model
## over the testing period
aaPred <- forecast(aaFit,h=length(test))

plot(aaPred)

## extract point forecasts
yhat <- aaPred$mean

## a few functions
## mean squared (prediction) error
MSE <- function(y, yhat){
  mean((y-yhat)**2)
}

## mean absolute (prediction) error
MAE <- function(y, yhat){
  mean(abs(y-yhat))
}

## mean absolute percentage (prediction) error
MAPE <- function(y,yhat,percent=TRUE){
  if(percent){
    100*mean(abs((y-yhat)/y))
  } else {
    mean(abs((y-yhat)/y))
  }
}

MSE(test,yhat)

MAE(test,yhat)

MAPE(test,yhat)

accuracy(aaPred, data)

## Examine measures for only forecast
accuracy(yhat,test)
--------------------------------------------------------------------
  
  
## predictive accuracy using Holtwinters double exponntial  
Data <- ts(myts, start=c(2005, 1), end=c(2015, 1), frequency=1)
train2 <- window(Data, end=c(2013, 1), frequency=1)
test2 <- window(Data,start=c(2014,1))
## fit a model on training data
aaFit2 <- HoltWinters(train2, gamma=FALSE)

fit <- HoltWinters(train2, gamma=FALSE)
plot(fit)

## forecaset training model
## over the testing period
aaPred2 <- forecast(aaFit2,h=length(test2))

plot(aaPred2)

## extract point forecasts
yhat2 <- aaPred2$mean

## a few functions
## mean squared (prediction) error
MSE <- function(y, yhat2){
  mean((y-yhat)**2)
}

## mean absolute (prediction) error
MAE <- function(y, yhat2){
  mean(abs(y-yhat))
}

## mean absolute percentage (prediction) error
MAPE <- function(y,yhat2,percent=TRUE){
  if(percent){
    100*mean(abs((y-yhat)/y))
  } else {
    mean(abs((y-yhat)/y))
  }
}

MSE(test,yhat2)

MAE(test,yhat2)

MAPE(test,yhat2)

accuracy(aaPred2, Data)

## Examine measures for only forecast
accuracy(yhat2,test2)

