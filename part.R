library(tidyverse)
library(tseries)
library(readxl)
library(neuralnet)
library(quantmod)
library(xts)
library(funtimes)
library(dplyr)
library(MLmetrics)

Exchange_usd <- read_excel("/Users/lakinduindipa/Desktop/Machine Learning CW/ExchangeUSD.xlsx") 


# data Exploration 

plot(Exchange_usd$`YYYY/MM/DD`)  # Plotting the YYYY/MM/DD Column
plot(Exchange_usd$`USD/EUR`)   # Plotting the USD/EUR Column


Exchange_usd$`YYYY/MM/DD` <- as.Date(Exchange_usd$`YYYY/MM/DD`) # Convert character string column to date


timeseries_data <- xts(Exchange_usd$`USD/EUR`,Exchange_usd$`YYYY/MM/DD`) # convert excel file to time series
timeseries_data   # Print time series

# target and predictor features
rate_original <-(timeseries_data)
rate_lag <- stats::lag(rate_original,1)
rate_all <- cbind(rate_original,rate_lag)
colnames(rate_all) <- c('rate_original', 'rate_lag')
rate_all <- na.exclude(rate_all)

# Normalizing 
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}

nomralizes_data <- as.data.frame(lapply(rate_all,normalize))

# Training and testing ranges
rate_train <- window(rate_all, end = '2013-05-21')
rate_test <- window(rate_all, start = '2013-05-21')

plot(rate_train) # plotting the train data 


set.seed(123)
# ANN Regression Fitting

nueral_fit <- neuralnet(rate_original~rate_lag, data=rate_train, hidden=1, act.fct= tanh) #ternage active function used
nueral_fit$result.matrix

# Graphic Neural network
plot(nueral_fit) 



# Test the accuracy of the model
temp_test <- subset(rate_test, select = 'rate_lag')
head(temp_test)
nueral_fit.results <- compute(nueral_fit, temp_test)
results <- data.frame(actual = rate_test$rate_original, predicted= nueral_fit.results$net.result)
results

predicted <- nueral_fit.results$net.result
actual <- rate_test$rate_original

# standard statistical indices
RMSE(predicted,actual) 
MAE(predicted,actual)
MAPE(predicted,actual)

data <-data.frame(actual=actual, predicted = predicted)

par(mfrow=c(1,2))
plot(data$rate_original,data$predicted,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')