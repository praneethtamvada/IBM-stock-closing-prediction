rm(list = ls())
ibm.stock.data <- read.csv("ibm-common-stock-closing-prices-.csv", header = T)
ibm.closing.price <- ibm.stock.data$IBM.common.stock.closing.prices..daily..29th.June.1959.to.30th.June.1960..N.255.

## Ploting the time series data to check the trend and also see the correlations

plot.ts(ibm.closing.price) #There is presence of trend but no seasonality or variance in the plot 
acf(ibm.closing.price, main = 'IBM stock closing prices',col = 'blue') # Presence of correlation which is decayinf very slowly shows it is a non-stationary data
pacf(ibm.closing.price, main = 'IBM stock closing prices',col = 'blue') # No partial correlations


## We see trend in the plot. So, we de-trend the data by differencing it with previous value

## plot the de-trend data

plot.ts(diff(ibm.closing.price)) ## Plot looks stationary now with stable mean and variance
acf(diff(ibm.closing.price), main = 'ACF for IBM stock closing prices(Differenced)',col = 'blue')
pacf(diff(ibm.closing.price), main = 'PACF for IBM stock closing prices(Differenced)',col = 'blue')
# The ACF and PACF suggests that there might be an AR of order 0 and MA of order 0,1,2

## Lets model This data with different orders of Moving Averages and AR(0) where d = 1

model1 <- arima(ibm.closing.price, order = c(0,1,0))
box_test1 <- Box.test(model1$residuals, lag=log(length(model1$residuals)))
SSE1 <- sum(model1$residuals**2)

model2 <- arima(ibm.closing.price, order = c(0,1,1))
box_test2 <- Box.test(model2$residuals, lag=log(length(model2$residuals)))
SSE2 <- sum(model2$residuals**2)


model3 <- arima(ibm.closing.price, order = c(0,1,2))
box_test3 <- Box.test(model3$residuals, lag=log(length(model3$residuals)))
SSE3 <- sum(model3$residuals**2)


### Creating a Data Frame to summerize all the models

models.df<-data.frame(row.names=c('AIC', 'SSE', 'p-value'), c(model1$aic, SSE1, box_test1$p.value), 
               c(model2$aic, SSE2, box_test2$p.value), c(model3$aic, SSE3, box_test3$p.value))
colnames(models.df)<-c('Model-1','Model-2', 'Model-3')


# ###             Model-1        Model-2        Model-3      #### #
#    AIC        1544.1082565   1544.5227287   1546.4673914        #
#    SSE        6443.1980248   6402.9416570   6401.5412388        #
#    p-value    0.7246518      0.9653249      0.9703054           #
#                                                                 #
###################################################################
# 
# Based on the summary of the above models we choose the MODEL-2 which has low SSE and shows
# strong evidence in p-value that there are no correlations. We can compromise with AIC value
# of model-1 as its very negligable.

## Forecasting the results

Predicted.closing.price <- forecast(model2)
plot(forecast(model2))

#    Predicted closing price with confidence intervels
# 
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 256       521.9809 515.5466 528.4152 512.1404 531.8213
# 257       521.9809 513.2389 530.7228 508.6112 535.3506
# 258       521.9809 511.4242 532.5375 505.8359 538.1259
# 259       521.9809 509.8786 534.0831 503.4721 540.4897
# 260       521.9809 508.5092 535.4526 501.3777 542.5840
# 261       521.9809 507.2667 536.6951 499.4775 544.4843
# 262       521.9809 506.1212 537.8405 497.7256 546.2361
# 263       521.9809 505.0531 538.9087 496.0921 547.8697
# 264       521.9809 504.0485 539.9133 494.5556 549.4061
# 265       521.9809 503.0972 540.8646 493.1008 550.8610
