souvenirs = scan("http://robjhyndman.com/tsdldata/data/fancy.dat")
souvenirstimeseries = ts(souvenirs, frequency = 12, start = c(1987,1))
plot(souvenirstimeseries)
## a time series that can be descibed using an addictive model with increasing or decreasing trend and seasonality, you can use Holt-winters exponential smoothing to make short forecasts.
##Holt-winters exponential smoothing estimates the level, slope and seasonal component at the current time point
## holt-winters exponential is controlled by three parameters: alpha bata and gamma. all three parameters have value between 0 and 1, which means
## relatively little weight is placed on the most recent observations when making forecasts of future values

souvenirstimeseries = log(souvenirstimeseries)
souvenirstimeseries
souvenirstimeseriesforecasts = HoltWinters(souvenirstimeseries)
souvenirstimeseriesforecasts$SSE

souvenirstimeseriesforecasts


## whether this model can be improved, we can use non-zero autocorrelations at lags l-20, by makinga correlogram and carrying out the Ljung-box test

acf(souvenirstimeseriesforecasts2$residuals, lag.max=20)
Box.test(souvenirstimeseriesforecasts2$residuals, lag=20, type="Ljung-Box")

## to check whether the forecast errors have constant variance over time. we should checkwhether they are normally distributed with zero 

plot.ts(souvenirstimeseriesforecasts2$residuals)
plot.forecast(souvenirstimeseriesforecasts2$residuals)

## conclusion:normally distributed means that holt-winter exponential smoothing provides an adequate predictive model of the log of sales

