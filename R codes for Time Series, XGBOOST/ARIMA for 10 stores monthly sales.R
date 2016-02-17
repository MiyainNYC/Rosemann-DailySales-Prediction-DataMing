## install.packages("tseries)
library(tseries)

## salesbymonthtimeseries is stationary because of an extremely small p value
adf = adf.test(salesbymonthtimeseries)
adf

## why kpss gives a totally different result???
kpss = kpss.test(salesbymonthtimeseries)
kpss

## Based on the unit test results we identify whether the data is stationary or not. If the data is stationary then we choose optimal ARIMA models and forecasts the future intervals. If the data is non- stationary, then we use Differencing - computing the differences between consecutive observations. Use ndiffs(),diff() functions to find the number of times differencing needed for the data &  to difference the data respectively.
## because of a small value of p in the kpss test, we try differencing here
ndiffs(salesbymonthtimeseries)
diff(salesbymonthtimeseries)

plot.ts(salesbymonthdiff1)   ## a stationary model is returned
## confirm the result with adff.test() and kpss.test()

adf.test(salesbymonthdiff1)
kpss.test(salesbymonthdiff1)

## so ARIMA(p,1,q) 


##  By taking the time series of first differences, we have removed the trend component of the time series of sales, and are left with an irregular component. We can now examine whether there are correlations between successive terms of this irregular component; if so, this could help us to make a predictive model.



## to choose an optimal ARIMA model (p,d,q)
auto.arima(salesbymonthdiff1)       
   ## to check the ARIMA, p ad q value

acf(salesbymonthdiff1)
pacf(salesbymonthdiff1, lag.max=20)

forecast(auto.arima(salesbymonthdiff1))
plot(forecast(auto.arima(salesbymonthdiff1),h = 2))


## estimate the parameters of an ARIMA(p,d,q) model
salesbymontharima = arima(salesbymonthtimeseries,c(1,1,0))

## use the model to make predictions
salesbymontharimaforecast = forecast.Arima(salesbymontharima,h = 2)

salesbymontharimaforecast

plot.forecast(salesbymontharimaforecast)


##to investigate whether the forecast errors of an ARIMA model are normally distributed with mean zero and constant variance, and whether the are correlations between successive forecast errors.
acf(salesbymontharimaforecast$residuals, lag.max=20)

Box.test(salesbymontharimaforecast$residuals, lag=20, type="Ljung-Box")


## Since the correlogram shows that one of the sample autocorrelations for lags 1-20 exceeds the significance bounds, and the p-value for the Ljung-Box test is 0.03, we can conclude that there is some evidence for non-zero autocorrelations in the forecast errors at lags 1-20.

## what is autocorrelation??

plot.ts(salesbymontharimaforecast$residuals)
plotForecastErrors(salesbymontharimaforecast$residuals)

mean(salesbymontharimaforecast$residuals)


## the arima model turns out to be ok

## Conclusion:
###The histogram of forecast errors (above) shows that although the mean value of the forecast errors is positive, the distribution of forecast errors is skewed to the left compared to a normal curve. Therefore, it seems that we cannot comfortably conclude that the forecast errors are normally distributed with mean zero and constant variance!


