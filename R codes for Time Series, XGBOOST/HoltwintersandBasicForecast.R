library(forecast)

##basic forecasts using Meanf(), naïve(), random walk with drift - rwf() methods

mf = meanf(salesbymonthtimeseries,h=2,level=c(90,95),fan=FALSE,lambda=NULL)
plot(mf)

mn = naive(salesbymonthtimeseries,h=2,level=c(90,95),fan=FALSE,lambda=NULL)
plot(mn)

md = rwf(salesbymonthtimeseries,h=2,drift=T,level=c(90,95),fan=FALSE,lambda=NULL)
plot(md)


## evaluate the performance of basic methods

accuracy(mf)
accuracy(mn)
accuracy(md)

plot.ts(salesbymonthtimeseries,ylab = "salesbyMonth")


## Holtwinters forecast

salesbymonthtimeseriesholtwiners = HoltWinters(salesbymonthtimeseries)
plot(salesbymonthtimeseriesholtwiners)

## take a look at salesbymonthtimeseriesholtwinters
salesbymonthtimeseriesholtwiners$SSE
salesbymonthtimeseriesholtwiners ##  alpha: 0.09613093 beta : 0.1127759  gamma: 1

## The value of beta is low, indicating that the estimate of the slope b of the trend component is not updated very much over the time series, and instead is set basically equal to its initial value, which in some way makes this model not very good. from the decomposed time series, we can see that the increasing trend is instead obvious
## the value of gamm is high, indicating that the estimate of the seasonal component at the current time point is just based upon very recent observations.
## the value of alpha is low, indicating that the level at the current point is based upon some observations in the distant past.


## to just see the forecast part holtwinters

salesbymonthtimeseriesholtwiners2 = forecast.HoltWinters(salesbymonthtimeseriesholtwiners,h = 2)
plot.forecast(salesbymonthtimeseriesholtwiners2)

##evaluate the holtwinters forecasting
acf(salesbymonthtimeseriesholtwiners2$residuals, lag.max=20)  ## to calculate a correlogram of the in-sample forecast errors for the data for lags 1-20

## plot the distribution of residuals
## define the function of plotForecastErrors


plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plot.ts(salesbymonthtimeseriesholtwiners2$residuals) ## the residuals don't stay constant over time

plotForecastErrors(salesbymonthtimeseriesholtwiners2$residuals) ## to plot the histgram of residuals


## conclusion:   holt winter doesn't work out



  
  