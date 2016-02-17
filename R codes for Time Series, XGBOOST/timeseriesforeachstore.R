## Fit a linear model with time series components. Time series linear model (TSLM) is used to fit linear 

## models to time series including trend and seasonality components.


library(data.table)
library(forecast)
library(plyr)

train = fread("train.csv")
store = fread("store.csv")
test = fread("test.csv")

train = merge(train,store,by = "Store")
test = merge(test,store,by = 'Store')

train[,Date = as.Date(Date)]
test[,Date = as.Date(Date)]

train = train[order(train$Store,train$Date)]
test = test[order(test$Store,test$Date)]

ts_fit = function(x){
  Sales = ts(x$Sales,frequency = 365)
  DayOfWeek = x$DayOfWeek
  Open = x$Open
  Promo = x$Promo
  StateHoliday = x$StateHoliday
  SchoolHoliday = x$SchoolHoliday
  fit = tslm(Sales~ trend +season+DayOfWeek+Open+Promo+StateHoliday+SchoolHoliday)
  return(fit)
}

out = dlply(merge3,.(Store),ts_fit)

ts_forecast = function(x,y){
  index <- x$Store[1]
  fitt <- y[[index]]
  return(data.frame(forecast(fitt, newdata = data.frame(DayOfWeek = x$DayOfWeek, 
                                                        Open = x$Open, Promo = x$Promo, 
                                                        StateHoliday = x$StateHoliday, 
                                                        SchoolHoliday = x$SchoolHoliday))))
}



predictions <- ddply(test, .(Store), ts_forecast, out)
predictions$Point.Forecast <- ifelse(predictions$Point.Forecast < 0, 0, predictions$Point.Forecast)

Avg_Sales <- train[,.(AS = mean(Sales,na.rm=T)),.(Store,DayOfWeek)]
test <- merge(test,Avg_Sales,by=c("Store","DayOfWeek"))
test <- test[order(Store,Date)]
test[,FPPredictions:=Open * predictions$Point.Forecast]
test[,FPredictions:=ifelse(is.na(predictions$Point.Forecast),AS,predictions$Point.Forecast)]

results <- data.frame(Id=test$Id, Sales=test$FPredictions)
results <- results[order(results$Id),]

write.csv(results, "Rossmann_TSLM.csv",row.names=F)
