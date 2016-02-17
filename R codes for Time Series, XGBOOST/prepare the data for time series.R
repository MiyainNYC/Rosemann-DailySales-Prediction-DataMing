##   install.packages("lubridate") ## to decompose the date
##   install.packages('doBy') ## to order the date

## read the data

train <- fread('train.csv')
test <- fread('test.csv')
store <- fread('store.csv')

merge2 <-fread('merge2.csv')
## ....
merge2[order(merge2$Date),]


##  add features:
merge2$CompetitionOpen = 12 * (merge2$y - merge2$CompetitionOpenSinceYear) + (merge2$m - merge2$CompetitionOpenSinceMonth)
merge2$weekofyear = week(merge2$Date)
merge2$Promo2Open = 12 * (merge2$y - merge2$Promo2SinceYear) + (merge2$weekofyear - merge2$Promo2SinceWeek)/4
merge2$CompetitionOpen[which(merge2$CompetitionOpen<0)]=0
merge2$Promo2Open[which(merge2$Promo2Open<0)]=0

## add parameters to the dataset test

test = merge(test,store,by = "Store")

library(lubridate)
y = ymd(salesbystate$Date)
y = data.frame(y=year(y), m=month(y), d=day(y))

test = cbind(test,y)
test$CompetitionOpenSinceMonth = as.integer(test$CompetitionOpenSinceMonth)
test$CompetitionOpenSinceYear = as.integer(test$CompetitionOpenSinceYear)
test$CompetitionOpen = 12 * (test$y - test$CompetitionOpenSinceYear) + (test$m - test$CompetitionOpenSinceMonth)

test$CompetitionOpen[which(test$CompetitionOpen<0)]=0

## subtract store b
## why b?? some visualization work here

storeb = subset(store,store$StoreType=='b')

str(storeb)
## 17 observations

storea = subset(store,store$StoreType=='a')
str(storea)

## subtract store type a, 602 observations, we choose b then


## subtract storetype b from the train dataset

trainstoreb = train[train$Store %in% storeb$Store,]

table(is.na(trainstoreb$Sales))
table(trainstoreb$Sales==0)
## no missing value but has 270 records with sales = 0
table(trainstoreb$Customers==0)
##269 records with sales =0


## validated:269 records with no sales nor no customers
storebnosalesnocustomers = trainstoreb[trainstoreb$Sales==0 & trainstoreb$Customers == 0,]
str(storebnosalesnocustomers)


## 7 store out of 17 have 0 value in the corresponding metrics
table(storebnosalesnocustomers$Date)
table(storebnosalesnocustomers$Store)


## subtract those 7 stores from the dataset storeb and trainstoreb--->>trainstorebconsecutive

storebconsecutive = storeb[!(storeb$Store %in% storebnosalesnocustomers$Store),]
trainstorebconsecutive = trainstoreb[!(trainstoreb$Store %in% storebnosalesnocustomers$Store),]
str(trainstorebconsecutive)
length(unique(trainstorebconsecutive$Store))

## 7161 out of 9420 have records of unique sales numbers
length(unique(trainstorebconsecutive$Sales))

## 3135 out of 9420 have unique records of customers
length(unique(trainstorebconsecutive$Customers))

## transform the date format
library(doBy)
trainstorebconsecutive$Date = as.Date(trainstorebconsecutive$Date)
orderBy(~Date,trainstorebconsecutive)

## locate the time period of the dataset 'trainstorebconsecutive$Date: 2013-1-1 to 2015-7-31

trainstorebconsecutive$Date[which.max(trainstorebconsecutive$Date)]
trainstorebconsecutive$Date[which.min(trainstorebconsecutive$Date)]


## aggregate the sales of 10 stores in the dataset of trainstorebconsecutive by date.
aggregatesalesbydate = aggregate(Sales~Date,trainstorebconsecutive,sum)
str(aggregatesalesbydate)
aggregatesalesbydate$Date = NULL

plot(decompose(salesbydatetimeseries),col = "blue")


## decompose the date 

library("lubridate")
dates = trainstorebconsecutive$Date
x = ymd(dates)
date = data.frame(y=year(x),m=month(x),d=day(x))
                  
                                             

## append the decomposed date into the data frame of trainstorebconsecutive
trainstorebconsecutive = cbind(trainstorebconsecutive,date)
str(trainstorebconsecutive)
## get rid of the specific date
trainstorebconsecutive$d=NULL

## calucalute sum of sales and mean of sales by year 

aggregatesalesbyYear = aggregate(Sales~y,trainstorebconsecutive,sum)
aggregatemeansalesbyYear = aggregate(Sales~y,trainstorebconsecutive,mean)

## combine two date matrix for future use
trainstorebconsecutive$year.month = paste(trainstorebconsecutive$y,trainstorebconsecutive$m,sep = '.')

## calculate sum of sales and mean of sales by year.month



aggregatemeansalesbyMonth = aggregate(Sales~year.month,data = trainstorebconsecutive,mean)

aggregatesalesbyMonth = data.frame(Sales = c(3425724,3299770,3908490,3576190,3894977,3620781,3642310,3574125,3507661,3751328,3783105,4351419,3596916,3417533,3733222,3823187,3979052,3867008,3771998,3687098,3599748,3806234,3941317,4386531,3741876,3551807,4002025,3991326,4099664,3932272,3979796))
aggregatesalesbyMonth




## let's first look deeper into the ten stores (storebconsecutive) before we squeeze them into a time series model
## visualization can be used in this stage


table(storebconsecutive$Assortment)
table(storebconsecutive$CompetitionDistance)
table(storebconsecutive$CompetitionOpenSinceMonth)
table(storebconsecutive$CompetitionOpenSinceYear)
table(storebconsecutive$Promo2)
table(storebconsecutive$Promo2SinceWeek)
table(storebconsecutive$PromoInterval)

## write the storebconsecutive into csv file to try to do visualization with other tools like excel or tableau
write.csv(storebconsecutive,file = "storebconsecutive.csv")


##prepare the  data for time series model


aggregatesalesbyYearprepared = aggregatesalesbyYear$Sales


## load the data into time series model
salesbyyeartimeseries = ts(aggregatesalesbyYearprepared,frequency = 1,start = c(2013))

salesbymonthtimeseries = ts(aggregatesalesbyMonth,frequency = 12,start = c(2013,1))
                                                                          
                                                                                           
## plot the time series model
plot.ts(salesbymonthtimeseries,ylab = "salesbyMonth")
plot.ts(salesbyyeartimeseries,ylab = "salesbyYear")

## decompose the time series model

decomposedsalesbymonth = decompose(salesbymonthtimeseries)
plot(decomposedsalesbymonth)
