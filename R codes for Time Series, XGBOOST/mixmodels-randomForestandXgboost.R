# Use only train data and not test data
# dum_train = (train + store data --> Add more Features ---> Convert to one-hot encoded)

########## Libraries ##############
library(xgboost)
library(Metrics)   
library(randomForest)

setwd("~/rossmanstoresales/")

outcomeName<-c('sales')
remaining = !(names(dum_train) %in% outcomeName)
predictors<-names(dum_train)[remaining]
predictors

# xgboost parameters
lastdepth=10
lastround = 250
lasteta=0.05
lastlambda=0.05
samplesize = 0.6

# select 40% of train data for model mixing and rest 60% for evaluation
noofrows<- floor(nrow(dum_train) * 0.4)
trainon<-dum_train[1:noofrows,]
teston<-dum_train[-c(1:noofrows),]

# First Predict using xgb
# Here is the model
bst<-xgboost(data = as.matrix(trainon[,predictors]),
             label = trainon[,outcomeName],
             max.depth = lastdepth, nround=lastround,eta=lasteta,subsample = samplesize,lambda=lastlambda,
             objective= "reg:linear",verbose = 0)

# And here is prediction
pred_xgb<-predict(bst,as.matrix(teston[,predictors]),outputmargin=TRUE)

# Build another model with randomForest
rf = randomForest(  log(sales+1) ~., data=trainon, ntree = 300, do.trace = TRUE, sampsize=80000)
# Make prediction
pred_rf = exp( predict(rf, teston[,predictors], type = "response")) - 1


# Generate weight sequence
wt<-seq(from=0, to = 1, by = 0.01)
lasterr=2000

# Write rmse on file
sink(file="mixererror.txt" )
   # File headers. 
  print(paste("weight_rf","lasterror",sep=","))          
sink()
last_i = 0

# Go through the weights one by one
for (i in wt) {
  # Combine predictions
  fpred<- (i * pred_rf + (1-i) * pred_xgb)
  # Calculate error
  error<-rmse(teston$sales,fpred)
  if (error < lasterr) {
    lasterr= error  
    last_i = i
    sink(file="mixerror.txt",append = TRUE )
      print(paste(i,error,sep=","))       
    sink()
  }
}

# Now read full results for test data as predicted by randomForest and xgb modeling
result_randomForest23112015 <- read.csv("~/rossmanstoresales/result_randomForest23112015.csv")
result_xgb_23112015 <- read.csv("~/rossmanstoresales/result_xgb_23112015.csv")
# Mix them and write
result<-data.frame(cbind("Id"=result_randomForest23112015$Id, "Sales"=result_randomForest23112015$Sales * last_i + result_xgb_23112015$Sales * (1-last_i)))
write.csv(result,file="result_combined_23112015.csv",row.names = FALSE)

