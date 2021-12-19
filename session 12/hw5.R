#homework 5
library(fpp2) 
# Pre-work
rm(list=ls())  
gc()  
cat("\f")  
#load the data 
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 12/KSales.csv')
anyNA(df)
ggAcf(df,60) # seasonal with 12 
df.ts=ts(df,start = c(1997,1),frequency = 12)
# pick seaasonal as it has seasonal aspect
diff.df=diff(df.ts)
ggAcf(diff.df) # there is still seasonality after differnencing, however since it is seasonal differncing we will only differnce one time as that is a rule
# value of D is 1 
test <- tail(df.ts, max(length(df.ts)*0.2, 4)) 
train <- head(df.ts,length(df.ts) - length(test))

#train the arima model 
model1=auto.arima(train,stepwise = FALSE,approximation = FALSE) #1,0,1 0,1,1 [12] AIC with drift 6.9 AIC:984
model2=auto.arima(train) # 1,0,2  0,1,1 [12] 6.9 AIC:984, 12 is freq

model1pred=forecast(model1,length(test))
model2pred=forecast(model2,length(test))

accuracy(model1pred,test)[2,]
accuracy(model2pred,test)[2,] # both of the models are identical and thus they have the same MAPE of 5.92

model=Arima(df.ts,order = c(1,0,2),seasonal = c(0,1,1),method = 'ML')

predictions= forecast(model,6)
predictions
autoplot(predictions)+autolayer(fitted(predictions),series = 'fitted')

#part2 
#homework 5.2
library(fpp2) 
# Pre-work
rm(list=ls())  
gc()  
cat("\f")  
#load the data 
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 12/CollegeStation.csv')
anyNA(df)
ggAcf(df,24)# seasonality at 3 and 12 
df.ts=ts(df,start =c(2000,1),frequency = 12)

test <- tail(df.ts, max(length(df.ts)*0.2, 3)) 
train <- head(df.ts,length(df.ts) - length(test))

model1.seasonal=auto.arima(train,stepwise = FALSE,approximation = FALSE,seasonal = TRUE) #seasonal exhastive 0,1,3 0,1,1 AIC: 766
model2.seasonal=auto.arima(train,seasonal = TRUE)# seasonal rough 0,1,1 0,1,1 AIC: 771
model1.nonseasonal=auto.arima(train,stepwise = FALSE,approximation = FALSE,seasonal = FALSE) #4,1,1 AIC:1039
model2.nonseasonal=auto.arima(train,seasonal = FALSE)# 2,1,0 AIC:1041

model1.seasonal.pred=forecast(model1.seasonal,length(test))
model2.seasonal.pred=forecast(model2.seasonal,length(test))
model1.nonseasonal.pred=forecast(model1.nonseasonal,length(test))
model2.nonseasonal.pred=forecast(model2.nonseasonal,length(test))

accuracy(model1.seasonal.pred,test)[2,] # RMSE 84.54
accuracy(model2.seasonal.pred,test)[2,] # RMSE 83.04
accuracy(model1.nonseasonal.pred,test)[2,]# RMSE 190 
accuracy(model2.nonseasonal.pred,test)[2,]# RMSE 183

finalmodel= Arima(df.ts,order =c(0,1,1),seasonal = c(0,1,1))
predictions=forecast(finalmodel,3)
predictions
autoplot(predictions)+autolayer(fitted(predictions),series = 'fitted')

