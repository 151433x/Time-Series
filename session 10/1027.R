# session 10 box jenkins/ submitable for 10.27.21
# arima steps
# 1: differentiate with differencing
# 2: ar+ma
# 3: integrate + trend
#prework
library(fpp2)
rm(list=ls())
gc()
cat('\f')

#quartly loans
df=read.csv('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 7/QuarterlyLoans.csv')

anyNA(df)
str(df)
ggAcf(df)

#trend no seasonality
df.ts=ts(df,start = c(2001,1),frequency = 4)
#1 partition
#2 model fitting on training
#3 performance on test

#apply arima to forcast

#part 1 
test=tail(df.ts,max(length(df.ts)*.2,4))
train=head(df.ts,length(df.ts)-length(test))
#part2
model1=auto.arima(train,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)# exahustive search
model2=auto.arima(train,seasonal = FALSE)# rough search
#part3
model1pred=forecast(model1,length(test))
model2pred=forecast(model2,length(test))
#part4
accuracy(model1pred,test)[2,]
accuracy(model2pred,test)[2,]
#apply arima model 0,2,1 to forecast for 4 quarters
model=Arima(df.ts,order =c(0,2,1))
#rebuild
predictions=forecast(model,4)
predictions
autoplot(predictions)+autolayer(fitted(predictions),series = 'fitted')

# question2 
library(fpp2)
rm(list=ls())
gc()
cat('\f')
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 7/31.csv')
anyNA(df)
ggAcf(df,30)#trend no seasonal
df.ts=ts(df,start = c(1,1))
# partion
test=tail(df.ts,max(length(df.ts)*.2,5))
train=head(df.ts,length(df.ts)-length(test))
# step 2 
model1=auto.arima(train,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)# 4,2,0
model2=auto.arima(train,seasonal = FALSE)# 0,2,0
#step 3
model1pred=forecast(model1,length(test))
model2pred=forecast(model2,length(test))
#step4
accuracy(model1pred,test)[2,]
accuracy(model2pred,test)[2,]
#apply arima forecast for 5
model=Arima(df.ts,order = c(4,2,0))
predictions=forecast(model,5)

autoplot(predictions)+autolayer(fitted(predictions),series = 'fitted')

