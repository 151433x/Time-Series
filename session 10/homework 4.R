#homework 4 for time series 

#prework
library('fpp2')
rm(list=ls())
gc()
cat('\f')

#import the data

df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 10/annualTemp.csv')
anyNA(df)
ggAcf(df) #got wrong order and years that are not needed 
#order the data
df=df[order(df$Year),]
df$Year=NULL
ggAcf(df)# trend found in data

#step 1 turn
df.ts=ts(df,start = c(1,1))
ggAcf(df.ts)
diff=diff(df.ts,1)
ggAcf(diff)
#create train and test set
test=tail(df.ts,max(length(df.ts)*.2,5))
train=head(df.ts,length(df.ts)-length(test))
#train model on training
model1=auto.arima(train,seasonal=FALSE,stepwise=FALSE,approximation=FALSE)# exahustive 1,1,3 157 AIC
model2=auto.arima(train,seasonal=FALSE)# non exahustive 1,1,3 157 AIC

model1pred=forecast(model1,length(test))
accuracy(model1pred,test)[2,] 

model=arima(df,order = c(1,1,3))
forecast1=forecast(model,5)

autoplot(forecast1)+autolayer(fitted(forecast1),series = 'fitted')

#homework 4 for time series 

#prework
library('fpp2')
rm(list=ls())
gc()
cat('\f')
#import the data
# problem 2
df=WWWusage # next 10
ggAcf(df,60) # trend
d1=diff(df)
ggAcf(d1)
d2=diff(d1)
ggAcf(d2)

test=tail(df,max(length(df)*.2,10))
train=head(df,length(df)-length(test))

model1=auto.arima(train,seasonal=FALSE,stepwise=FALSE,approximation=FALSE)# exahustive 1,1,1 no drift and AIC of 412
model2=auto.arima(train,seasonal=FALSE)# same model

model1pred=forecast(model1,length(test))
accuracy(model1pred,test)[2,] 

#homework 4 for time series 

#prework
library('fpp2')
rm(list=ls())
gc()
cat('\f')
#import the data
# problem 3
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 10/SeaLevel1000.csv')
ggAcf(df)
df.ts=ts(df[,3],start = c(1936,10),frequency = 12)
ggAcf(df.ts,1000) # no seasonality but trend

test=tail(df.ts,max(length(df.ts)*.2,12))
train=head(df.ts,length(df.ts)-length(test))

model1=auto.arima(train,seasonal=FALSE,stepwise=FALSE,approximation=FALSE)# exahustive 3,1,1 with no drift and an AIC 2717 
model2=auto.arima(train,seasonal=FALSE)# 0,1,5 with drift of 2e-04 and an AIC of 2717

model1pred=forecast(model1,length(test))
model2pred=forecast(model2,length(test))
accuracy(model1pred,test)[2,] # mae .08 
accuracy(model2pred,test)[2,] # mae of .06

model=arima(df.ts,order = c(0,1,5))
forecast1=forecast(model,length(test))
autoplot(forecast1)+autolayer(fitted(forecast1),series = 'fitted')
