# 11/3/21 Time serires 
#prework
rm(list=ls())
gc()
cat('\f')
library('fpp2')
#arimia seasonal

#must take out seasonal first, differncing using seasonal lags, not yt-yt-1 

#sawsales
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 6/SawsSales.csv')
ggAcf(df)
df.ts=ts(df,start = c(2002,1),frequency = 4)  
di=diff(df.ts,4)
ggAcf(di)
autoplot(di)
# this does elimnate seasonality completely

#a10
rm(list=ls())
gc()
cat('\f')
df=a10

ggAcf(df,24) # trend and seasonlaity
di=diff(df,12)
ggAcf(di)

#case 3 and 4 
rm(list=ls())
gc()
cat('\f')


df.ts=usmelec
ggAcf(df.ts) # seasonal at 12
di=diff(df.ts,12)
ggAcf(di)
#seasonal is needed
test=tail(df.ts,max(length(df.ts)*.2,12))
train=head(max(df.ts,length(df.ts)-length(test))

model1=auto.arima(train,stepwise = FALSE,approximation = FALSE) #1,0,2 0,1,1 [12] AIC 3282
model2=auto.arima(train) # 1,0,2  0,1,1 [12] 3282

model1pred=forecast(model1,length(test))
model2pred=forecast(model2,length(test))

accuracy(model1pred,test)[2,]
accuracy(model2pred,test)[2,]

model=Arima(df.ts,order = c(1,0,2),seasonal = c(0,1,1))

predictions= forecast(model,12)

autoplot(predictions)+autolayer(fitted(predictions),series = 'fitted')
#prework for ausbeer

