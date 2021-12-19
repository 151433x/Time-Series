library(fpp2)
rm(list=ls())
gc()
cat('\f')

df=read.csv("c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 10/DJTI.csv")
anyNA(df)
ggAcf(df,50) #trend 
#part 2
df.ts=ts(df,start = c(1,1),frequency = 1)
test=tail(df.ts,max(length(df.ts)*.2,1))
train=head(df.ts,max(length(df.ts)-length(test)))
#part 3
model1=auto.arima(train,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)#exahustive search gives 0,1,0
model2=auto.arima(train,seasonal = FALSE)#rough search gives, 0,1,0
#part 4
model1pred=forecast(model1,length(test))
model2pred=forecast(model2,length(test))
#part5 
accuracy(model1pred,test)[2,]
accuracy(model2pred,test)[2,]
#part6
model=Arima(df.ts,order = c(0,1,0))
predictions=forecast(model,1)
#plot
autoplot(predictions)+autolayer(fitted(predictions),series='fitted')
