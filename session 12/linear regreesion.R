#session 12
# regression with time series 
#prework
rm(list=ls())
gc()
cat('\f')
library('fpp2')
#load data 
df=uschange
anyNA(df)
str(df)
ggAcf(df[,1],50)# trend, consumption
ggAcf(df[,2],50)# white noise, income 
ggAcf(df[,3],50)# no trend, production
ggAcf(df[,4],50)# seasonal, savings 
ggAcf(df[,4],50)# seasonal, unemployment

df.ts=ts(df,start = c(1,1),frequency = 1)
#wrong
df.ts=ts(df[,1:5],start = c(1,1),frequency = 1)
autoplot(df.ts)
autoplot(df.ts[,1:5])
#wont work with dataframes only ts objects
library('GGally')
ggpairs(as.data.frame(df))

#step 1 
test=tail(df,max(length(df)*.2,4))
# incorrect
# if multiple ts variables in the ts then use nrows, but it will be wrong if there is only one collumns
test=tail(df,max(nrow(df)*.2,4))
train=head(df,nrow(df)-nrow(test))
#step2 fit model on train
model1=tslm(Consumption~Income+Unemployment+Savings,train)
model2=tslm(Consumption~Income+Unemployment,train)
model3=tslm(Consumption~Income+Savings,train)
summary(model1)
summary(model2)
summary(model3)
checkresiduals(model1) # not adequate
checkresiduals(model2) # adequate
checkresiduals(model3) # not adequate
# adequate 

#step3 test model on test set 

predict=forecast(model2,data.frame(test)) # need to convert to data freame if test has multiple columns


#produce errors
accuracy(predict,test[,"Consumption"])[2,]                 


### 
#session 13
# regression with time series 
#prework
rm(list=ls())
gc()
cat('\f')
library('fpp2')
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 12/myTS.csv')
# predict for 4 quarters need new values for adbudget and gdp to predict
new=data.frame(AdBudget=c(650,700,550,500),GDP=c(250,200,350,400))
forecast(mode,new)                 
#manually change time stamps to 2006 q1-q4
time(df.ts)
