# saw sales 10/6
#pre work
rm(list=ls()) #clear workspace
gc()#garbage collections/ clear occupied memeory
cat("\f") #clear console
#import the data and library
library('fpp2')
setwd('C:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 6/')
Sales.df=read.csv('SawsSales.csv')
# session 8 
ggAcf(Sales.df,lag=24)

sales.ts=ts(Sales.df,start=c(2002,1),frequency = 4)

# step 1: partion the data
#test
test= tail(sales.ts,max(length(sales.ts)*.2,4))
#training
train=head(sales.ts,length(sales.ts)-length(test))

#step 2: forecast on test and & adequecy check

AVG=meanf(train,length(test))
checkresiduals(AVG)

#step 4: performance

accuracy(AVG,test)[2,]

#predict 
predictions=meanf(sales.ts,4)
