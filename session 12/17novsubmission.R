#session 13 non linear regression on ts 
library('fpp2')
rm(list=ls())
gc()
cat('\f')
df=read.csv('c:/Users/xtorres1/Desktop/R/Datasets/Time Series/session 12/Sales_Expen.csv')

ggAcf(df$�..Sales,24)
ggAcf(df$Expend.,24)
plot(df$�..Sales,df$Expend.)
df.ts=ts(df[,1:2])

# nonlinear regression
test=tail(df.ts,max(nrow(df.ts)*.2,2))
train=head(df.ts,nrow(df.ts)-nrow(test))

#1/x
which(df.ts[,"Expend."]==0) # no zeros
df.tslm=tslm(�..Sales~I(1/Expend.),train)
summary(df.tslm)
checkresiduals(df.tslm)
#log 
min(df.ts[,"Expend."]) # all x's are positive
logx=tslm(�..Sales~I(log(Expend.)),train)
summary(logx)
checkresiduals(logx)
#linear regrsion
linear=tslm(�..Sales~Expend.,train)
summary(linear)
checkresiduals(linear)
#
#
min(df.ts[,c('Expend.')])
sqrt_x=tslm(�..Sales~I(sqrt(Expend.)),train)
summary(sqrt_x)
checkresiduals(sqrt_x)
# adequate 
x_sq=tslm(�..Sales~I(Expend.^2),train)
summary(x_sq)
checkresiduals(x_sq)
#adequate 

#make predictions on test set 
pred.ratio=forecast(df.tslm,as.data.frame(test))
pred.log=forecast(logx,as.data.frame(test))
pred.sqrt=forecast(sqrt_x,as.data.frame(test))
pred.sq=forecast(x_sq,as.data.frame(test))

accuracy(pred.ratio,test[,"�..Sales"])[2,]
accuracy(pred.log,test[,"�..Sales"])[2,]
accuracy(pred.sqrt,test[,"�..Sales"])[2,]
accuracy(pred.sq,test[,"�..Sales"])[2,]

new=data.frame(Expend.=c(14.5,13))
forecast(df.tslm,new)
#double check on time stamps, should be revised to 15 and 15
