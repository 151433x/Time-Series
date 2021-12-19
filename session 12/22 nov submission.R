# session 13 log log and 4 common tslm s 
library('fpp2')
rm(list=ls())
gc()
cat('\f')
df=marathon
anyNA(df)
str(df)
ggAcf(df,24) # trend and 1 seasonality
df.ts=ts(df,start =c(1897,1),frequency = 1)
df.window=window(df.ts,start=c(1930,1))

ggAcf(df.window,24) # trend and regular seasonality
times=ts(cbind(df.window,t=seq(from=1, to=length(df.window))))

test=tail(times,max(nrow(times)*.2,2))
train=head(times,nrow(times)-nrow(test))
# 1/x
ratio=tslm(df.window~I(1/t),train)
summary(ratio)
checkresiduals(ratio)
#log 
logx=tslm(df.window~I(log(t)),train)
summary(logx)
checkresiduals(logx)
#
# sqrtx
sqrtx=tslm(df.window~I(sqrt(t)),train)
summary(sqrtx)
checkresiduals(sqrtx)
# adequate and pretty good 
#xsq
xsq=tslm(df.window~I(t^2),train)
summary(xsq)
checkresiduals(xsq)
#not adequeate
predict.ratio=forecast(ratio,as.data.frame(test))
predict.logx=forecast(logx,as.data.frame(test))
predict.sqrt=forecast(sqrtx,as.data.frame(test))
predict.sq=forecast(xsq,as.data.frame(test))

accuracy(predict.ratio,test['df.window'])[2,]
