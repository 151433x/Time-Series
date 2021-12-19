# Session 12 Case 2
library(fpp2) 

# Pre-work
rm(list=ls())  
gc()  
cat("\f")  

df <- uschange
df.ts <- ts(df, start=c(1,1), frequency=1)
# turn into a ts object;

##### add lagged values #####
df.ts <- cbind(Income = df.ts[,"Income"],
                 Savings = df.ts[,"Savings"],
                  Consumption=df.ts[,"Consumption"],
                 lag1 = stats::lag(df.ts[,"Unemployment"],-1),
                 lag2 = stats::lag(df.ts[,"Unemployment"],-2))
# add lagged variables to the ts object
df.ts

df.ts <- na.omit(df.ts)
# delete records with NA
df.ts

####### Multiple Regression

## Step 1: partitioning
test <- tail(df.ts, max(nrow(df.ts)*0.2, 4)) 
test

training <- head(df.ts,nrow(df.ts) - nrow(test))
training

## Step 2: fit models on training

Model_lag1 <- tslm(Consumption ~ Income+Savings+lag1, training)
# no need to add trend or season
summary(Model_lag0)
# adequate, not spurious, well explained

Model_lag2 <- tslm(Consumption ~ Income+Savings+lag2, training)
# no need to add trend or season
# check residuals
checkresiduals(Model_lag1) # not adequate 
checkresiduals(Model_lag2) # not adequate 

# adequate, not spurious, well explained

## Step 3. predict on validation
Pred_lag1 <- forecast (Model_lag1,data.frame(test)) # mape of 77
Pred_lag2 <- forecast (Model_lag2,data.frame(test)) 

##  Step 4. performance measurement
accuracy(Pred_lag1,test[,"Consumption"])[2,] # mape of 77
accuracy(Pred_lag2,test[,"Consumption"])[2,]# mape of 80

## forecast for 4 quarters into the future based on 
# given/forecasted predictor values
New <- data.frame(Income = c(.04, -.012,.03,0,.01), 
                  lag1 = c(0,-.01,.05,.08, .009), lag2 = c(0,-.01,.05,.08, .009),Savings=c(.05,-.01,.03,.04,.015))

predictions1 <- forecast (Model_lag1,New) 
predictions
predictions2=forecast(Model_lag2,New)
predictions2
time(myTS_ts) # return time stamps of the original ts object
# time stamps should be adjusted to 2006Q1-2006Q4
