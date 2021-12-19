library(fpp2)
rm(list=ls())
gc()
cat('\f')

#load
df.ts=uschange
anyNA(df.ts)
ggAcf(df.ts,40)
str(df.ts)
consumption=window(df.ts,start=c(1970,1),end=c(2016,))

