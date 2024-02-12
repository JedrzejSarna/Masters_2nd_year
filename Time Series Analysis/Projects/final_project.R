setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

library(forecast)
?BoxCox.lambda

df <- read.csv("dane_AJWP.csv")
TS <- ts(df$numbers.of.V., frequency = 12, start = c(2012, 1))

par(mfrow=c(1,1))
plot(TS)

library(forecast)

lambda <- BoxCox.lambda(TS)
lambda

BC_TS<-BoxCox(TS,lambda = 'auto') # A time series with constant(?) variance 

TS
BC_TS

par(mfrow=c(2,1))
plot(TS)
plot(BC_TS)

library(astsa)
?astsa
