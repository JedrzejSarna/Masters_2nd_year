setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

plot(AirPassengers) 

library(forecast)
lambda <- BoxCox.lambda(AirPassengers,lower=0)
lambda
#Since lambda is close to zero, we can use logarithmic transforamation.
lambda<-0

BC_AP<-BoxCox(AirPassengers,lambda) # A time series with constant(?) variance 
plot(BC_AP)

D12_AP<-diff(BC_AP,lag=12)

par(mfrow=c(2,1))
plot(BC_AP)
plot(D12_AP)

par(mfrow=c(1,2))
LagMax<-48
acf(na.omit(D12_AP), lag.max=LagMax) #Watch out for missing data
pacf(na.omit(D12_AP), lag.max=LagMax) #Watch out for missing data

mean(D12_AP)
#fit the deseasonalized data to AR(1)
modelll<-arima(D12_AP,order=c(1,0,0),include.mean =TRUE) # Since mean(D12_AP is not zero we put include.mean =TRUE  )
future<-forecast(modelll,h=12,level=95 )
par(mfrow=c(1,1))
plot(future)

future$mean

A_future<-c(rep(0,12))

for(i in 1:12) {
  A_future[i]=exp(future$mean[i]+BC_AP[144-12+i])
}
A_future
#calculate bounds of confidence intervals.
CI_future<-data.frame(L=c(rep(0,12)),prediction=A_future,U=c(rep(0,12)))

for(i in 1:12) {
  CI_future$L[i]=exp(future$lower[i]+BC_AP[144-12+i])
  CI_future$U[i]=exp(future$upper[i]+BC_AP[144-12+i])
}
CI_future<-ts(CI_future,start=c(1961,1),end = c(1961,12),frequency = 12)
CI_future

#---- difference of deseasonalized data

D12_AP_diff <- diff(diff(BC_AP,lag=12),lag=12)
par(mfrow=c(3,1))
plot(BC_AP)
plot(D12_AP)
plot(D12_AP_diff)

par(mfrow=c(1,2))
LagMax<-48
acf(na.omit(D12_AP_diff), lag.max=LagMax) #Watch out for missing data
pacf(na.omit(D12_AP_diff), lag.max=LagMax) #Watch out for missing data
mean(D12_AP_diff)
#fit the deseasonalized data to AR(1)
modelll_2<-arima(D12_AP_diff,order=c(1,0,0),include.mean =TRUE) # Since mean(D12_AP is not zero we put include.mean =TRUE  )
future_2<-forecast(modelll_2,h=12,level=95 )
par(mfrow=c(1,1))
plot(future_2)


par(mfrow=c(2,1))
plot(future)
plot(future_2)

# COMPANY SALES TRY

data <- read.csv('Month_Value_1.csv')
data$Sales_quantity
par(mfrow=c(1,1))
plot(ts(data$Sales_quantity))

sum(is.na(data$Sales_quantity))

TS<-ts(data$Sales_quantity, start=c(2015,1), end=c(2020,4), frequency = 12) # The first month is January 2015 (see start=...) and there is  12 values for the year.
plot(TS)

lambda <- BoxCox.lambda(TS)
lambda

BC_TS<-BoxCox(TS,lambda = 'auto') # A time series with constant(?) variance 
plot(BC_TS)

par(mfrow=c(1,1))
plot(TS)

plot(BC_TS)

D12_TS<-diff(BC_TS,lag=12)

par(mfrow=c(2,1))
plot(BC_TS)
plot(D12_TS)

par(mfrow=c(2,1))

LagMax<-48
acf(na.omit(D12_TS), lag.max=LagMax) #Watch out for missing data
pacf(na.omit(D12_TS), lag.max=LagMax) #Watch out for missing data

mean(D12_TS)

#fit the deseasonalized data to MA(2)
modelll_TS<-arima(D12_TS,order=c(0,0,2),include.mean =TRUE)
future_TS<-forecast(modelll_TS,h=32,level=95 )
par(mfrow=c(1,1))
plot(future_TS)
future_TS

future_TS$mean

A_future_TS<-c(rep(0,32))

for(i in 1:32) {
  A_future_TS[i]=exp(future_TS$mean[i]+BC_TS[64-32+i])
}
A_future_TS


