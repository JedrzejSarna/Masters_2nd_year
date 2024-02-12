
library(astsa)
plot.ts(varve)
abline(h=mean(varve),col="966")

LV<- log(varve)
plot.ts(LV)
abline(h=mean(LV),col="966")

par(mfrow=c(1,2))
LM=80 # lag.max

acf(LV, lag.max = LM, main="")
pacf(LV, lag.max = LM, main="")

library(tseries)
adf.test(LV)

#cyclicity check
par(mfrow=c(3,2))

n = length(LV)
Per= Mod(fft(LV-mean(LV)))^2/n  # 
Freq = (1:n -1)/n
plot(Freq[1:n], Per[1:n], type='h', lwd=2, ylab="Periodogram", xlab="Frequency")    # type='h' for histogram-like vertical lines, lwd a vector of line widths, i.e widths of vertical lines

for ( i in 1:5 ) {
  mvspec(LV, kernel=kernel("daniell",c(1,i)), log = "no")
}

#identyfying the orders of the model
DLV <- diff(log(varve));

par(mfrow=c(1,1))
plot.ts(DLV); abline(h=mean(DLV),col="966")

par(mfrow=c(1,2))
LM=40 # lag.max

acf(DLV, lag.max = LM, main="")
pacf(DLV, lag.max = LM, main="")

adf.test(DLV)


#sarima

library(astsa)

mA<-sarima(USAccDeaths,0,1,1,1,1,0,12)# p,d,q, P,D,Q, s

mM<-sarima(USAccDeaths,0,1,1,0,1,1,12)# p,d,q, P,D,Q, s

#Both models fit well. So let us use order selection criteria.

c(mA$AIC, mA$AICc,mA$BIC);c(mM$AIC, mM$AICc,mM$BIC)

mM$ttable # to check if coefficients are significant 

modelll<-arima(USAccDeaths, order=c(0,1,1),seasonal = list(order = c(0,1,1), period = 12))# order=(p,d,q), seasonal=list(order=c(P,D,Q),period=s)

library(forecast)
par(mfrow=c(1,1))
future<-forecast(modelll,h=12,level=95)
plot(future)

pred<-sarima.for(USAccDeaths,n.ahead=12,0,1,1,0,1,1,12)
