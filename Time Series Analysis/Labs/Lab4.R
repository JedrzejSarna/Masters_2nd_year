setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

help(AirPassengers)
plot(AirPassengers) 

library(forecast)
help(BoxCox)
BC_AP<-BoxCox(AirPassengers,lambda="auto") # A time series with constant(?) variance 



par(mfrow=c(2,1))
plot(AirPassengers)
plot(BC_AP)

hist(AirPassengers)
hist(BC_AP)
#we suppose that this time series ahs trend and seasonality

#Estimation of Trend and Seasonal Components
w= c(.5,rep(1,11),.5)/12    #weights of moving average filter
print(w)
par(mfrow=c(1,1))
#STEP 1 - use moving average filter to eliminate seasonal component with period 12 (in our case yearly)
MAF_AP<-filter(BC_AP,filter=w,sides=2) # moving average filter
plot(MAF_AP)



#STEP 2 - compute w_k - the average of residuals, estimate seasonal component s_k
w_AP<- c(rep(0,12)) # It will be w_k k from the lecture. Later also s_k for k = 1, .., 12; k=1,..., d; d=12
for(k in 1:6){
  for(j in 1:11 ){
    w_AP[k]<- w_AP[k]+BC_AP[k+j*12]-MAF_AP[k+j*12]
  }
  
}

for(k in 7:12){
  for(j in 0:10 ){
    w_AP[k]<- w_AP[k]+BC_AP[k+j*12]-MAF_AP[k+j*12]
  }
  
}
w_AP=w_AP/12

w_AP=w_AP-mean(w_AP) # s_k for k=1, ... , d
s_AP<-rep(w_AP,12) #s_k for all d
print(s_AP)

#STEP 3 - estimate trend m_t from deseasonalized data
res_AP<-BC_AP-s_AP  # deseasonalized data
trend_AP<-filter(res_AP,filter=rep(1/7,7),sides=2) # Estimation of trend from deseasonalized data
print(trend_AP)

#FINAL - estimate and plot the noise series

residuals_AP<-BC_AP-trend_AP-s_AP

plot(residuals_AP) 

par(mfrow=c(3,1))

s_AP<-ts(s_AP, start=c(1949,1),frequency=12)
plot(trend_AP, main="trend"); plot( s_AP, main="seasonal component"); plot(residuals_AP, main="residuals")

acf(na.omit(residuals_AP), lag.max = 40) #Watch out for missing data


#NEW EXERCISE
D12_AP<-diff(BC_AP,lag=12)

par(mfrow=c(2,1))
plot(BC_AP)
plot(D12_AP)


par(mfrow=c(2,2))

for (k in 1:4) {
  plot(diff(D12_AP,lag=1,differences = k))
}

par(mfrow=c(2,1))

acf(D12_AP, lag.max = 40) #Watch out for missing data
acf(diff(D12_AP), lag.max = 40) #Watch out for missing data
D12_AP
library(randtests)
difference.sign.test(D12_AP)

mu_s = 0.5*129
std_s = 131/12

S <- rnorm(100000,mu_s, std_s)
S

temp <- c()
for (i in 1:length(S)) {
  temp[i] <-abs(S[i]-mu_s)/sqrt(std_s)>1.96
}
temp
sum(temp)
