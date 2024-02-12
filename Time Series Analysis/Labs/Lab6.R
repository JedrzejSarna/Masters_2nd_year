setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

armapq<-arima.sim(list(order=c(2,0,1),ar=c(-1,-.25),ma=.49),n=110) #generate dataset
help("arima")
?arima.sim

modelll<-arima(armapq[1:100],order=c(1,0,0),include.mean =FALSE) #set to FALSE to get AR(1)

modelll$coef

modelll$sigma2

future <- predict(modelll,n.ahead = 10)
future #gives two time series lists one with prediction and second one with its standard deviation

plot.ts(armapq)
lines(future$pred, col="red")

#now add limit lines, the confidence intervals

U<-future$pred+future$se
L<-future$pred-future$se
xx<-c(time(U),rev(time(U)))
yy<-c(L,rev(U))

U
rev(U) #reverse

plot.ts(armapq)
?polygon
polygon(xx,yy,col="lightgray",border="gray")
lines(future$pred, col="red")
lines(as.ts(armapq[1:110]),col="green")





#now with include.mean =TRUE
future1 <- predict(arima(armapq[1:100],order=c(1,0,0),include.mean =TRUE),n.ahead = 10)

plot.ts(armapq)
lines(future1$pred, col="brown")
lines(future$pred, col="red")

library('forecast')
help("forecast")

future1<-forecast(modelll,h=10,level=2*pnorm(1)-1 )
future1

plot(future1)
lines(armapq)

#lets plot 95% confidence interval

future1<-forecast(modelll,h=10,level=95 )
plot(future1)

lines(armapq)

lines(future1$x, col="green")
lines(future1$fitted,col="navyblue")
lines(future1$residuals,col="red")
lines(future1$lower, col="orange")
lines(future1$upper, col='brown')


#Coefficients of MA representation

help("ARMAtoMA")

MA.infty<-ARMAtoMA(ar=c(3/2,-17/16, 3/4, -9/32), ma= -2/3,lag.max=100)
plot(MA.infty,type="h")

#plot the ACF
plot(ARMAacf(ar=c(3/2,-17/16, 3/4, -9/32), ma= -2/3,lag.max=100),type="h")




#Lab 5
NN<-150 # number of observations
zz<- rnorm(NN,0,1)  # Generation of Gaussian noise

plot.ts(zz)

theta=c(-5/3,37/36,-13/54)  # Coefficients of a MA polynomial
phi<-0
#create MA time series
mama<-arima.sim(list(order(0,0,length(theta)), ma=theta),n=NN, innov =zz )
plot(mama, col="blue")

#draw sample/theoretical ACFs/ PACFs

par(mfrow=c(2,2))
LagMax<-40

acf(mama, lag.max=LagMax)  # sample ACF
pacf(mama, lag.max=LagMax)  # sample PACF

plot(ARMAacf(ma=theta, ar=phi, LagMax), type="h", xlab="lag", main="theoretical ACF");abline(h=0)
plot(ARMAacf(ma=theta, ar=phi, LagMax,pacf=TRUE), type="h", xlab="lag" , main="theoretical PACF");abline(h=0)  # argument pacf of ARMAacf is TRUE

#create ARMA time series
phi<-c(1, -13/16)

mama<-arima.sim(list(order(length(phi),0,length(theta)),ar=phi, ma=theta),n=NN, innov =zz )

par(mfrow=c(1,1))
plot(mama, col="blue")

par(mfrow=c(2,2))
LagMax<-40

acf(mama, lag.max=LagMax)
pacf(mama, lag.max=LagMax)
plot(ARMAacf(ma=theta, ar=phi, LagMax), type="h", xlab="lag", main="theoretical ACF");abline(h=0)
plot(ARMAacf(ma=theta, ar=phi, LagMax,pacf=TRUE), type="h", xlab="lag" , main="theoretical PACF");abline(h=0)
