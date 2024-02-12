setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

Month_Value<-read.csv(file="Month_Value_1.csv") # Load/import data (see lab 1) into the project.


Three_TS<-ts(Month_Value[2:4], start=c(2015,1), frequency = 12) 
# The first month is January 2015 (see start=...) and there is  12 values for the year.

Revenue<-na.omit(Three_TS[,1])
plot(Revenue)

#Estimate a trend by Least Squares method. Fit the time series to polynomial of degree 3,6,9

zzzz<-as.numeric(time(Revenue))

degree=3 # degree of polynomial
Rev<-lm(Revenue ~ poly(zzzz,degree),data=Revenue)
RRev<-ts(predict(Rev),start=c(2015,1), frequency = 12)
lines(RRev, col="blue")
Rev$coefficients

degree=6 # degree of polynomial
Rev<-lm(Revenue ~ poly(zzzz,degree),data=Revenue)
RRev<-ts(predict(Rev),start=c(2015,1), frequency = 12)
lines(RRev, col="red")

degree=9 # degree of polynomial
Rev<-lm(Revenue ~ poly(zzzz,degree),data=Revenue)
RRev<-ts(predict(Rev),start=c(2015,1), frequency = 12)
lines(RRev, col="pink")

#Smothing by Cubic Splines - we use smooth.spline with the parameter lambda = 0.
#Then we define cubic splines that were introduced too briefly in the lecture.

plot(Revenue)
lines(smooth.spline(time(Revenue), Revenue, lambda=0,nknots=15), lwd=2, col="red")
lines(smooth.spline(time(Revenue), Revenue, lambda=0,nknots=7), lwd=2, col="blue")
lines(smooth.spline(time(Revenue), Revenue, lambda=0,nknots=3), lty=2, lwd=2, col="green")

#The number of knots is related to sharpness/smoothness. How? Draw the residuals. One by one

res<-smooth.spline(time(Revenue), Revenue, lambda=0,nknots=15)
res<-Revenue-res$y
plot(res,col="red")

res<-smooth.spline(time(Revenue), Revenue, lambda=0,nknots=7)
res<-Revenue-res$y
lines(res,col="blue")

res<-smooth.spline(time(Revenue), Revenue, lambda=0,nknots=3)
res<-Revenue-res$y
lines(res,col="green")

#Kernel Smoothing - is a moving average smoother that uses a kernel function to average the observations
help("ksmooth")
#If kernel =“normal” the normal kernel is used.
#The argument kernel = “box” is related to a uniform distribution


#First we use a box kernel with various bandwidths
plot(Revenue)
lines(ksmooth(time(Revenue), Revenue, "box", bandwidth=1/32), lwd=2, col="orange") #not continous!
lines(ksmooth(time(Revenue), Revenue, "box", bandwidth=.25), lwd=2, col="green")
lines(ksmooth(time(Revenue), Revenue, "box", bandwidth=.5), lwd=2, col="blue")
lines(ksmooth(time(Revenue), Revenue, "box", bandwidth=1), lwd=2, col="red")
lines(ksmooth(time(Revenue), Revenue, "box", bandwidth=2), lwd=2, col="brown")
lines(ksmooth(time(Revenue), Revenue, "box", bandwidth=32), lwd=2, col="pink") #mean- big #bandwidth takes all arguments

#Diving into orange case

Oorange<-(ksmooth(time(Revenue), Revenue, "box", bandwidth=1/32))
plot(Oorange, col="orange")

plot.ts(Oorange$y, col="orange") #the bandwidth is too small

#A bigger value of the bandwidth results in a smoother trend estimate. As you can see, too large value of this 
#parameter gives a total mean as an estimate of the trend. On the other hand, too small value of the bandwidth 
#gives too sharp estimation and it may even cause problems in determining the estimator.


#Before choosing a bandwidth, consider how time is measured.
time(Revenue)

#Plot the residuals in the case a bandwidth equals 0.5 and 1.
res<-(ksmooth(time(Revenue), Revenue, "box", bandwidth=.5))
res<-res[["y"]]  #Compare res[["y"]] with res[2] or res[[2]]
res<-Revenue-res[1:64]
plot(res,col="blue")

res<-(ksmooth(time(Revenue), Revenue, "box", bandwidth=1))
res<-res[["y"]]  #Comapre res[["y"]] with res[2] or res[[2]]
res<-Revenue-res[1:64]
lines(res,col="red")


#now we use normal kernel
plot(Revenue)
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=1/32), lwd=2, col="orange")
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=.25), lwd=2, col="green")
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=.5), lwd=2, col="blue")
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=1), lwd=2, col="red")
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=2), lwd=2, col="brown")
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=32), lwd=2, col="pink")

#The normal kerner gives smoother estimater than the box one.
#Watch the orange line now. Compare its residuals with blue one and red one.

res<-(ksmooth(time(Revenue), Revenue, "normal", bandwidth=1/32))
res<-res[["y"]]  #Comapre res[["y"]] with res[2] or res[[2]]
res<-Revenue-res[1:64]
plot(res,col="orange")
res<-(ksmooth(time(Revenue), Revenue, "normal", bandwidth=.5))
res<-res[["y"]]  #Comapre res[["y"]] with res[2] or res[[2]]
res<-Revenue-res[1:64]
lines(res,col="blue")
res<-(ksmooth(time(Revenue), Revenue, "normal", bandwidth=1))
res<-res[["y"]]  #Comapre res[["y"]] with res[2] or res[[2]]
res<-Revenue-res[1:64]
lines(res,col="red")

plot(Revenue)
degreex=3 # degree of polynomial
Rev<-lm(Revenue ~ poly(zzzz,degree),data=Revenue)
RRev<-ts(predict(Rev),start=c(2015,1), frequency = 12)
lines(RRev, col="blue")
lines(smooth.spline(time(Revenue), Revenue, lambda=0,nknots=3), lty=2, lwd=2, col="green")
lines(ksmooth(time(Revenue), Revenue, "normal", bandwidth=1), lwd=2, col="red")












#ARIMA PROCESS - moving average
#Generate 200 observations of MA(q) process and draw its time series plots and its sample ACF.
#Draw a theoretical ACF as well.
help("arima.sim")
theta=c(.1,-.2)
maq<-arima.sim(list(order=c(0,0,2), ma=theta), n=2000) #tends to the theoretical ACF with n -> oo

layout(rbind(c(1,1),c(2,3)),widths=c(1,1), heights=c(2,3))

plot(maq, ylab="x", col="navyblue")
acf(maq,40)
plot(ARMAacf(0, ma=theta, 40),type="h", xlab="lag", main="theoretical ACF"); abline(h=0)


#Comparison of a random walk with AR(1)

#Generate 100 observation of a white noise {Zt}.
#In the same picture, plot a random walk and AR(1) process with ϕ=.9 both driven by the same white noise {Zt}.
#Repeat the second point with ϕ=.5 and ϕ=.99.
#Repeat the second and third points at least four times.

zz<- rnorm(100,0,1)
phi<-.9
ar1<-ts(c(0:100)); rw<-ts(c(0:100))
for(t in 1:100){
  ar1[t+1]<-phi*ar1[t]+zz[t]
  rw[t+1]<-rw[t]+zz[t]
}
dd=min(c(ar1,rw));uu=max(c(ar1,rw))
plot(ar1,col="blue",ylab=NULL,ylim=c(dd-.1,uu+.1)); lines(rw,col="red")

#now we increase phi
zz<- rnorm(100,0,1)
phi<-.99
ar1<-ts(c(0:100)); rw<-ts(c(0:100))
for(t in 1:100){
  ar1[t+1]<-phi*ar1[t]+zz[t]
  rw[t+1]<-rw[t]+zz[t]
}
dd=min(c(ar1,rw));uu=max(c(ar1,rw))
plot(ar1,col="blue",ylab=NULL,ylim=c(dd-.1,uu+.1)); lines(rw,col="red")

#now we decrease phi and see that ar1 does not approximate well
phi<-.5
ar1<-ts(c(0:100)); rw<-ts(c(0:100))
for(t in 1:100){
  ar1[t+1]<-phi*ar1[t]+zz[t]
  rw[t+1]<-rw[t]+zz[t]
}
dd=min(c(ar1,rw));uu=max(c(ar1,rw))
plot(ar1,col="blue",ylab=NULL,ylim=c(dd-.1,uu+.1)); lines(rw,col="red")

#Autoregressive process of order p.
#Generate 200 observations of AR(p) process and draw its time series plots and its sample ACF.
#Draw a theoretical ACF as well.

phi=c(.7,-.1)
ar.p<-arima.sim(list(order=c(2,0,0), ar=phi), n=20000)

layout(rbind(c(1,1),c(2,3)),widths=c(1,1), heights=c(2,3))
plot(ar.p, ylab="x", col="navyblue")
acf(ar.p,40)
plot(ARMAacf(0, ar=phi, 40),type="h", xlab="lag", main="theoretical ACF"); abline(h=0)

#different phi
phi=c(.2,-.5)
ar.p<-arima.sim(list(order=c(2,0,0), ar=phi), n=200)
layout(rbind(c(1,1),c(2,3)),widths=c(1,1), heights=c(2,3))
plot(ar.p, ylab="x", col="navyblue")
acf(ar.p,40)
plot(ARMAacf(0, ar=phi, 40),type="h", xlab="lag", main="theoretical ACF"); abline(h=0)

#different phi
phi=c(2.6,-2.74,1.44,-0.32)
ar.p<-arima.sim(list(order=c(4,0,0), ar=phi), n=200)
layout(rbind(c(1,1),c(2,3)),widths=c(1,1), heights=c(2,3))
plot(ar.p, ylab="x", col="navyblue")
acf(ar.p,40)
plot(ARMAacf(0, ar=phi, 40),type="h", xlab="lag", main="theoretical ACF"); abline(h=0)
