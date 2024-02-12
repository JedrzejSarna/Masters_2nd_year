setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

help("distribution")

normalWN<- ts(rnorm(200,sd=2 )) # sd=sqrt(4), mean is 0 as on page

plot(normalWN, col="red",ylab=NULL)
abline(h=0)
abline(h=mean(normalWN),lty=2) # "abline" adds a straight line

chi2WN<- ts(rchisq(200,df=2 )) # sd=sqrt(4)=sqrt(2*df), df= degrees of freedom
chi2WN<-chi2WN-2  # subtracting expected value = df
plot(chi2WN, col="navyblue",ylab=NULL) ;abline(h=0);abline(h=mean(chi2WN),lty=2)


dd=min(c(normalWN,chi2WN,-6.2)); uu=max(c(normalWN,chi2WN,6.2))
plot(chi2WN, col="navyblue",ylab=NULL,ylim=c(dd,uu)) 
lines(normalWN, col="red")
abline(h=0)
abline(h=-6,lty=2)
abline(h=6,lty=2)

UU<-ts(runif(100,min=-1,max=1)) # Generating an IID sample with a uniform distribution
UU<-UU*c(sqrt(1:100))  # Changing parameters of distributions
plot(UU, col="2022"); abline(h=0)

DrunkManWalk<-ts(c(0:500))
for(t in 1:500){
DrunkManWalk[t+1]<-DrunkManWalk[t]+rchisq(1,2)-2
}
plot(DrunkManWalk, col=1410,main="Random walk with positive skewed white noise")

help('rchisq')

DrunkGauss<- c(0,rnorm(500, sd=2))
DrunkGauss <- ts(cumsum(DrunkGauss)) 
plot(DrunkGauss, col="navyblue",ylab=NULL,main="Gaussian random walk")


x<-10
n<-500
ll<-11.5 # x= population size in generation zero, n=number of observations, ll = parameter of distribution
xx<-ts(c(x,rep(0,n-1)))
for(t in 1:(n-1)){
  for(j in 1:xx[t]){
    
    xx[t+1]<-xx[t+1]+rpois(1,lambda=ll)
  }
  if(xx[t+1] == 0){  # If no one has offspring then there will be no next generations
    break
  }
}

xx
plot(xx, col="1410")


n<-22*12
cycl<-ts(log(1:n) + cos(2*pi*1:n/12) + sin(2*pi*1:n/12/5)+rnorm(1:n, sd=1/2),start=c(2001,1),end = c(2022,12),frequency = 12)
cycl

plot(cycl, col="blue")
abline(h=0)
abline(v=c(2005,2010,2015, 2020), lty=2)
abline(v=c(2001:2022), lty=2, col='red')


n<-21*12+9
cycl<-ts(rnorm(1:n, sd=1.5+cos(2*pi*1:n/48)),start=c(2001,1),end = c(2022,9),frequency = 12)
cycl


plot(cycl, col="seagreen"); abline(h=0); abline(v=c(2004,2008,2012, 2016,2020), lty=2)
