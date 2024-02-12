setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

help(acf)

normalWN<- ts(rnorm(200,sd=2 ))   # sd=sqrt(4)

par(mfrow=c(2,1),mar=c(4,4,.1,1))    # mar = A numerical vector of the form c(bottom, left, top, right) 
#which gives the number of lines of margin to be specified on the four sides of the plot.

plot(normalWN, col="red",ylab=NULL) 
abline(h=mean(normalWN),lty=2);     # "abline adds a straight line
acf(normalWN, lag.max=40, ci=.95, main=" ")    # lag.max=maximum lag at which to calculate the acf (see help(acf) for more details), ci = coverage probability for confidence interval


chi2WN<- ts(rchisq(200,df=2 )) # sd=sqrt(4)=sqrt(2*df), df= degrees of freedom
chi2WN<-chi2WN-2  # subtracting expected value = df

par(mfrow=c(2,1),mar=c(4,4,.1,1))# mar = A numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot.
plot(chi2WN, col="navyblue",ylab=NULL); abline(h=mean(chi2WN),lty=2); # "abline adds a straight line
acf(chi2WN,  lag.max=40, ci=.95, main=" ") # lag.max=maximum lag at which to calculate the acf (see help(acf) for more details), ci = coverage probability for confidence interval

UU<-ts(runif(100,min=-1,max=1)) # Generating an IID sample with a uniform distribution
UU<-UU*c(sqrt(1:100))  # Changing parameters of distributions

par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(UU, col="2022",ylab=NULL); abline(h=mean(UU),lty=2); 
acf(UU,  lag.max=40, ci=.95, main=" ") 


DrunkGauss<- c(0,rnorm(500, sd=2)); DrunkGauss <- ts(cumsum(DrunkGauss)) 

par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(DrunkGauss, col="navyblue",ylab=NULL); abline(h=mean(DrunkGauss),lty=2); 
acf(DrunkGauss,  lag.max=40, ci=.95, main=" ") 

DrunkManWalk<-ts(c(0:500))
for(t in 1:500){
  DrunkManWalk[t+1]<-DrunkManWalk[t]+rchisq(1,2)-2
}
par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(DrunkManWalk, col="1410",ylab=NULL); abline(h=mean(DrunkManWalk),lty=2); 
acf(DrunkManWalk,  lag.max=40, ci=.95, main=" ") 



x<-10; n<-50; ll<-0.2 # x= population size in generation zero, n=number of observations, ll = parameter of distribution
xx<-ts(c(x,rep(0,n-1)))
for(t in 1:(n-1)){
  for(j in 1:xx[t]){
    
    xx[t+1]<-xx[t+1]+rpois(1,lambda=ll)
  }
  if(xx[t+1] == 0){  # If no one has offspring then there are no next generations
    break
  }
}

par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(xx, col="1410",ylab=NULL)
abline(h=mean(xx),lty=2); 
acf(xx,  lag.max=40, ci=.95, main=" ") 


n<-22*12
cycl<-ts(log(1:n) + cos(2*pi*1:n/12) + sin(2*pi*1:n/12/5)+rnorm(1:n, sd=1/2),start=c(2001,1),end = c(2022,12),frequency = 12)

par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(cycl, col="blue",ylab=NULL); abline(h=mean(cycl),lty=2); 
acf(cycl,  lag.max=2*60, ci=.95, main=" ")


n<-21*12+9
cycl<-ts(rnorm(1:n, sd=1.5+cos(2*pi*1:n/48)),start=c(2001,1),end = c(2022,9),frequency = 12)

par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(cycl, col="seagreen",ylab=NULL); abline(h=mean(cycl),lty=2); 
acf(cycl,  lag.max=2*48, ci=.95, main=" ") 


Month_Value<-read.csv(file="Month_Value_1.csv") 

Three_TS<-ts(Month_Value[2:4], start=c(2015,1), frequency = 12) # The first month is January 2015 (see start=...) and there is  12 values for the year.

plot(Three_TS)

acf(na.omit(Three_TS),lag.max=24)


#exercise

par(mfrow=c(2,1),mar=c(4,4,.1,1))
plot(nottem)
acf(nottem, lag.max=24)

bitcoin_dataset <- read.csv('Lab1_Bitcoin_Historical_Price.csv')
par(mfrow=c(2,1),mar=c(4,4,.1,1))
bitcoin_TS<-ts(bitcoin_dataset$Close,start = c(2013,4), frequency = 365)
plot(bitcoin_TS)

acf(bitcoin_TS, lag.max = 365)
