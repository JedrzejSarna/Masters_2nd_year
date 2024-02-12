setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Testowanie hipotez statystycznych')

# Test Manna-Whitneya
library(readxl)
Baza<- read_excel("Baza 6.1.xlsx")
Z<-Baza$"Czas"
G<- Baza$"Leczenie"
N<-length(Z)
X<-c()
Y<-c()
k<-1
for (i in 1:N) {
  if(G[i]==1) {
    X[k]=Z[i]
    k=k+1
    }
}
k<-1
for (i in 1:N) {
  if(G[i]==2) {
    Y[k]=Z[i]
    k=k+1
    }
}
m<-length(X)
n<-length(Y)
hist(X,prob=TRUE)
hist(Y,prob=TRUE)
plot(ecdf(X))
lines(ecdf(Y),col="blue")
R<-rank(Z)
plot(R)
RY<-c()
k<-1
for (i in 1:N) {
  if(G[i]==2) {
    RY[k]=R[i]
    k=k+1 
    }
}
RY
W<-(mean(RY)-(m+n+1)/2)/sqrt(m*(m+n+1)/(12*n)) #statystyka testowa Manna-Whitneya
#test w wersji asymptotycznej
p<-2*(1-pnorm(abs(W),0,1))
W
p #mala p-wartosc, wiec odrzucamy hipoteze zerowa ze delta = 0 (takie same rozklady)

#Gotowiec otrzymujemy: 
wilcox.test(X,Y,alternative = "two.sided")


#Obliczyć dokładną p wartość metoda MC: (najczęściej używana metoda)
WMC<-c()
MC<-1000000
for (k in 1:MC) {
  ZMC <- runif(m+n,0,1)
  RMC<-rank(ZMC)
  RYMC<-c()
  for (i in 1:n) {
    RYMC[i]=RMC[m+i] #rangi dla Y
    }
  WMC[k]<-(mean(RYMC)-(m+n+1)/2)/sqrt(m*(m+n+1)/(12*n)) #100tys wartosci ktore tworza rozklad empiryczny
}

pMC<-0
for (k in 1:MC) {
  if(abs(WMC[k])>=abs(W)){pMC=pMC+1} #zliczam liczbe przekroczen
}
pMC=pMC/MC #srednia liczba przekroczen

pMC

X <- rnorm(20,0,1)
#for (m in 1:10) {
#  Y[m] <- rnorm(30,m/10,1)
#}
Y <- rnorm(30,0.1,1)
plot(ecdf(X))
lines(ecdf(Y),col="blue")

RX <- rank(X)
RX
RY <- rank(Y)
RY

N <- c(X,Y)
RN <- rank(N)

W<-(mean(RN[1:30])-(50+1)/2)/sqrt(20*(50+1)/(12*30)) #statystyka testowa Manna-Whitneya
W
p<-1-pnorm(W,0,1)
p

WMC<-c()
MC<-1000000
for (k in 1:MC) {
  ZMC <- runif(50,0,1)
  RMC<-rank(ZMC)
  RYMC<-c()
  for (i in 1:30) {
    RYMC[i]=RMC[m+i] #rangi dla Y
  }
  WMC[k]<-(mean(RYMC)-(50+1)/2)/sqrt(20*(50+1)/(12*30)) #100tys wartosci ktore tworza rozklad empiryczny
}
pMC<-0
for (k in 1:MC) {
  if(abs(WMC[k])>=abs(W)){
    pMC=pMC+1
    } #zliczam liczbe przekroczen
}
pMC=pMC/MC #srednia liczba przekroczen
pMC
