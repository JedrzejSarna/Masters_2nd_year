###### Laboratorium 10

setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Testowanie hipotez statystycznych')

library(readxl)
Baza<- read_excel("Baza 10.1.xlsx")

X<-Baza$Dzień
Y<-Baza$Temperatura
n<-length(X)
plot(X,Y)
cor.test(X,Y) #dane nie sa liniowo zalezne
abline(lm(Y ~ X))

#za pomoca korelacji Spearmana
R<-rank(X,ties.method = "random")
S<-rank(Y,ties.method = "random")
plot(R,S)
abline(lm(S ~ R))
r<-1-6*sum((R-S)^2)/(n*(n^2-1)) #wzor na korelacje Spearmana
r #czy wystarczajaco dalekie od 0 zeby odrzucic hipoteze ze jest zerowa? (zmienne niezalezne)
#sprawdzmy to
mc<-10000
rMC<-c()
for (k in 1:mc) {
  XMC <- runif(n,0,1)
  YMC <- runif(n,0,1)
  RMC<-rank(XMC,ties.method = "random")
  SMC<-rank(YMC,ties.method = "random")
  rMC[k]<-1-6*sum((RMC-SMC)^2)/(n*(n^2-1))
}
pr<-0
for (j in 1:mc) {
  if(abs(rMC[j])>abs(r)){ 
    pr = pr +1
    }
}
pr=pr/mc
pr #0.46, czyli nie odrzucamy hipotezy zerowej (dane nie sa monotonicznie zalezne)

#pokazemy ze sa zalezne za pomoca kopuly
Z<-cbind(R,S) 
Z=Z[order(Z[,1],decreasing=FALSE),] #posortowane pary rang
Z
u<-c() #u i v to punkty siatki
v<-c()
R<-Z[,1]
S<-Z[,2] #wektor rang wiazanych 
for (i in 1:(n+1)) {
  u[i]=(i-1/2)/(n+1)
  v[i]=(i-1/2)/(n+1) 
  }
C<- matrix(0,nrow=n+1,ncol=n+1) #definiujemy kopule (czyli ta macierz jak w wykladzie) 
C
#algorytm wyliczania komorek macierzy, tak jak na wykladzie wytlumaczony
for (j in 1:(n+1)) {
  C[j,1]=0 
}
for (i in 1:n) {
  for (j in 1:(n+1)) {
    if(j-1<S[i]){
      C[j,i+1]=C[j,i]
      }
    else{
      C[j,i+1]=C[j,i]+1/n
    } 
  }
}
for (i in 1:(n+1)) {
  for (j in 1:(n+1)) {
    C[j,i]=C[j,i]-u[i]*v[j]
  } 
}
heatmap(C,Rowv=NA,Colv=NA,scale="none",col=colorRampPalette(c("blue","gray", "red"))(n = 100)) #zaleznosc rosnaco-malejaca
#teraz konstrukcja testu
C
CKS<-max(abs(C))
CKS

          #teraz te poprzednie kroki za pomoca monte carlo (10000 razy)
mc<-10000
CKSMC<-c()
for (k in 1:mc) {
  XMC <- runif(n,0,1)
  YMC <- runif(n,0,1)
  RMC<-rank(XMC,ties.method = "random")
  SMC<-rank(YMC,ties.method = "random")
  ZMC<-cbind(RMC,SMC)
  ZMC=ZMC[order(ZMC[,1],decreasing=FALSE),]
  SMC<-ZMC[,2]
  CMC<- matrix(0,nrow=n+1,ncol=n+1)
  for (j in 1:(n+1))
  {
    CMC[j,1]=0 }
  for (i in 1:n) {
    for (j in 1:(n+1)) {
      if(j-1<SMC[i]){
        CMC[j,i+1]=CMC[j,i]}
      else{
        CMC[j,i+1]=CMC[j,i]+1/n} }
  }
  for (i in 1:(n+1)) {
    for (j in 1:(n+1)) {
      CMC[j,i]=CMC[j,i]-u[i]*v[j] }
  }
  CKSMC[k]<-norm(CMC/sqrt(u*v*(1-u)*(1-v))) }
pCKS<-0
for (j in 1:mc) {
  if(abs(CKSMC[j])>abs(CKS)){ 
    pCKS = pCKS +1} 
  }
pCKS=pCKS/mc
pCKS

plot(ecdf(CKSMC))

