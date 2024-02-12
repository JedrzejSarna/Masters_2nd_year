setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Testowanie hipotez statystycznych')

#Test t-studenta wariant 1
#Wygenerujmy n=20 obserwacji z rozkładu 𝑁(𝑑, 𝑠) dla 𝑑 ∈ [0,10] , 𝑠 ∈ [1,20].
#Testujemy 𝐻􏰂: 𝑑 ≤ 0 przeciwko 𝐻􏰁: 𝑑 > 0. Wykonać test t-studenta dla 𝑑 = 1 i 𝑠 = 5 (oprogramować i gotowiec)
#Obliczyć moc empiryczną w tym przypadku. Sporządzić wykres mocy empirycznej w zależności od 𝑑, 𝑠.

n<-20
m<-0 #nasza podstawowa srednia
X<-rnorm(n,1,5) 
X
T <- sqrt(1/n)*(sum(X-m))/sqrt(sum((X-mean(X))^2)/(n-1)) # nasza statystyka z wykladu U(Y) czyli T(X), liczone na piechote

p<-1-pt(T,n-1) # nasza p-wartosc, ze wzoru z wykladu
T
p
#Gotowy test uzyskujemy formuła
t.test(X,mu=0,alternative="greater")

#Obliczmy teraz empiryczną moc
mc<- 10000
n <- 20
m<-0
alpha<-0.05
T <- c()
P <- c()
M <- c()
for (i in 1:mc)
{
  X <- rnorm(n,1,5)
  T[i]<-sqrt(1/n)*(sum(X-m))/sqrt(sum((X-mean(X))^2)/(n-1))
  P[i]=1-pt(T[i],n-1)
  M[i]<-0
  if(P[i]< alpha){
    M[i] <- 1
    } 
}
mean(M)

#Teraz sporządzimy wykres mocy w zależności od 𝑑, 𝑠 mc<- 1000
n <- 20
m<-0
alpha<-0.05
T <- c()
P <- c()
M <- c()
Moc<- matrix(0,nrow=20,ncol=21)
for (j in 1:21){
  for (k in 1:20) { 
    d<-(j-1)*0.5
    s<-k
    for (i in 1:mc) {
    X <- rnorm(n,d,s)
    T[i]<-sqrt(1/n)*(sum(X-m))/sqrt(sum((X-mean(X))^2)/(n-1))
    P[i]=1-pt(T[i],n-1)
    M[i]<-0
    if(P[i]< alpha) {
      M[i] <- 1}
    }
  Moc[k,j]<-mean(M)
  }
}
heatmap(Moc,Rowv=NA,Colv=NA,scale="none")





#Test t-studenta wariant 2

library(readxl)
spalanie <- read_excel("Baza 2.1.xlsx")
X<-spalanie$"Typ 1"-spalanie$"Typ 2"
X
n<-length(X)
T<-sqrt(1/n)*(sum(X-0))/sqrt(sum((X-mean(X))^2)/(n-1))
p<-2*(1-pt(abs(T),n-1)) #p wartosc dla 2 wariantu
mean(X)
T
p
#Gotowy test uzyskujemy formułą:
t.test(X,mu=0,alternative="two.sided")





#Test t-studenta wariant 3

Baza<- read_excel("Baza 2.2.xlsx")
X<-na.exclude(Baza$"Zdrowi")
Y<-na.exclude(Baza$"Chorzy")
nx<-length(X)
ny<-length(Y)
T<-sqrt((nx*ny)/(nx+ny))*((mean(Y)-mean(X))/sqrt((sum((X-mean(X))^2)+sum((Y-mean(Y))^2))/(nx+ny-2))) #wzor dla dwoch rozkladow
p<-1-pt(T,nx+ny-2) #wzor na p wartosc
mean(Y)-mean(X)
T
p
#Gotowy test uzyskujemy formułą:
t.test(Y,X,var.equal=TRUE,alternative="greater")





#Test t-studenta wariant 4
p<-2*(1-pt(abs(T),nx+ny-2))
abs(mean(Y)-mean(X))
T
p
#Gotowy test uzyskujemy formułą:
t.test(Y,X,var.equal=TRUE,alternative="two.sided")
