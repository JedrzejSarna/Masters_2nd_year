setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Testowanie hipotez statystycznych')

#test zgodnosci chi-kwadrat - hipoteza zlozona

#ENW jest podany w wykladzie, zadanie - policzyc go numerycznie, czyli zapisac funkcje wiarygodnosci
#i wyliczyc za pomoca R ENW

N<-c(110,235,155)
n<-sum(N)
T<-(2*N[1]+N[2])/(2*n) #ENW thety
P<-c(T^2,2*T*(1-T),(1-T)^2)
Q<-0
for (j in 1:3) {
  Q=Q+(N[j]- (n*P[j]))^2/(n*P[j]) } #statystyka testowa chi-kwadrat
p<-1-pchisq(Q,1)#p-wartosc dla tej statystyki
N/n #frakcje (bez zakladania H0)
P #wyliczone prawdopodobienstwa (dla H0)
p #nie ma podstaw do odrzucenia hipotezy zerowej (ze zakladany rozklad jest taki jak przy H0)

#Rangi
#Wygenerować 20 obserwacji z rozkładu N(0,1). Narysować punkty (𝑋􏰈, 𝑅􏰈).
n<-20
X<-rnorm(n,0,1)
R<-c()
for (i in 1:n) {
  R[i]=0
  for(j in 1:n) {
    if(X[j]<=X[i]){
      R[i]=R[i]+1}
    }
}
plot(X,R)
#Wygenerować 20 obserwacji 𝑋􏰆, ... , 𝑋􏰁􏰇 z rozkładu Poissona 𝑃(5). Narysować punkty (𝑋􏰈, 𝑅􏰈).
n<-20
X<-rpois(n,5)
R<-c()
for (i in 1:n){
  R[i]=0 
  for(j in 1:n) {
    if(X[j]<=X[i]){
      R[i]=R[i]+1} 
  }
}
plot(X,R)
R

#Narysować wykres jeszcze raz uwzględniając poprawki na powtarzające się rangi
U <- runif(n,0,1) #ta zmienna posluzy do losowania (przy tych samych rangach)
C<-R+U
RC<-c()
for (i in 1:n) {
  RC[i]=0
  for(j in 1:n) {
    if(C[j]<=C[i]){
      RC[i]=RC[i]+1} 
    }
}
plot(X,RC)
points(X,R,pch=3,col="red")
#Gotowiec:
R<-rank(X,ties.method = "max")
RC<-rank(X,ties.method = "random")
