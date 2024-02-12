setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/testowanie hipotez statystycznych')

#Wygenerujmy n=100 liczb z rozkładu U[0,1]
n<-100
U <- runif(n)

#Obliczmy średnią, wariancję, odchylenie standardowe, i kwartyle
mean(U)
var(U)
sd(U)
quantile(U,0.25)
quantile(U,0.5)
quantile(U,0.75)
#Rysujemy dystrybuantę empiryczną i prawdziwą na jednym rysunku
x<-seq(0,1,by=0.01)
y<-x
plot(ecdf(U))
lines(x,y,col='blue')
#Rysujemy unormowany histogram (estymator gęstości) i prawdziwą gęstość na jednym rysunku x<-seq(0,1,by=0.01)
y<-0*x+1
hist(U,prob=TRUE)
lines(x, y, col="blue", lwd=2)

#b) Inne rozkłady – metoda odwracania dystrybuanty
#Jeśli 𝑈 ma rozkład jednostajny to 𝐹􏰀􏰁(𝑈) ma rozkład o dystrybuancie 𝐹
#Generujemy n=100 obserwacji z rozkładu Beta(2,5)
X<-runif(n)
Y<- qbeta(X,2,5)
#Porównajmy dystrybuantę empiryczną z prawdziwą x<-seq(0,1,by=0.01)
y<-pbeta(x,2,5)
plot(ecdf(Y))
lines(x, y, col="blue", lwd=2)
#Porównajmy unormowany histogram z prawdziwą gęstością
x<-seq(0,1,by=0.01)
y<-dbeta(x,2,5)
hist(Y,prob=TRUE)
lines(x, y, col="blue", lwd=2)
#c) Inne rozkłady – metoda eliminacji
#Jeśli gęstość 𝑓 jest ograniczona przez 𝑀 oraz jej nośnik jest ograniczony i zawarty w przedziale [𝑎, 𝑏], to
#𝑈|𝑉 < 𝑓(𝑈) ma rozkład o gęstości 𝑓, gdzie 𝑈, 𝑉 są niezależne o rozkładach jednostajnych na [𝑎, 𝑏] oraz [0, 𝑀] odpowiednio. n <- 100
i<-0
a<-0
b<-1
M<-5
X<-c()
while(i<n) {
  U<- runif(1)*(b-a)+a
  V<-runif(1)*M
  if(V<dbeta(U,2,5))
  {
    X[i]=U
    i=i+1
  }
}
x<-seq(0,1,by=0.01)
y<-dbeta(x,2,5)
hist(X,prob=TRUE)
lines(x, y, col="blue", lwd=2)


#2. Empiryczna moc i empiryczny poziom istotności testu
#Niech 𝑋􏰁, ... , 𝑋􏰂 będzie próbą prostą z rozkładu 𝑁(𝑑, 1) testujemy 𝐻􏰃: 𝑑 
#≤ 0 przeciwko alternatywie 𝐻􏰁: 𝑑 > 0 testem jednostajnie najmocniejszym (patrz wykład, przykład 1 c).
#Obliczymy empiryczną moc testu dla różnych wartości 𝑛 i 𝑑 oraz porównamy ją z teoretyczną (prawdziwą) mocą testu.
m <- 10000
n <- 10
alpha<-0.05
d<- -0.5
T <- c()
P <- c()
M <- c()
for (i in 1:m) {
  X <- rnorm(n)+d
  T[i] <- sqrt(n)*mean(X)
  P[i] <- 1-pnorm(T[i],0,1)
  M[i]<-0
  if(P[i]< alpha) 
  {M[i] <- 1} 
  }
mean(M)
Moc<-1-pnorm(qnorm(1-alpha,0,1)-sqrt(n)*d,0,1)
Moc
hist(P,prob=TRUE)
#Powtórzymy powyższą procedurę dla d=-0.5, -0.1, 0,1, 0.5 i zobaczmy jak zmienia się rozkład p-warotści. 
#Teraz sporządzimy wykres mocy teoretycznej oraz empirycznej dla n=10 w zależności od 𝑑 ∈ [−1,2]
m <- 10000 # liczba testow dla 1 puntkow
k<-300 #siatka
n <- 10 #wielkosc proby
alpha<-0.05
d<-c() #wartosci z przedzialu -1 a 2
T <- c() #statystyka testowa, bedzie ich 3 000 000
P <- c() # pwartosc czyli unormowana statystyka testowa
M <- c() # dla pojedynczej proby moc 
Moc<-c()
for (j in 1:k) {
  d[j] <- j*0.01-1 #przesuwanie siatki
  for (i in 1:m) { #dla ustalonego d[j]
    X <- rnorm(n)+d[j]
    T[i] <- sqrt(n)*mean(X) #wyliczam statystyke testowa
    P[i] <- 1-pnorm(T[i],0,1) #wyliczam p wartosc
    M[i]<-0
    if(P[i]< alpha) { #wyliczam czy test odrzucil hipoteze, jest to w zasadzie funkcja fi
      M[i] <- 1
      } 
    }
  Moc[j]=mean(M) #srednia liczba odrzucen dla kazdego d[j]
}
plot(d, Moc, col='blue') #moce empiryczne czyli frakcje odrzucen
y<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(n)*d,0,1)
lines(d, y, col="red", lwd=2) #prawdziwa moc



#Teraz sporządźmy kilka wykresów mocy w zależności od d dla n=10, 50, 100, 200
alpha<-0.05
d<-seq(-1,2,by=0.01)
moc<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(10)*d,0,1)
y2<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(50)*d,0,1)
y3<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(100)*d,0,1)
y4<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(200)*d,0,1)
plot(d, moc,type="l",lwd=2)
lines(d, y2, col="blue", lwd=2)
lines(d, y3, col="green", lwd=2)
lines(d, y4, col="red", lwd=2)
#jak zwiekszam wielkosc proby, moc rosnie czyli z bardzo duzym prawdopodobienstwem odrzucic hipoteze zerowa


#Teraz sporządźmy kilka wykresów mocy w zależności od n dla d=0.1, 0.15, 0.2, 0.25 alpha<-0.05
n<-seq(1,400,by=1)
moc<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(n)*0.1,0,1)
y2<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(n)*0.15,0,1)
y3<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(n)*0.2,0,1)
y4<- 1-pnorm(qnorm(1-alpha,0,1)-sqrt(n)*0.25,0,1)
plot(n, moc,type="l",lwd=2, ylim=c(0, 1))
lines(n, y2, col="blue", lwd=2)
lines(n, y3, col="green", lwd=2)
lines(n, y4, col="red", lwd=2)



#zadanie samodzielne

#Niech 𝑋􏰁, ... , 𝑋􏰂 będzie próbą prostą z rozkładu 𝑁(𝑑, 1) testujemy 𝐻􏰃: 𝑑 = 0 przeci
#wko alternatywie 𝐻􏰁: 𝑑 ≠ 0 testem jednostajnie najmocniejszym wśród testów nieobciąż
#onych (patrz wykład, przykład 1b). Powtórzyć powyższe ćwiczenie w tym wariancie

m <- 10000 # liczba testow dla 1 puntkow
k<-400 #siatka
n <- 10 #wielkosc proby
alpha<-0.05
d<-c() #wartosci z przedzialu -1 a 2
T <- c() #statystyka testowa, bedzie ich 3 000 000
P <- c() # pwartosc czyli unormowana statystyka testowa
M <- c() # dla pojedynczej proby moc 
Moc<-c()
for (j in 1:k) {
  d[j] <- j*0.01-2 #przesuwanie siatki
  for (i in 1:m) { #dla ustalonego d[j]
    X <- rnorm(n)+d[j]
    T[i] <- sqrt(n)*mean(X) #wyliczam statystyke testowa
    P[i] <- 2*(1-pnorm(abs(T[i]),0,1)) #wyliczam p wartosc
    M[i]<-0
    if(P[i]< alpha) { #wyliczam czy test odrzucil hipoteze, jest to w zasadzie funkcja fi
      M[i] <- 1
    } 
  }
  Moc[j]=mean(M) #srednia liczba odrzucen dla kazdego d[j]
}
plot(d, Moc, col='blue') #moce empiryczne czyli frakcje odrzucen
y<- 1-pnorm(qnorm(1-alpha/2,0,1)-sqrt(n)*d,0,1) + pnorm(qnorm(alpha/2,0,1)-sqrt(n)*d,0,1)
lines(d, y, col="red", lwd=2) #prawdziwa moc
