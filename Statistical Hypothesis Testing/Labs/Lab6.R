setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Testowanie hipotez statystycznych')
library(readxl)
library(DescTools)

Baza<- read_excel("Baza 5.1.xlsx")
X<-Baza$"Wyniki"
n<-length(X)
N<-c()
#zliczanie licznosci
for (i in 1:6){
  N[i]<-0
  for (j in 1:n) {
    if(X[j]==i){
      N[i]=N[i]+1
      }
    }
}
P<-c(1/6,1/6,1/6,1/6,1/6,1/6)
GTest(x=N,p=P)
#liczenie statystyki G na piechote (test asymptotyczny)
G<-0
for (j in 1:6){
  if(N[j]!=0){
    G=G+2*N[j]*log(N[j]/(n*P[j]))
  }
}
p<-1-pchisq(G,5)
p

#Obliczmy teraz „dokładną nie asymptotyczną” p-wartość metod̨ MC
GY<-c()
MC<-100000
for (k in 1:MC){
  Y <- ceiling(runif(n,0,6)) #generujemy dane gdy zachodzi H0 (sufit z rozkladu jednostajnego ciaglego)
  NY<-c() #wektor licznosci
  for (i in 1:6){
    NY[i]<-0
    for (j in 1:n) {
      if(Y[j]==i){
        NY[i]<-NY[i]+1
      }
    }
  }
  GY[k]<-0 #wektor statystyk testowych 
  for (j in 1:6) {
    if(NY[j]!=0){
      GY[k]<-GY[k]+2*NY[j]*log(NY[j]/(n*P[j]))
    }
  }
}

#empiryczne wyliczenie p na podstawie wzoru z wykladu
pMC<-0
for (k in 1:MC) {
  if(GY[k]>=G){ #zliczam ile razy wartosc statystyki MC przekroczyla oryginalna statystyke testowa
    pMC=pMC+1
    }
}
pMC=pMC/MC
pMC

#klasyczny test chi-kwadrat
chisq.test(N,p=P)
#Obliczmy teraz „dokładną nie asymptotyczną” p-wartość metodą MC
chisq.test(N,p=P,simulate.p.value = TRUE,B=100000)
#Bez gotowca:
Q<-0
for (j in 1:6) {
  Q=Q+(N[j]- (n*P[j]))^2/(n*P[j]) 
}
p<-1-pchisq(Q,5)
p

#3. Test niezależności Chi-kwadrat
#W latach sześćdziesiątych przeprowadzono badania wśród amerykańskiej młodzieży szkolnej mające wyjaśnić,
#czy istnieje związek pomiędzy poglądami politycznymi a paleniem marihuany (wyniki w Baza 5.2).
#Zweryfikować hipotezę o niezależności stosunku do palenia marihuany (1-nigdy, 2-okazjonalnie, 3-często) od
#poglądów politycznych (1-liberalne, 2-konserwatywne, 3- inne).

Baza<- read_excel("Baza 5.2.xlsx")
X<-Baza$"Marichuana"
Y<-Baza$"Poglądy"
table(X,Y) # tabela licznosci3
chisq.test(table(X, Y)) #test niezaleznosci z wykladu
#X-squared to Q z wykladu

#bez gotowca
n<-length(X)
N<-table(X,Y)
m<-nrow(N)
k<-ncol(N)
Q<-0
for (i in 1:m)
{
  for (j in 1:k) {
    Q=Q+(N[i,j]-sum(N[i,])*sum(N[,j])/n)^2/(sum(N[i,])*sum(N[,j])/n) 
    }
} 
p<-1-pchisq(Q,k*m-(k+m-2)-1)
Q
p
