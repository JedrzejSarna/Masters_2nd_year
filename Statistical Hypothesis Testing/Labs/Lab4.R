setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Testowanie hipotez statystycznych')

#Oprogramować jednostajnie najmocniejszy test wśród testów nieobciążonych dla problemu testowania równości wariancji w modelu Gaussowskim. 
#Przetestować jednorodność wariancji na danych z Baza 2.2.
TV<-function(X,Y) {
  nx<-length(X)
  ny<-length(Y)
  T<- sum((Y-mean(Y))^2)/ (sum((Y-mean(Y))^2)+ sum((X-mean(X))^2)) 
  F<-function(x,t){
    pbeta(x,(ny-1)/2,(nx-1)/2) -
    pbeta(x,(ny+1)/2,(nx-1)/2) -
    pbeta(t,(ny-1)/2,(nx-1)/2) +
    pbeta(t,(ny+1)/2,(nx-1)/2)
    }
  if(T<((ny-1)/(nx+ny-2))) { #sprawdzamy czy jestesmy po lewej stronie wartosci oczekiwanej
    a<-c()
    b<-c()
    a[1]= ((ny-1)/(nx+ny-2)) #lewy koniec przedzialu to wartosc oczekiwana
    b[1]=1 #prawa strona to 1
    for (i in 1:40) #wykonujemy metode bisekcji
    {
      d<-(a[i]+b[i])/2
      if(F(d,T)<0) { #jezeli wartosc jest ujemna, prawy koniec przesuwam a lewy zostawiam
        a[i+1]=a[i]
        b[i+1]=d 
        }
      else { #jezeli dodatnia, przesuwam lewy koniec
        a[i+1]=d
        b[i+1]=b[i]
        }
    }
    d=(a[40]+b[40])/2 #nasz szukany pierwiastek (punkt gdzie zeruje sie funkcja) to bedzie srednia a[40] i b[40]
    p<-pbeta(T,(ny-1)/2,(nx-1)/2)+1-pbeta(d,(ny-1)/2,(nx-1)/2)
  }
  if(T>=((ny-1)/(nx+ny-2))) { #jezeli po prawej stronie
    a<-c()
    b<-c()
    a[1]= 0 #lewy koniec w 0
    b[1]=((ny-1)/(nx+ny-2)) #prawy w wartosci oczekiwanej
    for (i in 1:40) {
    d<-(a[i]+b[i])/2 
    if(F(d,T)<0) {
      a[i+1]=d
      b[i+1]=b[i] 
      }
    else {
      a[i+1]=a[i]
      b[i+1]=d 
      }
    }
    d=(a[40]+b[40])/2
    p<-pbeta(d,(ny-1)/2,(nx-1)/2)+1-pbeta(T,(ny-1)/2,(nx-1)/2)
  }
  return(p)
}
#wykonajmy test na danych

library(readxl)
Baza<- read_excel("Baza 2.2.xlsx")
X<-na.exclude(Baza$"Zdrowi")
Y<-na.exclude(Baza$"Chorzy")
TV(X,Y) #zwraca p-wartosc, to znaczy ze nie ma podstaw do twierdzenia ze hipoteza jest falszywa,
#a hipoteza mowila ze wariancje sa rowne


#Narysować wykres empirycznej mocy powyższego testu dla 20 obserwacji 𝑋 z rozkładu 𝑁(2,10) i 20 
#obserwacji 𝑌 z rozkładu 𝑁(5, 𝑠) gdzie 𝑠 ∈ [1,25].

#test nieobciazony, to moc dla wariancji roznej od 10 powinna rosnac, dla 10 powinna wynosic alfa

moc<-c()
m<-c()
mc<-1000
s<-seq(1,25,by=1) #sekwencja odchylen standardowych dla Y, 
for (k in 1:25) {
  for (j in 1:mc) { #przy ustalonej wariancji generuje moce testu
    X<-rnorm(20,2,10)
    Y<-rnorm(20,5,s[k])
    p<-TV(X,Y)
    m[j]=0 
    if(p<0.05){ #jezeli p wartosc mniejsza niz ustalone alfa to odrzucamy
      m[j]=1
      }
  }
  moc[k]<-mean(m) #srednia moc dla kazdej wariancji
}
plot(moc)
#jezeli moc dla wariancji rownej 10 wyjdzie 0.05 to test nieobciazony



#Gotowy test jednorodności wariancji (F-test) dla wektorów 𝑋 i 𝑌 uzyskujemy formuł̨
TEST = var.test(X,Y)
TEST$p.value


#Sporządzić na jednym rysunku wykres empirycznej mocy dla F-testu i testu jednostajnie najmocniejszego wśród 
#nieobciążonych dla 30 obserwacji 𝑋 z rozkładu 𝑁(2,10) i 70 obserwacji 𝑌 z rozkładu 𝑁(5, 𝑠) gdzie
#𝑠 ∈ [1,25]. Czy wykresy różnią się znacząco od siebie?
moc1<-c()
m1<-c()
moc2<-c()
m2<-c()
mc<-1000
s<-seq(1,25,by=1)
for (k in 1:25) {
  for (j in 1:mc) {
    X<-rnorm(30,2,10)
    Y<-rnorm(70,5,s[k])
    p1<-TV(X,Y)
    p2<-var.test(X,Y)$p.value
    m1[j]=0
    m2[j]=0
    if(p1<0.05){
      m1[j]=1
    } 
    if(p2<0.05){
      m2[j]=1
      }
  } 
  moc1[k]<-mean(m1)
  moc2[k]<-mean(m2)
}
plot(moc1,lwd=2)
lines(moc2, col="blue", lwd=2)
#test dziala prawie tak samo, ale mozna zauwazyc ze punkty z prawej strony sa powyzej linii a z lewej ponizej

#jezeli wezmiemy tyle samo X co Y (licznosc taka sama) to moce sie nie roznia
moc1<-c()
m1<-c()
moc2<-c()
m2<-c()
mc<-1000
s<-seq(1,25,by=1)
for (k in 1:25) {
  for (j in 1:mc) {
    X<-rnorm(30,2,10)
    Y<-rnorm(30,5,s[k])
    p1<-TV(X,Y)
    p2<-var.test(X,Y)$p.value
    m1[j]=0
    m2[j]=0
    if(p1<0.05){
      m1[j]=1
    } 
    if(p2<0.05){
      m2[j]=1
    }
  } 
  moc1[k]<-mean(m1)
  moc2[k]<-mean(m2)
}
plot(moc1,lwd=2)
lines(moc2, col="blue", lwd=2)

#Dlaczego teraz wykresy sa takie same? Wtedy rozklad beta jest symetryczny wzgledem 1/2, czyli mozna brac var.test ESSA

