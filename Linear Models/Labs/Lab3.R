setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Modele liniowe')

library(openxlsx)

Dane <- read.xlsx('Samochody.xlsx')
Dane

summary(Dane)
attach(Dane)
par(mfrow=c(3,2)) #ustawimy macierz 6-ciu rysunków 

plot(Zużycie ~ Pojemność) 

plot(Zużycie ~ Moc) 

plot(Zużycie ~ Ładowność) 

plot(Zużycie ~ Masa) 

plot(Zużycie ~ Długość) 

plot(Zużycie ~ Szerokość) 

par(mfrow=c(1,1)) #wracamy do jednego okna graficznego 

zm<-matrix(c(Pojemność,Moc,Ładowność,Masa,Długość,Szerokość),ncol=6) 

cor(Zużycie,zm) 


model <- lm(Zużycie ~ Pojemność + Moc +Ładowność + Masa + Długość + Szerokość,Dane) #pełny model 

summary(model) 

#Pr(>|t|) to jest p-wartosc, testowana jest hipoteza czy dana zmienna jest istotna

#F-statistic - testujemy hipoteze ze wszystkie zmienne sa nieistotne i wystarczy do estymacji tak naprawde srednia
#p-value mniejsze od 0.05 wiec odrzucamy hipoteze zerowa





library(car)
vif(model)




library(ppcor) # ładujemy pakiet do współczynników korelacji cząstkowej 

zm1<-matrix(c(Zużycie,Pojemność,Moc,Ładowność,Masa,Długość,Szerokość),ncol=7)#tworzymy macierz wszystkich zmiennych  

pcor(zm1)#wyznaczamy macierz współczynników korelacji cząstkowej 

pcor.test(zm1[,1],zm1[,2],zm1[,c(3:7)]) #współczynnik korelacji pomiędzy zmienną 1 i 2 po wyeliminowaniu wpływu pozostałych 

pcor.test(zm1[,1],zm1[,3],zm1[,c(2,4,5,6,7)]) #współczynnik korelacji pomiędzy zmienną 1 i 3 po wyeliminowaniu wpływu pozostałych 

model1 <- lm(Zużycie ~ Masa + Szerokość,Dane) 

w1 <- summary(model1)
w1$r.squared
print(w1)

par(mfrow=c(2,2)) 

plot(model1) 

par(mfrow=c(1,1))