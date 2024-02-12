setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Modele liniowe')

library(openxlsx) 

dane <- read.xlsx("Kolokwium_Egzamin.xlsx") 

attach(dane) 
#detach(dane) 

plot(Kolokwium, Egzamin_końcowy) 

#estymacja modelu
wyn<-lm(Egzamin_końcowy~Kolokwium,dane) 
summary(wyn) 

terms(wyn) 
coef(wyn) # współczynniki prostej regresji 
confint(wyn)  # przedziały ufności dla współczynników 

cooks.distance(wyn) #odległości Cooka 

index<-seq(1,19) #kolejne numery obserwacji 

plot(index,cooks.distance(wyn))  #wykres odległości Cooka 

plot(cooks.distance(wyn))  #wykres odległości Cooka - 2 metoda

rstandard(wyn) #reszty standaryzowane 
plot(index,rstandard(wyn)) #wykres reszty standaryzowane 

rstudent(wyn) #reszty studentyzowane 
plot(index,rstudent(wyn)) #wykres reszty studentyzowane 

anova(wyn) 
hatvalues(wyn) 

plot(Egzamin_końcowy~Kolokwium,dane)# wykres rozrzutu  
abline (coef(wyn), lty=5)# dorysowanie lini regresji 

plot(wyn) # wykresy diagnostyczne  





#Wyznaczanie prostej regresji na piechotę 
X <- model.matrix ( ~ Kolokwium, dane)#macierz X 
Y <- Egzamin_końcowy # odpowiedź y 
XtXinv <- solve (t(X) %*% X)# odwrotność Xt.X 
b<-XtXinv %*% t (X) %*% Y # współczynniki{b0,b1} 
H<- X %*% XtXinv %*% t(X) # macierz daszkowa H 
diag(H) # inaczej hatvalues(wyn) przekątna macierzy H 

H %*% Y #wartości przewidywane \hat Y=Hy 
wyn$fitted.values # wartości przewidywane z lm() 

plot(Kolokwium,wyn$fitted.values) 
abline (coef(wyn), lty=5) 



#Wykres linii regresji z  liniami ufności dla regresji i prognozy 
x<-Kolokwium 
y<-Egzamin_końcowy 
predict(lm(y ~ x)) 

new <- data.frame(x = seq(0, 25, 1)) 

predict(lm(y ~ x), new, se.fit = TRUE) 

pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction") 

pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence") 

require(graphics) 

matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),lty = c(1,2,2,3,3), type = "l",xlab="Kolokwium", ylab = "Egzamin końcowy") 

matpoints(Kolokwium,Egzamin_końcowy,type="p",pch=21) 




dane_std<-data.frame(scale(dane))#standaryzacja danych 
plot(Egzamin_końcowy~Kolokwium,dane_std) 
abline (0,1) 


g <- lm(Egzamin_końcowy~Kolokwium,dane_std) 
abline (coef(g), lty=5) 

cor (dane) 


(anscombe)#wyświetlenie danych Anscombe'a 
attach(anscombe) 

par(mfrow=c (2, 2)) 
# będzie rysowana macierz (2,2) wykresów 
plot (x1,y1) 
plot(x2,y2) 
plot(x3,y3) 
plot(x4,y4) 

par(mfrow=c (1, 1))# wracamy do pojedyńczego wykresu (1,1) 

#dane Anscomba 

x<-c(10,8,13,9,11,14,6,4,12,7,5) 
y<-c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68) 

x<-anscombe$x1 
y<-anscombe$y1 

wyn_Anscombe1<-lm(y~x) 

summary(wyn_Anscombe1) 
X <- model.matrix ( ~ x) 
Y<-y 

XtXinv <- solve (t(X) %*% X)# odwrotność Xt.X 
b<-XtXinv %*% t (X) %*% Y # współczynniki{b0,b1} 
H<- X %*% XtXinv %*% t(X) # macierz daszkowa H 


H %*% Y #wartości przewidywane \hat Y=Hy 
wyn_Anscombe1$fitted.values # wartości przewidywane z lm() 

plot(x,wyn_Anscombe1$fitted.values) 
abline (coef(wyn_Anscombe1), lty=5) 

plot(y~x) 





#test homoscedastyczności Breuscha Pagana 

require(car) 

ncvTest(wyn) #Non-constant Variance Score Test Breuscha Pagana 

leveragePlots(wyn,xlab="Kolokwium",ylab="Egzamin_końcowy") #wykres dźwigni 

plot(index,diag(H))#mój wykres dźwignis  

dataEllipse(Kolokwium, Egzamin_końcowy, levels=0.1*1:9, ellipse.label=0.1*1:9, lty=2, fill=TRUE, fill.alpha=0.1) 

confidenceEllipse(lm(Egzamin_końcowy~Kolokwium), Scheffe=TRUE)#elipsoida ufności typu Scheffe'go dla parametrów  

confidenceEllipse(lm(Egzamin_końcowy~Kolokwium), vcov.=hccm)#elipsoida ufności z korektą na heteroscedastyczność dla parametrów  

confidenceEllipse(lm(Egzamin_końcowy~Kolokwium), vcov.=vcov)#klasyczna elipsoida ufności dla parametrów 



require(lmtest) 

bptest(wyn) #studentyzowany test homoscedastyczności Breuscha-Pagan test 

influencePlot(wyn) 