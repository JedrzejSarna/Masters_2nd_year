data(longley) #Faraway PRA str 60
help('longley')
longley
g <- lm(Employed ~ GNP + Population, data=longley)
summary(g,cor=T)
cor(g$res[-1],g$res[-16])# estymujemy wsp??czynnik autokorelacji reszt
x <- model.matrix(g)
Sigma <- diag(16)# tworzymy macierz jednostkowa I_16
Sigma <- (cor(g$res[-1],g$res[-16]))^abs(row(Sigma)-col(Sigma))# tworzymy macierz kowariancyjna dla b??d?w z procesu AR(1)
Sigma
Sigi <- solve(Sigma)# tworzymy odwrotnosc powyzszej macierzy
xtxi <- solve(t(x) %*% Sigi %*% x)# wyznaczamy odwrotnosc macierzu X'(Sigma^-1) X
beta <- xtxi %*% t(x) %*% Sigi %*% longley$Empl # estymatory UMNK, zgodnie ze wzorem z wykladu
beta
res <- longley$Empl - x %*% beta #reszt z UMNK
sig <- sqrt(sum(res^2)/g$df)
sqrt(diag(xtxi))*sig # bledy std estymatorow UMNK
cor(res[-1],res[-16]) # nowy wspolczynnik autokorelacji reszt, zwiekszyl sie w stosunku do poprzedniego

sm<-chol(Sigma)
smi<-solve(t(sm))
sx<- smi %*% x
sy<- smi %*%longley$Empl
lm(sy~sx-1)$coef


library(nlme)

g1_ML <- gls(Employed ~ GNP + Population,correlation=corAR1(form= ~Year),method="ML", data=longley)
summary(g1_ML)
qqnorm(g1_ML$residuals)
qqline(resid(g1_ML), col = "dodgerblue", lwd = 2)

g2_ML <- gls(Employed ~ GNP + Population,correlation=corAR1(form= ~Year), data=longley)
#defaultowo robi za pomoca REML
summary(g2_ML)
qqnorm(g2_ML$residuals)
qqline(resid(g2_ML), col = "darkorange", lwd = 2)

#eksperyment
n=50
t<-1:n
e1<-c()
e2<-c()
y<-c()
set.seed(42)
ksi<-rnorm(n,0,1)
rho<- 0.9
e1[1]<-ksi[1]
e2[1]<-ksi[1]
for (i in 2:n) {
  e1[i]=rho*e1[i-1]+ksi[i]
  e2[i]=-rho*e2[i-1]+ksi[i]
  }
y1<-0.5*t+2+e1
y2<-0.5*t+2+e2
plot(t,y1,type="l")
plot(t,y2,type="l")

model_lm_y1 <- lm(y1 ~ t)
summary(model_lm_y1)
model_ML_y1 <- gls(y1 ~ t,correlation=corAR1(form= ~t),method="ML")
summary(model_ML_y1)
model_REML_y1 <- gls(y1 ~ t,correlation=corAR1(form= ~t),method="REML")
summary(model_REML_y1)

model_lm_y2 <- lm(y2 ~ t)
summary(model_lm_y2)
model_ML_y2 <- gls(y2 ~ t,correlation=corAR1(form= ~t),method="ML")
summary(model_ML_y2)
model_REML_y2 <- gls(y2 ~ t,correlation=corAR1(form= ~t),method="REML")
summary(model_REML_y2)


#estymacja metoda REML
g_REML <- gls(Employed ~ GNP + Population,
         correlation=corAR1(form= ~Year), data=longley)
summary(g_REML)
intervals(g_REML)
#estymacja metoda ML
g_ML <- gls(Employed ~ GNP + Population,
              correlation=corAR1(form= ~Year),method="ML", data=longley)
summary(g_ML)
intervals(g_ML)


#Metoda wazonych najmniejszych kwadratow
library(faraway)
data(strongx) #Faraway PRA str 62
help('strongx')
strongx
g <- lm(crossx ~ energy, strongx, weights=sd^-2)#model wazonych najmniejszych kwadratow
summary(g)

gu <- lm(crossx ~ energy, strongx) #model najmniejszych kwadratow, bez zadnych wag - standardowy
summary(gu)
plot(crossx ~ energy, data=strongx)
abline(g)
abline(gu,lty=2)
legend(gu)
#rozne modele jak widac na rysunku