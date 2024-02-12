setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Statistical Learning')

#Analiza dyskryminacyjna

#zaladowanie i wstepna analiza danych
install.packages("ISLR2")
library(ISLR2)

?Smarket
names(Smarket)

summary(Smarket)

pairs(Smarket)

cor(Smarket[,-9])

attach(Smarket)
plot(Volume)


#przygotowac zbior uczacy
train <- (Year<2005)
Smarket_2005 <-Smarket[!train,]
dim(Smarket_2005)

Direction_2005 <- Direction[!train]

library(MASS)

lda.fit <- lda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
lda.fit

lda.pred<- predict(lda.fit, Smarket_2005)
names(lda.pred)
lda.pred$posterior
lda.class <- lda.pred$class

table(lda.class, Direction_2005)

plot(lda.fit)


 #podejscie qda

qda.fit <- qda(Direction ~ Lag1 + Lag2, data=Smarket, subset=train)
qda.fit

qda.pred<- predict(qda.fit, Smarket_2005)

qda.class <- qda.pred$class
table(qda.class, Direction_2005)

#######
qda.fit2 <- qda(Direction ~ Lag1 + Lag2 + Volume, data=Smarket, subset=train)
qda.fit2

qda.pred2<- predict(qda.fit2, Smarket_2005)

qda.class2 <- qda.pred2$class
table(qda.class2, Direction_2005)



