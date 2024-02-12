#Drzewa klasyfikacyjne
library(tree)
library(ISLR2)

help(Carseats)

summary(Carseats)
attach(Carseats)
High <- factor(ifelse(Sales<=8,'No','Yes'))

Carseats <- data.frame(Carseats,High)

#budujemy drzewo na calym zbiorze
tree.carseats <- tree(High ~ . - Sales, Carseats)

summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=1)
tree.carseats

set.seed(2)
train <- sample(1:nrow(Carseats),200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]

tree.carseats_train <- tree(High ~ . - Sales, Carseats, subset=train)
summary(tree.carseats_train)
tree.carseats_train

tree.pred <- predict(tree.carseats_train, Carseats.test, type='class')
tree.pred
table(tree.pred, High.test)
1-46/(154+46)

#zbudowalismy model oparty na calym drzewie, chcemy teraz zrobic poddrzewo

#bedziemy to robili za pomoca walidacji krzyzowej (dobor alfy)
?cv.tree
set.seed(7)
cv.carseats <- cv.tree(tree.carseats_train, FUN = prune.misclass)
cv.carseats
#size <- liczba lisci dla kazdego drzewa
#dev <- blad klasyfikacji (mimo ze dev - dewiancja nazwa)
#k <- alpha z prezentacji
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type='b')
plot(cv.carseats$k, cv.carseats$dev, type='b')
cv.carseats
?prune.misclass
shortened_tree <- prune.misclass(tree.carseats_train, best=9)
summary(shortened_tree)

shortened_tree.pred <- predict(shortened_tree, Carseats.test, type='class')
shortened_tree.pred
table(shortened_tree.pred, High.test)
(97+58)/(97+58+25+20)

par(mfrow=c(1,2))
plot(shortened_tree)
text(shortened_tree, pretty=1)
plot(tree.carseats_train)
text(tree.carseats_train, pretty=1)

Boston
?Boston
View(Boston)
#zbudujmy drzewo badajace jak cechy wplywaja na ceny mieszkan
set.seed(1)
train <- sample(1:nrow(Boston),500)
train
tree.boston <- tree(medv ~ . ,Boston, subset=train)
tree.boston
summary(tree.boston)

plot(tree.boston)
text(tree.boston, pretty = 1)
cv.boston <- cv.tree(tree.boston)
par(mfrow=c(1,1))
plot(cv.boston$size, cv.boston$dev, type='b')

shortened_tree <- prune.(tree.boston, best=5)

yhat <- predict(tree.boston, newdata = Boston[-train], type='class')
