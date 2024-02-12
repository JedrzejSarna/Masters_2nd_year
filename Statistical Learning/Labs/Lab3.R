setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Statistical Learning')

library(ISLR2)
alpha.fn <- function(data,index){ #funkcja ktora chcemy badac stosujac bootstrap (badac zmiennosc np)
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y)-cov(X,Y))/(var(Y)+var(X)-2*cov(X,Y))
}

?Portfolio
alpha.fn(Portfolio, 1:100)

set.seed(7)
alpha.fn(Portfolio, sample(100,100,replace=T))
library(boot)
boot(Portfolio,alpha.fn,R=1000)

library()
boot.fn <- function(data,index){
  coef(lm(mpg~horsepower, data = data, subset = index))
}
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))

boot(Auto, boot.fn, R=1000)
