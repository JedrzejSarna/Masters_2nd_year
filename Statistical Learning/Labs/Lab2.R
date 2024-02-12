setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Statistical Learning')

library(ISLR2)
names(Auto)
dim(Auto)
?Auto

#WALIDACJA KRZYZOWA
library(boot)
glm.fit <- glm(mpg ~ horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

?cv.glm

cv.error <- rep(0,10)

for (i in 1:10){
  glm.fit <- glm(mpg ~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto,glm.fit)$delta[1]
}
cv.error

cv.error10 <- rep(0,10)

for (i in 1:10){
  glm.fit <- glm(mpg ~poly(horsepower, i), data=Auto)
  cv.error10[i] <- cv.glm(Auto,glm.fit, K=10)$delta[1]
}
cv.error10
