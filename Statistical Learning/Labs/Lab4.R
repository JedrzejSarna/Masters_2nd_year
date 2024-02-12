setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Statistical Learning')


library(ISLR2)
?Hitters
View(Hitters)
summary(Hitters)

is.na(Hitters$Salary)

sum(is.na(Hitters$Salary))

Hitters <- na.omit(Hitters)
dim(Hitters)

library(leaps)
regfit.full <- regsubsets(Salary ~ .,Hitters)
#funkcja szuka najlepszego modelu dla kazdego rozmiaru, domylsnie uzywa RSS

summary(regfit.full)
reg.summary <- summary(regfit.full)
names(reg.summary)

# Można zwiększyć dopuszczalną liczbę zmiennych niezależnych
regfit.full <- regsubsets(Salary ~ ., data = Hitters, nvmax = 19)

# summary() zwraca również m.in. R^2, RSS, skorygowany R^2, C_p, BIC
reg.summary <- summary(regfit.full)
names(reg.summary)
# dodatkowe miary można wykorzystać do wybrania
# najlepszego modelu ogólnie

# Analizujemy R^2 dla kolejnych 19 modeli:
reg.summary$rsq

# Narysowanie wykresów dla wszystkich miar ułatwi wybór najlepszego modelu
par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables",
     ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables",
     ylab = "Adjusted RSq", type = "l")

# Znajdujemy i zaznaczamy max dla Adjusted RSq:
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 2, pch = 20)

# Wykonujemy analogiczne rysunki dla C_p oraz BIC:
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)

points(10, reg.summary$cp[10], col = "red", cex = 2, pch = 20)

which.min(reg.summary$bic)

plot(reg.summary$bic, xlab = "Number of Variables",
     ylab = "BIC", type = "l")

points(6, reg.summary$bic[6], col = "red", cex = 2, pch = 20)


# regsubsets() ma wbudowaną funkcję plot(),
# którą można wyśwwietlić, które zmienne zostały wybrane do kolejnych modeli,
# posortowanych według wybranego kryterium
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
plot(regfit.full, scale = "Cp")


plot(regfit.full, scale = "bic")

regfit.fwd <- regsubsets(Salary ~ .,Hitters, nvmax = 19, method='forward')
summary(regfit.fwd)

regfit.bwd <- regsubsets(Salary ~ .,Hitters, nvmax = 19, method='backward')
summary(regfit.bwd)

coef(regfit.full, 6)
coef(regfit.fwd, 6)
coef(regfit.bwd, 6)

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(Hitters), replace = TRUE)
test <- (!train)

regfit.best <- regsubsets(Salary ~ . ,data=Hitters[train,], nvmax = 19)

summary(regfit.best)

#nie da sie uzyc funkcji regsubsets do predict wiec trzeba kombinowac (macierz na piechote robimy)

test.mat <- model.matrix(Salary ~ . ,data = Hitters[test,])
test.mat

val.errors <- rep(NA, 19)
for (i in 1:19){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean(Hitters$Salary[test] - pred)^2
}
val.errors
which.min(val.errors)
coef(regfit.best, 7)

#funkcja liczaca ta macierz i predict od razu
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[ , xvars] %*% coefi
}

k <- 10
n <- nrow(Hitters)
set.seed(1)
folds <- sample(rep(1:k, length = n))

cv.errors <- matrix(NA, k ,19 , dimnames = list(NULL, paste(1:19)))

#uruchamiamy CV

for (j in 1:k){
  best.fit <- regsubsets(Salary ~ . , data=Hitters[folds !=j,], nvmax = 19)
  for (i in 1:19){
    pred <- predict.regsubsets(best.fit, Hitters[folds == j,], id=i)
    cv.errors[j,i] <- mean(Hitters$Salary[folds==j]-pred)^2
  }
}
cv.errors
par(mfrow = c(1, 1))
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
plot(mean.cv.errors, type='b')


#REGRESJA GRZBIETOWA

library('glmnet')
Hitters <- na.omit(Hitters)

x <- model.matrix(Salary ~ . , Hitters)[,-1] #automatycznie koduje jakosciowe jako 0 i 1
y <- Hitters$Salary

grid <- 10^seq(10,-2,length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda = grid)

ridge.mod$lambda[50]
coef(ridge.mod)[,50]

coef(ridge.mod)[,60]

coef(ridge.mod)[,100]

coef(ridge.mod)[,10]


predict(ridge.mod, s=50, type='coefficients')[1:20] #wspolczynniki dla konkretnej lambdy (s)

set.seed(1)
train <- sample(1:nrow(x), nrow(x) /2)
test <- -(train)
y.test <- y[test]
y.test
train

ridge.mod <-glmnet(x[train,], y[train], alpha=0, lambda = grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx = x[test,])
mean((ridge.pred-y.test)^2)

#blad dla modelu z samym wyrazem wolnym czyli srednia z Y na trainie
mean((mean(y[train])-y.test)^2)


ridge.pred <- predict(ridge.mod, s=1e-10, newx = x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred <- predict(ridge.mod, s=0, newx = x[test,])
mean((ridge.pred-y.test)^2)
#tutaj sprawdzalismy jakosc modelu na podstawie bledu srednikwadratowego

#metoda walidacji krzyzowej
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx = x[test,])
mean((ridge.pred-y.test)^2)

out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients", s=bestlam)[1:20,]
lm(Salary ~ . ,data = Hitters)

lasso.mod <-glmnet(x[train,], y[train], alpha=1, lambda = grid)
plot(lasso.mod, 'lambda')

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam_lasso<-cv.out$lambda.min
bestlam_lasso

out <- glmnet(x,y,alpha=1, lambda=grid)
predict(out,type="coefficients", s=bestlam_lasso)[1:20,]
