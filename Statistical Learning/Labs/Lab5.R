library(ISLR2)

Hitters <- na.omit(Hitters)
x <- model.matrix(Salary ~ ., Hitters)[, -1]
y <- Hitters$Salary
set.seed(1)
train <- sample(1: nrow(x), nrow(x) / 2)
test <- (-train)
y.test <- y[test]

library(pls)
set.seed(2)
pcr.fit <- pcr(Salary ~., data=Hitters, scale=TRUE, validation = "CV")
pcr.fit$coefficients

validationplot(pcr.fit, val.type = "MSEP")

pcr_train.fit <- pcr(Salary ~., data=Hitters[train,], scale=TRUE, validation = "CV")
summary(pcr_train.fit)
validationplot(pcr_train.fit, val.type = "MSEP")

pcr.pred <- predict(pcr_train.fit, x[test,], ncomp = 5)
mean((pcr.pred-y.test)^2)

pcr_train.fit

#PLSR
set.seed(1)
pls.fit <- plsr(Salary ~., data=Hitters, scale=TRUE, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls_train.fit <- plsr(Salary ~., data=Hitters[train,], scale=TRUE, validation = "CV")
summary(pls_train.fit)
validationplot(pls_train.fit, val.type = "MSEP")

pls.pred <- predict(pls_train.fit, x[test,], ncomp = 3)
mean((pls.pred-y.test)^2)
