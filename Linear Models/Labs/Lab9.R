library(faraway)

data (meatspec)#Spektrometria miesa do oznaczania zawartosci tluszczu
#zmienne V1-V100 - absorbcja w zakresie 100 d?ugo?ci fali 
# 101-sza zmienna fat zawiera dane dotycz?ce tluszczu
# pierwszych 172 obserwacji potraktujemy jako zbi?r ucz?cy a reszt? 173 d0 215 tako zbi?r testowy

View(meatspec)
help("meatspec") 
model1 <- lm (fat~.,meatspec[1:172,]) # dopasowujemy model pe?ny na zbiorze ucz?cym
summary(model1)$r.squared
rmse <- function(x,y) sqrt(mean((x-y)^2))# funkcja wyznaczaj?ca RMSE 
rmse(model1$fit, meatspec$fat [1:172]) # rms dla zbioru ucz?cego
rmse(predict(model1,meatspec[173:215,]),meatspec$fat[173:215])# rms dla zbioru testowego


### REGRESJA KROKOWA ###

mode12 <-step(model1) #regresja krokowa dla zbioru uczacego, wyrzucil 28 zmiennych
rmse(mode12$fit,meatspec$fat[1:172])
rmse(predict(mode12,meatspec[173:215,]),meatspec$fat[173:215])



### REGRESJA GRZBIETOWA ###
library (MASS)
yc <- meatspec$fat[1:172]-mean(meatspec$fat[1:172]) #centrujemy zmienne bo trzeba
mm <- apply(meatspec[1:172,-101],2,mean)# wyznaczamy srednie dla 100 kolumn zbioru uczacego
trainx <- as.matrix(sweep(meatspec[1:172,-101],2,mm))#centrowanie predyktorow, otrzymujemy scentrowany zbior treningowy
gridge <- lm.ridge(yc~trainx,lambda=seq(0,5e-8,1e-9))
matplot(gridge$lambda,t(gridge$coef), type="l",lty=1,xlab=expression (lambda), ylab=expression (hat (beta)))
select(gridge)
abline (v=1.8e-8)
which.min(gridge$GCV) #wyznaczenie optymalnych wspolczynnikow metoda GCV (cross-validation)
ypredg <- scale(trainx,center=FALSE,scale=gridge$scales)%*% gridge$coef [, 19] + mean(meatspec$fat[1:172])
rmse(ypredg,meatspec$fat[1:172])
testx <- as.matrix(sweep (meatspec [173:215,-101], 2, mm))
ytpredg <- scale(testx,center=FALSE, scale=gridge$scales)%*% gridge$coef [, 19] + mean(meatspec$fat[1:172])
rmse(ytpredg, meatspec$fat[173:215])

#duze RMSE jednak duzy blad jest tylko w przypadku jednej obserwacji
ytpredg- meatspec$fat[173:215]
c(ytpredg[13],meatspec$fat [172+13])
#po jej usunieciu rmse jest calkiem dobry
rmse(ytpredg[-13],meatspec$fat[173:215][-13])


y1 <- meatspec$fat[1:172]
trainx1 <- as.matrix(meatspec[1:172,-101],2,mm)
gridge1 <- lm.ridge(y1~trainx1,lambda=seq(0,5e-8,1e-9))
matplot(gridge1$lambda,t(gridge1$coef), type="l",lty=1,xlab=expression (lambda), ylab=expression (hat (beta)))
select(gridge1)
#widac ze skalowanie nie jest potrzebne - takie same wyniki
which.min(gridge1$GCV) #wyznaczenie optymalnych wspolczynnikow metoda GCV (cross-validation)
ypredg1 <- trainx1 %*%gridge1$coef[,19]
rmse(ypredg1,meatspec$fat[1:172])
#JEDNAK ROZNE WYNIKI, WIEKSZE RMSE!!
testx1 <- as.matrix(meatspec [173:215,-101])
ytpredg1 <- testx1 %*%gridge1$coef[,19]
rmse(ytpredg1, meatspec$fat[173:215])


library(glmnet)
require(graphics)
#  zobacz     https://www.statology.org/lasso-regression-in-r/

summary(meatspec)
#To perform lasso regression, we?ll use functions from the glmnet package
#This package requires the response variable to be a vector and the set of predictor variables
#to be of the class data.matrix.


#define response variable
y <- meatspec[1:172,101]

#define matrix of predictor variables
x <- data.matrix(meatspec[1:172,1:100])

#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)
cv_model_ridge <- cv.glmnet(x, y, alpha = 0)
#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
best_lambda_ridge <- cv_model_ridge$lambda.min
best_lambda_ridge
#KOMPLETNIE INNA LAMBDA WYCHODZI XD



#produce plot of test MSE by lambda value
plot(cv_model)
plot(cv_model_ridge) 



#find coefficients of best model

#Note that setting alpha equal to 0 is equivalent to using ridge regression 
#and setting alpha to some value between 0 and 1 is equivalent to using an elastic net. 
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)
#define new observation
#new = as.matrix(meatspec[173:225,1:100]), nrow=1, ncol=100))
new = data.matrix(meatspec[173:215,1:100]) 

#use lasso regression model to predict response value
predict(best_model, s = best_lambda, newx = new)

#use fitted best model to make predictions
y_predicted <- predict(best_model, s = best_lambda, newx = new)

#find SST and SSE
y_test<-meatspec[173:215,101]

sst <- sum((y_test - mean(y_test))^2)
sse <- sum((y_predicted - y_test)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq

rmse (y_predicted, meatspec$fat[173:215])
y_predicted- meatspec$fat[173:215]

best_model_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge)
coef(best_model_ridge)
#use ridge regression model to predict response value
predict(best_model_ridge, s = best_lambda_ridge, newx = new)
#use fitted best model to make predictions
y_predicted_ridge <- predict(best_model_ridge, s = best_lambda_ridge,newx = new)
sse <- sum((y_predicted_ridge - y_test)^2)
#find R-Squared
rsq <- 1 - sse/sst
rsq
rmse (y_predicted_ridge, meatspec$fat[173:215])


### TO SAMO Z INNYMI DANYMI
library(glmnet) 
require(graphics) 
help("mtcars") 

pairs(mtcars, main = "mtcars data", gap = 1/4) 
coplot(mpg ~ disp | as.factor(cyl), data = mtcars,panel = panel.smooth, rows = 1) 

# possibly more meaningful, e.g., for summary() or bivariate plots: 

mtcars2 <- within(mtcars, { 
  
  vs <- factor(vs, labels = c("V", "S")) 
  
  am <- factor(am, labels = c("automatic", "manual")) 
  
  cyl  <- ordered(cyl) 
  
  gear <- ordered(gear) 
  
  carb <- ordered(carb) 
  
}) 

summary(mtcars2) 

#To perform lasso regression, we’ll use functions from the glmnet package 

#This package requires the response variable to be a vector and the set of predictor variables 

#to be of the class data.matrix. 

#define response variable 

y <- mtcars$hp 
#define matrix of predictor variables 

x <- data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')]) 



#perform k-fold cross-validation to find optimal lambda value 

cv_model <- cv.glmnet(x, y, alpha = 1) 

cv_model_ridge <- cv.glmnet(x, y, alpha = 0) 

#find optimal lambda value that minimizes test MSE 

best_lambda <- cv_model$lambda.min 

best_lambda

best_lambda_ridge <- cv_model_ridge$lambda.min 

best_lambda_ridge 

#produce plot of test MSE by lambda value 

plot(cv_model)  

plot(cv_model_ridge)  
#find coefficients of best model 
#Note that setting alpha equal to 0 is equivalent to using ridge regression  

#and setting alpha to some value between 0 and 1 is equivalent to using an elastic net.  

best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda) 

coef(best_model) 

#define new observation 

new = matrix(c(24, 2.5, 3.5, 18.5), nrow=1, ncol=4)  



#use lasso regression model to predict response value 

predict(best_model, s = best_lambda, newx = new) 





#use fitted best model to make predictions 

y_predicted <- predict(best_model, s = best_lambda, newx = x) 
#find SST and SSE 
sst <- sum((y - mean(y))^2) 

sse <- sum((y_predicted - y)^2) 

#find R-Squared 

rsq <- 1 - sse/sst 
rsq 
#To samo dla regresji grzbietowej 

best_model_ridge <- glmnet(x, y, alpha = 0, lambda = best_lambda_ridge) 

coef(best_model_ridge) 

#use ridge regression model to predict response value 

predict(best_model_ridge, s = best_lambda_ridge, newx = new) 

#use fitted best model to make predictions 

y_predicted_ridge <- predict(best_model_ridge, s = best_lambda_ridge, newx = x) 

sse <- sum((y_predicted_ridge - y)^2) 

#find R-Squared 

rsq <- 1 - sse/sst 

rsq 
