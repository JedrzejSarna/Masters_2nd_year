setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Modele liniowe')

#funkcja do generowania danych do modelu 1
sim_1 = function(sample_size = 500){
  x = runif(n=sample_size)*5
  y = 3 + 5*x + rnorm(n=sample_size, mean=0, sd=1)
  data.frame(x,y)
}

#funkcja do generowania danych do modelu 2
sim_2 = function(sample_size = 500){
  x = runif(n=sample_size)*5
  y = 3 + 5*x + rnorm(n=sample_size, mean=0, sd=x)
  data.frame(x,y)
}

#funkcja do generowania danych do modelu 3
sim_3 = function(sample_size = 500){
  x = runif(n=sample_size)*5
  y = 3 + 5*x^2 + rnorm(n=sample_size, mean=0, sd=5)
  data.frame(x,y)
}

#generowanie danych do modelu 1
set.seed(42)
sim_data_1 = sim_1()
head(sim_data_1)
par(mfrow =c(1,1))
plot(y~x, data=sim_data_1, col='grey', pch=20, main='Data from Model 2')
fit_1 = lm(y~x, data=sim_data_1)
abline(fit_1, col='darkorange', lwd=3)
plot(fitted(fit_1), resid(fit_1), xlab='Fitted', ylab='Residuals')
abline(h=0, col='darkorange', lwd=3)

#szybki sposob sprawdzania wariancji
summary(lm(abs(residuals(fit_1))~fitted(fit_1)))
plot(fitted(fit_1), abs(resid(fit_1)))
abline(coef(lm(abs(residuals(fit_1))~fitted(fit_1))),col='darkorange', lwd=3)



sim_data_2 = sim_2()
head(sim_data_2)
par(mfrow =c(1,1))
plot(y~x, data=sim_data_2, col='grey', pch=20, main='Data from Model 2')
fit_2 = lm(y~x, data=sim_data_2)
#generowanie danych do modelu 3

sim_data_3 = sim_3()
head(sim_data_3)
par(mfrow =c(1,1))
plot(y~x, data=sim_data_3, col='grey', pch=20, main='Data from Model 3')
fit_3 = lm(y~x, data=sim_data_3)
abline(fit_2, col='darkorange', lwd=3)
plot(fitted(fit_3), resid(fit_3), xlab='Fitted', ylab='Residuals')
abline(h=0, col='darkorange', lwd=3)

#testowanie homoscedastyczności

library(lmtest)
#test Breuscha-Pagana
bptest(fit_1)
bptest(fit_2) #test odrzuca hipoteze o stalosci wariancji - tu mamy do czynienia z homoscedastycznoscia
bptest(fit_3)

#Histogramy dla reszt
par(mfrow=c(1,3))
hist(resid(fit_1), main='Residuals of fit_1')
hist(resid(fit_2), main='Residuals of fit_2')
hist(resid(fit_3), main='Residuals of fit_3')
#lepsze od histogramu jest wykres normalnosci reszt (Q-Q plot)
qqnorm(resid(fit_1), main = 'Normal Q-Q plot',col='gray')
qqline(resid(fit_1), col='blue')
qqnorm(resid(fit_2), main = 'Normal Q-Q plot',col='gray')
qqline(resid(fit_2), col='blue')
qqnorm(resid(fit_3), main = 'Normal Q-Q plot',col='gray')
qqline(resid(fit_3), col='blue')

#cos tam bylo dalej o qqplocie 
