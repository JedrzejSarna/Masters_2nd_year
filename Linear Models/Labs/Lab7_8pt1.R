setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Modele liniowe')

####################  LAB 7. ######################


library(openxlsx)
#wyb?r optymalnego modelu
dane2 <- read.xlsx("Ott461.xlsx",colNames = TRUE)
dane3<-dane2 #przygotowanie danych 
colnames(dane3)<-c("Volume","Floor_space","Presc_RX","Parking","ShopCntr","Income")
dane3$ShopCntr <- factor(dane3$ShopCntr)
levels(dane3$ShopCntr) <- c("no","yes")
attach(dane3)
zm=c("Floor_space","Presc_RX","Parking","ShopCntr","Income")
#indeksy wszystkich podzbior?w indeksowanego zbioru length(zm)
library(e1071)
help("bincombinations")#zwraca macierz  2^p wektor?w o d?ugo?ci p
bincombinations(2)#przyk?ad 1
bincombinations(3) #przyk?ad 2
bincombinations(3)[-1,] #pomijamy pierwszy wiersz odpowiadaj?cy modelowi pustemu
wsp = (bincombinations(length(zm))==1 )[-1,]
params=matrix(0,nrow(wsp),5)#przygotowujemy macierz na wska?niki jako?ci poszczeg?lnych modeli
library(olsrr)# do Cp 
fullmodel<-lm(Volume ~ Floor_space+Presc_RX+Parking+ShopCntr+Income,data=dane3)#potrzebne do Cp

for (i in 1:nrow(wsp)) {
  form = as.formula(paste("Volume~",paste(zm[wsp[i,]],collapse="+")))
  model=lm(form,data=dane3)
  params[i,1]=AIC(model,k=log(nrow(dane3)))
  params[i,2]=model$rank
  params[i,3]=summary(model)$adj.r.squared
  params[i,4]=AIC(model)
  params[i,5]=ols_mallows_cp(model, fullmodel)
}

# model optymalny w sensie BIC
as.formula(paste("Volume~",paste(zm[wsp[which.min(params[,1]),]],collapse="+")))

# model optymalny w sensie R^2_adj 
as.formula(paste("Volume~",paste(zm[wsp[which.max(params[,3]),]],collapse="+")))

# model optymalny w sensie  AIC
as.formula(paste("Volume~",paste(zm[wsp[which.min(params[,4]),]],collapse="+")))

# model optymalny w sensie  Cp
as.formula(paste("Volume~",paste(zm[wsp[which.min(params[,5]),]],collapse="+"))) 

AIC(lm(Volume ~ Floor_space + Presc_RX))
#wyznaczenia na piechot? wska?nika AIC
-2*logLik(lm(Volume ~ Floor_space + Presc_RX))+8 #4 parametry:wyraz wolny,Floor_space,Presc_RX, sigma^2
#wyznaczenia na piechot? wska?nika Cp
deviance(lm(Volume ~ Floor_space + Presc_RX))
summary_fullmodel<-summary(fullmodel)
s<-summary_fullmodel$sigma
deviance(lm(Volume ~ Floor_space + Presc_RX))/s^2+6-20 #6=2*(2+1) 
deviance(lm(Volume ~ Floor_space + Presc_RX))/s^2+2*lm(Volume ~ Floor_space + Presc_RX)$rank-20 #6=2*(2+1)

#	Wyb?r modelu w oparciu o RMSELOOCV
#tworzymy dane do modelu y = 3 + x + 4 * x ^ 2

make_poly_data = function(sample_size = 11) {
  x = seq(0, 10)
  y = 3 + x + 4 * x ^ 2 + rnorm(n = sample_size, mean = 0, sd = 20)
  data.frame(x, y)
}
set.seed(1234)
poly_data = make_poly_data()

#dopasowujemy 2 modele
fit_quad = lm(y ~ poly(x, degree = 2), data = poly_data) # dopasowujemy wielomian st. 2
fit_big  = lm(y ~ poly(x, degree = 8), data = poly_data) # dopasowujemy wielomian st. 8
plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad, newdata = data.frame(x = xplot)),col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big, newdata = data.frame(x = xplot)),col = "darkorange", lwd = 2, lty = 2)
#Wyznaczamy RMSE dla obu modeli
sqrt(mean(resid(fit_quad) ^ 2)) 
sqrt(mean(resid(fit_big) ^ 2))

#funkcja wyznaczaj?ca RMSELOOCV
calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}
#wyznaczamy RMSELOOCV dla obu modeli
calc_loocv_rmse(fit_quad) 
calc_loocv_rmse(fit_big)

#Wida?, ?e  model big ma teraz znacznie wi?kszy RMSELOOCV ni? model  quad

#Poka?emy graficznie  jak zachowuj? si? oba modele  przy usuni?ciu trzeciej obserwacji
fit_quad_removed = lm(y ~ poly(x, degree = 2), data = poly_data[-3, ])
fit_big_removed  = lm(y ~ poly(x, degree = 8), data = poly_data[-3, ])
plot(y ~ x, data = poly_data, ylim = c(-100, 400), cex = 2, pch = 20)
xplot = seq(0, 10, by = 0.1)
lines(xplot, predict(fit_quad_removed, newdata = data.frame(x = xplot)),
      col = "dodgerblue", lwd = 2, lty = 1)
lines(xplot, predict(fit_big_removed, newdata = data.frame(x = xplot)),
      col = "darkorange", lwd = 2, lty = 2)




####################  LAB 8. ######################




library(faraway)
help(seatpos) #opis zbioru danych
seatpos
hipcenter_mod = lm(hipcenter ~ ., data = seatpos) #model addytywny ze wszystkimi zmiennymi
coef(hipcenter_mod)
extractAIC(hipcenter_mod) # zwraca  p (liczba parametrow) i AIC=nlog(RSS/n)+2p
AIC(hipcenter_mod) #zwykle AIC

help("step")
#regresja krokowa backward z kryterium AIC
hipcenter_mod_back_aic = step(hipcenter_mod, direction = "backward")
#odejmuje zmienne z najnizszym AIC do momentu az <none> bedzie najwyzej
#na podstawie algorytmu z wykladu

#regresja krokowa backward z kryterium BIC
n = length(resid(hipcenter_mod))
hipcenter_mod_back_bic = step(hipcenter_mod, direction = "backward", k = log(n))
#taka sama sytuacja, AIC powinno sie nazywac BIC tylko

#regresja krokowa forward z kryterium AIC
hipcenter_mod_start = lm(hipcenter ~ 1, data = seatpos) #model pusty
hipcenter_mod_forw_aic = step(
  hipcenter_mod_start, # od tego modelu startujemy
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg, # ostatni model z zakresu
  direction = "forward")
# wypluta zmienna na samej gorze zwieksza dopasowanie, ona bedzie dodawana

#regresja krokowa forward z kryterium BIC
hipcenter_mod_forw_bic = step(
  hipcenter_mod_start,
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
  direction = "forward", k = log(n))

#regresja krokowa both z kryterium AIC
hipcenter_mod_both_aic = step(
  hipcenter_mod_start,
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
  direction = "both")
#regresja krokowa both z kryterium BIC
hipcenter_mod_both_bic = step(
  hipcenter_mod_start,
  scope = hipcenter ~ Age + Weight + HtShoes + Ht + Seated + Arm + Thigh + Leg,
  direction = "both", k = log(n))



#regresja krokowa backward dla danych aptecznych
model_b=lm(Volume~.,data=dane3)
extractAIC(model_b)
model_b_aic = step(model_b, direction = "backward")  

#regresja krokowa forward dla danych aptecznych
model_start=lm(Volume ~ 1,data=dane3)
model_fwd_aic=step(
  model_start,
  scope=Volume ~ Floor_space + Presc_RX + Parking + ShopCntr + Income, 
  direction = "forward")

#Funkcja regsubsets() z pakietu leaps