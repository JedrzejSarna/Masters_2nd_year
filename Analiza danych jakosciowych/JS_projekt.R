setwd("/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Analiza danych jakosciowych")

library(MASS)
library(caret)
library(Hmisc)

#### OPIS DANYCH
pima_df <- read.csv("diabetes.csv")
names(pima_df)

#pima_df$Outcome <- factor(pima_df$Outcome)
pima_df$DiabetesPedigreeFunction <- NULL

#### WSTEPNA EKSPLORACJA DANYCH
summary(pima_df)
sum(is.na(pima_df))

par(mfrow=c(1,2))

# ZMIENNA PREGNANCIES
hist(pima_df$Pregnancies, main="Histogram liczby ciąż", xlab="Liczba ciąż", ylab="Częstotliwość")
boxplot(pima_df$Pregnancies, main="Wykres pudełkowy liczb ciąż", ylab="Liczba ciąż")
readline("Nacisnij ENTER")

summary(pima_df$Pregnancies)
categories <- c(-Inf, 0, 1, 2, 4, 7, Inf)
labels <- c("0", "1", "2","[3,4]", "[5,7]", ">7")
pima_df$Pregnancies_category <- cut(pima_df$Pregnancies, breaks = categories, labels = labels, right = TRUE)
table(pima_df$Pregnancies_category)
readline("Nacisnij ENTER")

pima_df$Pregnancies <- NULL

# ZMIENNA GLUCOSE
summary(pima_df$Glucose)
sum(pima_df$Glucose==0)
pima_df <- subset(pima_df, Glucose != 0)

hist(pima_df$Glucose, main="Histogram poziomu glukozy", xlab="Poziom glukozy", ylab="Częstotliwość")
boxplot(pima_df$Glucose, main="Wykres pudełkowy poziomu glukozy", ylab="Poziom glukozy")
readline("Nacisnij ENTER")

categories <- c(-Inf, 139, Inf)
labels <- c("prawidłowy","wysoki")
pima_df$Glucose_category <- cut(pima_df$Glucose, breaks = categories, labels = labels, right = TRUE)
table(pima_df$Glucose_category)
readline("Nacisnij ENTER")

pima_df$Glucose <- NULL

# ZMIENNA CISNIENIE
hist(pima_df$BloodPressure, main="Histogram ciśnienia", xlab="Ciśnienie rozkurczowe", ylab="Częstotliwość")
boxplot(pima_df$BloodPressure, main="Wykres pudełkowy ciśnienia", ylab="Ciśnienie rozkurczowe")
readline("Nacisnij ENTER")

sort(pima_df$BloodPressure)
sum(pima_df$BloodPressure==0)
pima_df <- subset(pima_df, BloodPressure != 0)
summary(pima_df$BloodPressure)

# ZMIENNA SKINTHICKNESS
par(mfrow=c(1,1))
hist(pima_df$SkinThickness)
sort(pima_df$SkinThickness)
sum(pima_df$SkinThickness==0)

pima_df$SkinThickness <- NULL

# ZMIENNA INSULIN
hist(pima_df$Insulin)
sort(pima_df$Insulin)
sum(pima_df$Insulin==0)

pima_df$Insulin <- NULL

# ZMIENNA BMI
par(mfrow=c(1,2))

hist(pima_df$BMI, main="Histogram BMI", xlab="BMI", ylab="Częstotliwość")
boxplot(pima_df$BMI, main="Wykres pudełkowy BMI", ylab="BMI")
readline("Nacisnij ENTER")

sum(pima_df$BMI==0)
sort(pima_df$BMI)
pima_df <- subset(pima_df, BMI != 0)

# ZMIENNA AGE
par(mfrow=c(1,1))
hist(pima_df$Age)
categories <- c(0, 23, 30,40, Inf)
labels <- c("<24", "[24,30]","[31,40]", ">40")

pima_df$Age_category <- cut(pima_df$Age, breaks = categories, labels = labels, right = TRUE)
table(pima_df$Age_category)

pima_df$Age <- NULL


#### ANALIZA ZMIENNYCH JAKOSCIOWYCH

# AGE
contingency_table_age <- table(pima_df$Age_category, pima_df$Outcome)
contingency_table_age
readline("Nacisnij ENTER")


cuk.prob_age <- tapply(pima_df$Outcome,pima_df$Age_category,mean)
cuk.prob_age

#do wyliczania przedzialow ufnosci
outcome_age_table <- table(pima_df$Outcome, pima_df$Age_category)

binconf(outcome_age_table[2,1],outcome_age_table[1,1]+outcome_age_table[2,1],method="asymptotic",alpha=0.05)
binconf(outcome_age_table[2,1],outcome_age_table[1,1]+outcome_age_table[2,1],method="wilson")

binconf(outcome_age_table[2,2],outcome_age_table[1,2]+outcome_age_table[2,2],method="asymptotic",alpha=0.05)
binconf(outcome_age_table[2,2],outcome_age_table[1,2]+outcome_age_table[2,2],method="wilson")

binconf(outcome_age_table[2,3],outcome_age_table[1,3]+outcome_age_table[2,3],method="asymptotic",alpha=0.05)
binconf(outcome_age_table[2,3],outcome_age_table[1,3]+outcome_age_table[2,3],method="wilson")

binconf(outcome_age_table[2,4],outcome_age_table[1,4]+outcome_age_table[2,4],method="asymptotic",alpha=0.05)
binconf(outcome_age_table[2,4],outcome_age_table[1,4]+outcome_age_table[2,4],method="wilson")

#test chi-kwadrat
result <- chisq.test(outcome_age_table)
print(result)

# GLUCOSE
contingency_table_gluc <- table(pima_df$Glucose_category, pima_df$Outcome)
contingency_table_gluc
readline("Nacisnij ENTER")

cuk.prob_gluc <- tapply(pima_df$Outcome,pima_df$Glucose_category,mean)
cuk.prob_gluc

#do wyliczania przedzialow ufnosci
outcome_gluc_table <- table(pima_df$Outcome, pima_df$Glucose_category)

binconf(outcome_gluc_table[2,1],outcome_gluc_table[1,1]+outcome_gluc_table[2,1],method="asymptotic",alpha=0.05)
binconf(outcome_gluc_table[2,1],outcome_gluc_table[1,1]+outcome_gluc_table[2,1],method="wilson")

binconf(outcome_gluc_table[2,2],outcome_gluc_table[1,2]+outcome_gluc_table[2,2],method="asymptotic",alpha=0.05)
binconf(outcome_gluc_table[2,2],outcome_gluc_table[1,2]+outcome_gluc_table[2,2],method="wilson")


# PREGNANCY
outcome_preg_table <- table(pima_df$Outcome, pima_df$Pregnancies_category)
outcome_preg_table
readline("Nacisnij ENTER")

#do wyliczania przedzialow ufnosci
binconf(outcome_preg_table[2,1],outcome_preg_table[1,1]+outcome_preg_table[2,1],method="asymptotic",alpha=0.05)
binconf(outcome_preg_table[2,1],outcome_preg_table[1,1]+outcome_preg_table[2,1],method="wilson")

binconf(outcome_preg_table[2,2],outcome_preg_table[1,2]+outcome_preg_table[2,2],method="asymptotic",alpha=0.05)
binconf(outcome_preg_table[2,2],outcome_preg_table[1,2]+outcome_preg_table[2,2],method="wilson")

binconf(outcome_preg_table[2,3],outcome_preg_table[1,2]+outcome_preg_table[2,3],method="asymptotic",alpha=0.05)
binconf(outcome_preg_table[2,3],outcome_preg_table[1,2]+outcome_preg_table[2,3],method="wilson")

binconf(outcome_preg_table[2,4],outcome_preg_table[1,2]+outcome_preg_table[2,4],method="asymptotic",alpha=0.05)
binconf(outcome_preg_table[2,4],outcome_preg_table[1,2]+outcome_preg_table[2,4],method="wilson")

binconf(outcome_preg_table[2,5],outcome_preg_table[1,2]+outcome_preg_table[2,5],method="asymptotic",alpha=0.05)
binconf(outcome_preg_table[2,5],outcome_preg_table[1,2]+outcome_preg_table[2,5],method="wilson")

binconf(outcome_preg_table[2,6],outcome_preg_table[1,2]+outcome_preg_table[2,6],method="asymptotic",alpha=0.05)
binconf(outcome_preg_table[2,6],outcome_preg_table[1,2]+outcome_preg_table[2,6],method="wilson")

#test chi-kwadrat
result <- chisq.test(outcome_preg_table)
print(result)


#### ANALIZA ZMIENNYCH ILOSCIOWYCH
pima_df$Outcome <- factor(pima_df$Outcome)
# BMI
bmi_model <- glm(Outcome ~ BMI, family = binomial, data = pima_df)
summary(bmi_model)
readline("Nacisnij ENTER")


# BLOODPRESSURE
bloodpr_model <- glm(Outcome ~ BloodPressure, family = binomial, data = pima_df)
summary(bloodpr_model)
readline("Nacisnij ENTER")




#### BUDOWA MODELU

#0. model pusty
null_model <- glm(Outcome ~ 1, data = pima_df, family = "binomial")
summary(null_model)
readline("Nacisnij ENTER")


#1. model z BMI
model <- glm(Outcome ~ BMI, family = binomial, data = pima_df)
summary(model)
readline("Nacisnij ENTER")


#wykres dla modelu z BMI
bmi_range <- seq(min(pima_df$BMI), max(pima_df$BMI), length.out = 300)
newdata <- data.frame(BMI = bmi_range)
predictions <- predict(model, newdata = newdata, type = "response")
par(mfrow=c(1,1))
plot(pima_df$BMI, pima_df$Outcome, xlab = "Wartość BMI", ylab = "Prawdopodobieństwo cukrzycy", pch = 20, col = ifelse(pima_df$Outcome == 1, "red", "blue"))
lines(bmi_range, predictions, col = "black", lwd = 2)
readline("Nacisnij ENTER")


#2. model z BMI i AGE_CATEGORY

model2 <- glm(Outcome ~ BMI + Age_category, family = binomial, data = pima_df)
summary(model2)
readline("Nacisnij ENTER")

confint(model2)

#tworzenie wykresu dla grup
res1<-predict(model2, type="response", newdata=data.frame(BMI=seq(18,68,1), Age_category="<24"))
res2<-predict(model2, type="response", newdata=data.frame(BMI=seq(18,68,1),Age_category="[24,30]"))
res3<-predict(model2, type="response", newdata=data.frame(BMI=seq(18,68,1),Age_category="[31,40]"))
res4<-predict(model2, type="response", newdata=data.frame(BMI=seq(18,68,1),Age_category=">40"))

plot(seq(18,68,1),res4,type="l",ylab="Przewidywane prawdobodobieństwo", axes=F, xlab="Wartość BMI", col="brown")
axis(2, at=seq(0,1,.2))
axis(1, at=seq(18,68,1))
lines(seq(18,68,1),res3, col="gold")
lines(seq(18,68,1),res2, col="darkgreen")
lines(seq(18,68,1),res1, col="green")
legend("bottomright", legend = c("<24", "[24,30]", "[31,40]", ">40"), col = c("green", "darkgreen","gold", "brown"), lty = 1)
readline("Nacisnij ENTER")

#3. model z BMI i GLUCOSE_CATEGORY
model3 <- glm(Outcome ~ BMI + Glucose_category, family = binomial, data = pima_df)
summary(model3)
readline("Nacisnij ENTER")

confint(model3)

#tworzenie wykresu dla grup
res1<-predict(model3, type="response", newdata=data.frame(BMI=seq(18,68,1), Glucose_category="prawidłowy"))
res2<-predict(model3, type="response", newdata=data.frame(BMI=seq(18,68,1),Glucose_category="wysoki"))

plot(seq(18,68,1),res1,type="l",ylab="Przewidywane prawdobodobieństwo", axes=F, xlab="Wartość BMI")
axis(2, at=seq(0,1,.2))
axis(1, at=seq(18,68,1))
lines(seq(18,68,1),res2, col = "red")
legend("bottomright", legend = c("prawidłowy", "wysoki"), col = c("black", "red"), lty = 1)
readline("Nacisnij ENTER")

#4. model z BMI i AGE_CATEGORY i GLUCOSE_CATEGORY
model4 <- glm(Outcome ~ BMI + Age_category + Glucose_category, family = binomial, data = pima_df)
summary(model4)
readline("Nacisnij ENTER")

confint(model4)


# RESZTA MODELI
model5 <- glm(Outcome ~ BMI + Age_category + Glucose_category + Pregnancies_category, family = binomial, data = pima_df)
summary(model5)
readline("Nacisnij ENTER")
confint(model5)

model6 <- glm(Outcome ~ BMI + Age_category + Glucose_category + BloodPressure, family = binomial, data = pima_df)
summary(model6)
readline("Nacisnij ENTER")
confint(model6)


model7 <- glm(Outcome ~ ., family = binomial, data = pima_df)
summary(model7)
readline("Nacisnij ENTER")
confint(model7)


#### OCENA MODELU 4

#za pomoca zbioru treningowego i testowego
set.seed(123) 
index <- createDataPartition(pima_df$Outcome, p = 0.8, list = FALSE)
train_data <- pima_df[index, ]
test_data <- pima_df[-index, ]

model4_train <- glm(Outcome ~ BMI + Glucose_category + Age_category, family = binomial, data = train_data)

test_data$predicted_prob <- predict(model4_train, newdata = test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)

confusionMatrix <- table(test_data$predicted_class, test_data$Outcome)
print(confusionMatrix)

accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(accuracy)

