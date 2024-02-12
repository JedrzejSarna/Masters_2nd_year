setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Modele liniowe')

library(faraway)
data("pima")
pima
summary(pima)
#problemy z danymi

pima$diastolic[pima$diastolic==0]<-NA
pima$glucose[pima$glucose==0]<-NA
pima$triceps[pima$triceps==0]<-NA
pima$insulin[pima$insulin==0]<-NA
pima$bmi[pima$bmi==0]<-NA

pima$test<-factor(pima$test)
levels(pima$test) <- c('negative', 'positive')
summary(pima)

hist(pima$diastolic)
plot(density(pima$diastolic, na.rm = TRUE))
plot(sort(pima$diastolic), pch=".")

plot(diabetes~test,pima)
pairs(pima)
subset<-pima[,c(1,2,3)]
pairs(subset)

#pierwsze podejscie do regresji
data("stat500")
head(stat500)
stat500 <- data.frame(scale(stat500))
stat500
plot(stat500)
plot(final~midterm, stat500)
abline(0,1)

g<-lm(final~midterm, stat500)
g
abline(coef(g), lty=5)
cor(stat500)
summary(g)

g1<-lm(final~midterm-1, stat500) #bez wyrazu wolnego, gdyz nie powinno go byc przez standaryzacje
summary(g1)
abline(c(0,coef(g1)), lty=5) #nowa linia regresji


help("teengamb")
data("teengamb")
head(teengamb)
summary(teengamb)

teengamb$sex<-factor(teengamb$sex)
levels(teengamb$sex) <- c('male','female')
teengamb <- data.frame(teengamb)
plot(density(teengamb$income, na.rm=TRUE))
plot(gamble~sex, teengamb)

g1<-lm(gamble~income, teengamb)
plot(gamble~income, teengamb)
abline(coef(g1), lty=5)


#tygodniowe zarobki amerykanskich pracownikow

data("uswages")
summary(uswages)
uswages2 <-uswages

uswages2$race <- factor(uswages2$race)
levels(uswages2$race)<-c('White', 'Black')
uswages2$place <- {1*uswages2$ne+2*uswages2$nw+3*uswages2$so+4*uswages2$we}
#dalej zamienianie danych na jakosciowe jak wyzej itp..


help("prostate")
data("prostate")
summary(prostate)
prostate$svi <-factor(prostate$svi)
levels(prostate$svi) <- c('no','yes')
plot(prostate$svi)


data(gala)
