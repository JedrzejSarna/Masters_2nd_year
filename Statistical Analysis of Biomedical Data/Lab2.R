setwd("/Users/jedrzejsarna/Desktop/II stopien 4 semestr/Statystyczna Analiza Danych Biomedycznych")

library(readxl)
Dane <- read_excel("Dane 1.1.xlsx")
D <- Dane$"D"
T <- Dane$"T"
n<-length(D)
Tab<-table(T,D)
D
T
Tab[1]
Tab[2]
#N11,N10
#N01,N00
Tab
#sens <- N11/N01+N11

#spec <- N00/N00+N10


# N11 ma rozklad dwumianowy z parametraim (N.1, i p to to co szukamuy)

spec <- Tab[1]/(Tab[2]+Tab[1])
spec

sens <- Tab[4]/(Tab[4]+Tab[3])
sens


spec_std <- sqrt(spec*(1-spec)/(Tab[2]+Tab[1]))
spec_std

sens_std <- sqrt(sens*(1-sens)/(Tab[4]+Tab[3]))
sens_std

p<- 0.08


PPV <- sens*p / (sens*p + (1-spec)*(1-p))
PPV

NPV <- spec*(1-p) / (spec*(1-p) + (1-sens)*p)
NPV

logit_PPV <- log(p/(1-p)) + log(sens) - log(1-spec)
logit_NPV <- log((1-p)/p) + log(spec) - log(1-sens)

logit_PPV_std <- sqrt((1-sens)/(sens*(Tab[3]+Tab[4])) + (spec)/((1-spec)*(Tab[1]+Tab[2])))
logit_PPV_std

logit_NPV_std <- sqrt((sens)/((1-sens)*(Tab[3]+Tab[4])) + (1-spec)/((spec)*(Tab[1]+Tab[2])))
logit_NPV_std


q=qnorm(0.975,0,1)


exp(logit_PPV-logit_PPV_std*q)/(1+exp(logit_PPV-logit_PPV_std*q))
exp(logit_PPV+logit_PPV_std*q)/(1+exp(logit_PPV+logit_PPV_std*q))

exp(logit_NPV-logit_NPV_std*q)/(1+exp(logit_NPV-logit_NPV_std*q))
exp(logit_NPV+logit_NPV_std*q)/(1+exp(logit_NPV+logit_NPV_std*q))

library(bdpv)
#Gotowiec
M<-matrix(c(Tab[2,2], Tab[1,2], Tab[2,1], Tab[1,1]), ncol=2)
BDtest(xmat=M, pr=0.08, conf.level = 0.95)


log_OR <- sqrt(Tab[1]+Tab[2]+Tab[3]+Tab[4])

OR <- (Tab[1]/Tab[2]) / (Tab[3]/Tab[4])
OR


