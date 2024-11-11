setwd("/Users/jedrzejsarna/Desktop/II stopien 4 semestr/Statystyczna Analiza Danych Biomedycznych")

library(readxl)
Dane <- read_excel("Dane 1.1.xlsx")
D <- Dane$"D"
T <- Dane$"T"
n<-length(D)
Tab<-table(T,D)
Tab

#N11,N10
#N01,N00

#sens <- N11/N01+N11

#spec <- N00/N00+N10


# N11 ma rozklad dwumianowy z parametraim (N.1, i p to to co szukamuy)

sens <- Tab[1]/(Tab[2]+Tab[1])
sens

spec <- Tab[4]/(Tab[4]+Tab[3])
spec

N11=Tab[2,2]
N10=Tab[2,1]
N01=Tab[1,2]
N00=Tab[1,1]
Sens=N11/(N11+N01)
Spec=N00/(N10+N00)


sens_std <- sqrt(sens*(1-sens)/(Tab[2]+Tab[1]))
sens_std

spec_std <- sqrt(spec*(1-spec)/(Tab[4]+Tab[3]))
spec_std


sens-sens_std*1.96
sens+sens_std*1.96

spec
spec-spec_std*1.96
spec+spec_std*1.96

for (i in seq(0, 1, 0.01)) {
  print(i)
}
q=qnorm(0.975,0,1)
q

library(bdpv)
#Gotowiec
M<-matrix(c(Tab[2,2], Tab[1,2], Tab[2,1], Tab[1,1]), ncol=2)
BDtest(xmat=M, pr=0.08, conf.level = 0.95)

