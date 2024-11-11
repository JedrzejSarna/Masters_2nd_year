setwd("/Users/jedrzejsarna/Desktop/II stopien 4 semestr/Statystyczna Analiza Danych Biomedycznych")

library(readxl)
Dane <- read_excel("Dane 4.1.xlsx")

D <- Dane$"D"
T <- Dane$"T"
T
plot(ecdf(T))
#estymator dystrybuant

T_D_minus <- Dane$T[Dane$D == 0]
T_D_plus <-Dane$T[Dane$D == 1]

distr_T_D_minus <- ecdf(T_D_minus)
distr_T_D_plus <- ecdf(T_D_plus)

x_values <- numeric(length(T))
y_values <- numeric(length(T))
youden_idx <- numeric(length(T))
for (i in seq_along(T)) {
  t <- T[i]
  x_values[i] <- 1 - distr_T_D_minus(t)
  y_values[i] <- 1 - distr_T_D_plus(t)
  youden_idx[i] <- y_values[i] - x_values[i]
}

plot(x_values, y_values)
youden_idx
max(youden_idx)
index_max_value<- which.max(youden_idx)
T[index_max_value]

1-x_values[index_max_value] #specyficznosc
y_values[index_max_value] #czulosc

points(x_values[index_max_value], y_values[index_max_value], col='red')




library(bdpv)
#Gotowiec
TD <- as.numeric(T>T[index_max_value])
Tab<-table(TD,D)
Tab
M<-matrix(c(Tab[2,2], Tab[1,2], Tab[2,1], Tab[1,1]), ncol=2)
BDtest(xmat=M, pr=0.08, conf.level = 0.95)
#czulosc i specyficznosc z gotowca oraz PPV i NPV