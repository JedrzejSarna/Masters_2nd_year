setwd("/Users/jedrzejsarna/Desktop/II stopien 4 semestr/Statystyczna Analiza Danych Biomedycznych")

# Załadowanie niezbędnych bibliotek
library(bdpv)
library(BioProbability)
library(readxl)

# Wczytanie danych z pliku Excel
Dane <- read_excel("Dane 3.1.xlsx")

# Przypisanie kolumn do zmiennych
D <- Dane$"D"
TA <- Dane$"TA"
TB <- Dane$"TB"
n <- length(D)

# Tworzenie tabel kontyngencji
Tab <- table(TA, TB, D)
TabA <- table(TA, D)
TabB <- table(TB, D)

# Tworzenie macierzy dla testów BD
MA <- matrix(c(TabA[2,2], TabA[1,2], TabA[2,1], TabA[1,1]), ncol=2)
MB <- matrix(c(TabB[2,2], TabB[1,2], TabB[2,1], TabB[1,1]), ncol=2)

# Wykonanie testów BD
BDtest(xmat=MA, pr=0.08, conf.level = 0.95)
BDtest(xmat=MB, pr=0.08, conf.level = 0.95)

# Obliczanie ilorazów szans
odds.ratio(MA, conf.int=TRUE)
odds.ratio(MB, conf.int=TRUE)

# Wyodrębnianie liczności z tabeli Tab
N111 <- Tab[2,2,2]
N110 <- Tab[2,2,1]
N101 <- Tab[2,1,2]
N100 <- Tab[2,1,1]
N011 <- Tab[1,2,2]
N010 <- Tab[1,2,1]
N001 <- Tab[1,1,2]
N000 <- Tab[1,1,1]

# Obliczanie prawdopodobieństw
p <- c(N111, N110, N101, N100, N011, N010, N001, N000) / n

# Inicjalizacja macierzy Sig
Sig <- matrix(nrow=8, ncol=8)

# Wypełnianie macierzy Sig
for (i in 1:8) {
  for (j in 1:8) {
    if (i == j) {
      Sig[i, j] <- p[i] * (1 - p[i])
    } else {
      Sig[i, j] <- -p[i] * p[j]
    }
  }
}

# Obliczanie ilorazów szans ORA i ORB
ORA <- (p[1] + p[3]) * (p[6] + p[8]) / ((p[2] + p[4]) * (p[5] + p[7]))
ORB <- (p[1] + p[5]) * (p[4] + p[8]) / ((p[2] + p[6]) * (p[3] + p[7]))

# Obliczanie różnic wektorów df
df <- numeric(8)
df[1] <- 1 / (p[1] + p[3]) - 1 / (p[1] + p[5])
df[2] <- 1 / (p[2] + p[6]) - 1 / (p[2] + p[4])
df[3] <- 1 / (p[3] + p[1]) + 1 / (p[3] + p[7])
df[4] <- -1 / (p[4] + p[2]) - 1 / (p[4] + p[8])
df[5] <- -1 / (p[5] + p[7]) - 1 / (p[5] + p[1])
df[6] <- 1 / (p[6] + p[8]) + 1 / (p[6] + p[2])
df[7] <- 1 / (p[7] + p[3]) - 1 / (p[7] + p[5])
df[8] <- 1 / (p[8] + p[6]) - 1 / (p[8] + p[4])

# Obliczanie odchylenia standardowego
s <- sqrt(t(df) %*% Sig %*% df / n)

# Obliczanie wartości p
pval <- 2 * (1 - pnorm(abs(log(ORA) - log(ORB)) / s, 0, 1))

# Wyświetlanie wyników
print(ORA)
print(ORB)
print(pval)
print(Tab)

# Obliczanie fałszywej wartości p
falsepval <- 2 * (1 - pnorm(abs(log(ORA) - log(ORB)) / sqrt(sum(1 / Tab)), 0, 1))
print(falsepval)
