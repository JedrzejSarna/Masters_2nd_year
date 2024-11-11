library(bdpv)
library(BioProbability)
library(readxl)

stworz_licznosci_N<- function(Tab) {
    N <- c(
      N11 = Tab[2,2],
      N01 = Tab[1,2],
      N10 = Tab[2,1],
      N00 = Tab[1,1]
    )
  return(N)
}

stworz_wektor_p <- function(N) {
  p <- N/n
  names(p) <- gsub("N", "p", names(p))
  return(p)
}

oblicz_macierz_Sigma <- function(p) {
  n <- length(p)
  Sig <- matrix(0, nrow = n, ncol = n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        Sig[i, j] <- p[i] * (1 - p[i])
      } else {
        Sig[i, j] <- -p[i] * p[j]
      }
    }
  }
  return(Sig)
}

oblicz_OR <- function(p) {
  OR <- (p[1] * p[4]) / (p[2] * p[3])
  return(as.numeric(OR))
}


oblicz_pochodne <- function(p) {
  df <- numeric(4)
  df[1] <- 1 / p[1]
  df[2] <- -1 / p[2]
  df[3] <- -1 / p[3]
  df[4] <- 1 / p[4]
  return(df)
}

oblicz_odchylenie_standardowe <- function(df, Sig) {
  s <- sqrt(t(df) %*% Sig %*% df / n)
  return(as.numeric(s))
}

oblicz_przedzialy_ufnosci <- function(OR, log_OR_std, alpha) {
  z_value <- qnorm(1 - alpha / 2)
  CI_lower <- OR * exp(- log_OR_std * z_value)
  CI_upper <- OR * exp(log_OR_std * z_value)
  return(c(CI_lower, CI_upper))
}

oblicz_p_wartosc <- function(OR, log_OR_std) {
  z_value <- abs(log(OR)) / log_OR_std
  p_value <- 2 * (1 - pnorm(z_value))
  return(p_value)
}

#####################################################################################

#ustawic sciezke
#setwd("")

Dane <- read_excel("Dane 1.1.xlsx")
D<-Dane$"D"
T<-Dane$"T"
n<-length(D)
Tab<-table(T,D)
Tab 

#wyniki z gotowca
odds.ratio(Tab,conf.int=TRUE)

N <- stworz_licznosci_N(Tab)
N
#odwolywanie sie do wartosci
N['N11']

p <- stworz_wektor_p(N)
p
#odwolywanie sie do wartosci
p['p11']

Sigma <- oblicz_macierz_Sigma(p)
Sigma


OR <- oblicz_OR(p)
OR
odds.ratio(Tab,conf.int=TRUE)$`Odds Ratio`
#takie same wyniki

df <- oblicz_pochodne(p)
df

log_OR_std <- oblicz_odchylenie_standardowe(df, Sigma)
log_OR_std
sqrt(1/N['N11'] + 1/N["N10"] + 1/N["N01"] + 1/N["N00"])
#takie same wyniki

oblicz_przedzialy_ufnosci(OR, log_OR_std, alpha = 0.05 )
odds.ratio(Tab,conf.int=TRUE)$`Confidence Interval of level 5%`
#takie same wyniki

oblicz_p_wartosc(OR, log_OR_std)
