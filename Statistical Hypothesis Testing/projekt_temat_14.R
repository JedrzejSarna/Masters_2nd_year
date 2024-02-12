# Ustawienia
k_values <- seq(0.5, 2, by = 0.05)
n <- 100
alpha <- 0.05

# Funkcja do generowania próby z rozkładu Weibulla
generate_weibull_sample <- function(k) {
  rweibull(n, shape = k, scale = 1) #precyzuje lambda = 1
}

# Funkcja wiarygodności dla rozkładu wykładniczego 
likelihood_exp <- function(data) {
  lambda <- 1/mean(data)
  lambda^n * exp(-lambda * sum(data))
}
# Funkcja wiarygodności dla rozkładu Weibulla 
likelihood_weibull <- function(data, k) {
  k^n * prod(data^(k-1)) * exp(-sum(data^k)) #juz dla lambdy = 1
}

# Test ilorazu wiarygodności, test asymptotycznie zgodny
lr_test <- function(data) {
  l_exp <- likelihood_exp(data) #wartosc funkcji wiarygodnosci dla danych przy H0
  l_weibull_max <- max(sapply(k_values, function(k) likelihood_weibull(data, k))) #supremum funkcji wiarygodnosci dla danych przy H1
  lr <- -2 * log(l_exp / l_weibull_max) #dla H0
  p_value <- pchisq(lr, df = 1, lower.tail = FALSE)
  list(lr = lr, p_value = p_value)
}


empirical_power <- numeric(length(k_values))
num_simulations <- 1000

# Symulacja
for (i in 1:length(k_values)) { #przechodzimy przez wszystkie k
  k <- k_values[i]
  rejections <- 0
  
  for (j in 1:num_simulations) {
    sample <- generate_weibull_sample(k) #generujemy probe z rozkladu Weibulla
    test_result <- lr_test(sample) #liczymy statystyke testowa
    if (test_result$p_value < alpha) {
      rejections <- rejections + 1
    }
  }
  
  empirical_power[i] <- rejections / num_simulations
}

# Tworzenie wykresu
plot(k_values, empirical_power, type = 'l', xlab = 'Parametr k', ylab = 'Moc empiryczna',
     main = 'Moc empiryczna testu w zależności od parametru k')

k_values[51]
empirical_power[51]
empirical_power[47:53]
