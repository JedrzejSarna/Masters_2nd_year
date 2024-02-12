# Ustawienia
k_values <- seq(0.2, 5, by = 0.025)
n <- 100
alpha <- 0.05

# Funkcja do generowania próby z rozkładu Weibulla
generate_weibull_sample <- function(k) {
  rweibull(n, shape = k, scale = 1) #przy H0
}

# Funkcja wiarygodności dla rozkładu wykładniczego 
likelihood_exp <- function(data) {
  lambda <- 1/mean(data)
  lambda^n * exp(-lambda * sum(data))
}

# Funkcja wiarygodności dla rozkładu Weibulla 
likelihood_weibull <- function(data, k) {
  lambda_hat <- (sum(data^k) / length(data))^(1/k)
  (k / lambda_hat)^n * prod((data / lambda_hat)^(k - 1)) * exp(-sum((data / lambda_hat)^k))
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

plot(k_values, empirical_power, type = 'b', xlab = 'Parametr k', ylab = 'Moc empiryczna',
     main = 'Moc empiryczna testu w zależności od parametru k')

# Tworzenie wykresu
plot(k_values, empirical_power, type = 'b', xlab = 'Parametr k', ylab = 'Moc empiryczna',
     main = 'Moc empiryczna testu w zależności od parametru k', xlim = c(0.5, 2))





#### POPRZEDNIE PODEJSCIE

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

empirical_power1 <- numeric(length(k_values))
num_simulations1 <- 1000

# Symulacja
for (i in 1:length(k_values)) { #przechodzimy przez wszystkie k
  k <- k_values[i]
  rejections1 <- 0
  
  for (j in 1:num_simulations1) {
    sample <- generate_weibull_sample(k) #generujemy probe z rozkladu Weibulla
    test_result <- lr_test(sample) #liczymy statystyke testowa
    if (test_result$p_value < alpha) {
      rejections1 <- rejections1 + 1
    }
  }
  
  empirical_power1[i] <- rejections1 / num_simulations1
}

# Tworzenie wykresu
plot(k_values, empirical_power, type = 'b', xlab = 'Parametr k', ylab = 'Moc empiryczna',
     main = 'Moc empiryczna testu w zależności od parametru k')
lines(k_values, empirical_power1, type = 'b', col = 'red')

plot(k_values, empirical_power, type = 'b', xlab = 'Parametr k', ylab = 'Moc empiryczna',
     main = 'Moc empiryczna testu w zależności od parametru k', xlim = c(0.5,2))
lines(k_values, empirical_power1, type = 'b', col = 'red',  xlim = c(0.5,2))

