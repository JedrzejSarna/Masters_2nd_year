# wczytanie danych
imdb_data <- read.csv("imdb_top_1000.csv")
View(imdb_data)

summary(imdb_data)

# usunięcie niepotrzebnych kolumn
preprocess_imdb_data <- subset(imdb_data, select = -c(Poster_Link, Series_Title))

# usunięcie " min" z całej kolumny Runtime oraz zamiana "," na "." w Gross
preprocess_imdb_data$Runtime <- sub(" min", "", preprocess_imdb_data$Runtime)
preprocess_imdb_data$Gross <- gsub(",", "", preprocess_imdb_data$Gross)

# zamiana typów kolumn na numeryczne
preprocess_imdb_data[, c("Released_Year", "Runtime", "Gross", "Meta_score")] <- 
  apply(preprocess_imdb_data[, c("Released_Year", "Runtime", "Gross")], 2, as.numeric)

# zamiana pustych strinów na NA
preprocess_imdb_data[] <- lapply(preprocess_imdb_data, function(x) ifelse(x == "", NA, x))

colSums(is.na(preprocess_imdb_data))

View(preprocess_imdb_data)

unique(preprocess_imdb_data$Certificate)

distinct_values <- unique(unlist(strsplit(preprocess_imdb_data$Genre, "\\,")))
print(distinct_values)
distinct_values <- sub(" ", "", distinct_values)
final_distinct_columns <- unique(distinct_values)
print(final_distinct_columns)
for (genre in final_distinct_columns){
  preprocess_imdb_data[genre] <- ifelse(grepl(genre, preprocess_imdb_data$Genre),1,0)
}
View(preprocess_imdb_data)

for (genre in final_distinct_columns){
  print(paste(sum(preprocess_imdb_data[genre]), genre))
}
########## Potrzebne są dodatkowe działania do analizy ale to już zrobimy na zajęciach ##########
table(preprocess_imdb_data$Star1)

# podział danych na zbiór treningowy i testowy
set.seed(1234)

# Ustalenie proporcji podziału (70% trening, 30% test)
prop <- 0.7

# Wygenerowanie indeksów dla zbioru treningowego
train_idx <- sample(1:nrow(preprocess_imdb_data), prop * nrow(preprocess_imdb_data))

# Podział danych na zbiór treningowy i testowy
train <- preprocess_imdb_data[train_idx, ]
test <- preprocess_imdb_data[-train_idx, ]

# Obliczenie średniej wartości kolumny IMDB_Rating dla zbioru treningowego i testowego
mean(train$Gross)
mean(test$Gross)
