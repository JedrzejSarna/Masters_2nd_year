setwd("/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Statistical Learning")

library(ggplot2)
library(dplyr)
library(tree)
library(randomForest)
library(Cubist)
library(gbm)
library(reshape2)
library(caret)
library(xgboost)


data<-read.csv("nba_salaries.csv")

#### WSTEPNA EKSPLORACJA DANYCH
colnames(data) <- gsub("\\.", "_perc", colnames(data))

#usuniecie bezuzytecznych kolumn
data<- data[,-c(1,ncol(data))]

#analiza zarobkow
data <- data[-((nrow(data)-1):nrow(data)),]

hist(data$Salary, 
     xlab = "Zarobki", 
     ylab = "Liczba graczy", 
     main= NULL,
     col = "darkorange",
     axes = FALSE)
axis(2)
ax <- pretty(data$Salary)
new_labels <- sapply(ax, function(x) paste(format(round(x / 1e6), big.mark = " "), "milionów"))
axis(1, at = ax, labels = new_labels)
box()


#### SZCZEGOLOWA ANALIZA STATYSTYK MECZOWYCH
sum(is.na(data))
na_summary <- data %>% summarize_all(~sum(is.na(.)))
na_summary
#NA wystepuja przy zmiennych procentowych okreslajacych ratio
na_rows <- data[!complete.cases(data), ]
#wszystkie NA zastepujemy 0
data[is.na(data)] <- 0

#statystyki
stats <- data[,c(6:ncol(data))]
general_info <- data[, c(1:5)]

#korelacje z salary
correlations <- sapply(stats, function(x) cor(x, data$Salary, use = "complete.obs"))
salary_correlations <- data.frame(variable = names(correlations), correlation = correlations)

ggplot(salary_correlations, aes(x = reorder(variable, correlation), y = correlation, fill = correlation)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1)) +  
  scale_fill_gradient(low = "lightpink", high = "red") +
  labs(x = 'Statystyka', y = 'Korelacja ze zmienną Salary') +
  theme_minimal()

#macierz korelacji
correlation_matrix <- cor(stats)
melted_correlation_matrix <- melt(correlation_matrix)

ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank())

#usuwamy wysoko skorelowane
high_correalted <- c("TRB", "FGA", "X3PA", "X2PA", "FTA")
stats_2 <- stats[,!(names(stats) %in% high_correalted)]



#### SZCZEGOLOWA ANALIZA OGOLNYCH INFORMACJI

#Pozycja
unique(general_info$Position)
general_info$Position <- gsub("-.*", "", general_info$Position)
table(general_info$Position)

ggplot(general_info, aes(x = Position, y = Salary, fill = Position)) + 
  geom_boxplot() +
  scale_fill_brewer(palette = "Set1") +  
  labs(x = "Team", y = "Salary") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

general_info$Position <- ifelse(general_info$Position %in% c('PG'), 'PG', 
                            ifelse(general_info$Position %in% c("PF", "SG", "SF", "C"), 'OTHER_POS', general_info$Position))

table(general_info$Position)

#Zespol
unique(general_info$Team)
general_info$Team <- gsub("/.*", "", general_info$Team)

group_1 <- c("GSW", "NYK", "LAL")
group_2 <- c("BOS", "LAC", "CHI", "DAL", "HOU", "PHI", "TOR", "PHO", "MIA", "BRK", "WAS")
group_3 <- c("DEN", "CLE", "SAC", "ATL", "SAS", "MIL", "UTA", "POR", "DET", "OKC", "CHO", "ORL", "IND")
group_4 <- c("NOP", "MIN", "MEM")

get_group_name <- function(abbreviation) {
  if (abbreviation %in% group_1) {
    return('BEST_BUDGET')
  } else if (abbreviation %in% group_2) {
    return('GOOD_BUDGET')
  } else if (abbreviation %in% group_3) {
    return('AVERAGE_BUDGET')
  } else if (abbreviation %in% group_4) {
    return('WORST_BUDGET')
  } else {
    return('UNKNOWN')
  }
}
general_info$Team <- sapply(general_info$Team, get_group_name)
table(general_info$Team)

ggplot(general_info, aes(x = Team, y = Salary, fill = Team)) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = "Set1") + 
  labs(x = "Team", y = "Salary") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),  
        legend.position = "none")

#zamiana na dwie grupy
general_info$Team <- ifelse(general_info$Team %in% c('GOOD_BUDGET', 'BEST_BUDGET'), 'GOOD_BUDGET', 
                            ifelse(general_info$Team %in% c('WORST_BUDGET', 'AVERAGE_BUDGET'), 'BAD_BUDGET', general_info$Team))

table(general_info$Team)


#usuniecie imion i nazwisk
general_info <- general_info[,-1]

#polaczenie w jeden zbior
prepared_data <- cbind(general_info, stats_2)
View(prepared_data)

#przerobienie zmiennych jakosciowych na factor
prepared_data$Position <- factor(prepared_data$Position)
prepared_data$Team <- factor(prepared_data$Team)

#sprawdzenie typow kolumn
column_types <- sapply(prepared_data, class)
type_counts <- table(column_types)
print(type_counts)


#### BUDOWA MODELU

set.seed(1234)
train <- sample(1:nrow(prepared_data),nrow(prepared_data)*0.8)
data.train <- prepared_data[train,]
data.test <- prepared_data[-train,]

hist(data.train$Salary)
hist(data.test$Salary)
summary(data.train$Salary)
summary(data.test$Salary)

model_tree_train1 <- tree(Salary ~ ., data = data.train)

plot(model_tree_train1)
text(model_tree_train1, pretty=0, cex=0.6)




#### OCENA MODELU
prediction_tree_1 <- predict(model_tree_train1, data.test)

summary(prediction_tree_1)
summary(data.test$Salary)

MAE <- function(actual, predicted){
  mean(abs(actual-predicted))
}
over_under_priced <- function(actual, predicted){
  sum(actual-predicted)
}
biggest_error_abs <- function(actual, predicted){
  sort(abs(actual-predicted))
}
biggest_error <- function(actual, predicted){
  sort(actual-predicted)
}

MAE(data.test$Salary, prediction_tree_1)
over_under_priced(data.test$Salary, prediction_tree_1)
biggest_error_abs(data.test$Salary, prediction_tree_1)
biggest_error(data.test$Salary, prediction_tree_1)

prediction_tree_1
data.test$Salary

#Przeprowadzenie walidacji krzyżowej
cv_model_tree_train1 <- cv.tree(model_tree_train1, FUN = prune.tree)

#Wykres błędu walidacji krzyżowej
plot(cv_model_tree_train1$size, cv_model_tree_train1$dev, type = 'b',
     xlab = "Liczba liści", ylab = "Błąd walidacji krzyżowej")

#Znalezienie rozmiaru drzewa z minimalnym błędem
opt_size <- which.min(cv_model_tree_train1$dev)
cv_model_tree_train1$dev
opt_size <- cv_model_tree_train1$size[opt_size]

#Przycinanie drzewa
pruned_tree <- prune.tree(model_tree_train1, best = opt_size)

#przycięte drzewo
plot(pruned_tree)
text(pruned_tree, pretty = 0, cex=0.6)

prediction_pruned_tree <- predict(pruned_tree, data.test)

summary(prediction_pruned_tree)
summary(data.test$Salary)

MAE(data.test$Salary, prediction_pruned_tree)
over_under_priced(data.test$Salary, prediction_pruned_tree)
biggest_error_abs(data.test$Salary, prediction_pruned_tree)
biggest_error(data.test$Salary, prediction_pruned_tree)

#### DOPRACOWANIE MODELU

#### REGRESJA W LISCIACH
model_reg_tree <- cubist(x=data.train[-1], y=data.train$Salary)
model_reg_tree

summary(model_reg_tree)

prediction_tree_2 <- predict(model_reg_tree, data.test)
summary(prediction_tree_2)
summary(data.test$Salary)

MAE(data.test$Salary, prediction_tree_2)
over_under_priced(data.test$Salary, prediction_tree_2)
biggest_error_abs(data.test$Salary, prediction_tree_2)
biggest_error(data.test$Salary, prediction_tree_2)

prediction_tree_2
data.test$Salary

MAE_and_SD <- function(actual, predicted) {
  abs_errors <- abs(actual - predicted)

  mae <- mean(abs_errors)
  sd_abs_errors <- sd(abs_errors)
  return(list(MAE = mae, SD_Errors = sd_abs_errors))
}

MAE_and_SD(data.test$Salary, prediction_tree_2)
MAE_and_SD(data.test$Salary, prediction_tree_1)


#### BAGGING I LASY LOSOWE

#bagging
bag.tree <- randomForest(Salary ~ ., data = prepared_data, subset = train,
                          mtry = 23, importance = TRUE, ntree = 500)
bag.tree$importance

plot(bag.tree$mse)

prediction_tree_3 <- predict(bag.tree, newdata = data.test)
prediction_tree_3
plot(prediction_tree_3, data.test$Salary)
abline(0, 1)  
MAE_3 <- MAE(prediction_tree_3, data.test$Salary)

over_under_priced(data.test$Salary, prediction_tree_3)
biggest_error_abs(data.test$Salary, prediction_tree_3)
biggest_error(data.test$Salary, prediction_tree_3)
MAE_and_SD(data.test$Salary, prediction_tree_3)

#lasy losowe
rf.tree <- randomForest(Salary ~ ., data = prepared_data, subset = train,
                         mtry = 8, importance = TRUE)
rf.tree$importance

plot(rf.tree$mse)

prediction_tree_4 <- predict(rf.tree, newdata = data.test)
plot(prediction_tree_4, data.test$Salary)
abline(0, 1)  

MAE_4 <- MAE(prediction_tree_4, data.test$Salary)

over_under_priced(data.test$Salary, prediction_tree_4)
biggest_error_abs(data.test$Salary, prediction_tree_4)
biggest_error(data.test$Salary, prediction_tree_4)
MAE_and_SD(data.test$Salary, prediction_tree_4)


#bledy MAE dla 5 wykonan dla kazdego z paremetrow mtry
#UWAGA WYKONANIE TRWA PARE MINUT
all_mae_values <- list()

for (i in 1:5) {
  mae_values <- numeric(23)
  
  for (m in 1:23) {
    rf.tree <- randomForest(Salary ~ ., data = prepared_data, subset = train,
                            mtry = m, importance = TRUE, ntree = 500)
    prediction_tree <- predict(rf.tree, newdata = data.test)
    
    mae_values[m] <- MAE(data.test$Salary, prediction_tree)
  }
  all_mae_values[[i]] <- mae_values
}
all_mae_values
mae_df <- do.call(rbind, lapply(1:length(all_mae_values), function(i) {
  data.frame(mtry = 1:23, MAE = all_mae_values[[i]], Iteration = as.factor(i))
}))

ggplot(mae_df, aes(x = mtry, y = MAE, group = Iteration, color = Iteration)) +
  geom_line() +
  geom_point() +
  scale_color_brewer(type = "qual", palette = "Set1") +
  labs(x = "Liczba cech (mtry)", y = "Średni błąd bezwzględny (MAE)") +
  theme_minimal() +
  theme(legend.title = element_blank())  


#### BOOSTING i XGBOOSTING

#boosting

# Określenie siatki parametrów do przetestowania
grid <- expand.grid(interaction.depth = c(1, 3, 5),
                    n.trees = c(100, 500, 1000),
                    shrinkage = c(0.001, 0.01, 0.1, 0.3),
                    n.minobsinnode = c(10, 20))

# Ustawienie  walidacji krzyżowej
control <- trainControl(method = "cv", number = 5)

# Dostrojenie modelu
set.seed(1234)
model <- train(Salary ~ ., data = prepared_data[train, ],
               method = "gbm",
               trControl = control,
               tuneGrid = grid,
               verbose = FALSE)

print(model$bestTune)

final_model <- gbm(Salary ~ ., data = prepared_data[train, ],
                   distribution = "gaussian",
                   n.trees = model$bestTune$n.trees,
                   interaction.depth = model$bestTune$interaction.depth,
                   shrinkage = model$bestTune$shrinkage,
                   n.minobsinnode = model$bestTune$n.minobsinnode,
                   verbose = FALSE)

summary(final_model)

plot(final_model, i = "Age")
plot(final_model, i = "TOV")
plot(final_model, i = "FT")
plot(final_model, i = "MP")
plot(final_model, i = "AST")
plot(final_model, i = "PTS")


prediction_tree_final <- predict(final_model, newdata = data.test)

plot(prediction_tree_final, data.test$Salary, xlab="Predykcje zarobków", ylab = "Rzeczywiste zarobki")
abline(0, 1)  

MAE_final <- MAE(prediction_tree_final, data.test$Salary)

over_under_priced(data.test$Salary, prediction_tree_final)
biggest_error_abs(data.test$Salary, prediction_tree_final)
biggest_error(data.test$Salary, prediction_tree_final)
MAE_and_SD(data.test$Salary, prediction_tree_final)

# xgboosting
train_x <- data.matrix(data.train[,-1])
train_y <- data.train[[1]]

test_x <- data.matrix(data.test[,-1])
test_y <- data.test[[1]]

xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

watchlist <- list(train=xgb_train, test=xgb_test)

set.seed(1234)

model_depth3 <- xgb.train(data = xgb_train, max.depth = 3, watchlist = watchlist, nrounds = 200)
model_depth4 <- xgb.train(data = xgb_train, max.depth = 4, watchlist = watchlist, nrounds = 200)
model_depth5 <- xgb.train(data = xgb_train, max.depth = 5, watchlist = watchlist, nrounds = 200)
model_depth6 <- xgb.train(data = xgb_train, max.depth = 6, watchlist = watchlist, nrounds = 200)

plot(model_depth3$evaluation_log$test_rmse, type='l', xlab = "Liczba iteracji", ylab = "Błąd RMSE na zbiorze testowym")
lines(model_depth4$evaluation_log$test_rmse, type='l', col='red')
lines(model_depth5$evaluation_log$test_rmse, type='l', col='blue')
lines(model_depth6$evaluation_log$test_rmse, type='l', col='green')
legend("topright", 
       legend = c("3", "4", "5", "6"), 
       col = c("black", "red", "blue", "green"), 
       lty = 1, # Styl linii
       cex = 0.8,
       title = "Głębokość drzewa")

print(min(model_depth4$evaluation_log$test_rmse))
print(which.min(model_depth4$evaluation_log$test_rmse))

#najlepszy model
final_model_xgb <- xgboost(data = xgb_train, max.depth = 4, nrounds = 31, verbose = 0)

summary(final_model_xgb)

prediction_tree_final_xgb <- predict(final_model_xgb, xgb_test)

plot(prediction_tree_final_xgb, data.test$Salary, xlab="Predykcje zarobków", ylab = "Rzeczywiste zarobki")
abline(0, 1)  

MAE(prediction_tree_final_xgb, test_y)

over_under_priced(data.test$Salary, prediction_tree_final_xgb)
biggest_error_abs(data.test$Salary, prediction_tree_final_xgb)
biggest_error(data.test$Salary, prediction_tree_final_xgb)
MAE_and_SD(data.test$Salary, prediction_tree_final_xgb)

plot(prediction_tree_final_xgb, data.test$Salary, 
     xlab="Predykcje zarobków", ylab = "Rzeczywiste zarobki",
     col = 'red',
     pch = 16)

points(prediction_tree_final, data.test$Salary, 
       col = 'blue',
       pch = 17)

abline(0, 1,lwd = 2)

legend("bottomright",
       legend = c("XGB", "Tree Model"), 
       col = c("red", "blue"),
       pch = c(16, 17), 
       title = "Model")
