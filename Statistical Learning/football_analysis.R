setwd("/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Statistical Learning")

#### ZALADOWANIE BIBLIOTEK
library(readxl)
library(countrycode)
library(rpart)
library(rpart.plot)
library(tree)
library(xgboost)
library(stringdist)
library(ggplot2)
library(reshape2)
library(gridExtra)


#### WSTEPNE ZALADOWANIE DANYCH
data <- read_excel('final_dataset.xlsx')
#View(data)

#### ANALIZA ZMIENNEJ VALUE
hist(data$Value, 
     xlab = "Wartość rynkowa", 
     ylab = "Liczba wystąpień", 
     main= NULL,
     col = "darkgreen",
     axes = FALSE)

axis(2)
ax <- pretty(data$Value)
new_labels <- sapply(ax, function(x) paste(format(round(x / 1e6), big.mark = " "), "milionów"))
axis(1, at = ax, labels = new_labels)
box()

summary(data$Value)

#### WSTEPNE PRZYGOTOWANIE DANYCH

data <- data[data$Position != 'Goalkeeper', ]

# Assuming 'df' is your dataframe and 'column_name' is the column based on which you want to remove duplicates
data <- data[!duplicated(data$Player), ]


table(data$`Contract Years Left`)
#przy zmiennej Contract Years Left mamy dziwna pozycje 'fail', jest ich tylko 43 wiec usuwamy

data <- data[data$`Contract Years Left` != 'fail', ]
data$`Contract Years Left` <- as.numeric(data$`Contract Years Left`)

#branie tych ktorzy grali w ostatnich 4 sezonach (niektorzy maja mniej)
clubs <- c('Squad (20/21)', 'Squad (19/20)', 'Squad (18/19)', 'Squad (17/18)')
complete_rows <- complete.cases(data[clubs])
data_complete <- data[complete_rows, ]


#### SZCZEGOLOWA ANALIZA STATYSTYK
column_names <- names(data_complete)
column_names
stats <- column_names[-(1:8)]

non_stat_columns <- data_complete[, c(1:8)]
#zachowanie wierszy nie bedacych 'statystykami'

data_clean <- data_complete[ , -c(1:8)]

stats <- stats[!(stats %in% clubs)]
#usuniecie klubow z danego sezonu, bo tego nie da sie zsumowac
data_stats <- data_clean[, !names(data_clean) %in% clubs] #odpowiadajacy dataframe
colnames(data_stats) <- stats #przypisanie mu tych nazw


#analiza typow kolumn po wstepnej manipulacji danymi
column_types <- sapply(data_stats, class)
type_counts <- table(column_types)
print(type_counts)
#same zmienne numeryczne wiec super, mozemy przejsc do laczenia

#laczenie statystyk
stats_without_year <- sub("^(.*)\\(.*$", "\\1", stats)
stats_without_year
colnames(data_stats) <- stats_without_year

sum_and_replace <- function(data) {
  unique_names <- unique(names(data))
  new_data <- data.frame(matrix(ncol = length(unique_names), nrow = nrow(data)))
  names(new_data) <- unique_names  
  for (name in unique_names) {
    cols_to_sum <- which(names(data) == name)
    new_data[[name]] <- rowSums(data[, cols_to_sum, drop = FALSE], na.rm = TRUE)
  }
  return(new_data)
}
#zsumowane statystyki, nowy zbior danych
new_data <- sum_and_replace(data_stats)

modify_perc_columns <- function(data) {
  cols_to_modify <- grep("%", names(data))
  for (col in cols_to_modify) {
    data[, col] <- data[, col] / 4
    names(data)[col] <- gsub("%", "PERC", names(data)[col])
  }
  return(data)
}
new_data <- modify_perc_columns(new_data)
names(new_data)

pass_columns_to_delete <- c("Passes Completed (All pass-types) ", 
                            "Passes Attempted (All pass-types) ", 
                            "Passes Completed (Short Passes) ", 
                            "Passes Attempted (Short Passes) ", 
                            "Passes Completed (Medium Passes) ", 
                            "Passes Attempted (Medium Passes) ", 
                            "Passes Completed (Long Passes) ", 
                            "Passes Attempted (Long Passes) ")

other_columns_to_delete <- c(
  "Aerial Duel Won ",
  "Aerial Duel Lost ",
  "Number of Times Player was Pass Target ",
  "Number of Times Received Pass ",
  "Total Successful Dribbles ",
  "Total Attempted Dribbles ",
  "Number of Dribblers Tackled ",
  "Times Dribbled Past + Total Tackles ",
  "Number of Pressing Actions ",
  "Times Squad gained Possession within 5 seconds of Pressing Actions ",
  "Total Shots ",
  "Total Shots on Target "
)

cols_90 <- grep("/90", names(new_data))
time_columns_to_delete <- names(new_data[, cols_90])

columns_to_delete <- c(pass_columns_to_delete, other_columns_to_delete, time_columns_to_delete)
columns_to_delete
final_data <- new_data[, !names(new_data) %in% columns_to_delete]

names(final_data) <- make.names(names(final_data), unique = TRUE)
names(final_data)

final_data$Goals.Shots.Ratio <- final_data$Goals.Shots./4
final_data$Goals.Shots. <- NULL
final_data$Goals.Shots.on.Target.Ratio <- final_data$Goals.Shots.on.Target./4
final_data$Goals.Shots.on.Target. <- NULL

names(final_data)

#analiza za pomoca macierzy korelacji oraz wlasnej oceny
cor_matrix <- function(metric_list){
  df <- final_data[, metric_list]
  correlation_matrix_goals <- cor(df)
  melted_correlation_matrix <- melt(correlation_matrix_goals)
  
  ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank())
}
#dla grupy GOALS
GOALS <- c("Gls.", "Non.Penalty.Goals.", "Penalties.Scored.", "Penalties.Attempted.",
           "xG.","Non.Penalty.xG.", "Non.penalty.xG..xA.","Shots.on.TargetPERC.",                                             
           "Goals.Shots.Ratio", "Freekick.Shots.", "Non.Penalty.xG.Shots.", "Shots.Leading.to.Goals.",                                                      
           "Goals.Shots.on.Target.Ratio","Goals.Scored.minus.xG.","Non.Penalty.Goals.Scored.minus.Non.Penalty.xG.")

cor_matrix(GOALS)

low_correlated_goals <- c("Gls.", "Shots.on.TargetPERC.","Goals.Shots.Ratio", "Freekick.Shots.", "Non.Penalty.xG.Shots.", "Shots.Leading.to.Goals.",                                                      
                          "Goals.Shots.on.Target.Ratio","Goals.Scored.minus.xG.")

cor_matrix(low_correlated_goals)


#dla grupy ASSIST
setdiff(names(final_data),GOALS)

ASSIST <- c("Ast.", "Shot.Creating.Actions.", "xA.", "Passes.Leading.to.Shot.Attempt.", "Set.Piece.Leading.to.Shot.Attempt.",
            "Dribbles.Leading.to.Shot.Attempt.", "Shots.Leading.to.Shot.Attempt.", "Goal.Creating.Actions.",
            "Passes.Leading.to.Goals.", "Set.Piece.Leading.to.Goals.", "Dribbles.Leading.to.Goals.", "Dribbles.Leading.to.Goals.",
            "Fouls.Drawn.Leading.to.Goals.", "Defensive.Actions.Leading.to.Goals.", "Total.Assists.", "xG.Assisted." ,
            "Assist.minus.xG.Assisted.", "Completed.passes.that.enter.Final.3rd.", "Completed.passes.that.enter.Penalty.Box.",
            "Completed.Crosses.that.enter.Penalty.Box.", "Total.Completed.Progressive.Passes.", "Crosses." )

cor_matrix(ASSIST)

low_correlated_assists <- c("Ast.", "Set.Piece.Leading.to.Shot.Attempt.",
                            "Dribbles.Leading.to.Goals.",
                            "Fouls.Drawn.Leading.to.Goals.", "Defensive.Actions.Leading.to.Goals.","Assist.minus.xG.Assisted.",
                            "Total.Completed.Progressive.Passes.", "Crosses." )

cor_matrix(low_correlated_assists)

#dla grupy PASSES_TOUCHES
setdiff(setdiff(names(final_data),GOALS), ASSIST)

PASSES_TOUCHES <- c("Touches.", "Touches.in.Defensive.Penalty.Box.", "Touches.in.Attacking.Penalty.Box.",
                    "Touches.in.Open.play.","PERC.of.Times.Successfully.Received.Pass.",
                    "Progressive.Passes.Received.", "Pass.Completion.PERC..All.pass.types..",
                    "Touches.in.Defensive.3rd.", "Touches.in.Midfield.3rd.", "Touches.in.Attacking.3rd.",
                    "Total.Distance.of.Completed.Passes..All.Pass.types..", "Total.Distance.of.Completed.Progressive.Passes..All.Pass.types..",
                    "Pass.Completion.PERC..Short.Passes.." , "Pass.Completion.PERC..Medium.Passes..",
                    "Pass.Completion.PERC..Long.Passes..")

cor_matrix(PASSES_TOUCHES)

low_correlated_pass_touch <- setdiff(PASSES_TOUCHES,c("Touches.in.Open.play.", "Touches.in.Defensive.Penalty.Box.", "Total.Distance.of.Completed.Passes..All.Pass.types..", "Progressive.Passes.Received.",
                                                      "Touches.in.Defensive.3rd.", "Touches.in.Midfield.3rd.", "Touches.in.Attacking.3rd.", "Pass.Completion.PERC..All.pass.types.."))

cor_matrix(low_correlated_pass_touch)

#dla grupy DEFENSIVE
setdiff(setdiff(setdiff(names(final_data),GOALS), ASSIST),PASSES_TOUCHES)
DEFENSIVE <- c(
  "Total.Number.of.Players.Tackled.",
  "Total.Tackles.Won.",
  "Tackles.in.Defensive.3rd.",
  "Tackles.in.Midfield.3rd.",
  "Tackles.in.Attacking.3rd.",
  "PERC.of.Dribblers.Tackled.",
  "Number.of.Times.Dribbled.Past.",
  "Successful.Pressure.PERC.",
  "Number.of.Presses.in.Defensive.Third.",
  "Number.of.Presses.in.Midfield.Third.",
  "Number.of.Presses.in.Attacking.Third.",
  "Total.Defensive.Blocks.",
  "Total.Shots.Blocked.",
  "Goal.Saving.Blocks.",
  "Times.blocked.a.Pass.",
  "Total.Interceptions.",
  "Total.Players.Tackled...Total.Interceptions.",
  "Total.Clearances.",
  "Mistakes.leading.to.Opponent.Shots.",
  "X2nd.Yellow.Cards.",
  "Fouls.Committed.",
  "Fouls.Drawn.",
  "Interceptions.",
  "Penalties.Conceded.",
  "Own.Goals.",
  "Total.Loose.Balls.Recovered.",
  "PERC.Aerial.Duels.Won.",
  "Yellow.Cards.",
  "Red.Cards."
)
cor_matrix(DEFENSIVE)

highly_correlated_defensive <- c( "Total.Number.of.Players.Tackled.",
                                  "Total.Tackles.Won.",
                                  "Tackles.in.Defensive.3rd.",
                                  "Total.Players.Tackled...Total.Interceptions.")
low_correlated_defensive <- setdiff(DEFENSIVE, highly_correlated_defensive)

cor_matrix(low_correlated_defensive)

#dla grupy REST
REST <- setdiff(setdiff(setdiff(setdiff(names(final_data),GOALS), ASSIST),PASSES_TOUCHES), DEFENSIVE)

cor_matrix(REST)

highly_correlated_rest <- c( "MP.","Starts.","Total.Carries.",
                             "Total.Distance.Carried.the.Ball.in.Forward.Direction.",
                             "Total.Carries.in.Forward.Direction.")

low_correlated_rest <- setdiff(REST, highly_correlated_rest)

cor_matrix(low_correlated_rest)

#analiza pozostalych kolumn
columns_to_take <- c(low_correlated_goals, low_correlated_assists, low_correlated_defensive, low_correlated_pass_touch, low_correlated_rest)
columns_to_take

cor_matrix(columns_to_take)

#stworzenie rysunku do projektu, wizualizacja macierzy
cor_matrix_no_axis <- function(metric_list){
  df <- final_data[, metric_list]
  correlation_matrix_goals <- cor(df)
  melted_correlation_matrix <- melt(correlation_matrix_goals)
  
  ggplot(data = melted_correlation_matrix, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    ggtitle(as.character(substitute(metric_list)))+
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 8),
          legend.title = element_text(size = 6))
}
#przed transformacja
cor_matrix_list <- list()

cor_matrix_list[[1]] <- cor_matrix_no_axis(GOALS)
cor_matrix_list[[2]] <- cor_matrix_no_axis(ASSIST)
cor_matrix_list[[3]] <- cor_matrix_no_axis(PASSES_TOUCHES)
cor_matrix_list[[4]] <- cor_matrix_no_axis(DEFENSIVE)
cor_matrix_list[[5]] <- cor_matrix_no_axis(REST)

grid.arrange(grobs = cor_matrix_list, ncol = 2)

#po transformacji
cor_matrix_list <- list()

cor_matrix_list[[1]] <- cor_matrix_no_axis(low_correlated_goals)
cor_matrix_list[[2]] <- cor_matrix_no_axis(low_correlated_assists)
cor_matrix_list[[3]] <- cor_matrix_no_axis(low_correlated_pass_touch)
cor_matrix_list[[4]] <- cor_matrix_no_axis(low_correlated_defensive)
cor_matrix_list[[5]] <- cor_matrix_no_axis(low_correlated_rest)

grid.arrange(grobs = cor_matrix_list, ncol = 2)

#reset
par(mfrow = c(1, 1))



#### SZCZEGOLOWA ANALIZA OGOLNYCH INFORMACJI O ZAWODNIKACH
final_data <- final_data[, (names(final_data) %in% columns_to_take)]
df_tree<- cbind(non_stat_columns, final_data)

unique(df_tree$Position)
unique(df_tree$Nation) #zamiana na kontynenty
unique(df_tree$League)

#zamiana krajow na kontynenty
df_tree$Nation<- countrycode(df_tree$Nation, "country.name", "continent")
df_tree$Nation

#zgodnie z warningiem niektore kraje zostaly zamienione na NA (sa w Europie wiec zamieniamy na Europe)
na_entries <- is.na(df_tree$Nation)
df_tree$Nation[na_entries] <- "Europe"

column_types <- sapply(df_tree, class)
type_counts <- table(column_types)
print(type_counts)

#rankujemy zespoly
top6team <- c(
  'Manchester City', 'Liverpool', 'Manchester United', 'Chelsea', 'Tottenham Hotspur',
  'Arsenal', 'Juventus', 'Inter Mediolan', 'SSC Napoli', 'Atalanta', 'Milan', 'Lazio',
  'FC Barcelona', 'Real Madryt', 'Atlético de Madrid', 'Sevilla FC', 'Valencia CF',
  'Villarreal CF', 'Paris Saint-Germain', 'Olympique Lyon', 'Olympique Marseille',
  'LOSC Lille', 'AS Monaco', 'Stade Rennais FC', 'Bayern Munich', 'Borussia Dortmund',
  'Leipzig', 'Bayer Leverkusen', 'Borussia Monchengladbach', 'Eintracht Frankfurt'
)
midtableteam <- c(
  'Leicester City', 'Everton', 'West Ham United', 'Burnley', 'Crystal Palace',
  'Newcastle', 'Southampton', 'Wolverhampton Wanderers', 'Brighton &amp; Hove Albion',
  'Watford', 'Roma', 'Sampdoria', 'Sassuolo', 'Torino', 'ACF Fiorentina',
  'Bologna FC 1909', 'Udinese Calcio', 'Cagliari', 'Genoa CFC', 'Hellas Verona',
  'Real Sociedad', 'Real Betis Balompié', 'Getafe CF', 'Athletic Bilbao',
  'Levante UD', 'Celta Vigo', 'Deportivo Alaves', 'SD Eibar', 'RCD Espanyol',
  'CD Leganes', 'Montpellier HSC', 'OGC Nice', 'AS Saint-Etienne', 'Girondins Bordeaux',
  'FC Nantes', 'SCO Angers', 'RC Strasbourg Alsace', 'Stade de Reims', 'Dijon FCO',
  'Nimes Olympique', 'TSG 1899 Hoffenheim', 'VfL Wolfsburg', 'SC Freiburg',
  'Hertha BSC', '1. FSV Mainz 05', 'FC Augsburg', 'FC Schalke 04',
  'SV Werder Bremen', 'VfB Stuttgart', '1.FC Union Berlin'
)
bottomteam <- c(
  "Aston Villa", "Parma Calcio 1913", "Real Valladolid", "FC Metz", "1. FC Köln",
  "Bournemouth", "SPAL", "Granada CF", "Toulouse FC", "Fortuna Düsseldorf",
  "Leeds United", "Crotone", "CA Osasuna", "Amiens SC", "Hannover 96",
  "West Bromwich Albion", "Chievo Verona", "Girona FC", "Stade Brestois 29", "Arminia Bielefeld",
  "Sheffield United", "Benevento Calcio", "SD Huesca", "EA Guingamp", "Werder Brema",
  "Fulham", "Spezia Calcio", "Cádiz CF", "SM Caen", "Hamburger SV",
  "Huddersfield Town", "Empoli", "Elche CF", "RC Lens", "Fortuna Düsseldorf",
  "Watford", "Lecce", "RCD Mallorca", "Lorient", "VfB Stuttgart",
  "Bournemouth", "Frosinone", "Rayo Vallecano", "Troyes", "1. FC Köln",
  "Cardiff City", "Brescia", "Deportivo La Coruña", "Amiens S.C.", "Hannover 96",
  "Stoke City", "UD Las Palmas", "SC Paderborn 07", "Swansea City", "Málaga CF",
  "1. FC Nürnberg", "FC Schalke 04", "Norwich City", "Huddersfield Town"
)

#Funkcja znajdujaca najblizsze slowo, bo nie sa identyczne (zbior danych i wikipedia)
findClosestTeam <- function(teamName, teamVector, threshold = 3) {
  distances <- stringdist::stringdist(teamName, teamVector)
  if (min(distances) <= threshold) {
    return(teamVector[which.min(distances)])
  } else {
    return(NA)
  }
}

#Mapowanie slow
df_tree$Club <- sapply(df_tree$Club, function(x) {
  closestTop6 <- findClosestTeam(x, top6team)
  closestMidtable <- findClosestTeam(x, midtableteam)
  closestBottomtable <- findClosestTeam(x, bottomteam)
  
  if (!is.na(closestTop6)) {
    return("TOP6")
  } else if (!is.na(closestMidtable)) {
    return("MIDTABLE")
  } else if (!is.na(closestBottomtable)) {
    return("BOTTOM")
  } else {
    return(x)
  }
})

unique(df_tree$Club)
table(df_tree$Club)

#usuniecie imion i nazwisk graczy
df_tree_1 <- df_tree[, !(names(df_tree) %in% c("Player"))]
#reszte zamieniamy na typ factor (malo poziomow jest)
df_tree_1$Nation <- factor(df_tree_1$Nation)
df_tree_1$Position <- factor(df_tree_1$Position)
df_tree_1$League <- factor(df_tree_1$League)
df_tree_1$Club <- factor(df_tree_1$Club)

#upewniamy sie czy wszystkie typy kolumn sa odpowiednie
column_types <- sapply(df_tree_1, class)
type_counts <- table(column_types)
print(type_counts)

#zliczamy NA
sum(is.na(df_tree_1))

#nie ma NA

#zliczamy duplikaty
duplicate_counts <- table(df_tree_1[duplicated(df_tree_1$Player) | duplicated(df_tree_1$Player, fromLast = TRUE), "Player"])
duplicate_counts
#nie ma duplikatow


#ostateczny zbior danych
prepared_data <- df_tree_1
write.csv(prepared_data, file = "final_data.csv", row.names = FALSE)

names(prepared_data)

to_delete <- c("Avg.Shot.Distance..yds..", "Shots.Leading.to.Goals." ,  "Defensive.Actions.Leading.to.Shot.Attempt.",
               "Fouls.Drawn.Leading.to.Shot.Attempt." , "Total.Nutmegs.", "Total.Distance.Carried.the.Ball.", "Carries.into.Final.Third.",
               "Total.Failed.Attempts.at.Controlling.Ball.", "PERC.of.Times.Successfully.Received.Pass." ,
               "Number.of.Times.Tackled.when.Dribbling.", "X2nd.Yellow.Cards.", "Interceptions.",
               "Total.no..of.Players.Dribbles.Past.", "Total.Distance.of.Completed.Progressive.Passes..All.Pass.types..",
               "Goals.Shots.on.Target.Ratio", "Fouls.Drawn.Leading.to.Goals.")


prepared_data <- prepared_data[, !names(prepared_data) %in% to_delete]


#histogram przewidywanych wartosci w przygotowanym zbiorze
hist(prepared_data$Value)
summary(prepared_data$Value)


#### BUDOWANIE DRZEWA 
names(prepared_data) <- make.names(names(prepared_data), unique = TRUE)


library(dplyr)
breaks <- c(-Inf, 1000000, 2000000, 3000000, 4000000, 5000000, 7500000, 10000000, 12500000, 15000000, 17500000, 20000000, 25000000, 30000000, 40000000, 50000000, Inf)
labels <- c("0-1M", "1-2M", "2-3M", "3-4M", "4-5M", "5-7.5M", "7.5-10M", "10-12.5M", "12.5-15M", "15-17.5M", "17.5-20M", "20-25M", "25-30M", "30-40M", "40-50M", ">50M")

prepared_data <- prepared_data %>%
  mutate(ValueGroup = cut(Value, breaks = breaks, labels = labels, right = FALSE))

prepared_data <- prepared_data %>%
  group_by(ValueGroup) %>%
  mutate(GroupMean = mean(Value), Value = GroupMean) %>%
  ungroup()

prepared_data <- select(prepared_data, -ValueGroup, -GroupMean)

unique(prepared_data$Value)

#na calych danych
tree.model <- tree(Value ~ ., data = prepared_data)
par(mfrow=c(1,1))
plot(tree.model)
text(tree.model, pretty=0, cex=0.4)




set.seed(15)
train <- sample(1:nrow(prepared_data),nrow(prepared_data)*0.8)
prepared_data.train <- prepared_data[train,]
prepared_data.test <- prepared_data[-train,]

summary(prepared_data.train$Value)
summary(prepared_data.test$Value)

model_tree_train1 <- tree(Value ~ ., data = prepared_data.train)

plot(model_tree_train1)
text(model_tree_train1, pretty=0, cex=0.4)

prediction_tree_1 <- predict(model_tree_train1, prepared_data.test)
summary(prediction_tree_1)
summary(prepared_data.test$Value)

prediction_tree_1
prepared_data.test$Value

inspect_nodes <- function(data, tree_model) {
  num_nodes <- as.integer(length(tree_model$frame$var))
  for (node in 1:num_nodes) {
    node_indices <- which(tree_model$where == node)
    data_in_node <- data[node_indices, 'Value']
    print(summary(data_in_node))
    print(node)
  }
}


inspect_nodes(prepared_data, model_tree_train1)


cor(prediction_tree_1, prepared_data.test$Value)

MAE <- function(actual, predicted){
  mean(abs(actual-predicted))
}
mean_absolute_percentage_error <- function(y_true, y_pred) {
  # Obliczenie MAPE
  mape <- mean(abs((y_true - y_pred) / y_true)) * 100
  return(mape)
}
mean_absolute_percentage_error(prepared_data.test$Value, prediction_tree_1)


MAE_1 <- MAE(prepared_data.test$Value, prediction_tree_1)
MAE_1

library(Cubist)

model_reg_tree <- cubist(x=prepared_data.train[-5], y=prepared_data.train$Value)
model_reg_tree

summary(model_reg_tree)

prediction_tree_2 <- predict(model_reg_tree, prepared_data.test)

summary(prediction_tree_2)
summary(prepared_data.test$Value)

prediction_tree_2

MAE_2 <- MAE(prepared_data.test$Value, prediction_tree_2)
MAE_2

library(randomForest)

#BAGGING
bag.tree2 <- randomForest(Value ~ ., data = prepared_data, subset = train,
                          mtry = 53, importance = TRUE)

bag.tree2$importance
bag.tree2

prediction_tree_3 <- predict(bag.tree2, newdata = prepared_data.test)

MAE_3 <- MAE(prediction_tree_3, prepared_data.test$Value)

#RANDOM FOREST
rf.tree2 <- randomForest(Value ~ ., data = prepared_data, subset = train,
                         mtry = 18, importance = TRUE)
rf.tree2
prediction_tree_4 <- predict(rf.tree2, newdata = prepared_data.test)

MAE_4 <- MAE(prediction_tree_4, prepared_data.test$Value)

library(xgboost)
library(caret)  


train_x <- data.matrix(prepared_data.train[,-5])
train_y <- prepared_data.train[[5]]

test_x <- data.matrix(prepared_data.test[,-5])
test_y <- prepared_data.test[[5]]


xgb_train <- xgb.DMatrix(data = train_x, label = train_y)
xgb_test <- xgb.DMatrix(data = test_x, label = test_y)

watchlist <- list(train=xgb_train, test=xgb_test)


model <- xgb.train(data = xgb_train, max.depth = 3, watchlist = watchlist, nrounds = 200)

final <- xgboost(data = xgb_train, max.depth = 3, nrounds = 25, verbose = 0)

pred_y <- predict(final, xgb_test)

MAE(pred_y, test_y)


mean_absolute_percentage_error <- function(y_true, y_pred) {
  # Obliczenie MAPE
  mape <- mean(abs((y_true - y_pred) / y_true)) * 100
  return(mape)
}

# Przykładowe dane
y_true <- c(20, 30, 40, 50, 60)  # Rzeczywiste wartości
y_pred <- c(18, 33, 37, 52, 61)  # Przewidywane wartości

# Obliczanie MAPE
mape <- mean_absolute_percentage_error(y_true, y_pred)

# Wyświetlanie wyniku
print(paste("MAPE:", mape, "%"))

