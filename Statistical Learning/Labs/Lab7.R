#Bagging

library(randomForest)
library(MASS)
set.seed(1)
data(Boston)
?Boston
train <- sample(1:nrow(Boston),500)
train
bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 12, ntry = 100, importance = TRUE)
bag.boston

predictions <- predict(bag.boston, newdata = Boston[-train, ])
predictions

actual_values <- Boston$medv[-train]
mse <- mean((actual_values - predictions)^2)
mse

# Random Forest

library(randomForest)

bag.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
bag.boston

predictions <- predict(bag.boston, newdata = Boston[-train, ])


actual_values <- Boston$medv[-train]
mse <- mean((actual_values - predictions)^2)
mse


rf.boston <- randomForest(medv ~ ., data = Boston, subset = train, mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf - actual_values)^2)

varImpPlot(rf.boston)

### Boosting

set.seed(1)
library("MASS")
train <- sample(1:nrow(Boston), nrow(Boston)/2)
library(tree)
tree.boston <- tree(medv ~ ., Boston, subset = train)
cv.boston <- cv.tree(tree.boston)
boston.test <- Boston[-train, "medv"]
yhat<- predict(tree.boston, newdata = Boston[-train, ])
mean((yhat - boston.test)^2)


library(gbm)
?gbm
set.seed(1)
boost.boston <- gbm(medv ~ ., data = Boston[train, ],
                    distribution = "gaussian", n.trees = 5000, 
                    interaction.depth = 4)
summary(boost.boston)
plot(boost.boston, i = "rm")
yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - boston.test)^2)

yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 500)
mean((yhat.boost - boston.test)^2)

boost.boston1 <- gbm(medv ~ ., data = Boston[train, ],
                     distribution = "gaussian", n.trees = 500, 
                     interaction.depth = 4)

yhat.boost1 <- predict(boost.boston1, newdata = Boston[-train, ], n.trees = 500)
mean((yhat.boost1 - boston.test)^2)

library(BART)
set.seed(1)
Boston[train,]
Boston[train, ]
bartfit <- gbart(Boston[train, ], Boston[-train,], x.test = Boston[-train, "medv"])
?Boston
View(Boston)
