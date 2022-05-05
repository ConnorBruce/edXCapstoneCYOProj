#Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
library(tidyverse)
library(caret)
library(ggplot2)
library(randomForest)
library(corrplot)

#Importing Data
games = read.csv("Data/games.csv")

#Data Cleaning
##Getting relevant columns and adjusting team indication
games_sm = games %>%
  select(winner:firstRiftHerald)
games_sm[games_sm == 2] = -1
games_sm$winner=as.factor(games_sm$winner)
games_sm$firstBlood=as.factor(games_sm$firstBlood)
games_sm$firstTower=as.factor(games_sm$firstTower)
games_sm$firstInhibitor=as.factor(games_sm$firstInhibitor)
games_sm$firstDragon=as.factor(games_sm$firstDragon)
games_sm$firstRiftHerald=as.factor(games_sm$firstRiftHerald)
games_sm$firstBaron=as.factor(games_sm$firstBaron)

##Removing games with no first blood (removes 555 games)
games_sm = games_sm %>%
  filter(firstBlood != 0)

#Data Exploration

##Correlation Plot
games_sm_cor = cor(games %>%
                     select(winner:firstRiftHerald))
corrplot(games_sm_cor)

##Distribution of wins
games_sm %>%
  count(winner) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = winner, y = Prop, fill = winner)) +
  geom_bar(stat = "identity")

##Distribution of firstBlood
games_sm %>%
  count(firstBlood) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = firstBlood, y = Prop, fill = firstBlood)) +
  geom_bar(stat = "identity")

##Distribution of firstTower
games_sm %>%
  count(firstTower) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = firstTower, y = Prop, fill = firstTower)) +
  geom_bar(stat = "identity")

##Distribution of firstInhibitor
games_sm %>%
  count(firstInhibitor) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = firstInhibitor, y = Prop, fill = firstInhibitor)) +
  geom_bar(stat = "identity")

##Distribution of firstBaron
games_sm %>%
  count(firstBaron) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = firstBaron, y = Prop, fill = firstBaron)) +
  geom_bar(stat = "identity")

##Distribution of firstDragon
games_sm %>%
  count(firstDragon) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = firstDragon, y = Prop, fill = firstDragon)) +
  geom_bar(stat = "identity")

##Distribution of firstRiftHerald
games_sm %>%
  count(firstRiftHerald) %>%
  mutate(Prop = n/sum(n)) %>%
  ggplot(aes(x = firstRiftHerald, y = Prop, fill = firstRiftHerald)) +
  geom_bar(stat = "identity")

#Break the data into a test set and training set
y = games_sm$winner
test_index = createDataPartition(y, times = 1, p = .2, list = FALSE) 
train_game = games_sm[-test_index,]
test_game = games_sm[test_index,]

#Deciding to use random forest
fit_rf = randomForest(train_game$winner ~ .,
                   data = train_game,
                   ntree = 100)
#Getting Predictions and Making sure they are factors
preds_rf = predict(fit_rf, test_game)
preds_rf = as.factor(preds_rf)
confusionMatrix(preds_rf, as.factor(test_game$winner))$overall["Accuracy"]
#Getting variable importance of RF model
var_imp = as.data.frame(importance(fit_rf))
var_imp %>% rename(Importance = MeanDecreaseGini) %>%
  arrange(desc(Importance))

#Using Logistic Regression
fit_glm = train(winner ~ ., data = train_game, method = "glm")
#Getting predictions
preds_glm = predict(fit_glm, test_game)

#Getting accuracies
glm_acc = confusionMatrix(preds_glm, as.factor(test_game$winner))$overall["Accuracy"]
rf_acc = confusionMatrix(preds_rf, as.factor(test_game$winner))$overall["Accuracy"]
knitr::kable(data.frame(Model = c("Logisitic","Random Forest"),
                        Accuracy = c(glm_acc,rf_acc)))
