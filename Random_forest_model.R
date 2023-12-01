################################################################333
###Indlæsning af pakker og data:
library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)

set.seed(777)
leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

test_leafs <- read.csv('Data/test_leafs.csv')
test_roots <- read.csv('Data/test_roots.csv')
test_wood <- read.csv('Data/test_wood.csv')

train_leafs_x = data.frame(Sc = leafs_train[,1])
train_leafs_y = leafs_train[,2]
test_leafs_x = data.frame(Sc=test_leafs[,1])
test_leafs_y = test_leafs[,2]

train_wood_x = data.frame(Sc = wood_train[,1])
train_wood_y = wood_train[,2]
test_wood_x = data.frame(Sc=test_wood[,1])
test_wood_y = test_wood[,2]

train_roots_x = data.frame(Sc = roots_train[,1])
train_roots_y = roots_train[,2]
test_roots_x = data.frame(Sc=test_roots[,1])
test_roots_y = test_roots[,2]


##############################################################
#Implementering af random forest modellen, i sammenligning med QRF for at se hvad der foregår:

#Fitting the model
rf_leafs_model <- randomForest(x = train_leafs_x, y = train_leafs_y, nodesize = 5)
nrow(getTree(rf_leafs_model))

for (i in 1:50){
  tree <- getTree(rf_leafs_model, k = i)
  leng <- nrow(tree)
  print(leng)
}


#Using it to predict:
rf_leafs_pred <- predict(rf_leafs_model, newdata = test_leafs_x)
leafs_pred_mean <- predict(leafs_qrf, test_leafs_x, what = mean)

#Function for creating the dataframe to be plotted:
plot_data <- function(pred, obs, Sc){
  plot_data <- data.frame(pred, observed = obs, Sc = Sc)
  }

plot_leafs <- plot_data(rf_leafs_pred, test_leafs_y, test_leafs_x)

ggplot(plot_leafs) + 
  geom_point(aes(x = Sc, y = observed), color = 'darkolivegreen4', size = 1.5) +
  geom_point(aes(x = Sc, y = pred), color = 'hotpink4', size = 1.5) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")













