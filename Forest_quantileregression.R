library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)

leafs <- read.csv('Data/leafs.csv')
sample_size_leafs = floor(0.8*nrow(leafs))
set.seed(777)
picked_leafs = sample(seq_len(nrow(leafs)),size = sample_size_leafs)
train_leafs = leafs[picked_leafs,]
test_leafs = leafs[-picked_leafs,]

#Tree

tree <- rpart(Kgp~., data=train_leafs)
rpart.plot(tree)
printcp(tree)
plotcp(tree)


?printcp

#Random Forrest

rf <- randomForest(Kgp~., data=train_leafs_log)
print(rf)
plot(rf)

predict(rf, test_leafs_log)
