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

train_leafs_x = data.frame(Sc = leafs[picked_leafs,1])
train_leafs_y = leafs[picked_leafs,2]
test_leafs_x = data.frame(Sc=leafs[-picked_leafs,1])
test_leafs_y = leafs[-picked_leafs,2]

#Tree

tree <- rpart(Kgp~., data=train_leafs)
rpart.plot(tree)
printcp(tree)
plotcp(tree)

?rpart

?printcp

#Random Forest

rf <- randomForest(Kgp~., data=train_leafs)
print(rf)
plot(rf)

predict(rf, test_leafs)


#Quantile regression forest

qrf <- quantregForest(x = train_leafs_x, y =train_leafs_y)
plot(qrf)

conditionalQuantiles <- predict(qrf, test_leafs_x)
print(conditionalQuantiles[1:4,])

plot_data <- data.frame(test_leafs_x, conditionalQuantiles)
head(plot_data)

plot_data <- arrange(plot_data, Sc)
?order

colors <- c("0.1" = "hotpink", "0.5" = "darkolivegreen", "0.9" = "darkolivegreen2")

ggplot(plot_data, aes(x = Sc)) +
  geom_point(aes(y=quantile..0.1, color = "0.1")) +
  geom_point(aes(y=quantile..0.5, color = "0.5")) +
  geom_point(aes(y=quantile..0.9, color = "0.9" )) +
  geom_smooth(aes(y=quantile..0.1, color = "0.1"), se = F)+
  geom_smooth(aes(y=quantile..0.5, color = "0.5"), se = F)+
  geom_smooth(aes(y=quantile..0.9, color = "0.9"), se = F)+
  labs(color = "quantile",
       y = "Quantiles",
       x = "Crown size")+
  theme_bw()+
scale_color_manual(values = colors)
