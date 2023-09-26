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
?rpart
summary(tree)
printcp(tree)
plotcp(tree)

#Random Forest
rf <- randomForest(Kgp~., data=train_leafs)
plot(rf)
predict(rf, test_leafs)


#Quantile regression forest

qrf <- quantregForest(x = train_leafs_x, y =train_leafs_y, nodesize = 20)
treesize(qrf)

conditionalMean <- predict(qrf, test_leafs_x, what = mean)
MSE <- mean((conditionalMean - test_leafs_y)^2)

#k-cv MSE

cv <- function(data, k, min_node_size) {
  MSE_cv <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    train_x <- data.frame(Sc = data[group != i,1])
    train_y<- data[group != i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = min_node_size)
    
    #MSE
    conditionalMean_cv <- predict(qrf_cv, data.frame(Sc = data[group == i,1]), what = mean)
    MSE_cv[i] <- mean((conditionalMean_cv - data[group == i,2])^2)
  }
  return(tibble("MSE" = MSE_cv))
}

cv(leafs, 2, 20)

for (i in (seq(10,100,10))){
  if (i == 10){
    result <- tibble(MSi)
    print(result)
  }
  else{
    name <- paste("MSE_", i, sep = "")
    result <- result %>% add_column(name = i)
  } 
}

tibble(colnames = c("a", "b"))

?tibble

name <- paste("MSE_", i, sep = "")


#Virker ikke endnu
for (i in (seq(10,100,10))){
  if (i == 10){
    name <- paste("MSE_", i, sep = "")
    result <- tibble(cv(leafs, 3, i)$MSE)
  }
  else{
    result <- result %>% add_column(name[i] = cv(leafs, 3, i)$MSE)
  }
}

name <- paste("MSE_", i, sep = "")
print(result)


conditionalQuantiles <- predict(qrf, test_leafs_x)

print(conditionalQuantiles[1:4,])

plot_data <- data.frame(test_leafs_x, conditionalQuantiles, mean = conditionalMean)
head(plot_data)

plot_data <- arrange(plot_data, Sc)

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


#Med wood i stedet:
wood <- read.csv('Data/wood.csv')
sample_size_wood = floor(0.8*nrow(wood))
set.seed(777)
picked_wood = sample(seq_len(nrow(wood)),size = sample_size_wood)

train_wood_x = data.frame(Sc = wood[picked_wood,1])
train_wood_y = wood[picked_wood,2]
test_wood_x = data.frame(Sc=wood[-picked_wood,1])
test_wood_y = wood[-picked_wood,2]

qrf <- quantregForest(x = train_wood_x, y =train_wood_y)
plot(qrf)

conditionalQuantiles <- predict(qrf, test_wood_x)
print(conditionalQuantiles[1:4,])

plot_data <- data.frame(test_wood_x, conditionalQuantiles)
head(plot_data)

plot_data <- arrange(plot_data, Sc)
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


#Med roots i stedet:
roots <- read.csv('Data/roots.csv')
sample_size_roots = floor(0.8*nrow(roots))
set.seed(777)
picked_roots = sample(seq_len(nrow(roots)),size = sample_size_roots)

train_roots_x = data.frame(Sc = roots[picked_roots,1])
train_roots_y = roots[picked_roots,2]
test_roots_x = data.frame(Sc=roots[-picked_roots,1])
test_roots_y = roots[-picked_roots,2]

qrf <- quantregForest(x = train_roots_x, y =train_roots_y)
plot(qrf)

conditionalQuantiles <- predict(qrf, test_roots_x)
print(conditionalQuantiles[1:4,])

plot_data <- data.frame(test_roots_x, conditionalQuantiles)
head(plot_data)

plot_data <- arrange(plot_data, Sc)
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
