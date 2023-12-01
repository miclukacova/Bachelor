library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(dplyr)
library(foreign)
library(xtable)
library(stargazer)

train_leafs_log <- read.csv('Data/train_leafs_log.csv')
train_roots_log <- read.csv('Data/train_roots_log.csv')
train_wood_log<- read.csv('Data/train_wood_log.csv')

test_leafs_log <- read.csv('Data/test_leafs_log.csv')
test_roots_log <- read.csv('Data/test_roots_log.csv')
test_wood_log<- read.csv('Data/test_wood_log.csv')

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

test_leafs <- read.csv('Data/test_leafs.csv')
test_roots <- read.csv('Data/test_roots.csv')
test_wood <- read.csv('Data/test_wood.csv')

leafs <- read.csv('Data/leafs.csv')
wood <- read.csv('Data/wood.csv')
roots <- read.csv('Data/.csv')

#Quantile regression forest

train_leafs_x = data.frame(Sc = leafs[picked_leafs,1])
train_leafs_y = leafs[picked_leafs,2]
test_leafs_x = data.frame(Sc=leafs[-picked_leafs,1])
test_leafs_y = leafs[-picked_leafs,2]

train_wood_x = data.frame(Sc = wood[picked_wood,1])
train_wood_y = wood[picked_wood,2]
test_wood_x = data.frame(Sc=wood[-picked_wood,1])
test_wood_y = wood[-picked_wood,2]

train_roots_x = data.frame(Sc = roots[picked_roots,1])
train_roots_y = roots[picked_roots,2]
test_roots_x = data.frame(Sc=roots[-picked_roots,1])
test_roots_y = roots[-picked_roots,2]

set.seed(253)

qrf_leafs <- quantregForest(x = train_leafs_x, y =train_leafs_y)
qrf_wood <- quantregForest(x = train_wood_x, y =train_wood_y)
qrf_roots <- quantregForest(x = train_roots_x, y =train_roots_y)

conditionalMean_leafs <- predict(qrf_leafs, test_leafs_x, what = mean)
conditionalMean_wood <- predict(qrf_wood, test_wood_x, what = mean)
conditionalMean_roots <- predict(qrf_roots, test_roots_x, what = mean)


MSE <- tibble(" " = c("Leafs", "Wood", "Roots"), "MSE" = 
                c(mean((conditionalMean_leafs - test_leafs_y)^2),
                  mean((conditionalMean_wood - test_wood_y)^2),
                  mean((conditionalMean_roots - test_roots_y)^2)))

#Into tex
MSE %>% xtable(type = "latex", file = "table.tex")

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

#10 fold cv to find optimal node size function and summary function:

opt_node_size <- function(data, node_sizes){
  hej <- TRUE
  for (i in node_sizes){
    if (hej){
      result <- data.frame(cv(data, 10, i)$MSE)
      colnames(result) <- paste("MSE_", i, sep = "")
      k = 2
      hej <- FALSE
    }
    else{
      result <- result %>% add_column(cv(data, 10, i)$MSE)
      colnames(result)[k] <- paste("MSE_", i, sep = "")
      k = k + 1
    }
  }
  return(result)
}

summ <- function(result) {
  mean_cv <- c()
  var_cv <- c()
  for (i in (1:ncol(result))){
    mean_cv[i] <- mean(result[,i])
    var_cv[i] <- var(result[,i])
  }
  results <- tibble(names = colnames(result), mean_cv = mean_cv, var_cv = var_cv)
  return(results)
}

# For leafs: 

set.seed(41)
results_leafs <- opt_node_size(train_leafs, c(1,2,5, seq(10, 100, 10), seq(120, 260, 20)))
results_leafs <- summ(results_leafs)

which.min(results_leafs$mean_cv)
which.min(results_leafs$var_cv)

which.min(results_wood$mean_cv)
results_wood[9,4]

which.min(results_wood$var_cv)
results_wood[16,4]

results_leafs <- results_leafs %>% add_column(index = c(1,2,5, seq(10, 100, 10), seq(120, 260, 20)))

ggplot(results_leafs, aes(y = mean_cv, x = index)) +
  geom_point(color = "hotpink")+
  labs(title = "Leafs",
       y = "Mean MSE",
       x = "Nodesize")+
  geom_smooth(se = FALSE, color = "darkolivegreen4")+
  theme_bw()


# For wood: 

set.seed(41)
results_wood <- opt_node_size(train_wood, c(1,2, seq(5, 95, 5), seq(100, 200, 20)))
results_wood <- summ(results_wood)

which.min(results_wood$mean_cv)
results_wood[7,4]

which.min(results_wood$var_cv)
results_wood[11,4]

results_wood <- results_wood %>% add_column(index = c(1,2, seq(5, 95, 5), seq(100, 200, 20)))

ggplot(results_wood, aes(x = index, y = mean_cv)) +
  geom_point(,  color = "hotpink")+
  labs(title = "Wood",
       y = "Mean MSE",
       x = "Nodesize")+
  geom_smooth(se = FALSE, color = "darkolivegreen4")+
  theme_bw()


# For roots: 
set.seed(41)
results_roots <- opt_node_size(train_roots, seq(1:10))
results_roots <- summ(results_roots)

which.min(results_roots$mean_cv)
which.min(results_roots$var_cv)

results_roots <- results_roots %>% add_column(index = seq(1:10))

ggplot(results_roots, aes(x = index, y = mean_cv)) +
  geom_point(color = "hotpink")+
  geom_smooth(se = FALSE, color = "darkolivegreen4")+
  labs(title = "Roots",
       y = "Mean MSE",
       x = "Nodesize")+
  theme_bw()


#Into tex

results_leafs[seq(1,nrow(results_leafs), by = 2),1:3] %>% xtable(type = "latex")
results_wood[round(seq(1,nrow(results_wood), length.out = 11)),1:3] %>% xtable(type = "latex")
results_roots[,1:3] %>% xtable(type = "latex")

ggarrange(leafs_mse_cv_plot, wood_mse_cv_plot, roots_mse_cv_plot, ncol = 3, nrow = 1)

#QRF


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

############################Forsøg med max- i stedet for min-nodesize:
#10 fold cv to find optimal node size function and summary function:

cv <- function(data, k, max_nodes) {
  MSE_cv <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    train_x <- data.frame(Sc = data[group != i,1])
    train_y<- data[group != i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, maxnodes = max_nodes)
    
    #MSE
    conditionalMean_cv <- predict(qrf_cv, data.frame(Sc = data[group == i,1]), what = mean)
    MSE_cv[i] <- mean((conditionalMean_cv - data[group == i,2])^2)
  }
  return(tibble("MSE" = MSE_cv))
}

opt_node_size <- function(data, max_nodes){
  hej <- TRUE
  for (i in max_nodes){
    if (hej){
      result <- data.frame(cv(data, 10, i)$MSE)
      colnames(result) <- paste("MSE_", i, sep = "")
      k = 2
      hej <- FALSE
    }
    else{
      result <- result %>% add_column(cv(data, 10, i)$MSE)
      colnames(result)[k] <- paste("MSE_", i, sep = "")
      k = k + 1
    }
  }
  return(result)
}

summ <- function(result) {
  mean_cv <- c()
  var_cv <- c()
  for (i in (1:ncol(result))){
    mean_cv[i] <- mean(result[,i])
    var_cv[i] <- var(result[,i])
  }
  results <- tibble(names = colnames(result), mean_cv = mean_cv, var_cv = var_cv)
  return(results)
}

# For leafs: 

set.seed(41)
results_leafs <- opt_node_size(leafs_train, c(1,2,5, seq(10, 100, 10), seq(120, 260, 20)))
results_leafs <- summ(results_leafs)

which.min(results_leafs$mean_cv)
which.min(results_leafs$var_cv)

which.min(results_wood$mean_cv)
results_wood[9,4]

which.min(results_wood$var_cv)
results_wood[16,4]

results_leafs <- results_leafs %>% add_column(index = c(1,2,5, seq(10, 100, 10), seq(120, 260, 20)))

ggplot(results_leafs, aes(y = mean_cv, x = index)) +
  geom_point(color = "hotpink")+
  labs(title = "Leafs",
       y = "Mean MSE",
       x = "Nodesize")+
  geom_smooth(se = FALSE, color = "darkolivegreen4")+
  theme_bw()


# For wood: 

set.seed(41)
results_wood <- opt_node_size(wood_train, c(1,2, seq(5, 95, 5), seq(100, 200, 20)))
results_wood <- summ(results_wood)

which.min(results_wood$mean_cv)
results_wood[7,4]

which.min(results_wood$var_cv)
results_wood[11,4]

results_wood <- results_wood %>% add_column(index = c(1,2, seq(5, 95, 5), seq(100, 200, 20)))

ggplot(results_wood, aes(x = index, y = mean_cv)) +
  geom_point(,  color = "hotpink")+
  labs(title = "Wood",
       y = "Mean MSE",
       x = "Nodesize")+
  geom_smooth(se = FALSE, color = "darkolivegreen4")+
  theme_bw()


# For roots: 
set.seed(41)
results_roots <- opt_node_size(roots_train, seq(1:10))
results_roots <- summ(results_roots)

which.min(results_roots$mean_cv)
which.min(results_roots$var_cv)

results_roots <- results_roots %>% add_column(index = seq(1:10))

ggplot(results_roots, aes(x = index, y = mean_cv)) +
  geom_point(color = "hotpink")+
  geom_smooth(se = FALSE, color = "darkolivegreen4")+
  labs(title = "Roots",
       y = "Mean MSE",
       x = "No. of nodes")+
  theme_bw()


#Into tex

results_leafs[seq(1,nrow(results_leafs), by = 2),1:3] %>% xtable(type = "latex")
results_wood[round(seq(1,nrow(results_wood), length.out = 11)),1:3] %>% xtable(type = "latex")
results_roots[,1:3] %>% xtable(type = "latex")

#Random

#tree <- rpart(Kgp~., data=train_leafs)
#rpart.plot(tree)
#?rpart
#summary(tree)
#printcp(tree)
#plotcp(tree)

##Random Forest
#rf <- randomForest(Kgp~., data=train_leafs)
#plot(rf)
#predict(rf, test_leafs)

