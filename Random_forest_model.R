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
leafs <- read.csv('Data/leafs.csv')
wood<- read.csv('Data/wood.csv')
roots <- read.csv('Data/roots.csv')



##############################################################
#Implementering af random forest modellen, i sammenligning med QRF for at se hvad der foregår:

#Fitting the model
rf_leafs_model <- randomForest(x = train_leafs_x, y = train_leafs_y, nodesize = 100)

#Checking number of splits
for (i in 1:500){
  tree = data.frame(getTree(rf_leafs_model, k = i))
  endnodes = tree[tree$status == -1,]
  leng <- nrow(endnodes)
  size <- 900/leng
  print(leng)
}

#Using it to predict:
rf_leafs_pred <- predict(rf_leafs_model, newdata = test_leafs_x)

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

#Random Forest

node_grid <-c(5,100,150)

node_size_choose <- function(data, k = 3, node_grid) {
  set.seed(4)
  MSE_ns <- c()
  group <- sample(rep(1:k, length.out = nrow(data)))
  for (j in node_grid){
    MSE <- c()
    for (i in (1:k)){
      #Fit model
      train_x <- data.frame(Sc = data[group != i,1])
      train_y<- data[group != i,2]
      qrf_ns <- quantregForest(x = train_x, y = train_y, nodesize = j)
      
      #MSE
      conditionalMean_ns <- predict(qrf_ns, data.frame(Sc = data[group == i,1]), what = mean)
      MSE[i] <- mean((conditionalMean_ns - data[group == i,2])^2)
    }
    MSE_ns <- append(MSE_ns, mean(MSE)) 
  }
  node_size <- node_grid[which.min(MSE_ns)]
  return(node_size)
}


loo_rf <- function(data) {
  set.seed(777)
  pred <- c() ; obs <- c() ; q0.1 <- c() ; q0.9 <- c() ;mean_cv <- c()
  for (i in (1:nrow(data))){
    print(i)
    #choose nodesize
    nodesize <- node_size_choose(data = data, node_grid = node_grid)
    
    #Fit model
    train_x <- data.frame(Sc = data[-i,1])
    train_y <- data[-i,2]
    test_x <- data.frame(Sc=data[i,1])
    test_y <- data[i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = nodesize)
    
    #Getting quantiles
    conditionalQuantiles_cv <- predict(qrf_cv, test_x, what = c(0.1,0.9))
    conditionalMean_cv <- predict(qrf_cv, test_x, what = mean)
    
    #Creating vectors with observations and quantiles
    pred    <- append(pred, data[i,1])
    obs     <- append(obs, test_y)
    q0.1    <- append(q0.1, conditionalQuantiles_cv[,1])
    q0.9    <- append(q0.9, conditionalQuantiles_cv[,2])
    mean_cv <- append(mean_cv, conditionalMean_cv)
  }
  return("obs_int" = data.frame(observed = obs, quantile..0.1 = q0.1,quantile..0.9 = q0.9, pred=pred, mean_cv = mean_cv))
}

leafs_pred <- loo_rf(leafs)
wood_pred <- loo_rf(wood)
roots_pred <- loo_rf(roots)

#Constructing data to be plotted:

plot_f <- function(data, title){
  data <- data.frame(data, x = leafs$Sc)
  ggplot(data)+
    geom_point(aes(x = obs, y = Sc), color = 'darkolivegreen')+
    #geom_point(aes(x = obs, y = mean_pred), color = 'hotpink3')+
    xlab("Sc")+
    ylab("Kgp")+
    ggtitle(title)+
    theme_bw()
}

plot_f(leafs_pred, "Leafs")
plot_f(wood_pred, "Wood")
plot_f(roots_pred, "Roots")

cv <- function(data, k) {
  set.seed(777)
  n <- nrow(data)
  group <- sample(rep(1: k, length.out = n))
  pred <- c()
  obs <- c()
  q0.1 <- c()
  q0.9 <- c()
  for (i in (1:k)){
    #Fit model
    train_x <- data.frame(Sc = data[group != i,1])
    train_y <- data[group != i,2]
    test_x <- data.frame(Sc=data[group == i,1])
    test_y <- data[group == i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = 76)
    
    #Getting quantiles
    conditionalQuantiles_cv <- predict(qrf_cv, test_x, what = c(0.05,0.95))
    
    #Creating vectors with observations and quantiles
    if (i == 1) {
      pred <- c(data[group == i,1])
      obs <- c(test_y)
      q0.1 <- c(conditionalQuantiles_cv[,1])
      q0.9 <- c(conditionalQuantiles_cv[,2])
    }
    else {
      pred <- c(pred, data[group == i,1])
      obs <- c(obs, test_y)
      q0.1 <- c(q0.1, conditionalQuantiles_cv[,1])
      q0.9 <- c(q0.9, conditionalQuantiles_cv[,2])
    }
  }
  return("obs_int" = data.frame(observed = obs, quantile..0.1 = q0.1,quantile..0.9 = q0.9, pred=pred))
}

#########################Real one###########################333
#You need to run the code in document Forest_plots

#Plot
Sc_plot <- function(data,title){
  plot_data <- data.frame(data)
  plot_data <- plot_data %>%
    mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))
  #Plot
  ggplot(plot_data, aes(x = pred)) +
    geom_point(aes(y=observed), color = "darkolivegreen", alpha=0.5, fill = "darkolivegreen3") +
    geom_point(aes(y=mean_cv), color = 'hotpink3', fill = "hotpink2") +
    labs(title = title)+
    xlab("Sc")+
    ylab('Kgp')+
    theme_bw()
}

Sc_plot(cv_intervals_leafs, "Leafs")
Sc_plot(cv_intervals_wood, "Wood")
Sc_plot(cv_intervals_roots, "Roots")

roll_cov <- function(pred_int, alpha = 0.2, bin_size = 50, title){
  roll_cov <- c()
  
leafs_a <- leafs %>%
    arrange(Sc)

leafs_a[800:900,]



##############fFAKE MODEL######################################
#En fake model for sjov

#Fitting the model
rf_fake_model <- randomForest(x = fake_train_x, y = fake_train_y, nodesize = 5)
nrow(getTree(rf_leafs_model))

length(fake_train_y)
nrow(fake_train_x)

#Checking number of splits
for (i in 1:50){
  tree <- getTree(rf_leafs_model, k = i)
  leng <- nrow(tree)
  print(leng)
}


#Using it to predict:
rf_fake_pred <- predict(rf_fake_model, newdata = fake_test_x)
leafs_pred_mean <- predict(leafs_qrf, test_leafs_x, what = mean)

#Function for creating the dataframe to be plotted:
plot_data <- function(pred, obs, Sc){
  plot_data <- data.frame(pred, observed = obs, Sc = Sc)
}

plot_fake <- data.frame(pred = rf_fake_pred, obs = fake_train_y, Sc = fake_test_x)

ggplot(plot_fake) + 
  geom_point(aes(x = Sc, y = pred), color = 'hotpink4', size = 1.5) +
  geom_function(fun = function(x){alpha*x+beta})+
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")










