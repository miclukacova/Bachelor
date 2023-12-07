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


#---------------Fitting the QRF and calculating predictions + intervals:

#Defining the nodegrids to be searched:
node_grid_l <-c(5,70,100,150)
node_grid_w <-c(5,70,130)
node_grid_r <-c(5)

#Function performing CV to choose minimal nodesize
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


#LOOCV-function that produces predictions for both mean and intervals:
loo_rf <- function(data, node_grid) {
  set.seed(777)
  pred <- c() ; obs <- c() ; q0.1 <- c() ; q0.9 <- c() ;mean_cv <- c(); node_size <- c()
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
    node_size <- append(node_size, nodesize)
  }
  return("obs_int" = data.frame(Kgp = obs, quantile..0.1 = q0.1,quantile..0.9 = q0.9, Sc=pred, predicted_value = mean_cv, nodesize = nodesize))
}


leafs_pred <- loo_rf(leafs, node_grid_l)
wood_pred <- loo_rf(wood, node_grid_w)
roots_pred <- loo_rf(roots, node_grid_r)

#Skriver det til en CSV så det er nemmere at arbejde med


#Til Michaelas computer:
write.csv(leafs_pred, "/Users/michaelalukacova/Bachelor1/Data/QRF_intervals_leafs.csv", row.names=F)
write.csv(wood_pred, "/Users/michaelalukacova/Bachelor1/Data/QRF_intervals_wood.csv", row.names=F)
write.csv(roots_pred, "/Users/michaelalukacova/Bachelor1/Data/QRF_intervals_roots.csv", row.names=F)


#Til Dinas computer
write.csv(leafs_pred, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/QRF_intervals_leafs.csv", row.names=F)
write.csv(wood_pred, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/QRF_intervals_wood.csv", row.names=F)
write.csv(roots_pred, "C:/Users/Bruger/OneDrive - University of Copenhagen/4. år/Bachelor/Bachelor1/Data/QRF_intervals_roots.csv", row.names=F)


#---------Indlæsning af predicted values and prediction intervals:--------
#OBS man skal have CSV-filerne liggende før det giver mening:

leafs_pred <- read.csv("Data/QRF_intervals_leafs.csv")
wood_pred <- read.csv("Data/QRF_intervals_wood.csv")
roots_pred <- read.csv("Data/QRF_intervals_roots.csv")

#-----------------MODEL--------------------------------------------
#Defining function to plot the prediction of the mean:
Sc_plot <- function(data,title){
  plot_data <- data.frame(data)
  #Plot
  ggplot(plot_data, aes(x = Sc)) +
    geom_point(aes(y=Kgp), color = "darkolivegreen", alpha=0.5, fill = "darkolivegreen3") +
    geom_point(aes(y=mean_cv), color = 'hotpink3', fill = "hotpink2") +
    labs(title = title)+
    xlab("Sc")+
    ylab('Kgp')+
    theme_bw()
}

Sc_plot(leafs_pred, "Leafs")
Sc_plot(wood_pred, "Wood")
Sc_plot(roots_pred, "Roots")


#Calculating MSE and bias:


#--------------PREDICTION INTERVALS------------------------------

#Defining function for plotting prediction intervals:
plot_maker <- function(pred_int, title){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((quantile..0.1 <= Kgp)&(Kgp <= quantile..0.9),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 0.8, alpha = 0.7) + 
    geom_point(aes(x = Sc, y = quantile..0.9), color = "hotpink", size = 0.6, alpha = 0.7) + 
    geom_point(aes(x = Sc, y = quantile..0.1), color = "hotpink", size = 0.6, alpha = 0.7) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    scale_color_manual(values = color)+ 
    theme(legend.position = "none")
}

plot_maker(leafs_pred, "Leafs")
plot_maker(wood_pred, "Wood")
plot_maker(roots_pred, "Roots")

#.------------------------------Initial investigation.----------------
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


##-------------------FAKE MODEL--------------------------------
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










