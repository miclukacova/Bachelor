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


#---------------Fitting the QRF and calculating predictions + intervals:------

#Defining the nodegrids to be searched:
node_grid_l <-c(5,70,100,150)
node_grid_w <-c(5,70,130)
node_grid_r <-c(26)

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
  ggplot(plot_data, aes(x = Sc, y = Kgp)) +
    geom_point(color = 'darkolivegreen', fill = 'darkolivegreen2', alpha = 0.8, size = 1.5, shape = 21) +
    geom_point(aes(y=predicted_value), color = 'hotpink4', fill = 'hotpink' ,size = 1.5, shape = 23, alpha = 0.8) +
    ggtitle(title)+
    xlab("Sc")+
    ylab('Kgp')+
    theme_bw()+
    theme(plot.title = element_text(size = 17),
          axis.title = element_text(size = 13))
}

Sc_plot(leafs_pred, "Leafs")
Sc_plot(wood_pred, "Wood")
Sc_plot(roots_pred, "Roots")


#Calculating MSE and bias:
#Naive - where we fit on all data:

MSE_bias_naive <- function(data, nodesize){
  set.seed(4)
  rf <- quantregForest(x = data.frame(data$Sc), y = data$Kgp, nodesize = nodesize)
  predicted <- predict(rf, data.frame(data$Sc), what = mean)
  MSE <- mean((predicted - data$Kgp)^2)
  bias <- mean(predicted - data$Kgp)
  return(tibble("MSE" = MSE, "Bias" = bias))
}

MSE_bias <- tibble("Leafs" = MSE_bias_naive(leafs, 100),"Wood" = MSE_bias_naive(wood, 70),"Roots" = MSE_bias_naive(roots, 26) )


#And here on the LOOCV:
MSE_l <- mean((leafs_pred$Kgp-leafs_pred$predicted_value)^2)
Bias_l <-  mean((leafs_pred$Kgp-leafs_pred$predicted_value))

MSE_w <- mean((wood_pred$Kgp-wood_pred$predicted_value)^2)
Bias_w <-  mean((wood_pred$Kgp-wood_pred$predicted_value))

MSE_r <- mean((roots_pred$Kgp-roots_pred$predicted_value)^2)
Bias_r <-  mean((roots_pred$Kgp-roots_pred$predicted_value))

#MSE
MSE_l
MSE_w
MSE_r

#Bias
Bias_l
Bias_w
Bias_r

#--------------PREDICTION INTERVALS------------------------------

#Defining function for plotting prediction intervals:
plot_maker <- function(pred_int, title){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((quantile..0.1 <= Kgp)&(Kgp <= quantile..0.9),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen2")
  
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    #geom_segment(aes(x = Sc, y = quantile..0.1, xend = Sc, yend = quantile..0.9),
                 #color = "hotpink3", alpha = 0.2, lwd = 0.1)+
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1.5, alpha = 0.7, fill) + 
    geom_point(aes(x = Sc, y = quantile..0.9), color = "hotpink", size = 1, alpha = 0.7) + 
    geom_point(aes(x = Sc, y = quantile..0.1), color = "hotpink", size = 1,, alpha = 0.7) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    scale_color_manual(values = color)+ 
    theme(legend.position = "none", plot.title = element_text(size = 17),
                  axis.title = element_text(size = 13))
}

plot_maker(leafs_pred, "Leafs")
plot_maker(wood_pred, "Wood")
plot_maker(roots_pred, "Roots")

head(leafs_pred)

centered_plot <- function(data, title){
  cv_intervals <- data.frame(data, 
                             interval = data$quantile..0.9-data$quantile..0.1)
  cv_intervals <- arrange(cv_intervals, interval)
  q_mean <- apply(data[2:3], MARGIN = 1, FUN = mean)
  
  cv_intervals_cent <- cv_intervals-q_mean
  
  cv_intervals_cent$indx <- as.numeric(row.names(cv_intervals_cent))
  
  cv_intervals_cent <- cv_intervals_cent %>%
    mutate(indicator = if_else((quantile..0.1 <= Kgp)&(Kgp<= quantile..0.9),"in", "out"))
  
  colors <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(data = cv_intervals_cent, aes(x = indx)) +
    geom_segment(aes(x = indx, y = quantile..0.1, xend = indx, yend = quantile..0.9),
                 color = "hotpink", alpha = 0.2, lwd = 0.4) +
    geom_point(aes(y=quantile..0.1), color = 'hotpink',
               fill = 'hotpink', size = 0.8, shape = 24, alpha = 0.4) +
    geom_point(aes(y=quantile..0.9), color = 'hotpink', 
               fill = 'hotpink', size = 0.8, shape = 25, alpha = 0.4)+
    geom_point(aes(y=Kgp, color = indicator), size = 0.7)+
    scale_color_manual(values = colors)+
    ylab('Kgp')+ ggtitle(title)+
    xlab('Ordered Samples')+
    theme_bw()
}

cv_int <- function(x){
  cv_intervals <- data.frame(x, 
                             interval = x$quantile..0.9-x$quantile..0.1)
  cv_intervals <- arrange(cv_intervals, interval)
  return(cv_intervals)
}

cv_intervals_leafs <- cv_int(leafs_pred)
cv_intervals_wood <- cv_int(wood_pred)
cv_intervals_roots <- cv_int(roots_pred)



centered_plot <- function(data, title){
  q_mean <- apply(data[2:3], MARGIN = 1, FUN = mean)
  cv_intervals_cent <- data-q_mean
  cv_intervals_cent$indx <- as.numeric(row.names(cv_intervals_cent))
  cv_intervals_cent <- cv_intervals_cent %>%
    mutate(indicator = if_else((quantile..0.1 <= Kgp)&(Kgp <= quantile..0.9),"in", "out"))
  colors <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  ggplot(data = cv_intervals_cent, aes(x = indx)) +
    geom_segment(aes(x = indx, y = quantile..0.1, xend = indx, yend = quantile..0.9),
                 color = "hotpink", alpha = 0.1, lwd = 0.4) +
    geom_point(aes(y=quantile..0.1), color = 'hotpink',
               fill = 'hotpink', size = 0.8, shape = 24, alpha = 0.2) +
    geom_point(aes(y=quantile..0.9), color = 'hotpink', 
               fill = 'hotpink', size = 0.8, shape = 25, alpha = 0.2)+
    geom_point(aes(y=Kgp, color = indicator), size = 0.7)+
    scale_color_manual(values = colors)+
    ylab("Kgp")+ ggtitle(title)+
    xlab('Ordered Samples')+
    theme_bw()+
    theme(legend.position = "none")
}

centered_plot(cv_intervals_leafs, "Leafs")
centered_plot(cv_intervals_wood, "Wood")


centered_plot_roots <- function(data){
  q_mean <- apply(data[2:3], MARGIN = 1, FUN = mean)
  cv_intervals_cent <- data-q_mean
  cv_intervals_cent$indx <- as.numeric(row.names(cv_intervals_cent))
  cv_intervals_cent <- cv_intervals_cent %>%
    mutate(indicator = if_else((quantile..0.1 <= Kgp)&(Kgp <= quantile..0.9),"in", "out"))
  colors <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  ggplot(data = cv_intervals_cent, aes(x = indx)) +
    geom_segment(aes(x = indx, y = quantile..0.1, xend = indx, yend = quantile..0.9),
                 color = "hotpink", alpha = 0.5, lwd = 0.9) +
    geom_point(aes(y=quantile..0.1), color = 'hotpink',
               fill = 'hotpink', size = 0.9, shape = 24, alpha = 0.9) +
    geom_point(aes(y=quantile..0.9), color = 'hotpink', 
               fill = 'hotpink', size = 0.9, shape = 25, alpha = 0.9)+
    geom_point(aes(y=Kgp, color = indicator))+
    scale_color_manual(values = colors)+
    ylab('Kgp')+ ggtitle('Roots')+
    xlab('Ordered Samples')+
    theme_bw()+
    theme(legend.position = "none")
}

centered_plot_roots(cv_intervals_roots)

#Calculating coverage:
 cov_l <- mean(leafs_pred$quantile..0.1 <= leafs_pred$Kgp & leafs_pred$quantile..0.9 >= leafs_pred$Kgp)
 cov_w <- mean(wood_pred$quantile..0.1 <= wood_pred$Kgp & wood_pred$quantile..0.9 >= wood_pred$Kgp)
 cov_r <- mean(roots_pred$quantile..0.1 <= roots_pred$Kgp & roots_pred$quantile..0.9 >= roots_pred$Kgp)
 
 xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
               "Coverage" =c(cov_l, cov_w, cov_r )), type = latex)
 

#----------------------------Distribution of coverage by resampling-----------------------------------------

pred_int_making <- function(data, node_size = 70, alpha = 0.2) {
  #Formatting data
  train_x <- data.frame(Sc = data[,1])
  train_y <- data[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  
  f_hat <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
  lower <- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = c(alpha/2)))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = c(alpha/2)))
  }
  upper<- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = c(1-alpha/2)))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = c(1-alpha/2)))
  }
  
  return(list(f_hat, upper, lower))
}

pred_int_qrf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = 0.2)
pred_int_qrf_w <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2)
pred_int_qrf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2, node_size = 26)


#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 100, alpha = 0.2, pred_int_maker = pred_int_qrf_l)
b <- rs_cov(data = wood, k = 100, alpha = 0.2, pred_int_maker = pred_int_qrf_w)
c <- rs_cov(data = roots, k = 100, alpha = 0.2, pred_int_maker = pred_int_qrf_r)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs", 0.2)
rs_plot_maker(b, "Wood", 0.2)
rs_plot_maker(c, "Roots", 0.2)

head(leafs_pred)

#Rolling coverage:
roll_cov <- function(pred_int, alpha = 0.2, bin_size = 50, title){
  roll_cov <- c()
  
  data_arr <- pred_int %>%
    arrange(Sc)
  
  for (i in seq(1,nrow(data_arr)-bin_size)){
    data_cov <- data_arr %>%
      slice(i:(i+bin_size))
    roll_cov[i] <-mean((data_cov$quantile..0.1 <= data_cov$Kgp) & (data_cov$quantile..0.9 >= data_cov$Kgp))
  }
  
  my_tib <- tibble("Bin" = seq(1,nrow(data_arr)-bin_size), "Roll_cov" = roll_cov)
  
  up_binom <- qbinom(alpha/2, bin_size, 1-alpha)/bin_size 
  down_binom <- qbinom(1-alpha/2,bin_size,1-alpha)/bin_size
  
  ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
    geom_line(linewidth = 0.6, aes(color = Roll_cov)) + 
    geom_hline(yintercept = 1-alpha, color = "purple")+
    geom_hline(yintercept = up_binom, color = "purple", linetype = "dashed", linewidth = 0.3)+
    geom_hline(yintercept = down_binom, color = "purple", linetype = "dashed", linewidth = 0.3)+
    theme_bw() +
    xlab('Bin') + 
    ylab('Coverage')+
    ylim(c(0,1))+
    labs(title = title)+
    scale_color_gradient2(low = 'blue', mid = 'purple', high = 'red', midpoint = 0.8, limits = c(0.6,1),
                          na.value = "blue")+
    theme(legend.position = "none", plot.title = element_text(size = 17),
          axis.title = element_text(size = 13))
  
}



roll_cov(leafs_pred, alpha = 0.2, bin_size = 50, "Leafs")
roll_cov(wood_pred, alpha = 0.2, bin_size = 50, "Wood")
roll_cov(roots_pred, alpha = 0.2, bin_size = 5, "Roots")




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












#-----------------Lidt RMSE
leafs_n <- c(4.12 , 4.27 , 5.80 , 3.99 , 3.63)
wood_n <- c(6576.57 , 6835.36 , 7663.16 , 6587.67 , 5210.00)
roots_n <- c(2617.58 , 5105.20 , 2780.65 , 1411.15 , 3251.00)

leafs_cv <- c(4.15 , 4.28 , 5.83 , 4.03 , 4.13)
wood_cv <- c(6650.10 , 6868.05 , 7717.64 , 6698.82 , 6545.69)
roots_cv <- c(5761.43 , 5105.20 , 3481.70 , 2208.37 , 9084.12)

#Calculating RMSE:

sqrt(leafs_n)
sqrt(wood_n)
sqrt(roots_n)

sqrt(leafs_cv)
sqrt(wood_cv)
sqrt(roots_cv)
