library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
library(randomForest)
library(quantregForest)

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

#------------------------------- Full Conformal Function -----------------------

full_conformal <- function(data, alpha = 0.1, reg_alg, new_data){
  n <- nrow(data)
  low <- c()
  high <- c()
  y_trial <- seq(from = 0.000000001, to = max(data$Kgp)*3, length.out = 1000)
  for (x in new_data){
    pi_y <- c()
    for (y in y_trial){
      data_conf <- data %>% add_row(Sc = x, Kgp = y)
      f_hat <- reg_alg(data_conf)
      res_conf <- abs(data_conf$Kgp - f_hat(data_conf$Sc))
      res_n1 <- res_conf[n+1] 
      pi_y <- append(pi_y, (1 + sum((res_conf[1:n] <= res_n1)))/(n+1))
    }
    c_conf <-y_trial[(n+1)*pi_y <= ceiling((1-alpha)*(n+1))]
    low <- append(low, min(c_conf))
    high <- append(high, max(c_conf))
  }
  return(tibble(Sc = new_data, Low = low, High = high))
}
full_conformal_loo <- function(data, alpha = 0.1, reg_alg){
  for (i in (1:nrow(data))){
    print(i)
    if (i == 1){
      pred_int <- full_conformal(data = data[-i,], alpha = alpha, reg_alg = reg_alg, new_data = data[i,]$Sc)
    }
    else{
      new_pred_int <- full_conformal(data = data[-i,], alpha = alpha, reg_alg = reg_alg, new_data = data[i,]$Sc)
      pred_int <- pred_int %>% add_row(new_pred_int)
      }
  }
  pred_int <- pred_int %>% mutate("Kgp" = data$Kgp)
  cov <- mean((pred_int$Low <= data$Kgp)&(data$Kgp <= pred_int$High))
  return(list(pred_int,cov))
}

#------------------------------- De forskellige model algoritmer ---------------

ols_alg <- function(data){
  model <- lm(Kgp ~ Sc, data)
  f_hat <- function(x) model$coef[[1]] + model$coef[[2]]*x
  return(f_hat)
}
log_ols_alg <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  return(f_hat)
}
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}
nlr_alg <- function(data, start_point){
  mod <- optim(par = start_point, fn = MSE_NLR, data = data)
  f_hat <- function(x) mod$par[[1]]*x^mod$par[[2]]
  return(f_hat)
}
nlr_alg_l <- function(data) nlr_alg(data, c(0.61,0.81))
nlr_alg_w <- function(data) nlr_alg(data, c(5.51,0.73))
nlr_alg_r <- function(data) nlr_alg(data, c(3.66,0.1))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

pred_int_l <- full_conformal_loo(leafs, 0.2, log_ols_alg)
pred_int_l[[1]] <-pred_int_l[[1]] %>% mutate("Fitted" = log_ols_alg(leafs)(leafs$Sc))

pred_int_w <- full_conformal_loo(wood, 0.2, log_ols_alg)
pred_int_w[[1]] <-pred_int_w[[1]] %>% mutate("Fitted" = log_ols_alg(wood)(wood$Sc))

pred_int_r <- full_conformal_loo(roots, 0.2, log_ols_alg)
pred_int_r[[1]] <-pred_int_r[[1]] %>% mutate("Fitted" = log_ols_alg(roots)(roots$Sc))

plot_maker(pred_int_l[[1]], "Leafs")
plot_maker(pred_int_w[[1]], "Wood")
plot_maker(pred_int_r[[1]], "Roots")

#Rolling coverage

roll_cov(pred_int_l, alpha = 0.2, bin_size = 50, "Leafs")
roll_cov(pred_int_w, alpha = 0.2, bin_size = 50, "Wood")

#Distribution of coverage

rs_cov <- function(data, k, alpha, reg_alg) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    if (i == 1){
      print(i)
    }
    # Test and train
    
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    #Model fit
    
    full_conf <- full_conformal(train_rs, alpha, reg_alg, test_rs$Sc)
    
    #Definere
    cov[i] <- mean(full_conf$low <= test_rs$Kgp 
                   & full_conf$high >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

set.seed(4)
a <- rs_cov(leafs, 30, 0.2, log_ols_alg)
b <- rs_cov(wood, 30, 0.1, log_ols_alg)
c <- rs_cov(roots, 30, 0.1, log_ols_alg)

#----------------------------------------------------------------------------
# Full conformal med score funktion 2
#----------------------------------------------------------------------------

#Regression algorithm

log_ols_alg_2 <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  var_hat <- sum(model$residuals^2)/(nrow(data)-1)
  sd_hat <- function(x) sqrt(exp(2*(model$coefficients[[1]]+log(x)*model$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
  return(list(f_hat, sd_hat))
}

#Full conformal algorithm 

full_conformal_2 <- function(data, alpha = 0.1, reg_alg, new_data){
  
  #Definering
  n <- nrow(data) ; low <- c(); high <- c()
  
  #Grid of y-values to try
  y_trial <- c(seq(from = 1e-50, to = 1, length.out = 100), seq(from = 1, to = max(data$Kgp)*2, length.out = 1000))
  for (x in new_data){
    pi_y <- c()
    for (y in y_trial){
      #Creating model
      data_conf <- data %>% add_row(Sc = x, Kgp = y)
      funcs <- reg_alg(data_conf)
      f_hat <-  funcs[[1]] ; sd_hat <- funcs[[2]]
      
      #Calculating scores
      res_conf <- abs(data_conf$Kgp - f_hat(data_conf$Sc))/sd_hat(data_conf$Sc)
      res_n1 <- res_conf[n+1] 
      
      #Finding pi_y
      pi_y <- append(pi_y, (1 + sum((res_conf[1:n] <= res_n1)))/(n+1))
    }
    
    #Prediction band
    c_conf <-y_trial[(n+1)*pi_y <= ceiling((1-alpha)*(n+1))]
    
    #If prediction band is empty output error message
    try(if(length(c_conf) < 1) print(pi_y))
    
    #Lower and upper bound
    low <- append(low, min(c_conf))
    high <- append(high, max(c_conf))
  }
  return(tibble(Sc = new_data, Low = low, High = high))
}

full_conformal_loo_2 <- function(data, alpha = 0.1, reg_alg){
  for (i in (1:nrow(data))){
    print(i)
    if (i == 1){
      pred_int <- full_conformal_2(data = data[-i,], alpha = alpha, reg_alg = reg_alg, new_data = data[i,]$Sc)
    }
    else{
      new_pred_int <- full_conformal_2(data = data[-i,], alpha = alpha, reg_alg = reg_alg, new_data = data[i,]$Sc)
      pred_int <- pred_int %>% add_row(new_pred_int)
    }
  }
  pred_int <- pred_int %>% mutate("Kgp" = data$Kgp)
  cov <- mean((pred_int$Low <= data$Kgp)&(data$Kgp <= pred_int$High))
  return(list(pred_int,cov))
}

set.seed(4)

pred_int_l <- full_conformal_loo_2(leafs, 0.2, log_ols_alg_2)
pred_int_l[[1]] <-pred_int_l[[1]] %>% mutate("Fitted" = log_ols_alg_2(leafs)[[1]](leafs$Sc))

#Coverage 0.78

pred_int_w <- full_conformal_loo_2(wood, 0.2, log_ols_alg_2)
pred_int_w[[1]] <-pred_int_w[[1]] %>% mutate("Fitted" = log_ols_alg_2(wood)[[1]](wood$Sc))

#Coverage: 0.786533

pred_int_r <- full_conformal_loo_2(roots, 0.2, log_ols_alg_2)
pred_int_r[[1]] <-pred_int_r[[1]] %>% mutate("Fitted" = log_ols_alg_2(roots)[[1]](roots$Sc))

#Coverage: 0.6923077

roots[c(19,22),]

plot_maker(pred_int_l[[1]], "Leafs")
plot_maker(pred_int_w[[1]], "Wood")
plot_maker(pred_int_r[[1]], "Roots")


write.csv(pred_int_l[[1]], "/Users/michaelalukacova/Bachelor1/Data/full_conf_leafs_s2.csv", row.names=F)
write.csv(pred_int_w[[1]], "/Users/michaelalukacova/Bachelor1/Data/full_conf_wood_s2.csv", row.names=F)
write.csv(pred_int_r[[1]], "/Users/michaelalukacova/Bachelor1/Data/full_conf_roots_s2.csv", row.names=F)

boot_leafs <- read.csv('Data/boot_leafs_NLR.csv')
boot_wood <- read.csv('Data/boot_wood_NLR.csv')
boot_roots <- read.csv('Data/boot_roots_NLR.csv')
