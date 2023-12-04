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


#Log-log ols--------------------------------------------------------

set.seed(5)

#Score funktion Absolute error
pred_int_log_ols_conf_adj <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  
  # Heuristic notion of uncertainty
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat_adj <- score_adj[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat_adj(x) + q_hat_adj
  lower <- function(x) f_hat_adj(x) - q_hat_adj
  
  return(list(f_hat_adj, upper,lower))
}
pred_int_log_ols_conf <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper, lower))
}

#Score funktion Absolute error / sd hat(Y) (VIRKER IKKE SÅ GODT)
pred_int_log_ols_conf_2_adj <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
  
  # Heuristic notion of uncertainty
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat_adj <- score_adj[quanti]
  
  #Prediction intervals
  
  upper_adj <- function(x) f_hat_adj(x) + q_hat_adj*sd_y_hat(x)
  lower_adj <- function(x) f_hat_adj(x) - q_hat_adj*sd_y_hat(x)
  
  return(list(f_hat_adj, upper_adj, lower_adj))
}
pred_int_log_ols_conf_2 <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction intervals
  upper <- function(x) f_hat(x) + q_hat*sd_y_hat(x)
  lower <- function(x) f_hat(x) - q_hat*sd_y_hat(x)
  
  return(list(f_hat, upper, lower))
}


#Leafs

#Abs error

loo_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj[[1]], "Leafs")
plot_maker(loo[[1]], "Leafs")

#Abs error /sd

loo2_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
loo2 <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2) 


plot_maker(loo2_adj[[1]], "Leafs")
plot_maker(loo2[[1]], "Leafs")

#Wood

#Abs error

loo_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj_w[[1]], "Wood")
plot_maker(loo_w[[1]], "Wood")

#Abs error /sd

loo2_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
loo2_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2) 


plot_maker(loo2_adj_w[[1]], "Wood")
plot_maker(loo2_w[[1]], "Wood")



##Roots
#
##Abs error
#
#loo_adj_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_adj) 
#loo_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf)
#
#plot_maker(loo_adj_r[[1]], "roots")
#plot_maker(loo_r[[1]], "roots")
#
##Abs error /sd
#
#loo2_adj_w <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
#loo2_w <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2) 
#
#
#plot_maker(loo2_adj_w[[1]], "roots")
#plot_maker(loo2_w[[1]], "roots")
#

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_log_ols_conf_adj)
cov_alpha_w <- diff_alohas(wood, pred_int_log_ols_conf_adj)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_adj)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_adj)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_adj)

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


#----------------------------Rolling coverage---------------------------------------------

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_adj)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_adj)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_adj)

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)

####################################################################

#NLR----------------------------------------------------------------------------

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

nlr_alg <- function(data, start_point){
  mod <- optim(par = start_point, fn = MSE_NLR, data = data)
  f_hat <- function(x) mod$par[[1]]*x^mod$par[[2]]
  return(f_hat)
}

pred_int_nlr <- function(data, alpha = 0.2, starting_points) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  f_hat <- nlr_alg(data, starting_points)
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper,lower))
}

pred_int_nlr_l <- function(data, alpha = 0.2) pred_int_nlr(data, alpha = 0.2, c(0.61,0.81))
pred_int_nlr_w <- function(data, alpha = 0.2) pred_int_nlr(data, alpha = 0.2, c(5.51,0.73))
pred_int_nlr_r <- function(data, alpha = 0.2) pred_int_nl(data, alpha = 0.2, c(3.66,0.1))

#----------------------------Pred intervaller###################################

set.seed(6)

loo_l <- loo_pred_int(leafs, alpha = 0.2, pred_int_nlr_l) 
loo_w <- loo_pred_int(wood, alpha = 0.2, pred_int_nlr_w)
loo_r <- loo_pred_int(roots, alpha = 0.2, pred_int_nlr_w)


plot_maker(loo_l[[1]], "Leafs")
plot_maker(loo_w[[1]], "Woods")
plot_maker(loo_r[[1]], "Roots")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_nlr_l)
cov_alpha_w <- diff_alohas(wood, pred_int_nlr_w)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_w)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_r)

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


#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs")
roll_cov(pred_int = loo_w, title = "Wood")
roll_cov(pred_int = loo_r, title = "Roots", bin_size = 5)

#Regression forest--------------------------------------------------------------

#----------------------------Pred intervaller-----------------------------------
pred_int_making <- function(data, node_size = 70, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  f_hat <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
    
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper, lower ))
}

pred_int_rf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = 0.2)
pred_int_rf_w <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2)
pred_int_rf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2, node_size = 5)

set.seed(4)
loo_l <- loo_pred_int(leafs, pred_int = pred_int_rf_l)
loo_w <- loo_pred_int(wood, pred_int = pred_int_rf_w)
loo_r <- loo_pred_int(roots, pred_int = pred_int_rf_r)

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), 
              "Covergae" = c(loo_l[[2]], loo_w[[2]], loo_r[[2]])), type = latex)

plot_maker(loo_l[[1]],"Leafs")
plot_maker(loo_w[[1]],"Wood")
plot_maker(loo_r[[1]],"Roots")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_rf_l)
cov_alpha_w <- diff_alohas(wood, pred_int_rf_w)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_w)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_r)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

beta_l <- function(x) dbeta(x, nrow(leafs)+1-floor((nrow(leafs)+1)*0.2),
                            floor((nrow(leafs)+1)*0.2))
beta_w <- function(x) dbeta(x, nrow(wood)+1-floor((nrow(wood)+1)*0.2),
                            floor((nrow(wood)+1)*0.2))

rs_plot_maker(a, "Leafs", 0.2) + geom_function(fun = beta_l)
rs_plot_maker(b, "Wood", 0.2) + geom_function(fun = beta_l)
rs_plot_maker(c, "Roots", 0.2)
 

#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs")
roll_cov(pred_int = loo_w, title = "Wood")
roll_cov(pred_int = loo_r, title = "Roots", bin_size = 5)

####################################################################
#Conformalised quantile regression forest---------------------------------------
####################################################################

pred_int_making <- function(data, node_size = 70, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.5*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  
  f_hat <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
  t_05 <- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = 0.05))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = 0.05))
  }
  t_95<- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = 0.95))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = 0.95))
  }
  
  # Heuristic notion of uncertainty
  score <- c()
  for (i in (1:nrow(cali))){
    score[i] <- max((t_05(cali[i,]) - cali[i,]$Kgp), (cali[i,]$Kgp- t_95(cali[i,])))
  }
  score <- sort(score)
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) t_95(x) + q_hat
  lower <- function(x) t_05(x) - q_hat
  
  return(list(f_hat, upper, lower ))
}

pred_int_qrf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = 0.2)
pred_int_qrf_w <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2)
pred_int_qrf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2, node_size = 5)

set.seed(4)
loo_l <- loo_pred_int(leafs, pred_int = pred_int_qrf_l)
loo_w <- loo_pred_int(wood, pred_int = pred_int_qrf_w)
loo_r <- loo_pred_int(roots, pred_int = pred_int_qrf_r)

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), 
              "Covergae" = c(loo_l[[2]], loo_w[[2]], loo_r[[2]])), type = latex)

plot_maker(loo_l[[1]],"Leafs")
plot_maker(loo_w[[1]],"Leafs")
plot_maker(loo_r[[1]],"Leafs")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_qrf_l)
cov_alpha_w <- diff_alohas(wood, pred_int_qrf_w)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_qrf_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_qrf_w)
b <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_qrf_r)

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

#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs")
roll_cov(pred_int = loo_w, title = "Wood")
roll_cov(pred_int = loo_r, title = "Roots", bin_size = 5)

