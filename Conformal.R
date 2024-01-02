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

set.seed(4)

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


#Leafs

#Abs error

set.seed(4)

loo_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj[[1]], "Leafs - logOLSB", ols_log_adj_l)
plot_maker(loo[[1]], "Leafs")

#Abs error /sd
set.seed(4)
loo2_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
plot_maker(loo2_adj[[1]], "Leafs")

#Wood

#Abs error

set.seed(4)

loo_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj_w[[1]], "Wood - logOLSB", ols_log_adj_w)
plot_maker(loo_w[[1]], "Wood", ols_log_w)

#Abs error /sd
set.seed(4)
loo2_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
plot_maker(loo2_adj_w[[1]], "Wood", ols_log_adj_w)


#Roots

#Abs error

set.seed(4)

loo_adj_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj_r[[1]], "Roots", ols_log_adj_r) +   
  geom_segment(aes(x = Sc, y = Low, xend = Sc, yend = High),
               color = "hotpink", alpha = 0.4, lwd = 0.6)
plot_maker(loo_r[[1]], "Roots", ols_log_r) 
head(loo_r)
#Abs error /sd

set.seed(4)
loo2_adj_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
plot_maker(loo2_adj_r[[1]], "Roots") + geom_segment(aes(x = Sc, y = Low, xend = Sc, yend = High),
                                                   color = "hotpink", alpha = 0.4, lwd = 0.6)


#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, pred_int_log_ols_conf_adj, k = 5)
cov_alpha_w <- diff_alohas(wood, pred_int_log_ols_conf_adj, k = 5)
cov_alpha_r <- diff_alohas(roots, pred_int_log_ols_conf_adj, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


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

rs_plot_maker(a, "Leafs - logOLSB", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood - logOLSB", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2, conformal = TRUE, n = nrow(roots))


#----------------------------Rolling coverage---------------------------------------------

set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_adj)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_adj)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_adj)

roll_cov(pred_int = leafs_pred_int, title = "Leafs - logOLSB")
roll_cov(pred_int = wood_pred_int, title = "Wood - logOLSB")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)

####################################################################
#Diagnostics for logOLSB WITH SD-score:
###############################################################3####

#Checking coverage:
loo2_adj[[2]]
loo2_adj_r[[2]]
loo2_adj_w[[2]]


#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.3, 0.4)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, pred_int_log_ols_conf_2_adj, k = 5)
cov_alpha_w <- diff_alohas(wood, pred_int_log_ols_conf_2_adj, k = 5)
cov_alpha_r <- diff_alohas(roots, pred_int_log_ols_conf_2_adj, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_2_adj)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_2_adj)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_2_adj)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2, conformal = TRUE, n = nrow(roots))


#----------------------------Rolling coverage---------------------------------------------

set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2_adj)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2_adj)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2_adj)

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)







#NLR----------------------------------------------------------------------------

MSE_NLR <- function(par, data){
  with(data, sqrt(sum((Kgp-par[1]*Sc^par[2])^2)/nrow(data)))
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

pred_int_nlr_l <- function(data, alpha = 0.2) pred_int_nlr(data, alpha, c(0.2693082, 0.9441130))
pred_int_nlr_w <- function(data, alpha = 0.2) pred_int_nlr(data, alpha, c(3.944818, 1.106841))
pred_int_nlr_r <- function(data, alpha = 0.2) pred_int_nlr(data, alpha = 0.2, c(0.8339087, 1.1730237))


#----------------------------Pred intervaller###################################

set.seed(4)

loo_l_NLR <- loo_pred_int(leafs, alpha = 0.2, pred_int_nlr_l) 
loo_w_NLR <- loo_pred_int(wood, alpha = 0.2, pred_int_nlr_w)
loo_r_NLR <- loo_pred_int(roots, alpha = 0.2, pred_int_nlr_r)


plot_maker(loo_l_NLR[[1]], "Leafs - NLR", nlr_l)
plot_maker(loo_w_NLR[[1]], "Woods - NLR", nlr_w)
plot_maker(loo_r_NLR[[1]], "Roots", nlr_r)


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

rs_plot_maker(a, "Leafs - NLR", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood - NLR", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2, conformal = TRUE, n = nrow(roots))

#For different alphas:
alphas <- c(0.05, 0.1, 0.2,0.3)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, pred_int_nlr_l, k = 5)
cov_alpha_w <- diff_alohas(wood, pred_int_nlr_w, k = 5)
cov_alpha_r <- diff_alohas(roots, pred_int_nlr_r, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))


#----------------------------Rolling coverage---------------------------------------------

set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_nlr_l)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_nlr_w)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_nlr_r)

roll_cov(pred_int = leafs_pred_int, title = "Leafs - NLR")
roll_cov(pred_int = wood_pred_int, title = "Wood - NLR")
roll_cov(pred_int = roots_pred_int, title = "Roots - NLR", bin_size = 5)




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

pred_int_rf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = alpha)
pred_int_rf_w <- function(data, alpha = 0.2) pred_int_making(data, node_size = 70, alpha = alpha)
pred_int_rf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = alpha, node_size = 26)

set.seed(4)
loo_l <- loo_pred_int(leafs, pred_int = pred_int_rf_l)
loo_w <- loo_pred_int(wood, pred_int = pred_int_rf_w)
loo_r <- loo_pred_int(roots, pred_int = pred_int_rf_r)

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), 
              "Covergae" = c(loo_l[[2]], loo_w[[2]], loo_r[[2]])), type = latex)

plot_maker(loo_l[[1]],"Leafs - RF")
plot_maker(loo_w[[1]],"Wood - RF")
plot_maker(loo_r[[1]],"Roots")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(4)
cov_alpha_l <- diff_alohas(leafs, pred_int_rf_l, k = 5)
cov_alpha_w <- diff_alohas(wood, pred_int_rf_w, k = 5)
cov_alpha_r <- diff_alohas(wood, pred_int_rf_r, k = 5)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_rf_w)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

rs_plot_maker(a, "Leafs - RF", 0.2, conformal = TRUE, n = nrow(leafs)) 
rs_plot_maker(b, "Wood - RF", 0.2, conformal = TRUE, n = nrow(wood)) 
 

#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs - RF")
roll_cov(pred_int = loo_w, title = "Wood - RF")
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
  t_down <- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = alpha/2))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = alpha/2))
  }
  t_up<- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = 1-alpha/2))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = 1-alpha/2))
  }
  
  # Heuristic notion of uncertainty
  score <- c()
  for (i in (1:nrow(cali))){
    score[i] <- max((t_down(cali[i,]) - cali[i,]$Kgp), (cali[i,]$Kgp- t_up(cali[i,])))
  }
  score <- sort(score)
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) t_up(x) + q_hat
  lower <- function(x) t_down(x) - q_hat
  
  return(list(f_hat, upper, lower ))
}

pred_int_qrf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = 0.2)
pred_int_qrf_w <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2)
pred_int_qrf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2, node_size = 26)

set.seed(4)
#loo_l <- loo_pred_int(leafs, pred_int = pred_int_qrf_l)
#loo_w <- loo_pred_int(wood, pred_int = pred_int_qrf_w)
#Den her mangler at køre
#loo_r <- loo_pred_int(roots, pred_int = pred_int_qrf_r)


#write.csv(loo_l, "/Users/michaelalukacova/Bachelor1/Data/loo_l_conf_qr.csv", row.names=F)
#write.csv(loo_w, "/Users/michaelalukacova/Bachelor1/Data/loo_w_conf_qr.csv", row.names=F)
#write.csv(loo_r, "/Users/michaelalukacova/Bachelor1/Data/loo_r_conf_qr.csv", row.names=F)

loo_l <- read.csv('Data//loo_l_conf.csv')
loo_w <- read.csv('Data/loo_w_conf.csv')
#loo_r <- read.csv('Data/loo_r_conf.csv')


xtable(tibble(" " = c("Leafs", "Wood"), 
              "Covergae" = c(loo_l[[2]], loo_w[[1]])), type = latex)

plot_maker(loo_l,"Leafs")
mean(loo_l$Low <= loo_l$Kgp & loo_l$High >= loo_l$Kgp)
plot_maker(loo_w[[1]], "Wood")

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

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

rs_plot_maker(a, "Leafs", 0.2, conformal = TRUE, n = nrow(leafs))
rs_plot_maker(b, "Wood", 0.2, conformal = TRUE, n = nrow(wood))
rs_plot_maker(c, "Roots", 0.2)

#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs")
roll_cov(pred_int = loo_w, title = "Wood")

