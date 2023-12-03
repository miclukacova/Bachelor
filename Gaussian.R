#################################--Indlæsning af pakker og data---##############

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
################################################################################

################################################################################
#################################--Log-log OLS model---#########################
################################################################################


pred_int_log_ols <- function(alpha = 0.2, data){
  #model fit
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  f_hat_log <- function(x) model$coef[[1]] + x*model$coef[[2]]
  
  #creation of prediction interval
  P <- solve((t(model.matrix(model)) %*% model.matrix(model)))
  upper0 <- function(x) {
    f_hat_log(x) - qt(alpha/2, nrow(data)-2)*
       sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  upper <- function(x) exp(upper0(log(x)))
  lower0 <- function(x) {
    f_hat_log(x) - qt(1-alpha/2, nrow(data)-2)*
       sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  lower <- function(x) exp(lower0(log(x)))
  return(list(f_hat, upper, lower))
}

leafs_pred_int <- loo_pred_int(data = leafs, pred_int = pred_int_log_ols, alpha = 0.2)
wood_pred_int <- loo_pred_int(data = wood, pred_int = pred_int_log_ols, alpha = 0.2)  
roots_pred_int <- loo_pred_int(data = roots, pred_int = pred_int_log_ols, alpha = 0.2)

plot_maker(leafs_pred_int[[1]], "Leafs")
plot_maker(wood_pred_int[[1]], "Wood")
plot_maker(roots_pred_int[[1]], "Roots")

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), 
              "Coverage" = c(leafs_pred_int[[2]],wood_pred_int[[2]],
                             roots_pred_int[[2]])), type = latex)


#---------------------Random split to assess coverage---------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols)

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


#------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)



#-------------------Checking coverage for different alphas----------------------

cov_alpha_l <- diff_alohas(data = leafs, pred_int = pred_int_log_ols)
cov_alpha_w <- diff_alohas(data = wood, pred_int = pred_int_log_ols)
cov_alpha_r <- diff_alohas(data = roots, pred_int = pred_int_log_ols)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))







################################################################################
#################################----- OLS model ------#########################
################################################################################

pred_int_ols <- function(alpha = 0.2, data){
  #model fit
  model <- lm(Kgp ~ Sc, data)
  f_hat <- function(x) model$coef[[1]] + x*model$coef[[2]]
  
  #creation of prediction interval
  P <- solve((t(model.matrix(model)) %*% model.matrix(model)))
  upper <- function(x) {
    f_hat(x) - qt(alpha/2, nrow(data)-2)*
      sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  lower <- function(x) {
    f_hat(x) - qt(1-alpha/2, nrow(data)-2)*
      sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  return(list(f_hat, upper, lower))
}

leafs_pred_int <- loo_pred_int(data = leafs, pred_int = pred_int_ols, alpha = 0.2)
wood_pred_int <- loo_pred_int(data = wood, pred_int = pred_int_ols, alpha = 0.2)  
roots_pred_int <- loo_pred_int(data = roots, pred_int = pred_int_ols, alpha = 0.2)

plot_maker(leafs_pred_int[[1]], "Leafs")
plot_maker(wood_pred_int[[1]], "Wood")
plot_maker(roots_pred_int[[1]], "Roots")

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), 
              "Coverage" = c(leafs_pred_int[[2]],wood_pred_int[[2]],
                             roots_pred_int[[2]])), type = latex)



#---------------------Random split to assess coverage---------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_ols)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_ols)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_ols)

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

#------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)


#-------------------Checking coverage for different alphas----------------------


cov_alpha_l <- diff_alohas(data = leafs, pred_int = pred_int_ols)
cov_alpha_w <- diff_alohas(data = wood, pred_int = pred_int_ols)
cov_alpha_r <- diff_alohas(data = roots, pred_int = pred_int_ols)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))




