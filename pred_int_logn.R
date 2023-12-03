### Prediction intervals for lognormal variables

#################################--Indlæsning af pakker og data---###############################

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(xtable)
################################################################################################


pred_int_quant <- function(alpha = 0.2, data){
  
  #model fit
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  f_hat_log <- function(x) model$coef[[1]] + log(x)*model$coef[[2]]
  
  #creation of prediction interval

  upper <- function(x) qlnorm(1-alpha/2, meanlog = f_hat_log(x), sdlog = sqrt(var(model$residuals))) 
  lower <- function(x)  qlnorm(alpha/2, meanlog = f_hat_log(x), sdlog = sqrt(var(model$residuals)))
  
  return(list(f_hat, upper, lower))
}

leafs_pred_int <- loo_pred_int(data = leafs, pred_int = pred_int_quant, alpha = 0.2)
wood_pred_int <- loo_pred_int(data = wood, pred_int = pred_int_quant, alpha = 0.2)  
roots_pred_int <- loo_pred_int(data = roots, pred_int = pred_int_quant, alpha = 0.2)

plot_maker(leafs_pred_int[[1]], "Leafs")
plot_maker(wood_pred_int[[1]], "Wood")
plot_maker(roots_pred_int[[1]], "Roots")

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), 
              "Coverage" = c(leafs_pred_int[[2]],wood_pred_int[[2]],
                             roots_pred_int[[2]])), type = latex)



#---------------------Random split to assess coverage---------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_quant)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_quant)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_quant)

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

alphas <- c(0.05, 0.1, 0.2, 0.3)

cov_alpha_l <- diff_alohas(data = leafs, pred_int = pred_int_quant)
cov_alpha_w <- diff_alohas(data = wood, pred_int = pred_int_quant)
cov_alpha_r <- diff_alohas(data = roots, pred_int = pred_int_quant)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))
