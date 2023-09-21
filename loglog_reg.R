#################################--Indlæsning af pakker og data---###############################

leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

library(tidyverse)
library(readr)
library(infer)
################################################################################################

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log_train)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log_train)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log_train)

#Estimater

hat_beta <- c(lm_leafs_log$coefficients[[1]],
          lm_roots_log$coefficients[[1]],
          lm_wood_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
          lm_roots_log$coefficients[[2]],
          lm_wood_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_roots_log$residuals),
             var(lm_wood_log$residuals))


#Y_hat estimater uden bias correction:

f_hat_leafs <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)
f_hat_roots <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)
f_hat_wood <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)


#Y_hat estimater med bias correction: 
f_hat_leafs_adj <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)*exp(var_hat[1]/2)
f_hat_roots_adj <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)*exp(var_hat[2]/2)
f_hat_wood_adj <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)*exp(var_hat[3]/2)

#Den bias corrected model gør det ikke så godt på test sættet

mean((f_hat_leafs(leafs_log_test$Sc)-exp(leafs_log_test$Kgp))^2)
mean((f_hat_leafs_adj(leafs_log_test$Sc)-exp(leafs_log_test$Kgp))^2)

mean((f_hat_wood(roots_log_test$Sc)-exp(roots_log_test$Kgp))^2)
mean((f_hat_wood_adj(roots_log_test$Sc)-exp(roots_log_test$Kgp))^2)

mean((f_hat_roots(wood_log_test$Sc)-exp(wood_log_test$Kgp))^2)
mean((f_hat_roots_adj(wood_log_test$Sc)-exp(wood_log_test$Kgp))^2)

# k-fold cv vurdering af de to modeller

cv <- function(data, k) {
  MSE <- c()
  MSE_adj <- c() 
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    
    #MSE
    MSE[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))-exp(data[group == i, ]$Kgp))^2)
    MSE_adj[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))*
                         exp(var(lm_cv$residuals)/4)-exp(data[group == i, ]$Kgp))^2)
  }
  return(tibble("MSE" = MSE, "MSE Bias Corrected" = MSE_adj))
}

a <- cv(wood_log, 10)
mean(a[[1]])
mean(a[[2]])

cv(wood_log, 10)

cv(roots_log, 26)  

#Bias

cv_bias <- function(data, k) {
  bias <- c()
  bias_adj <- c() 
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    
    #Definere
    bias[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))-exp(data[group == i, ]$Kgp)))
    bias_adj[i] <- mean((exp(predict(lm_cv, newdata = data[group == i, ]))*
                          exp(var(lm_cv$residuals)/2)-exp(data[group == i, ]$Kgp)))
  }
  return(tibble("Bias" = bias, "Bias Bias Corrected" = bias_adj))
}

#

cv_bias(leafs_log, 10)
cv(leafs_log, 10)

cv(wood_log, 10)

cv(roots_log, 2)
  