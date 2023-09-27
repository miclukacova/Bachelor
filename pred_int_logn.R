### Prediction intervals for lognormal variables

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

hat_beta_log <- c(lm_leafs_log$coefficients[[1]],
              lm_roots_log$coefficients[[1]],
              lm_wood_log$coefficients[[1]])

hat_alpha_log <- c(lm_leafs_log$coefficients[[2]],
               lm_roots_log$coefficients[[2]],
               lm_wood_log$coefficients[[2]])

var_hat_log <- c(var(lm_leafs_log$residuals),
             var(lm_roots_log$residuals),
             var(lm_wood_log$residuals))

# Estimater for hat Y fordeling

mean_hat_y_leafs <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)*exp(var_hat[1]/2)
mean_hat_y_roots <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)*exp(var_hat[2]/2)
mean_hat_y_wood <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)*exp(var_hat[3]/2)

sd_hat_y_leafs <- function(x) exp(mean_y_leafs*2 + var_hat_log[1])*(exp(var_hat_log[1]-1))
sd_hat_y_roots <- function(x) exp(mean_y_roots*2 + var_hat_log[2])*(exp(var_hat_log[2]-1))
sd_hat_y_wood <- function(x) exp(mean_y_wood*2 + var_hat_log[3])*(exp(var_hat_log[3]-1))


# Y*Hat{Y} fordelings estimater

sigma_hat_leafs <- var(lm_leafs_log$residuals)

# Y - hat{Y} varians

var_yy <- ((leafs_log_test$Sc)^2*sigma_hat_leafs)/(sum(train_leafs_x^2)) + sigma_hat_leafs

# exp(Y - hat{Y}) middelværdi og varians

mean_yy_leafs <- exp(var_yy/2)
var_yy_leafs <- exp(2*0+var_yy)*(exp(var_yy)-1)

kvantil_op <- qlnorm(0.975, meanlog = mean_yy_leafs, sdlog = sqrt(var_yy_leafs))

kvantil_ned <- qlnorm(0.025, meanlog = mean_yy_leafs, sdlog = sqrt(var_yy_leafs))

pred_int <- tibble("Low" = kvantil_ned/(mean_hat_y_leafs(leafs_log_test$Sc)),
                  "Up" = kvantil_op/(mean_hat_y_leafs(leafs_log_test$Sc)))


mean(pred_int$Low < leafs_log_test$Kgp & leafs_log_test$Kgp < pred_int$Up)
