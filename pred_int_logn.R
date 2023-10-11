### Prediction intervals for lognormal variables

#################################--Indlæsning af pakker og data---###############################

leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

leafs_test <- read.csv('Data/test_leafs.csv')
roots_test <- read.csv('Data/test_roots.csv')
wood_test <- read.csv('Data/test_wood.csv')

library(tidyverse)
library(readr)
library(infer)
################################################################################################

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log_train)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log_train)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log_train)

#Estimater fra lm

hat_beta <- c(lm_leafs_log$coefficients[[1]],
              lm_roots_log$coefficients[[1]],
              lm_wood_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
               lm_roots_log$coefficients[[2]],
               lm_wood_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_roots_log$residuals),
             var(lm_wood_log$residuals))

# hat log Y som funktion af log x

hat_log_y_leafs <- function(x) hat_beta[1] + hat_alpha[1]*x
hat_log_y_roots <- function(x) hat_beta[2] + hat_alpha[2]*x
hat_log_y_wood <- function(x) hat_beta[3] + hat_alpha[3]*x

# hat Y som funktion af x

hat_y_leafs <- function(x) exp(hat_beta[1])* x^hat_alpha[1]*exp(var_hat[1]/2)
hat_y_roots <- function(x) exp(hat_beta[2])* x^hat_alpha[2]*exp(var_hat[2]/2)
hat_y_wood <- function(x) exp(hat_beta[3])* x^hat_alpha[3]*exp(var_hat[3]/2)

# hat varians af log Y - hat log Y som funktion af log x

var_y_y_hat_leafs <- function(x) x*var_hat[1]/(sum(leafs_log_train^2))+var_hat[1] 
var_y_y_hat_wood <- function(x) x*var_hat[2]/(sum(wood_log_train^2))+var_hat[2] 
var_y_y_hat_roots <- function(x) x*var_hat[3]/(sum(roots_log_train^2))+var_hat[3] 

# Middelværdi og varians af exp(logY - hat log Y) som funktion af log x

mean_exp_y_y_hat_leafs <- function(x) exp(var_y_y_hat_leafs(x)/2) 
mean_exp_y_y_hat_wood <- function(x) exp(var_y_y_hat_wood(x)/2) 
mean_exp_y_y_hat_roots <- function(x) exp(var_y_y_hat_roots(x)/2) 

var_exp_y_y_hat_leafs <- function(x) exp(var_y_y_hat_leafs(x))*(exp(var_y_y_hat_leafs(x))-1) 
var_exp_y_y_hat_wood <- function(x) exp(var_y_y_hat_wood(x))*(exp(var_y_y_hat_wood(x))-1) 
var_exp_y_y_hat_roots <- function(x) exp(var_y_y_hat_roots(x))*(exp(var_y_y_hat_roots(x))-1) 

#Kvantil som funktion af log x

kvant_low <- function(x) qlnorm(0.2, meanlog = mean_exp_y_y_hat_leafs(x), sdlog = sqrt(var_exp_y_y_hat_leafs(x)))
kvant_up <- function(x) qlnorm(0.7, meanlog = mean_exp_y_y_hat_leafs(x), sdlog = sqrt(var_exp_y_y_hat_leafs(x)))

#Prediktionsinterval som funktion af log x

pred_int_low <- function(x) kvant_low(x) * mean_hat_y_leafs(x)
pred_int_up <- function(x) kvant_up(x) * mean_hat_y_leafs(x)

#Prediktionsinterval som funktion af x

pred_int_low_log <- function(x) pred_int_low(log(x))
pred_int_up_log <- function(x) pred_int_up(log(x))

mean(pred_int_low_log(leafs_test$Sc)<= leafs_test$Sc & pred_int_up_log(leafs_test$Sc) >= leafs_test$Sc)

l <- pred_int_low_log(leafs_test$Sc)
u <- pred_int_up_log(leafs_test$Sc)

color <- c("darkolivegreen1", "darkolivegreen4")
#aes(color = Indicator)

ggplot(data.frame(leafs_test, l=l, u=u), aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  geom_point(aes(y=l)+
  geom_point(aes(y=u)+
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  #geom_function(fun = hat_y_leafs, colour = "hotpink1") +
  #geom_function(fun = pred_int_low_log, colour = "hotpink4") +
  geom_function(fun = pred_int_up_log, colour = "hotpink4") +
  labs(title = "Leafs")
#+scale_color_manual(values = color)

