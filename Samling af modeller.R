#################################--Indlæsning af pakker og data---###############################

leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs_train <- read.csv('Data/train_leafs_log.csv')
roots_train <- read.csv('Data/train_roots_log.csv')
wood_train<- read.csv('Data/train_wood_log.csv')

leafs_test <- read.csv('Data/test_leafs.csv')
roots_test <- read.csv('Data/test_roots.csv')
wood_test <- read.csv('Data/test_wood.csv')

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)

###################################################################################################

#Lineære modeller af log-log

lm_log_l <- lm(Kgp ~ Sc, data = leafs_log)
lm_log_w <- lm(Kgp ~ Sc, data = wood_log)
lm_log_r <- lm(Kgp ~ Sc, data = roots_log)

#Lineære modeller

lm_l <- lm(Kgp ~ Sc, data = leafs)
lm_w <- lm(Kgp ~ Sc, data = wood)
lm_r <- lm(Kgp ~ Sc, data = roots)

#Log log OLS 

ols_log_l <- function(x) exp(lm_log_l$coefficients[[1]] + lm_log_l$coefficients[[2]]*log(x))
ols_log_w <- function(x) exp(lm_log_w$coefficients[[1]] + lm_log_w$coefficients[[2]]*log(x))
ols_log_r <- function(x) exp(lm_log_r$coefficients[[1]] + lm_log_r$coefficients[[2]]*log(x))

ols_log_adj_l <- function(x) exp(lm_log_l$coefficients[[1]] + 
                                     lm_log_l$coefficients[[2]]*log(x))*exp(var(lm_log_l$residuals)/2)
ols_log_adj_w <- function(x) exp(lm_log_w$coefficients[[1]] +
                                  lm_log_w$coefficients[[2]]*log(x))*exp(var_hat[2]/2)
ols_log_adj_r <- function(x) exp(lm_log_r$coefficients[[1]] + 
                                     lm_log_r$coefficients[[2]]*log(x))*exp(var_hat[3]/2)

#OLS 

ols_l <- function(x) lm_l$coefficients[[1]] + lm_l$coefficients[[2]]*x
ols_w <- function(x) lm_w$coefficients[[1]] + lm_w$coefficients[[2]]*x
ols_r <- function(x) lm_r$coefficients[[1]] + lm_r$coefficients[[2]]*x

#NLR

nlr_l <- function(x) 0.5685498*x^0.7160795
nlr_w <- function(x) 6.9528858*x^0.9841403
nlr_r <- function(x) 0.1206226*x^1.7372279

#Random Forest
                                 

###################################################################################################


#Plot på alt data real scale

cols <- c("darkolivegreen1","darkolivegreen4", "hotpink1", "hotpink4")

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, size = 0.3, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  geom_function(fun = ols_l, aes(col = "OLS"))+
  geom_function(fun = nlr_l, aes(col = "NLR"))+
  geom_function(fun = ols_log_l, aes(col = "OLS log Bias adj."))+
  geom_function(fun = ols_log_adj_l, aes(col = "OLS log"))+
  ylab('Dry mass (kg/plant)')+
  labs(title = "Foliage")+
  scale_colour_manual(values = cols)

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6,  size = 0.3, shape = 21) + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('log(Dry mass (kg/plant))')+
  geom_function(fun = ols_w, aes(col = "OLS"))+
  geom_function(fun = nlr_w, aes(col = "NLR"))+
  geom_function(fun = ols_log_w, aes(col = "OLS log Bias adj."))+
  geom_function(fun = ols_log_adj_w, aes(col = "OLS log"))+
  labs(title = "Wood")+
  scale_colour_manual(values = cols)

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'pink', fill = 'hotpink', alpha = 0.6, size = 0.3, shape = 21) + 
  theme_bw() +
  xlab(bquote(log('Crown area'~m^2/plant))) + 
  ylab('Dry mass (kg/plant)')+
  geom_function(fun = ols_r, aes(col = "OLS"))+
  geom_function(fun = nlr_r, aes(col = "NLR"))+
  geom_function(fun = ols_log_r, aes(col = "OLS log Bias adj."))+
  geom_function(fun = ols_log_adj_r, aes(col = "OLS log"))+
  labs(title = "Roots")+
  scale_colour_manual(values = cols)

