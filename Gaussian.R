leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

library(tidyverse)
library(readr)
library(infer)


# The linear model

lm <- lm(Kgp ~ Sc, train_leafs_log)

sd_hat <- sqrt(sum(lm$residuals^2)/(nrow(train_leafs_log)-1))

f_hat <- function(x) lm$coefficients[[2]]*x + lm$coefficients[[1]] 

alpha <- 0.1

upper <- function(x) {
  f_hat(x) - qt(alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sd_hat
}

lower <- function(x) {
  f_hat(x) - qt(1-alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sd_hat
}

#Plot with prediction intervals

ggplot(test_leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat, colour = "red") +
  geom_function(fun = upper, colour = "blue") +
  geom_function(fun = lower, colour = "blue") +
  labs(title = "Kgp as function of Sc with Standard Gaussian prediction intervals")

f_hat_exp <- function(x) exp(lm$coefficients[[2]]*x + lm$coefficients[[1]]) 

alpha <- 0.1

upper_exp <- function(x) {
  exp(f_hat(x) - qt(alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sd_hat)
}

lower_exp <- function(x) {
  exp(f_hat(x) - qt(1-alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sd_hat)
}

ggplot(test_leafs_log, aes(x = Sc, y = exp(Kgp))) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_exp, colour = "red") +
  geom_function(fun = upper_exp, colour = "blue") +
  geom_function(fun = lower_exp, colour = "blue") +
  labs(title = "Kgp as function of Sc with Standard Gaussian prediction intervals")


# Coverage

mean(lower(leafs_log_test$Sc) <= leafs_log_test$Kgp &upper(leafs_log_test$Sc) >= leafs_log_test$Kgp)

#Cv on coverage: 

cv_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    n_cv <- nrow(data[group != i, ])
    lm_cv <- lm(Kgp ~ Sc, data = data[group != i, ])
    sd_hat <- sqrt(sum(lm_cv$residuals^2)/(n_cv-1))
    f_hat <- function(x) lm_cv$coefficients[[2]]*x + lm_cv$coefficients[[1]] 
    
    #Quantiles
    upper <- function(x) {
      f_hat(x) - qt(alpha/2, n_cv-1)*sqrt(x^2/sum(data[group != i, ]$Sc^2)+1)*sd_hat
    }
    
    lower <- function(x) {
      f_hat(x) - qt(1-alpha/2, n_cv-1)*sqrt(x^2/sum(data[group != i, ]$Sc^2)+1)*sd_hat
    }
    
    #Definere
    cov[i] <- mean(lower(data[group = i, ]$Sc) <= data[group = i, ]$Kgp 
                   &upper(data[group = i, ]$Sc) >= data[group = i, ]$Kgp)
  }
  return(tibble("Coverage" = cov))
}

cv_cov(leafs_log, 10, 0.1)

#transformeres???