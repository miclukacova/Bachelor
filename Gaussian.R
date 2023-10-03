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

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
###################################################################################################

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

#QQ-plot

residuals_leafs <- data.frame(residual = lm_leafs_log$residuals/sd(lm_leafs_log$residuals))
residuals_wood <- data.frame(residual = lm_wood_log$residuals/sd(lm_wood_log$residuals))
residuals_roots <- data.frame(residual = lm_roots_log$residuals/sd(lm_roots_log$residuals))

residuals_leafs %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink") +
  stat_function(fun = dnorm, color = "darkolivegreen")+
  labs(title = "Foliage")

residuals_wood %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink") +
  stat_function(fun = dnorm, color = "darkolivegreen")+
  labs(title = "Wood")

residuals_roots %>%
  ggplot() +
  geom_histogram(aes(x = residual, y = ..density..), color = "white", fill = "darkolivegreen3") +
  geom_vline(xintercept = 0, color = "hotpink") +
  stat_function(fun = dnorm, color = "darkolivegreen")+
  labs(title = "Roots")

ggplot(data = residuals_leafs, aes(sample = residual)) +
           stat_qq() + stat_qq_line(color = "hotpink") + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen")+
  labs(title = "Foliage")

ggplot(data = residuals_wood, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink") + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen")+
  labs(title = "Wood")

ggplot(data = residuals_roots, aes(sample = residual)) +
  stat_qq() + stat_qq_line(color = "hotpink") + 
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen")+
  labs(title = "Roots")

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