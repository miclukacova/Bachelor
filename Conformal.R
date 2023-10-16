library(tidyverse)
library(ggplot2)
library(dplyr)

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

train_leafs_log <- read.csv('Data/train_leafs_log.csv')
train_roots_log <- read.csv('Data/train_roots_log.csv')
train_wood_log<- read.csv('Data/train_wood_log.csv')

test_leafs_log <- read.csv('Data/test_leafs_log.csv')
test_roots_log <- read.csv('Data/test_roots_log.csv')
test_wood_log<- read.csv('Data/test_wood_log.csv')

leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

test_leafs <- read.csv('Data/test_leafs.csv')
test_roots <- read.csv('Data/test_roots.csv')
test_wood <- read.csv('Data/test_wood.csv')

####################################################################
#Log-log ols--------------------------------------------------------
####################################################################

pred_int_making_1 <- function(train_data) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  cali <- cali %>% mutate(Sc = exp(Sc), Kgp = exp(Kgp))

  #Linear model
  lm <- lm(Kgp ~ Sc, train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  var_y_hat <- function(x) exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1)

  # Heuristic notion of uncertainty
  mse <- sort((f_hat(cali$Sc) - cali$Kgp)^2)
  mse_adj <- sort((f_hat_adj(cali$Sc) - cali$Kgp)^2)
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- mse[quanti]
  q_hat_adj <- mse_adj[quanti]
  
  #Prediction interval functions
  
  upper_1 <- function(x) f_hat(x) + sqrt(q_hat)
  lower_1 <- function(x) f_hat(x) - sqrt(q_hat)
  
  upper_2 <- function(x) f_hat_adj(x) + sqrt(q_hat_adj)
  lower_2 <- function(x) f_hat_adj(x) - sqrt(q_hat_adj)
  
  return(list(f_hat_adj, f_hat, lower_1, upper_1, lower_2, upper_2))
}
pred_int_making_2 <- function(train_data) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  cali <- cali %>% mutate(Sc = exp(Sc), Kgp = exp(Kgp))
  
  #Linear model
  lm <- lm(Kgp ~ Sc, train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  var_y_hat <- function(x) exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1)
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp)/var_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  
  #Prediction intervals
  upper <- function(x) f_hat(x) + q_hat*var_y_hat(x)
  lower <- function(x) f_hat(x) - q_hat*var_y_hat(x)
  
  return(list(f_hat, lower, upper))
}

a <- pred_int_making_1(train_leafs_log)
b <- pred_int_making_1(train_wood_log)
#c <- pred_int_making(train_roots_log)
c <- pred_int_making_2(train_leafs_log)
d <- pred_int_making_2(train_wood_log)

#Der er ikke nok data punkter i roots --  vi får NA's

#Coverage på test set

mean(a[[3]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[4]](test_leafs$Sc))
mean(b[[3]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[4]](test_wood$Sc))
mean(a[[5]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[6]](test_leafs$Sc))
mean(b[[5]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[6]](test_wood$Sc))
mean(c[[2]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= c[[3]](test_leafs$Sc))
mean(d[[2]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= d[[3]](test_wood$Sc))

ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "red") +
  geom_function(fun = a[[5]], colour = "blue") +
  geom_function(fun = a[[6]], colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")


#De her to er efter metoderne i noten, men de bliver bare virkelig bred

ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = c[[1]], colour = "red") +
  geom_function(fun = c[[2]], colour = "blue") +
  geom_function(fun = c[[3]], colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")

ggplot(test_wood, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = d[[1]], colour = "red") +
  geom_function(fun = d[[2]], colour = "blue") +
  geom_function(fun = d[[3]], colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")


#Distribution of coverage by resampling

cv_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    if (i != k){
      data_cv <- data[group != i & group != (i+1), ]
      n_cv <- nrow(data_cv)
      n_cali <- nrow(data[group == i, ])
      
      lm_cv <- lm(Kgp ~ Sc, data = data_cv)
      f_hat <- function(x) lm_cv$coefficients[[2]]*x + lm_cv$coefficients[[1]]
      
      # Heuristic notion of uncertainty
      mse <- sort((f_hat(data[group == i, ]$Sc) - data[group == i, ]$Kgp)^2)
      quanti <- ceiling((n_cali+1)*(1-alpha))
      q_hat <- mse[quanti]
      print(q_hat)
      
      # Coverage
      up_cv <- f_hat(data[group == (i+1), ]$Sc) + sqrt(q_hat)
      low_cv <- f_hat(data[group == (i+1), ]$Sc) - sqrt(q_hat)
      
      cov[i] <- mean(low_cv <= data[group == (i+1), ]$Kgp & data[group == (i+1), ]$Kgp  <= up_cv)
    }
    else {
      data_cv <- data[group != k & group != 1, ]
      n_cv <- nrow(data_cv)
      n_cali <- nrow(data[group == k, ])
      
      lm_cv <- lm(Kgp ~ Sc, data = data_cv)
      f_hat <- function(x) lm_cv$coefficients[[2]]*x + lm_cv$coefficients[[1]]
      
      # Heuristic notion of uncertainty
      
      mse <- sort((f_hat(data[group == k, ]$Sc) - data[group == k, ]$Kgp)^2)
      quanti <- ceiling((n_cali+1)*(1-alpha))
      q_hat <- mse[quanti]
      
      # Conformal prediction interval
      up_cv <- f_hat(data[group == 1, ]$Sc) + sqrt(q_hat)
      low_cv <- f_hat(data[group == 1, ]$Sc) - sqrt(q_hat)
      
      # Coverage
      cov[i] <- mean(low_cv <= data[group == 1, ]$Kgp & data[group == 1, ]$Kgp  <= up_cv)
    }
  }
  return(tibble("Coverage" = cov))
}

cv_cov(leafs_log,10,0.1)


# Conformal prediction på den tilbagetransformerede model

lm_leafs_log <- lm(Kgp ~ Sc, data = train_leafs_log)
var_hat <- var(lm_leafs_log$residuals)
f_hat_leafs <- function(x) exp(lm_leafs_log$coefficients[[1]] + lm_leafs_log$coefficients[[2]]*x)

mse <- sort((f_hat_leafs(cali_leafs_log$Sc) - exp(cali_leafs_log$Kgp))^2)
quanti <- ceiling((nrow(cali_leafs_log)+1)*(1-0.1))
q_hat <- mse[quanti]

upper <- function(x) f_hat_leafs(exp(x)) + sqrt(q_hat)
lower <- function(x) f_hat_leafs(exp(x)) - sqrt(q_hat)

ggplot(test_leafs_log, aes(x = Sc, y = exp(Kgp))) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_leafs, colour = "red") +
  geom_function(fun = upper_1, colour = "blue") +
  geom_function(fun = lower_1, colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")

up <- f_hat_leafs(test_leafs_log$Sc) + sqrt(q_hat)
low <- f_hat_leafs(test_leafs_log$Sc) - sqrt(q_hat)
mean(low <= exp(test_leafs_log$Kgp) & exp(test_leafs_log$Kgp)  <= up)

#Adjusted

f_hat_leafs_adj <- function(x) f_hat_leafs(x)*exp(var_hat[1]/2)

mse <- sort((f_hat_leafs_adj(cali_leafs_log$Sc) - exp(cali_leafs_log$Kgp))^2)
quanti <- ceiling((nrow(cali_leafs_log)+1)*(1-0.1))
q_hat <- mse[quanti]

# Conformal prediction interval

upper_1 <- function(x) f_hat_leafs_adj(x) + sqrt(q_hat)
lower_1 <- function(x) f_hat_leafs_adj(x) - sqrt(q_hat)

ggplot(test_leafs_log, aes(x = Sc, y = exp(Kgp))) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_leafs_adj, colour = "red") +
  geom_function(fun = upper_1, colour = "blue") +
  geom_function(fun = lower_1, colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")

#Coverage

up <- f_hat_leafs_adj(test_leafs_log$Sc) + sqrt(q_hat)
low <- f_hat_leafs_adj(test_leafs_log$Sc) - sqrt(q_hat)
mean(low <= exp(test_leafs_log$Kgp) & exp(test_leafs_log$Kgp)  <= up)


#cv

cv_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    if (i != k){
      data_cv <- data[group != i & group != (i+1), ]
      n_cv <- nrow(data_cv)
      n_cali <- nrow(data[group == i, ])
      
      lm_cv <- lm(Kgp ~ Sc, data = data_cv)
      f_hat <- function(x) lm_cv$coefficients[[2]]*x + lm_cv$coefficients[[1]]
      
      # Heuristic notion of uncertainty
      mse <- sort((f_hat(data[group == i, ]$Sc) - data[group == i, ]$Kgp)^2)
      quanti <- ceiling((n_cali+1)*(1-alpha))
      q_hat <- mse[quanti]
      print(q_hat)
      
      # Coverage
      up_cv <- f_hat(data[group == (i+1), ]$Sc) + sqrt(q_hat)
      low_cv <- f_hat(data[group == (i+1), ]$Sc) - sqrt(q_hat)
      
      cov[i] <- mean(low_cv <= data[group == (i+1), ]$Kgp & data[group == (i+1), ]$Kgp  <= up_cv)
    }
    else {
      data_cv <- data[group != k & group != 1, ]
      n_cv <- nrow(data_cv)
      n_cali <- nrow(data[group == k, ])
      
      lm_cv <- lm(Kgp ~ Sc, data = data_cv)
      f_hat <- function(x) lm_cv$coefficients[[2]]*x + lm_cv$coefficients[[1]]
      
      # Heuristic notion of uncertainty
      
      mse <- sort((f_hat(data[group == k, ]$Sc) - data[group == k, ]$Kgp)^2)
      quanti <- ceiling((n_cali+1)*(1-alpha))
      q_hat <- mse[quanti]
      
      # Conformal prediction interval
      up_cv <- f_hat(data[group == 1, ]$Sc) + sqrt(q_hat)
      low_cv <- f_hat(data[group == 1, ]$Sc) - sqrt(q_hat)
      
      # Coverage
      cov[i] <- mean(low_cv <= data[group == 1, ]$Kgp & data[group == 1, ]$Kgp  <= up_cv)
    }
  }
  return(tibble("Coverage" = cov))
}

cv_cov(leafs_log,10,0.1)

