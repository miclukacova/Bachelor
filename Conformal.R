library(tidyverse)
library(ggplot2)

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

#On test data log-scale

pred_int_making <- function(train_data, test_data) {
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
  var_y_hat <- function(x) exp(2*f_hat_adj(x)+var_hat)*(exp(var_hat)-1)

  # Heuristic notion of uncertainty
  mse <- sort((f_hat(cali$Sc) - cali$Kgp)^2)
  mse_adj <- sort((f_hat_adj(cali$Sc) - cali$Kgp)^2)
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- mse[quanti]
  q_hat_adj <- mse_adj[quanti]

  #Prediction interval
  upper_adj <- function(x) f_hat_adj(x) + sqrt(q_hat_adj)*var_y_hat(x)^(1/2)
  lower_adj <- function(x) f_hat_adj(x) - sqrt(q_hat_adj)*var_y_hat(x)^(1/2)

  #hmmm
  upper <- function(x) f_hat(x) + sqrt(q_hat)*exp(var_hat^(1/2))
  lower <- function(x) f_hat(x) - sqrt(q_hat)*exp(var_hat^(1/2))

  #En anden mulighed
  
  upper_1 <- function(x) f_hat(x) + sqrt(q_hat)
  lower_1 <- function(x) f_hat(x) - sqrt(q_hat)
  
  upper_2 <- function(x) f_hat_adj(x) + sqrt(q_hat_adj)
  lower_2 <- function(x) f_hat_adj(x) - sqrt(q_hata_adj)
  
  return(list(f_hat_adj, f_hat, upper_adj, lower_adj, upper, lower, upper_1, lower_1, upper_2, lower_2))
}

#Test and calibration
picked <- sample(seq(1, nrow(leafs_log_train)), 0.8*nrow(leafs_log_train))
train <- leafs_log_train[picked,]
cali <- leafs_log_train[-picked,]
cali <- cali %>% mutate(Sc = exp(Sc), Kgp = exp(Kgp))

#Linear model
lm <- lm(Kgp ~ Sc, train)
var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
var_y_hat <- function(x) exp(2*f_hat_adj(x)+var_hat)*(exp(var_hat)-1)

# Heuristic notion of uncertainty
mse <- sort((f_hat(cali$Sc) - cali$Kgp)^2)
mse_adj <- sort((f_hat_adj(cali$Sc) - cali$Kgp)^2)
quanti <- ceiling((nrow(cali)+1)*(1-0.1))
q_hat <- mse[quanti]
q_hat_adj <- mse_adj[quanti]

#Prediction interval
upper_adj <- function(x) f_hat_adj(x) + q_hat_adj*var_y_hat(x)^(1/2)
lower_adj <- function(x) f_hat_adj(x) - q_hat_adj*var_y_hat(x)^(1/2)

#hmmm
upper <- function(x) f_hat(x) + q_hat*exp(var_hat)^(1/2)
lower <- function(x) f_hat(x) - q_hat*exp(var_hat)^(1/2)

#En anden mulighed

upper_1 <- function(x) f_hat(x) + sqrt(q_hat)
lower_1 <- function(x) f_hat(x) - sqrt(q_hat)

upper_2 <- function(x) f_hat_adj(x) + sqrt(q_hat_adj)
lower_2 <- function(x) f_hat_adj(x) - sqrt(q_hat_adj)

a <- pred_int_making(train_leafs_log, test_leafs_log)

ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "red") +
  geom_function(fun = a[[1]], colour = "blue") +
  geom_function(fun = a[[2]], colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")


ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_adj, colour = "red") +
  geom_function(fun = upper, colour = "blue") +
  geom_function(fun = lower, colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")

#Coverage

up1 <- f_hat(test_leafs_log$Sc) + sqrt(q_hat)
low1 <- f_hat(test_leafs_log$Sc) - sqrt(q_hat)
up <- f_hat(test_leafs_log$Sc) + q_hat*sigma_hat^(1/2)
low <- f_hat(test_leafs_log$Sc) - q_hat*sigma_hat^(1/2)

mean(low1 <= test_leafs_log$Kgp & test_leafs_log$Kgp  <= up1)
mean(low <= test_leafs_log$Kgp & test_leafs_log$Kgp  <= up)

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

