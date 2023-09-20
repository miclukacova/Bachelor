library(readODS)
library(tidyverse)
library(ggplot2)

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

# Split Data 1 
sample_size1 = floor(0.7*nrow(leafs_log))
set.seed(777)

# Randomly split data
picked = sample(seq_len(nrow(leafs_log)),size = sample_size1)
train_leafs_log = leafs_log[picked,]
cali_leafs_log = leafs_log[-picked,]

# Split Data 2

sample_size2 = floor(0.5*nrow(cali_leafs_log))
set.seed(777)

# Randomly split data 2
picked = sample(seq_len(nrow(cali_leafs_log)),size = sample_size2)
test_leafs_log = cali_leafs_log[picked,]
cali_leafs_log = cali_leafs_log[-picked,]

# The linear model

lm <- lm(Kgp ~ Sc, train_leafs_log)
sd_hat <- sum(lm$residuals^2)/(nrow(train_leafs_log)-1)
f_hat <- function(x) lm$coefficients[[2]]*x + lm$coefficients[[1]]

# Heuristic notion of uncertainty

mse <- sort((f_hat(cali_leafs_log$Sc) - cali_leafs_log$Kgp)^2)

quanti <- ceiling((nrow(cali_leafs_log)+1)*(1-0.1))

q_hat <- mse[quanti]

# Conformal prediction interval

# Som i noten

upper <- function(x) f_hat(x) + q_hat*sigma_hat^(1/2)
lower <- function(x) f_hat(x) - q_hat*sigma_hat^(1/2)

ggplot(test_leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat, colour = "red") +
  geom_function(fun = upper, colour = "blue") +
  geom_function(fun = lower, colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")

#Som jeg tænkter (næsten det samme?????)

upper_1 <- function(x) f_hat(x) + sqrt(q_hat)
lower_1 <- function(x) f_hat(x) - sqrt(q_hat)

ggplot(test_leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat, colour = "red") +
  geom_function(fun = upper_1, colour = "blue") +
  geom_function(fun = lower_1, colour = "blue") +
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




