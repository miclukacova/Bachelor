library(readODS)
library(tidyverse)
library(ggplot2)

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

nrow(leafs_log)

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

nrow(train_leafs_log)
nrow(cali_leafs_log)
nrow(test_leafs_log)

# The linear model

lm <- lm(Bfkg ~ Sc, train_leafs_log)

sigma_hat <- sum(lm$residuals^2)/(nrow(train_leafs_log)-1)

f_hat <- function(x) lm$coefficients[[2]]*x + lm$coefficients[[1]] 

ggplot(leafs_log, aes(x = Sc, y = Bfkg)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Bfkg)')+
  geom_function(fun = f_hat, colour = "red") +
  labs(title = "Bfkg as function of Sc")

# Heuristic notion of uncertainty

s <- sort(abs(f_hat(cali_leafs_log$Sc) - cali_leafs_log$Bfkg))/(sigma_hat^(1/2))

quantile <- ceiling((nrow(cali_leafs_log)+1)*(1-0.1))

q_hat <- s[quantile]

# Conformal prediction interval

upper <- function(x) f_hat(x) + q_hat*sigma_hat^(1/2)
lower <- function(x) f_hat(x) - q_hat*sigma_hat^(1/2)


ggplot(test_leafs_log, aes(x = Sc, y = Bfkg)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Bfkg)')+
  geom_function(fun = f_hat, colour = "red") +
  geom_function(fun = upper, colour = "blue") +
  geom_function(fun = lower, colour = "blue") +
  labs(title = "Bfkg as function of Sc with conformal prediction intervals")





