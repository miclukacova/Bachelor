library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
library(randomForest)
library(quantregForest)

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

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

#------------------------------- Full Conformal Function -----------------------

full_conformal <- function(data, alpha = 0.1, reg_alg, new_data){
  n <- nrow(data)
  low <- c()
  high <- c()
  y_trial <- seq(from = 0.000000001, to = max(data$Kgp)*3, length.out = 1000)
  for (x in new_data){
    pi_y <- c()
    for (y in y_trial){
      data_conf <- data %>% add_row(Sc = x, Kgp = y)
      f_hat <- reg_alg(data_conf)
      res_conf <- abs(data_conf$Kgp - f_hat(data_conf$Sc))
      res_n1 <- res_conf[n+1] 
      pi_y <- append(pi_y, (1 + sum((res_conf[1:n] <= res_n1)))/(n+1))
    }
    c_conf <-y_trial[(n+1)*pi_y <= ceiling((1-alpha)*(n+1))]
    low <- append(low, min(c_conf))
    high <- append(high, max(c_conf))
  }
  return(tibble(x = new_data, low = low, high = high))
}

#------------------------------- OLS -----------------------------------

ols_alg <- function(data){
  model <- lm(Kgp ~ Sc, data)
  f_hat <- function(x) model$coef[[1]] + model$coef[[2]]*x
  return(f_hat)
}

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

#Leafs

pred_int_l <- full_conformal(leafs_train, 0.1, ols_alg, test_leafs$Sc)


test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((pred_int_l$low <= Kgp)&(Kgp <= pred_int_l$high),"in", "out"))%>%
  add_column(low = pred_int_l$low, high = pred_int_l$high)

ols_l <- lm(Kgp ~ Sc, leafs_train)
f_hat_l <- function(x) ols_l$coef[[1]] + ols_l$coef[[2]]*x

ggplot(test_leafs_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_l, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

cov_l <- mean((pred_int_l$low <= test_leafs$Kgp)&(test_leafs$Kgp <= pred_int_l$high))

#Wood

pred_int_w <- full_conformal(wood_train, 0.1, ols_alg, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

ols_w <- lm(Kgp ~ Sc, wood_train)
f_hat_w <- function(x) ols_w$coef[[1]] + ols_w$coef[[2]]*x

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_w, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Wood

pred_int_r <- full_conformal(roots_train, 0.1, ols_alg, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

ols_r <- lm(Kgp ~ Sc, roots_train)
f_hat_r <- function(x) ols_r$coef[[1]] + ols_r$coef[[2]]*x

ggplot(test_roots_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_r, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

cov_r <- mean((pred_int_r$low <= test_roots$Kgp)&(test_roots$Kgp <= pred_int_r$high))

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), "Coverage" = c(cov_l, cov_w, cov_r)))

#--------------------------- Log-log OLS (bias adj.) ---------------------------

log_ols_alg <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  return(f_hat)
}

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")


#Leafs

pred_int_l <- full_conformal(leafs_train, 0.1, log_ols_alg, test_leafs$Sc)


test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((pred_int_l$low <= Kgp)&(Kgp <= pred_int_l$high),"in", "out"))%>%
  add_column(low = pred_int_l$low, high = pred_int_l$high)

model <- lm(log(Kgp) ~ log(Sc), leafs_train)
f_hat_l <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_leafs_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_l, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

cov_l <- mean((pred_int_l$low <= test_leafs$Kgp)&(test_leafs$Kgp <= pred_int_l$high))

#Wood

pred_int_w <- full_conformal(wood_train, 0.1, ols_log_alg, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

model <- lm(log(Kgp) ~ log(Sc), wood_train)
f_hat_w <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  geom_point(aes(x = Sc, y = high, color = "hotpink")) + 
  geom_point(aes(x = Sc, y = low, color = "hotpink")) +
  geom_function(fun = f_hat_w, colour = "hotpink4") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Wood

pred_int_r <- full_conformal(roots_train, 0.1, ols_log_alg, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

model <- lm(log(Kgp) ~ log(Sc), roots_train)
f_hat_r <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_roots_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  geom_point(aes(x = Sc, y = high, color = "hotpink")) + 
  geom_point(aes(x = Sc, y = low, color = "hotpink")) +
  geom_function(fun = f_hat_r, colour = "hotpink4") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "roots")+
  scale_color_manual(values = color)

cov_r <- mean((pred_int_r$low <= test_roots$Kgp)&(test_roots$Kgp <= pred_int_r$high))

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), "Coverage" = c(cov_l, cov_w, cov_r)))


