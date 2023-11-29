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

pred_int_w <- full_conformal(wood_train, 0.1, log_ols_alg, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

model <- lm(log(Kgp) ~ log(Sc), wood_train)
f_hat_w <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.8, alpha = 0.8) +
  geom_function(fun = f_hat_w, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Roots

#Der går måske noget galt her

pred_int_r <- full_conformal(roots_train, 0.1, log_ols_alg, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

model <- lm(log(Kgp) ~ log(Sc), roots_train)
f_hat_r <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_roots_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.8, alpha = 0.8) +
  geom_function(fun = f_hat_r, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

cov_r <- mean((pred_int_r$low <= test_roots$Kgp)&(test_roots$Kgp <= pred_int_r$high))

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), "Coverage" = c(cov_l, cov_w, cov_r)))



#--------------------------------------- NLR ----------------------------------- 

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

nlr_alg <- function(data, start_point){
  mod <- optim(par = start_point, fn = MSE_NLR, data = data)
  f_hat <- function(x) mod$par[[1]]*x^mod$par[[2]]
  return(f_hat)
}

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")


#Leafs

nlr_alg_l <- function(data) nlr_alg(data, c(0.61,0.81))

pred_int_l <- full_conformal(leafs_train, 0.1, nlr_alg_l, test_leafs$Sc)


test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((pred_int_l$low <= Kgp)&(Kgp <= pred_int_l$high),"in", "out"))%>%
  add_column(low = pred_int_l$low, high = pred_int_l$high)

f_hat_l <- nlr_alg_l(leafs_train)

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

nlr_alg_w <- function(data) nlr_alg(data, c(5.51,0.73))

pred_int_w <- full_conformal(wood_train, 0.1, nlr_alg_w, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

f_hat_w <- nlr_alg_w(wood_train)

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_w, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Roots

nlr_alg_r <- function(data) nlr_alg(data, c(3.66,0.1))
pred_int_r <- full_conformal(roots_train, 0.1, nlr_alg_r, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

f_hat_r <- nlr_alg_r(roots_train)

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








#------------------------------ Investigation of coverage ----------------------
#------------------------------ For different alphas ---------------------------
alphas <- c(0.01, 0.05, 0.1, 0.2)

set.seed(2)
conf_diff_alphas <- function(data_train, reg_alg, new_data){
  cov_alpha <- c()
  for (i in (1:4)){
    a <- full_conformal(data_train, alpha = alphas[i], reg_alg, new_data$Sc)
    cov_alpha[i] <- mean(a$low <= new_data$Kgp & new_data$Kgp  <= a$high)
  }
  return(cov_alpha)
}


cov_alpha_l <- conf_diff_alphas(leafs_train, log_ols_alg, test_leafs)
cov_alpha_w <- conf_diff_alphas(wood_train, log_ols_alg, test_wood)
cov_alpha_r <- conf_diff_alphas(roots_train, log_ols_alg, test_roots)


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))








#----------------------------Distribution of coverage by resampling-----------------------------------------

rs_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    #Calibration
    
    picked <- sample(seq(1, nrow(train_rs)), 0.8*nrow(train_rs))
    cali_rs <- train_rs[-picked,]
    train_rs <- train_rs[picked,] %>% mutate(Sc = log(Sc), Kgp = log(Kgp))
    
    lm <- lm(Kgp ~ Sc, train_rs)
    var_hat <- sum(lm$residuals^2)/(nrow(train_rs)-1)
    f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
    
    # Heuristic notion of uncertainty
    
    score <- sort(abs(f_hat(cali_rs$Sc) - cali_rs$Kgp))
    quanti <- ceiling((nrow(cali_rs)+1)*(1-0.1))
    q_hat <- score[quanti]
    
    #Prediction interval functions
    
    upper <- function(x) f_hat(x) + q_hat
    lower <- function(x) f_hat(x) - q_hat
    
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                   &upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

set.seed(4)
a <- rs_cov(leafs, 30, 0.1)
b <- rs_cov(wood, 30, 0.1)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

#Ret jævnt fordelt - plot evt. den teoretiske fordeling


#----------------------------Rolling coverage---------------------------------------------

#Leafs

set.seed(7)
a <- pred_int_making_1(train_leafs_log, alpha = 0.1)

bin_size <- 50
roll_cov <- c()

leafs_arr <- test_leafs %>%
  arrange(Sc)

for (i in seq(1,nrow(test_leafs)-bin_size)){
  data_cov <- leafs_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[5]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[6]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_leafs)-bin_size), "Roll_cov" = roll_cov)

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-0.1, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')


#Wood

set.seed(7)
a <- pred_int_making_1(train_wood_log, alpha = 0.1)

bin_size <- 50
roll_cov <- c()

wood_arr <- test_wood %>%
  arrange(Sc)

for (i in seq(1,nrow(test_wood)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[5]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[6]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_wood)-bin_size), "Roll_cov" = roll_cov)


ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 0.9, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_gradient(low = 'blue', high = 'red')






#Forsøg på at bruge pakke#######################################################

#########################---FULL CONFORMAIL---##################################
################################################################################

################################ OLS model #####################################

#library(devtools)
#install_github(repo="ryantibs/conformal", subdir="conformalInference")
library(conformalInference)


################################# Logget data ##################################

#Pred og train funktioner til kommandoen

funs <- lm.funs()

#Full conformal 

conformal_leafs <- conformal.pred(x = train_leafs_log$Sc, 
                                  y = train_leafs_log$Kgp, 
                                  x0 = test_leafs_log[,1],
                                  train.fun = funs$train.fun, 
                                  predict.fun = funs$predict.fun)

conformal_wood <- conformal.pred(train_wood_log$Sc,
                                 train_wood_log$Kgp, 
                                 test_wood_log[,1],
                                 train.fun = funs$train.fun, 
                                 predict.fun = funs$predict.fun)

#Plot leafs - log scale

data_plot_leafs <- tibble("Sc" = test_leafs_log[,1], "Kgp" = test_leafs_log[,2], "Fit" = conformal_leafs$pred[,1], 
                          "Up" = conformal_leafs$up[,1], "Low" = conformal_leafs$lo[,1])

ggplot(data_plot_leafs) + 
  geom_point(aes(x = Sc, y = Fit), color = 'hotpink3', fill = 'hotpink4', shape = 21) + 
  geom_point(aes(x = Sc, y = Low), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Up), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Kgp), color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")

# Plot leafs - real scale (created by simply exponentiating)

data_plot_leafs <- tibble("Sc" = test_leafs[,1], "Kgp" = test_leafs[,2], 
                          "Up" = exp(conformal_leafs$up[,1]), "Low" = exp(conformal_leafs$lo[,1]))

ggplot(data_plot_leafs) + 
  geom_point(aes(x = Sc, y = Low), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Up), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Kgp), color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")

#Plot wood - log scale

data_plot_wood <- tibble("Sc" = test_wood_log[,1], "Kgp" = test_wood_log[,2], "Fit" = conformal_wood$pred[,1], 
                         "Up" = conformal_wood$up[,1], "Low" = conformal_wood$lo[,1])

ggplot(data_plot_wood) + 
  geom_point(aes(x = Sc, y = Fit), color = "Hotpink4") + 
  geom_point(aes(x = Sc, y = Low), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Up), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Kgp), color = "Darkolivegreen4") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")

# Plot wood - real scale (created by simply exponentiating)

data_plot_wood <- tibble("Sc" = test_wood[,1], "Kgp" = test_wood[,2], 
                         "Up" = exp(conformal_wood$up[,1]), "Low" = exp(conformal_wood$lo[,1]))

ggplot(data_plot_wood) + 
  geom_point(aes(x = Sc, y = Low), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Up), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Kgp), color = "Darkolivegreen4") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")


###################### Ægte data ###############################################

funs <- lm.funs()

#Conformal prediction

conformal_leafs <- conformal.pred(x = leafs_train$Sc, 
                                  y = leafs_train$Kgp, 
                                  x0 = test_leafs[,1],
                                  train.fun = funs$train.fun, 
                                  predict.fun = funs$predict.fun)

conformal_wood <- conformal.pred(train_wood_log$Sc, 
                                 train_wood_log$Kgp, 
                                 test_wood_log[,1],
                                 train.fun = funs$train.fun, 
                                 predict.fun = funs$predict.fun)

#Plot leafs

data_plot_leafs <- tibble("Sc" = test_leafs[,1], "Kgp" = test_leafs[,2], "Fit" = conformal_leafs$pred[,1], 
                          "Up" = conformal_leafs$up[,1], "Low" = conformal_leafs$lo[,1])

ggplot(data_plot_leafs) + 
  geom_point(aes(x = Sc, y = Fit), color = 'hotpink3', fill = 'hotpink4', shape = 21) + 
  geom_point(aes(x = Sc, y = Low), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Up), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Kgp), color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")

#Plot wood

data_plot_wood <- tibble("Sc" = test_wood_log[,1], "Kgp" = test_wood_log[,2], "Fit" = conformal_wood$pred[,1], 
                         "Up" = conformal_wood$up[,1], "Low" = conformal_wood$lo[,1])

ggplot(data_plot_wood) + 
  geom_point(aes(x = Sc, y = Fit), color = "Hotpink4") + 
  geom_point(aes(x = Sc, y = Low), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Up), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Kgp), color = "Darkolivegreen4") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")


#################################################
# Af en eller anden grund vil den ikke lade mig definere de her funktioner som jeg vil
# Den ender med at tage log af log :(((((((( hjælp Dina


train_function <- function(x,y, out = NULL) {
  if (any(is.na(log(y)))){
    stop("log(y) is NA")
  }
  if (any(is.na(log(x)))){
    stop("log(y) is NA")
  }
  model <- lm(Kgp ~ Sc, data = data.frame(Sc = log(x), Kgp = log(y)))
  coefs <- model$coefficients
  sigma2 <- var(model$residuals)
  return(list(coefs = coefs, sigma2 = sigma2))
}

predict_function <- function(out, newx) {
  exp(out$coefs[[1]])*newx^out$coefs[[2]]*exp(out$sigma2/2)
} 

train_function <- function (x, y, out = NULL, intercept = TRUE) {
  n = nrow(x)
  p = ncol(x)
  v = rep(1, p)
  x = cbind(rep(1, n), log(x))
  v = c(0, v)
  chol.R = vector(mode = "list", length = 1)
  chol.R[[1]] = chol(crossprod(x))
  beta = matrix(0, p + intercept, 1)
  beta[, 1] = chol.solve(chol.R[[1]], t(x) %*% y)
  return(list(beta = beta, chol.R = chol.R))
}

predict_function <- function (out, newx, intercept = TRUE, lambda = 0) {
  pred = exp(out$beta[1])*newx^(out$beta[2])
  return(pred)
}

conformal_leafs <- conformal.pred(x = leafs_train$Sc, y = leafs_train$Kgp, x0 = test_leafs[,1], 
                                  train_function, predict_function)

data_plot_leafs <- tibble("Sc" = test_leafs[,1], "Kgp" = test_leafs[,2], "Fit" = conformal_leafs$pred[,1], 
                          "Up" = conformal_leafs$up[,1], "Low" = conformal_leafs$lo[,1])

ggplot(data_plot_leafs) + 
  geom_point(aes(x = Sc, y = Low), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Up), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Kgp), color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")


#NLR model

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

starting_point_leafs <- c(0.610, 0.807)
starting_point_wood <- c(5.51, 0.730)

train_function <- function(x,y, out) {
  optim(par = starting_point_leafs, fn = MSE_NLR, data = tibble(Sc = x,Kgp = y))
}
predict_function <- function(out, newx) {
  exp(out$par[[1]])*newx^out$par[[2]]
} 


conformal_leafs <- conformal.pred(leafs_train$Sc, leafs_train$Kgp, test_leafs[1,1], train_function, predict_function)
conformal_wood <- conformal.pred(wood_train$Sc, wood_train$Kgp, test_wood[,1], train_function, predict_function)

data_plot_leafs <- tibble("Sc" = test_leafs[,1], "Kgp" = test_leafs[,2], "Fit" = conformal_leafs$pred[,1], 
                          "Up" = conformal_leafs$up[,1], "Low" = conformal_leafs$lo[,1])

data_plot_wood <- tibble("Sc" = test_wood[,1], "Kgp" = test_wood[,2], "Fit" = conformal_wood$pred[,1], 
                         "Up" = conformal_wood$up[,1], "Low" = conformal_wood$lo[,1])

ggplot(data_plot_leafs) + 
  geom_point(aes(x = Sc, y = Fit), color = 'hotpink3', fill = 'hotpink4', shape = 21) + 
  geom_point(aes(x = Sc, y = Low), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Up), color = 'hotpink', fill = 'hotpink2', shape = 21) +
  geom_point(aes(x = Sc, y = Kgp), color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.6, shape = 21) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")

ggplot(data_plot_wood) + 
  geom_point(aes(x = Sc, y = Fit), color = "Hotpink4") + 
  geom_point(aes(x = Sc, y = Low), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Up), color = "Hotpink") +
  geom_point(aes(x = Sc, y = Kgp), color = "Darkolivegreen4") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")






#mean(test$lo <= test_leafs_log$Kgp &  test_leafs_log$Kgp  <= test$up)
#mean(test$lo- test$up)
#max(test$lo- test$up)
#min(test$lo- test$up)
#mean(test$pred)

