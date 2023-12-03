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

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')


#Log-log ols--------------------------------------------------------

set.seed(5)

#Score funktion Absolute error
pred_int_log_ols_conf_adj <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  
  # Heuristic notion of uncertainty
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat_adj <- score_adj[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat_adj(x) + q_hat_adj
  lower <- function(x) f_hat_adj(x) - q_hat_adj
  
  return(list(f_hat_adj, upper,lower))
}
pred_int_log_ols_conf <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper, lower))
}

#Score funktion Absolute error / sd hat(Y) (VIRKER IKKE SÅ GODT)
pred_int_log_ols_conf_2_adj <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
  
  # Heuristic notion of uncertainty
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat_adj <- score_adj[quanti]
  
  #Prediction intervals
  
  upper_adj <- function(x) f_hat_adj(x) + q_hat_adj*sd_y_hat(x)
  lower_adj <- function(x) f_hat_adj(x) - q_hat_adj*sd_y_hat(x)
  
  return(list(f_hat_adj, upper_adj, lower_adj))
}
pred_int_log_ols_conf_2 <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction intervals
  upper <- function(x) f_hat(x) + q_hat*sd_y_hat(x)
  lower <- function(x) f_hat(x) - q_hat*sd_y_hat(x)
  
  return(list(f_hat, upper, lower))
}


#Leafs

#Abs error

loo_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj[[1]], "Leafs")
plot_maker(loo[[1]], "Leafs")

#Abs error /sd

loo2_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
loo2 <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2) 


plot_maker(loo2_adj[[1]], "Leafs")
plot_maker(loo2[[1]], "Leafs")

#Wood

#Abs error

loo_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_adj) 
loo_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf)

plot_maker(loo_adj_w[[1]], "Wood")
plot_maker(loo_w[[1]], "Wood")

#Abs error /sd

loo2_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
loo2_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2) 


plot_maker(loo2_adj_w[[1]], "Wood")
plot_maker(loo2_w[[1]], "Wood")



##Roots
#
##Abs error
#
#loo_adj_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_adj) 
#loo_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf)
#
#plot_maker(loo_adj_r[[1]], "roots")
#plot_maker(loo_r[[1]], "roots")
#
##Abs error /sd
#
#loo2_adj_w <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
#loo2_w <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2) 
#
#
#plot_maker(loo2_adj_w[[1]], "roots")
#plot_maker(loo2_w[[1]], "roots")
#

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_log_ols_conf_adj)
cov_alpha_w <- diff_alohas(wood, pred_int_log_ols_conf_adj)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_adj)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_adj)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_log_ols_conf_adj)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs", 0.2)
rs_plot_maker(b, "Wood", 0.2)
rs_plot_maker(c, "Roots", 0.2)


#----------------------------Rolling coverage---------------------------------------------

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_adj)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_adj)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_adj)

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)

####################################################################

#NLR----------------------------------------------------------------------------

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

nlr_alg <- function(data, start_point){
  mod <- optim(par = start_point, fn = MSE_NLR, data = data)
  f_hat <- function(x) mod$par[[1]]*x^mod$par[[2]]
  return(f_hat)
}

pred_int_nlr <- function(data, alpha = 0.2, starting_points) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  f_hat <- nlr_alg(data, starting_points)
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper,lower))
}

pred_int_nlr_l <- function(data, alpha = 0.2) pred_int_nlr(data, alpha = 0.2, c(0.61,0.81))
pred_int_nlr_w <- function(data, alpha = 0.2) pred_int_nlr(data, alpha = 0.2, c(5.51,0.73))
pred_int_nlr_r <- function(data, alpha = 0.2) pred_int_nl(data, alpha = 0.2, c(3.66,0.1))

#----------------------------Pred intervaller###################################

loo_pred_int <- function(data, alpha = 0.2, pred_int) {
  low <- c()
  high <- c()
  fitted <- c()
  for (i in (1:nrow(data))){
    if (i == nrow(data)/2){
      print("halfway!")
    }
    pred <- pred_int(data = data[-i,], alpha = alpha)
    low[i] <- pred[[3]](data[i,]$Sc)
    high[i] <- pred[[2]](data[i,]$Sc)
    fitted[i] <- pred[[1]](data[i,]$Sc)
  }
  pred <- tibble("Low" = low, "High" = high, "Fitted" = fitted, "Sc" = data$Sc, "Kgp" = data$Kgp)
  cov <- mean(low <= data$Kgp 
              & high >= data$Kgp)
  return(list(pred, cov))
}

set.seed(6)

loo_l <- loo_pred_int(leafs, alpha = 0.2, pred_int_nlr_l) 
loo_w <- loo_pred_int(wood, alpha = 0.2, pred_int_nlr_w)
loo_r <- loo_pred_int(roots, alpha = 0.2, pred_int_nlr_w)


plot_maker(loo_l[[1]], "Leafs")
plot_maker(loo_w[[1]], "Woods")
plot_maker(loo_r[[1]], "Roots")

#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_nlr_l)
cov_alpha_w <- diff_alohas(wood, pred_int_nlr_w)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_w)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_r)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs", 0.2)
rs_plot_maker(b, "Wood", 0.2)
rs_plot_maker(c, "Roots", 0.2)


#Regression forest--------------------------------------------------------------

#----------------------------Pred intervaller-----------------------------------
pred_int_making <- function(train_dat, node_size = 70, alpha = 0.1) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_dat)), 0.8*nrow(train_dat))
  train <- train_dat[picked,]
  cali <- train_dat[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  qrf_func <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
    
  # Heuristic notion of uncertainty
  score <- sort(abs(qrf_func(cali) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) qrf_func(x) + q_hat
  lower <- function(x) qrf_func(x) - q_hat
  
  return(list(lower, upper, qrf_func))
}

set.seed(4)
a <- pred_int_making(leafs_train, node_size = 100)
b <- pred_int_making(wood_train, node_size = 70)
z1 <- mean(a[[1]](test_leafs) <= test_leafs$Kgp &
             test_leafs$Kgp  <= a[[2]](test_leafs))
z2 <- mean(b[[1]](test_wood) <= test_wood$Kgp &
             test_wood$Kgp  <= b[[2]](test_wood))

xtable(tibble(" " = c("Leafs", "Wood"), "Covergae" = c(z1, z2)), type = latex)

test_leafs_plot <- test_leafs %>%
  mutate(Indicator = if_else((a[[1]](test_leafs) <= Kgp) &
                               (Kgp <= a[[2]](test_leafs)),"in", "out"))

test_wood_plot <- test_wood %>%
  mutate(Indicator = if_else((b[[1]](test_wood) <= Kgp)&
                               (Kgp <= b[[2]](test_wood)),"in", "out"))


color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "hotpink4") +
  geom_function(fun = a[[2]], colour = "hotpink4") +
  geom_function(fun = a[[3]], colour = "hotpink") +
  scale_color_manual(values = color) +
  labs(title = "Leafs")

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = b[[1]], colour = "hotpink4") +
  geom_function(fun = b[[2]], colour = "hotpink4") +
  geom_function(fun = b[[3]], colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)


#----------------------------Distribution of coverage by resampling-------------

rs_cov <- function(data, k, alpha, node_size = 100) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked <- sample(n,size = sample_size)
    train_data = data[picked,]
    test_rs = data[-picked,]
    
    # Train and cali
    picked <- sample(seq(1, nrow(train_data)), 0.5*nrow(train_data))
    train <- train_data[picked,]
    cali <- train_data[-picked,]
    train_x <- data.frame(Sc = train[,1])
    train_y <- train[,2]
    qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
    
    #Functions
    qrf_func <- function(x) {
      if (is.atomic(x)){
        x <- data.frame(Sc = x)
        return(predict(qrf, x, what = mean))
      }
      return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
    }
    score <- sort(abs(qrf_func(cali) - cali$Kgp))
    quanti <- ceiling((nrow(cali)+1)*(1-0.1))
    q_hat <- score[quanti]
      
    upper <- function(x) qrf_func(x) + q_hat
    lower <- function(x) qrf_func(x) - q_hat
      
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                     &upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}


set.seed(4)
a <- rs_cov(leafs, 30, 0.1, 100)
b <- rs_cov(wood, 30, 0.1, 70)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 40)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 40)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

#Ret jævnt fordelt

#----------------------------Checking coverage for different alphas-------------

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()

set.seed(2)
for (i in (1:4)){
  a <- pred_int_making(leafs_train, node_size = 100, alpha = alphas[i])
  cov_alpha_l[i] <- mean(a[[1]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[2]](test_leafs$Sc))
}

for (i in (1:4)){
  b <- pred_int_making(wood_train, alpha = alphas[i])
  cov_alpha_w[i] <- mean(b[[1]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[2]](test_wood$Sc))
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))

#----------------------------Rolling coverage---------------------------------------------

#Leafs

set.seed(7)
a <- pred_int_making(leafs_train, node_size = 100)

bin_size <- 50
roll_cov <- c()

leafs_arr <- test_leafs %>%
  arrange(Sc)

for (i in seq(1,nrow(test_leafs)-bin_size)){
  data_cov <- leafs_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[1]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[2]](data_cov$Sc))
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
a <- pred_int_making(wood_train, node_size = 70)

bin_size <- 50
roll_cov <- c()

wood_arr <- test_wood %>%
  arrange(Sc)

for (i in seq(1,nrow(test_wood)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[1]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[2]](data_cov$Sc))
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


####################################################################
#Conformalised quantile regression forest---------------------------------------
####################################################################

pred_int_making <- function(train_data, node_size = 70) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.5*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  
  #Functions
  qrf_func <- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
  t_05 <- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = 0.05))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = 0.05))
  }
  t_95<- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = 0.95))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = 0.95))
  }
  
  #Scores
  score <- c()
  for (i in (1:nrow(cali))){
    score[i] <- max((t_05(cali[i,]) - cali[i,]$Kgp), (cali[i,]$Kgp- t_95(cali[i,])))
  }
  score <- sort(score)
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) t_95(x) + q_hat
  lower <- function(x) t_05(x) - q_hat
  
  return(list(lower, upper, qrf_func))
}

set.seed(4)
a <- pred_int_making(leafs_train, node_size = 100)
b <- pred_int_making(wood_train, node_size = 70)
z1 <- mean(a[[1]](test_leafs) <= test_leafs$Kgp &
             test_leafs$Kgp  <= a[[2]](test_leafs))
z2 <- mean(b[[1]](test_wood) <= test_wood$Kgp &
             test_wood$Kgp  <= b[[2]](test_wood))

xtable(tibble(" " = c("Leafs", "Wood"), "Covergae" = c(z1, z2)), type = latex)

test_leafs_plot <- test_leafs %>%
  mutate(Indicator = if_else((a[[1]](test_leafs) <= Kgp) &
                               (Kgp <= a[[2]](test_leafs)),"in", "out"))

test_wood_plot <- test_wood %>%
  mutate(Indicator = if_else((b[[1]](test_wood) <= Kgp)&
                               (Kgp <= b[[2]](test_wood)),"in", "out"))


color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "hotpink4") +
  geom_function(fun = a[[2]], colour = "hotpink4") +
  geom_function(fun = a[[3]], colour = "hotpink") +
  scale_color_manual(values = color) +
  labs(title = "Leafs")

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = b[[1]], colour = "hotpink4") +
  geom_function(fun = b[[2]], colour = "hotpink4") +
  geom_function(fun = b[[3]], colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)

#Distribution of coverage by resampling

rs_cov <- function(data, k, alpha, node_size = 100) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked <- sample(n,size = sample_size)
    train_data = data[picked,]
    test = data[-picked,]
    
    # Train and cali
    picked <- sample(seq(1, nrow(train_data)), 0.5*nrow(train_data))
    train <- train_data[picked,]
    cali <- train_data[-picked,]
    train_x <- data.frame(Sc = train[,1])
    train_y <- train[,2]
    qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
    
    #Functions
    qrf_func <- function(x) {
      if (is.atomic(x)){
        return(predict(qrf, data.frame(Sc = x), what = mean))
      }
      return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
    }
    t_05 <- function(x) {
      if (is.atomic(x)){
        return(predict(qrf, data.frame(Sc = x), what = 0.05))
      }
      return(predict(qrf, data.frame(Sc = x$Sc), what = 0.05))
    }
    t_95<- function(x) {
      if (is.atomic(x)){
        return(predict(qrf, data.frame(Sc = x), what = 0.95))
      }
      return(predict(qrf, data.frame(Sc = x$Sc), what = 0.95))
    
    score <- sort(abs(model(cali_rs$Sc) - cali_rs$Kgp))
    quanti <- ceiling((nrow(cali_rs)+1)*(1-0.1))
    q_hat <- score[quanti] 
    
    upper <- function(x) model(x) + q_hat
    lower <- function(x) model(x) - q_hat
    
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                   &upper(test_rs$Sc) >= test_rs$Kgp)
    }
    
    #Scores
    score <- c()
    for (i in (1:nrow(cali))){
      score[i] <- max((t_05(cali[i,]) - cali[i,]$Kgp), (cali[i,]$Kgp- t_95(cali[i,])))
    }
    score <- sort(score)
    quanti <- ceiling((nrow(cali)+1)*(1-0.1))
    q_hat <- score[quanti]
    cov[i] <- mean(t_05(test$Sc) - q_hat <= test$Kgp &
                     test$Kgp <= t_95(test$Sc) + q_hat)
  }
  return(tibble("Coverage" = cov))
}


set.seed(4)
a <- rs_cov(leafs, 1, 0.1, 100)
b <- rs_cov(wood, 30, 0.1, 70)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 50)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1)+
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 40)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1))+
  labs(title = "Wood")

#Ret jævnt fordelt






################################################################################
