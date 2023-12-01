#################################--Indlæsning af pakker og data---##############

train_leafs_log <- read.csv('Data/train_leafs_log.csv')
train_roots_log <- read.csv('Data/train_roots_log.csv')
train_wood_log<- read.csv('Data/train_wood_log.csv')

test_leafs_log <- read.csv('Data/test_leafs_log.csv')
test_roots_log <- read.csv('Data/test_roots_log.csv')
test_wood_log<- read.csv('Data/test_wood_log.csv')

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

test_leafs <- read.csv('Data/test_leafs.csv')
test_roots <- read.csv('Data/test_roots.csv')
test_wood <- read.csv('Data/test_wood.csv')

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
################################################################################

################################################################################
#################################--Log-log OLS model---#########################
################################################################################


pred_int_log_ols <- function(alpha = 0.2, data){
  #model fit
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  f_hat_log <- function(x) model$coef[[1]] + x*model$coef[[2]]
  
  #creation of prediction interval
  P <- solve((t(model.matrix(model)) %*% model.matrix(model)))
  upper0 <- function(x) {
    f_hat_log(x) - qt(alpha/2, nrow(data)-2)*
       sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  upper <- function(x) exp(upper0(log(x)))
  lower0 <- function(x) {
    f_hat_log(x) - qt(1-alpha/2, nrow(data)-2)*
       sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  lower <- function(x) exp(lower0(log(x)))
  return(list(f_hat, upper, lower))
}

loo_pred_int <- function(data, alpha = 0.2, pred_int) {
  low <- c()
  high <- c()
  fitted <- c()
  for (i in (1:nrow(data))){
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

plot_maker <- function(pred_int, title){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((Low <= Kgp)&(Kgp <= High),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 0.8, alpha = 0.5) + 
    geom_line(aes(x = Sc, y = High), color = "hotpink", linewidth = 0.9) + 
    geom_line(aes(x = Sc, y = Low), color = "hotpink", linewidth = 0.9) +
    geom_line(aes(x = Sc, y = Fitted), color = "hotpink4", linewidth = 0.9) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    scale_color_manual(values = color)+ 
    theme(legend.position = "none")
}

leafs_pred_int <- loo_pred_int(data = leafs, pred_int = pred_int_log_ols, alpha = 0.2)
wood_pred_int <- loo_pred_int(data = wood, pred_int = pred_int_log_ols, alpha = 0.2)  
roots_pred_int <- loo_pred_int(data = roots, pred_int = pred_int_log_ols, alpha = 0.2)

plot_maker(leafs_pred_int[[1]], "Leafs")
plot_maker(wood_pred_int[[1]], "Wood")
plot_maker(roots_pred_int[[1]], "Roots")

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), 
              "Coverage" = c(leafs_pred_int[[2]],wood_pred_int[[2]],
                             roots_pred_int[[2]])), type = latex)


#---------------------Random split to assess coverage---------------------------

rs_cov <- function(data, k, alpha, pred_int_maker) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit model
    model_rs <- pred_int_maker(data = train_rs, alpha = alpha)
    low <- model_rs[[3]](test_rs$Sc)
    high <- model_rs[[2]](test_rs$Sc)
    
    #Definere
    cov[i] <- mean(low <= test_rs$Kgp & high >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

rs_plot_maker <- function(rs_cov, title, alpha){
  rs_cov %>%
    ggplot() +
    geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                   fill = "darkolivegreen3", bins = 40)+
    geom_vline(xintercept = 1-alpha, color = "hotpink") +
    xlim(0,1.1)+
    theme_bw()+
    labs(title = title)
}

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 30, alpha = 0.2, pred_int_maker = pred_int_log_ols)
b <- rs_cov(data = wood, k = 30, alpha = 0.2, pred_int_maker = pred_int_log_ols)
c <- rs_cov(data = roots, k = 30, alpha = 0.2, pred_int_maker = pred_int_log_ols)

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


#------------------Rolling coverage---------------------------------------------

roll_cov <- function(pred_int, alpha = 0.2, bin_size = 50, title){
  roll_cov <- c()
  
  data_arr <- pred_int[[1]] %>%
    arrange(Sc)
  
  for (i in seq(1,nrow(data_arr)-bin_size)){
    data_cov <- data_arr %>%
      slice(i:(i+bin_size))
    roll_cov[i] <-mean((data_cov$Low <= data_cov$Kgp) & (data_cov$High >= data_cov$Kgp))
  }
  
  my_tib <- tibble("Bin" = seq(1,nrow(data_arr)-bin_size), "Roll_cov" = roll_cov)
  
  up_binom <- qbinom(alpha/2, bin_size, 1-alpha)/bin_size 
  down_binom <- qbinom(1-alpha/2,bin_size,1-alpha)/bin_size
  
  ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
    geom_line(size = 0.6, aes(color = Roll_cov)) + 
    geom_hline(yintercept = 1-alpha, color = "purple")+
    geom_hline(yintercept = up_binom, color = "purple", linetype = "dashed", size = 0.3)+
    geom_hline(yintercept = down_binom, color = "purple", linetype = "dashed", size = 0.3)+
    theme_bw() +
    xlab('Sc') + 
    ylab('Coverage')+
    labs(title = title)+
    scale_color_gradient(low = 'blue', high = 'red')
}

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)



#-------------------Checking coverage for different alphas----------------------

diff_alohas <- function(data, pred_int){
  alphas <- c(0.05, 0.1, 0.2, 0.3)
  cov_alpha <- c()
  for (i in (1:4)){
    alpha <- alphas[i]
    pred <- loo_pred_int(data = data, pred_int = pred_int, alpha = alpha)
    cov_alpha[i] <- pred[[2]]
  }
  return(cov_alpha)
}

cov_alpha_l <- diff_alohas(data = leafs, pred_int = pred_int_log_ols)
cov_alpha_w <- diff_alohas(data = wood, pred_int = pred_int_log_ols)
cov_alpha_r <- diff_alohas(data = roots, pred_int = pred_int_log_ols)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))








################################################################################
#################################----- OLS model ------#########################
################################################################################

pred_int_ols <- function(alpha = 0.2, data){
  #model fit
  model <- lm(Kgp ~ Sc, data)
  f_hat <- function(x) model$coef[[1]] + x*model$coef[[2]]
  
  #creation of prediction interval
  P <- solve((t(model.matrix(model)) %*% model.matrix(model)))
  upper <- function(x) {
    f_hat(x) - qt(alpha/2, nrow(data)-2)*
      sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  lower <- function(x) {
    f_hat(x) - qt(1-alpha/2, nrow(data)-2)*
      sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)*sqrt(var(model$residuals))
  }
  return(list(f_hat, upper, lower))
}

loo_pred_int <- function(data, alpha=0.2, pred_int) {
  low <- c()
  high <- c()
  fitted <- c()
  for (i in (1:nrow(data))){
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

plot_maker <- function(pred_int, title){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((Low <= Kgp)&(Kgp <= High),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 0.8, alpha = 0.5) + 
    geom_line(aes(x = Sc, y = High), color = "hotpink", linewidth = 0.9) + 
    geom_line(aes(x = Sc, y = Low), color = "hotpink", linewidth = 0.9) +
    geom_line(aes(x = Sc, y = Fitted), color = "hotpink4", linewidth = 0.9) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    scale_color_manual(values = color)+ 
    theme(legend.position = "none")
}
leafs_pred_int <- loo_pred_int(data = leafs, pred_int = pred_int_ols, alpha = 0.2)
wood_pred_int <- loo_pred_int(data = wood, pred_int = pred_int_ols, alpha = 0.2)  
roots_pred_int <- loo_pred_int(data = roots, pred_int = pred_int_ols, alpha = 0.2)

plot_maker(leafs_pred_int[[1]], "Leafs")
plot_maker(wood_pred_int[[1]], "Wood")
plot_maker(roots_pred_int[[1]], "Roots")

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), 
              "Coverage" = c(leafs_pred_int[[2]],wood_pred_int[[2]],
                             roots_pred_int[[2]])), type = latex)



#---------------------Random split to assess coverage---------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 30, alpha = 0.2, pred_int_maker = pred_int_ols)
b <- rs_cov(data = wood, k = 30, alpha = 0.2, pred_int_maker = pred_int_ols)
c <- rs_cov(data = roots, k = 30, alpha = 0.2, pred_int_maker = pred_int_ols)

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

#------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)


#-------------------Checking coverage for different alphas----------------------



cov_alpha_l <- diff_alohas(data = leafs, pred_int = pred_int_ols)
cov_alpha_w <- diff_alohas(data = wood, pred_int = pred_int_ols)
cov_alpha_r <- diff_alohas(data = roots, pred_int = pred_int_ols)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))




