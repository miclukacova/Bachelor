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

####################################################################
#########################---SPLIT CONFORMAIL---#####################
####################################################################

#Log-log ols--------------------------------------------------------

#To forskellige score funktioner

#Absolute error
pred_int_making_1 <- function(train_data, alpha = 0.1) {
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

  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  q_hat_adj <- score_adj[quanti]
  
  #Prediction interval functions
  
  upper_1 <- function(x) f_hat(x) + q_hat
  lower_1 <- function(x) f_hat(x) - q_hat
  
  upper_2 <- function(x) f_hat_adj(x) + q_hat_adj
  lower_2 <- function(x) f_hat_adj(x) - q_hat_adj
  
  return(list(f_hat_adj, f_hat, lower_1, upper_1, lower_2, upper_2))
}

#Absolute error / sd hat(Y) (VIRKER IKKE SÅ GODT)
pred_int_making_2 <- function(train_data, alpha = 0.1) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.5*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  cali <- cali %>% mutate(Sc = exp(Sc), Kgp = exp(Kgp))
  
  #Linear model
  lm <- lm(Kgp ~ Sc, train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))

  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  q_hat_adj <- score_adj[quanti]
  
  #Prediction intervals
  upper <- function(x) f_hat(x) + q_hat*sd_y_hat(x)
  lower <- function(x) f_hat(x) - q_hat*sd_y_hat(x)
  
  upper_adj <- function(x) f_hat_adj(x) + q_hat_adj*sd_y_hat(x)
  lower_adj <- function(x) f_hat_adj(x) - q_hat_adj*sd_y_hat(x)
  
  return(list(f_hat_adj, f_hat, upper, lower, upper_adj, lower_adj))
}

################################################################################
# Score funktion foreslået af noten 
## Denne giver mærkelige pred intervaller - problematisk. Grundet høje varians estimater
##########################Note metoderne########################################
#Pred intervallerne 

set.seed(1)
c <- pred_int_making_2(train_leafs_log)
d <- pred_int_making_2(train_wood_log)

#Coverage på test sæt

z1 <- mean(c[[4]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= c[[3]](test_leafs$Sc))
z2 <- mean(d[[4]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= d[[3]](test_wood$Sc))
z3 <- mean(c[[6]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= c[[5]](test_leafs$Sc))
z4 <- mean(d[[6]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= d[[5]](test_wood$Sc))

xtable(tibble(" " = c("Leafs", "Wood"), "Naive" = c(z1, z2), "Bias adjusted" = c(z3, z4)))

test_leafs_plot1 <- test_leafs %>%
  mutate(indicator = if_else((c[[6]](Sc) <= Kgp)&(Kgp <= c[[5]](Sc)),"in", "out"))

test_wood_plot1 <- test_wood %>%
  mutate(indicator = if_else((d[[6]](Sc) <= Kgp)&(Kgp <= d[[5]](Sc)),"in", "out"))

ggplot(test_leafs_plot1, aes(x = Sc, y = Kgp)) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = c[[1]], colour = "hotpink4") +
  geom_function(fun = c[[5]], colour = "hotpink") +
  geom_function(fun = c[[6]], colour = "hotpink") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_wood_plot1, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = d[[1]], colour = "hotpink4") +
  geom_function(fun = d[[5]], colour = "hotpink") +
  geom_function(fun = d[[6]], colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)


##################################Absolute error score##########################
##---------------------------På OLS log log, naiv og bias adjusted
#----------------------------Pred intervaller--------------------------------

set.seed(1)
a <- pred_int_making_1(train_leafs_log)
b <- pred_int_making_1(train_wood_log)
#c <- pred_int_making(train_roots_log)
#Der er ikke nok data punkter i roots --  vi får NA's

#Coverage på test sæt

z1 <- mean(a[[3]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[4]](test_leafs$Sc))
z2 <- mean(b[[3]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[4]](test_wood$Sc))
z3 <- mean(a[[5]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[6]](test_leafs$Sc))
z4 <- mean(b[[5]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[6]](test_wood$Sc))

xtable(tibble(" " = c("Leafs", "Wood"), "Naive" = c(z1, z2), "Bias adjusted" = c(z3, z4)))

#----------------------------Checking coverage for different alphas-----------------------------------------

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()

set.seed(2)
for (i in (1:4)){
  a <- pred_int_making_1(train_leafs_log, alpha = alphas[i])
  cov_alpha_l[i] <- mean(a[[5]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[6]](test_leafs$Sc))
}

for (i in (1:4)){
  b <- pred_int_making_1(train_wood_log, alpha = alphas[i])
  cov_alpha_w[i] <- mean(b[[5]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[6]](test_wood$Sc))
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))

#Plots

test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((a[[5]](Sc) <= Kgp)&(Kgp <= a[[6]](Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "hotpink4") +
  geom_function(fun = a[[5]], colour = "hotpink") +
  geom_function(fun = a[[6]], colour = "hotpink") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)


test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((b[[3]](Sc) <= Kgp)&(Kgp <= b[[4]](Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = b[[2]], colour = "hotpink4") +
  geom_function(fun = b[[3]], colour = "hotpink") +
  geom_function(fun = b[[4]], colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)


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

####################################################################

#NLR----------------------------------------------------------------------------

#----------------------------Pred intervaller#########################

f_hat_l <- function(x) 0.5686253*x^0.7160437
f_hat_w <- function(x) 6.9512896*x^0.9841950


#Evt. optimer train cali split, og også måske noget med et usikkerhedsmål
pred_int_making <- function(train_data, model, alpha = 0.1) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(model(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) model(x) + q_hat
  lower <- function(x) model(x) - q_hat
  
  return(list(lower, upper))
}

a <- pred_int_making(leafs_train, f_hat_l)
b <- pred_int_making(wood_train, f_hat_w)
z1 <- mean(a[[1]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[2]](test_leafs$Sc))
z2 <- mean(b[[1]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[2]](test_wood$Sc))

xtable(tibble(" " = c("Leafs", "Wood"), "Covergae" = c(z1, z2)), type = latex)

test_leafs_plot <- test_leafs %>%
  mutate(Indicator = if_else((a[[1]](Sc) <= Kgp)&
                               (Kgp <= a[[2]](Sc)),"in", "out"))

test_wood_plot <- test_wood %>%
  mutate(Indicator = if_else((b[[1]](Sc) <= Kgp)&
                               (Kgp <= b[[2]](Sc)),"in", "out"))


color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "hotpink4") +
  geom_function(fun = a[[2]], colour = "hotpink4") +
  geom_function(fun = f_hat_l, colour = "hotpink") +
  scale_color_manual(values = color) +
  labs(title = "Leafs")

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = b[[1]], colour = "hotpink4") +
  geom_function(fun = b[[2]], colour = "hotpink4") +
  geom_function(fun = f_hat_w, colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)


#Distribution of coverage by resampling for the NLR model
#----------------------------Distribution of coverage by resampling-------------
rs_cov <- function(data, k, alpha, model) {
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
    train_rs <- train_rs[picked,]
    
    score <- sort(abs(model(cali_rs$Sc) - cali_rs$Kgp))
    quanti <- ceiling((nrow(cali_rs)+1)*(1-0.1))
    q_hat <- score[quanti] 
    
    upper <- function(x) model(x) + q_hat
    lower <- function(x) model(x) - q_hat
    
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                   & upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

set.seed(4)
a <- rs_cov(leafs, 30, 0.1, f_hat_l)
b <- rs_cov(wood, 30, 0.1, f_hat_w)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
var_a <- var(a$Coverage)
var_b <- var(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b),
              "Variance of coverage" =c(var_a, var_b)),
              type = latex)

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

#Ret jævnt fordelt
#----------------------------Checking coverage for different alphas-------------------

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()

set.seed(2)
for (i in (1:4)){
  a <- pred_int_making(leafs_train, f_hat_l, alpha = alphas[i])
  cov_alpha_l[i] <- mean(a[[1]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[2]](test_leafs$Sc))
}

for (i in (1:4)){
  b <- pred_int_making(wood_train, f_hat_w, alpha = alphas[i])
  cov_alpha_w[i] <- mean(b[[1]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[2]](test_wood$Sc))
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Rolling coverage-----------------------------------

#Leafs

set.seed(7)
a <- pred_int_making(leafs_train, f_hat_l, alpha = 0.1)

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
a <- pred_int_making(wood_train, f_hat_w, alpha = 0.1)

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
