library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)

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

#To forskellige score funktioner

#Absolute error
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

  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  q_hat_adj <- score_adj[quanti]
  
  #Prediction interval functions
  
  upper_1 <- function(x) f_hat(x) + q_hat
  lower_1 <- function(x) f_hat(x) - q_hat
  
  upper_2 <- function(x) f_hat_adj(x) + q_hat_adj
  lower_2 <- function(x) f_hat_adj(x) - q_hat_adj
  
  return(list(f_hat_adj, f_hat, lower_1, upper_1, lower_2, upper_2))
}

#Absolute error / sd hat(Y)
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
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))

  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  
  #Prediction intervals
  upper <- function(x) f_hat(x) + q_hat*sd_y_hat(x)
  lower <- function(x) f_hat(x) - q_hat*sd_y_hat(x)
  
  return(list(f_hat, lower, upper))
}

# Absolute error score på OLS log log, naiv og bias adjusted

set.seed(1)
a <- pred_int_making_1(train_leafs_log)
b <- pred_int_making_1(train_wood_log)
#c <- pred_int_making(train_roots_log)
#Der er ikke nok data punkter i roots --  vi får NA's


z1 <- mean(a[[3]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[4]](test_leafs$Sc))
z2 <- mean(b[[3]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[4]](test_wood$Sc))
z3 <- mean(a[[5]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[6]](test_leafs$Sc))
z4 <- mean(b[[5]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[6]](test_wood$Sc))

xtable(tibble(" " = c("Leafs", "Wood"), "Naive" = c(z1, z2), "Bias adjusted" = c(z3, z4)))

ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "hotpink") +
  geom_function(fun = a[[5]], colour = "darkolivegreen4") +
  geom_function(fun = a[[6]], colour = "darkolivegreen4") +
  labs(title = "Leafs")

# Score funktion foreslået af noten 
## Denne giver mærkelige pred intervaller - problematisk. Grundet høje varians estimater
## De høje varians estimater kan ses her:

lm <- lm(Kgp ~ Sc, train_leafs_log)
var_hat <- sum(lm$residuals^2)/(nrow(train_leafs_log)-1)
func1 <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
ggplot(leafs_train, aes(x = Sc, y = Kgp)) + 
  theme_bw() +
  xlab('Sc') + 
  geom_function(fun = func1, colour = "darkolivegreen") +
  labs(title = "Variance estimate")

#Pred intervallerne 

set.seed(1)
c <- pred_int_making_2(train_leafs_log)
d <- pred_int_making_2(train_wood_log)

#Note metoderne

ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = c[[1]], colour = "hotpink") +
  geom_function(fun = c[[2]], colour = "darkolivegreen4") +
  geom_function(fun = c[[3]], colour = "darkolivegreen4") +
  labs(title = "Leafs")

ggplot(test_wood, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = d[[1]], colour = "hotpink") +
  geom_function(fun = d[[2]], colour = "darkolivegreen4") +
  geom_function(fun = d[[3]], colour = "darkolivegreen4") +
  labs(title = "Wood")

z1 <- mean(c[[2]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= c[[3]](test_leafs$Sc))
z2 <- mean(d[[2]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= d[[3]](test_wood$Sc))

xtable(tibble(" " = c("Leafs", "Wood"), "Bias adj." = c(z1, z2)))

#Distribution of coverage by resampling

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

#Ret jævnt fordelt - plot evt. den teoretiske fordeling

####################################################################
#NLR--------------------------------------------------------
####################################################################

f_hat_l <- function(x) 0.5686253*x^0.7160437
f_hat_w <- function(x) 6.9512896*x^0.9841950
f_hat_r <- function(x) 0.1206638*x^1.7371556


#Evt. optimer train cali split, og også måske noget med et usikkerhedsmål
pred_int_making <- function(train_data, model) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(model(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) model(x) + q_hat
  lower <- function(x) model(x) - q_hat
  
  return(list(lower, upper))
}

a <- pred_int_making(leafs_train, f_hat_l)
b <- pred_int_making(wood_train, f_hat_w)
mean(a[[1]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[2]](test_leafs$Sc))
mean(b[[1]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[2]](test_wood$Sc))

ggplot(test_leafs, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "red") +
  geom_function(fun = a[[2]], colour = "blue") +
  geom_function(fun = f_hat_l, colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")

ggplot(test_wood, aes(x = Sc, y = Kgp)) + 
  geom_point() + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = b[[1]], colour = "red") +
  geom_function(fun = b[[2]], colour = "blue") +
  geom_function(fun = f_hat_w, colour = "blue") +
  labs(title = "Kgp as function of Sc with conformal prediction intervals")


#Distribution of coverage by resampling for the NLR model

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
                   &upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

set.seed(4)
a <- rs_cov(leafs, 30, 0.1, f_hat_l)
b <- rs_cov(wood, 30, 0.1, f_hat_w)

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

