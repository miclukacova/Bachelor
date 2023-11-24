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

####################################################################
#Log-log ols--------------------------------------------------------

####################################################################
#To forskellige score funktioner
####################################################################

#Absolute error
pred_int_making_1 <- function(train_data, , alpha = 0.1) {
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

####################################################################
# Absolute error score på OLS log log, naiv og bias adjusted

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

#Plots

test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((a[[3]](Sc) <= Kgp)&(Kgp <= a[[4]](Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[2]], colour = "hotpink4") +
  geom_function(fun = a[[3]], colour = "hotpink") +
  geom_function(fun = a[[4]], colour = "hotpink") +
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

####################################################################
# Score funktion foreslået af noten 
## Denne giver mærkelige pred intervaller - problematisk. Grundet høje varians estimater
####################################################################

#Note metoderne
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
  mutate(indicator = if_else((c[[4]](Sc) <= Kgp)&(Kgp <= c[[3]](Sc)),"in", "out"))

test_wood_plot1 <- test_wood %>%
  mutate(indicator = if_else((d[[4]](Sc) <= Kgp)&(Kgp <= d[[3]](Sc)),"in", "out"))

ggplot(test_leafs_plot1, aes(x = Sc, y = Kgp)) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = c[[2]], colour = "hotpink4") +
  geom_function(fun = c[[3]], colour = "hotpink") +
  geom_function(fun = c[[4]], colour = "hotpink") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_wood_plot1, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = d[[2]], colour = "hotpink4") +
  geom_function(fun = d[[3]], colour = "hotpink") +
  geom_function(fun = d[[4]], colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)

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


####################################################################
#Regression forest--------------------------------------------------------
####################################################################

pred_int_making <- function(train_data) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = 30)
  qrf_func <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
    
  # Heuristic notion of uncertainty
  score <- sort(abs(qrf_func(cali) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-0.1))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) qrf_func(x) + q_hat
  lower <- function(x) qrf_func(x) - q_hat
  
  return(list(lower, upper, qrf_func))
}

set.seed(4)
a <- pred_int_making(leafs_train)
b <- pred_int_making(wood_train)
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

####################################################################
#Conformalised quantile regression forest---------------------------------------
####################################################################

pred_int_making <- function(train_data) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.5*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = 30)
  
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
a <- pred_int_making(leafs_train)
b <- pred_int_making(wood_train)
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

#Distribution of coverage by resampling for the NLR model

rs_cov <- function(data, k, alpha) {
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
    qrf <- quantregForest(x = train_x, y = train_y, nodesize = 30)
    
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
    
    print(mean(t_05(test$Sc) - q_hat <= test$Kgp))
    print(mean(test$Kgp <= t_95(test$Sc) + q_hat))
    print(mean(test$Kgp <= t_95(test$Sc) + q_hat &
                 test$Kgp <= t_95(test$Sc) + q_hat))
    cov[i] <- mean(t_05(test$Sc) - q_hat <= test$Kgp &
                     test$Kgp <= t_95(test$Sc) + q_hat)
  }
  return(tibble("Coverage" = cov))
}


set.seed(4)
a <- rs_cov(leafs, 1, 0.1)
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

#Ret jævnt fordelt


################################################################################
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
