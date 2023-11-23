#################################--Indlæsning af pakker og data---###############################

leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log<- read.csv('Data/wood_log.csv')

leafs_train <- read.csv('Data/train_leafs_log.csv')
roots_train <- read.csv('Data/train_roots_log.csv')
wood_train<- read.csv('Data/train_wood_log.csv')

leafs_test <- read.csv('Data/test_leafs.csv')
roots_test <- read.csv('Data/test_roots.csv')
wood_test <- read.csv('Data/test_wood.csv')

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
## ------------------------------ sum model OLS ----------------------------- ##
################################################################################

lm_leafs <- lm(Kgp ~ Sc, data = leafs)
lm_wood <- lm(Kgp ~ Sc, data = wood)
lm_roots <- lm(Kgp ~ Sc, data = roots)

hat_beta <- c(lm_leafs$coefficients[[1]],
              lm_wood$coefficients[[1]],
              lm_roots$coefficients[[1]])

hat_alpha <- c(lm_leafs$coefficients[[2]],
               lm_wood$coefficients[[2]],
               lm_roots$coefficients[[2]])

var_hat <- c(var(lm_leafs$residuals),
               var(lm_wood$residuals),
               var(lm_roots$residuals))



sum_leafs <- function(x) hat_alpha[1] * sum(x) + length(x)*hat_beta[1]
sum_wood <- function(x) hat_alpha[2] * sum(x) + length(x)*hat_beta[2]
sum_roots <- function(x) hat_alpha[3] * sum(x) + length(x)*hat_beta[3]

sum_biomass <- function(x,y,z) sum_leafs(x) + sum_wood(y) + sum_roots(z)

################################################################################
#Gaussian prediction interval
################################################################################

#Leafs

alpha <- 0.1

upper_leafs <- function(x) {
  sum_leafs(x) - qt(alpha/2, nrow(leafs)-2)*sqrt((sum(x))^2/sum(leafs$Sc^2)+1)*sqrt(var_hat[1])
}
lower_leafs <- function(x) {
  sum_leafs(x) - qt(1-alpha/2, nrow(leafs)-2)*sqrt(sum(x)^2/sum(leafs$Sc^2)+1)*sqrt(var_hat[1])
}

pred_leafs <- c()
act_leafs <- c()
agg_sc <- c()
upp <- c()
down <- c()

for (i in (1:nrow(leafs))){
  pred_leafs[i] <- sum_leafs(leafs$Sc[1:i])
  act_leafs[i] <- sum(leafs$Kgp[1:i])
  agg_sc[i] <- sum(leafs$Sc[1:i])
  upp[i] <- upper_leafs(leafs$Sc[1:i])
  down[i] <- lower_leafs(leafs$Sc[1:i])
}

my_tib <- tibble(pred = pred_leafs, act = act_leafs, 
                 Sc = agg_sc, Up = upp, Down = down)

ggplot(my_tib) + 
  geom_point(aes(x = Sc, y = pred), color = 'blue', size = 0.2) +
  geom_point(aes(x = Sc, y = act), color = 'red', size = 0.2) + 
  geom_point(aes(x = Sc, y = Up), color = 'violet', size = 0.2) +
  geom_point(aes(x = Sc, y = Down), color = 'violet', size = 0.2) +
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

mean(down <= act_leafs & act_leafs <= upp)

#Wood

alpha <- 0.1

upper_wood <- function(x) {
  sum_wood(x) - qt(alpha/2, nrow(wood)-2)*sqrt(sum(x)^2/sum(wood$Sc^2)+1)*sqrt(var_hat[2])
}
lower_wood <- function(x) {
  sum_wood(x) - qt(1-alpha/2, nrow(wood)-2)*sqrt(sum(x)^2/sum(wood$Sc^2)+1)*sqrt(var_hat[2])
}


pred_wood <- c()
act_wood <- c()
agg_sc <- c()
upp <- c()
down <- c()

for (i in (1:nrow(wood))){
  pred_wood[i] <- sum_wood(wood$Sc[1:i])
  act_wood[i] <- sum(wood$Kgp[1:i])
  agg_sc[i] <- sum(wood$Sc[1:i])
  upp[i] <- upper_wood(wood$Sc[1:i])
  down[i] <- lower_wood(wood$Sc[1:i])
}

my_tib <- tibble(pred = pred_wood, act = act_wood, 
                 Sc = agg_sc, Up = upp, Down = down)

ggplot(my_tib) + 
  geom_point(aes(x = Sc, y = pred), color = 'blue', size = 0.2) +
  geom_point(aes(x = Sc, y = act), color = 'red', size = 0.2) + 
  geom_point(aes(x = Sc, y = Up), color = 'violet', size = 0.2) +
  geom_point(aes(x = Sc, y = Down), color = 'violet', size = 0.2) +
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

mean(down <= act_wood & act_wood <= upp)


#roots

alpha <- 0.1

upper_roots <- function(x) {
  sum_roots(x) - qt(alpha/2, nrow(roots)-2)*sqrt(sum(x)^2/sum(roots$Sc^2)+1)*sqrt(var_hat[2])
}
lower_roots <- function(x) {
  sum_roots(x) - qt(1-alpha/2, nrow(roots)-2)*sqrt(sum(x)^2/sum(roots$Sc^2)+1)*sqrt(var_hat[2])
}


pred_roots <- c()
act_roots <- c()
agg_sc <- c()
upp <- c()
down <- c()

for (i in (1:nrow(roots))){
  pred_roots[i] <- sum_roots(roots$Sc[1:i])
  act_roots[i] <- sum(roots$Kgp[1:i])
  agg_sc[i] <- sum(roots$Sc[1:i])
  upp[i] <- upper_roots(roots$Sc[1:i])
  down[i] <- lower_roots(roots$Sc[1:i])
}

my_tib <- tibble(pred = pred_roots, act = act_roots, 
                 Sc = agg_sc, Up = upp, Down = down)

ggplot(my_tib) + 
  geom_point(aes(x = Sc, y = pred), color = 'blue', size = 0.7) +
  geom_point(aes(x = Sc, y = act), color = 'red', size = 0.7) + 
  geom_point(aes(x = Sc, y = Up), color = 'violet', size = 0.7) +
  geom_point(aes(x = Sc, y = Down), color = 'violet', size = 0.7) +
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

mean(down <= act_roots & act_roots <= upp)

#På test sæt:

lm_leafs <- lm(Kgp ~ Sc, data = leafs_train)
lm_wood <- lm(Kgp ~ Sc, data = wood_train)
lm_roots <- lm(Kgp ~ Sc, data = roots_train)

hat_beta <- c(lm_leafs$coefficients[[1]],
              lm_wood$coefficients[[1]],
              lm_roots$coefficients[[1]])

hat_alpha <- c(lm_leafs$coefficients[[2]],
               lm_wood$coefficients[[2]],
               lm_roots$coefficients[[2]])

var_hat <- c(var(lm_leafs$residuals),
             var(lm_wood$residuals),
             var(lm_roots$residuals))



sum_leafs <- function(x) hat_alpha[1] * sum(x) + length(x)*hat_beta[1]
sum_wood <- function(x) hat_alpha[2] * sum(x) + length(x)*hat_beta[2]
sum_roots <- function(x) hat_alpha[3] * sum(x) + length(x)*hat_beta[3]

sum_biomass <- function(x,y,z) sum_leafs(x) + sum_wood(y) + sum_roots(z)

#Gaussian prediction interval

#Leafs

alpha <- 0.1

upper_leafs <- function(x) {
  sum_leafs(x) - qt(alpha/2, nrow(leafs_train)-2)*sqrt((sum(x))^2/sum(leafs_train$Sc^2)+1)*sqrt(var_hat[1])
}
lower_leafs <- function(x) {
  sum_leafs(x) - qt(1-alpha/2, nrow(leafs_train)-2)*sqrt(sum(x)^2/sum(leafs_train$Sc^2)+1)*sqrt(var_hat[1])
}

pred_leafs <- c()
act_leafs <- c()
agg_sc <- c()
upp <- c()
down <- c()

for (i in (1:nrow(leafs_test))){
  pred_leafs[i] <- sum_leafs(leafs_test$Sc[1:i])
  act_leafs[i] <- sum(leafs_test$Kgp[1:i])
  agg_sc[i] <- sum(leafs_test$Sc[1:i])
  upp[i] <- upper_leafs(leafs_test$Sc[1:i])
  down[i] <- lower_leafs(leafs_test$Sc[1:i])
}

my_tib <- tibble(pred = pred_leafs, act = act_leafs, 
                 Sc = agg_sc, Up = upp, Down = down)

ggplot(my_tib) + 
  geom_point(aes(x = Sc, y = pred), color = 'blue', size = 0.2) +
  geom_point(aes(x = Sc, y = act), color = 'red', size = 0.2) + 
  geom_point(aes(x = Sc, y = Up), color = 'violet', size = 0.2) +
  geom_point(aes(x = Sc, y = Down), color = 'violet', size = 0.2) +
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

mean(down <= act_leafs & act_leafs <= upp)

#Wood


alpha <- 0.1

upper_wood <- function(x) {
  sum_wood(x) - qt(alpha/2, nrow(wood_train)-2)*sqrt((sum(x))^2/sum(wood_train$Sc^2)+1)*sqrt(var_hat[1])
}
lower_wood <- function(x) {
  sum_wood(x) - qt(1-alpha/2, nrow(wood_train)-2)*sqrt(sum(x)^2/sum(wood_train$Sc^2)+1)*sqrt(var_hat[1])
}

pred_wood <- c()
act_wood <- c()
agg_sc <- c()
upp <- c()
down <- c()

for (i in (1:nrow(wood_test))){
  pred_wood[i] <- sum_wood(wood_test$Sc[1:i])
  act_wood[i] <- sum(wood_test$Kgp[1:i])
  agg_sc[i] <- sum(wood_test$Sc[1:i])
  upp[i] <- upper_wood(wood_test$Sc[1:i])
  down[i] <- lower_wood(wood_test$Sc[1:i])
}

my_tib <- tibble(pred = pred_wood, act = act_wood, 
                 Sc = agg_sc, Up = upp, Down = down)

ggplot(my_tib) + 
  geom_point(aes(x = Sc, y = pred), color = 'blue', size = 0.2) +
  geom_point(aes(x = Sc, y = act), color = 'red', size = 0.2) + 
  geom_point(aes(x = Sc, y = Up), color = 'violet', size = 0.2) +
  geom_point(aes(x = Sc, y = Down), color = 'violet', size = 0.2) +
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

mean(down <= act_wood & act_wood <= upp)


#roots
alpha <- 0.1

upper_roots <- function(x) {
  sum_roots(x) - qt(alpha/2, nrow(roots_train)-2)*sqrt((sum(x))^2/sum(roots_train$Sc^2)+1)*sqrt(var_hat[1])
}
lower_roots <- function(x) {
  sum_roots(x) - qt(1-alpha/2, nrow(roots_train)-2)*sqrt(sum(x)^2/sum(roots_train$Sc^2)+1)*sqrt(var_hat[1])
}

pred_roots <- c()
act_roots <- c()
agg_sc <- c()
upp <- c()
down <- c()

for (i in (1:nrow(roots_test))){
  pred_roots[i] <- sum_roots(roots_test$Sc[1:i])
  act_roots[i] <- sum(roots_test$Kgp[1:i])
  agg_sc[i] <- sum(roots_test$Sc[1:i])
  upp[i] <- upper_roots(roots_test$Sc[1:i])
  down[i] <- lower_roots(roots_test$Sc[1:i])
}

my_tib <- tibble(pred = pred_roots, act = act_roots, 
                 Sc = agg_sc, Up = upp, Down = down)

ggplot(my_tib) + 
  geom_point(aes(x = Sc, y = pred), color = 'blue', size = 0.2) +
  geom_point(aes(x = Sc, y = act), color = 'red', size = 0.2) + 
  geom_point(aes(x = Sc, y = Up), color = 'violet', size = 0.2) +
  geom_point(aes(x = Sc, y = Down), color = 'violet', size = 0.2) +
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

mean(down <= act_roots & act_roots <= upp)

# Hele træer

sum_biomass(leafs$Sc, wood$Sc, roots$Sc) - (sum(leafs$Kgp) + sum(wood$Kgp) + sum(roots$Kgp))


pred_bio <- c()

for (i in (1:26)) {
  pred_bio[i] <- sum_biomass(leafs$Sc[1:i], wood$Sc[1:i], roots$Sc[1:i])
}

act_bio <- c()

for (i in (1:26)) {
  if (i == 1){
    act_bio[i] <- leafs$Kgp[i] + wood$Kgp[i] + roots$Kgp[i]
  }
  else{
    act_bio[i] <- leafs$Kgp[i] + wood$Kgp[i] + roots$Kgp[i] + act_bio[i-1]
  }
}

my_tib <- tibble(pred = pred_bio, act = act_bio, num_tree = seq(1:26))

ggplot(my_tib) + 
  geom_point(aes(x = num_tree, y = act_bio), color = 'blue') +
  geom_point(aes(x = num_tree, y = pred_bio), color = 'red') + 
  theme_bw() +
  xlab(bquote('Crown area'~m^2/plant)) + 
  ylab('Dry mass (kg/plant)')+
  labs(title = "Sum of biomass")

################################################################################
#De gaussiske virker ikke så godt...
################################################################################



################################################################################
# ----------------------------------- Bootstrap ------------------------------ #
################################################################################


################################################################################
# ----------------------------------- Conformal ------------------------------ #
################################################################################

#Absolute error
pred_int_making <- function(train_data, alpha=0.1) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  cali <- cali %>% mutate(Sc = exp(Sc), Kgp = exp(Kgp))
  
  #Linear model
  lm <- lm(Kgp ~ Sc, train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) lm$coefficients[[1]]+ x*lm$coefficients[[2]]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper_1 <- function(x) f_hat(x) + q_hat
  lower_1 <- function(x) f_hat(x) - q_hat

  return(list(f_hat_adj, f_hat, lower_1, upper_1, lower_2, upper_2))
}

#Kan vi det her???








