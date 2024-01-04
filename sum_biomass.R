#################################--Indlæsning af pakker og data---###############################

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
## ------------------------- Creating sum data ------------------------------ ##
################################################################################

#Labelling data: 
label_func <- function(data){
  group <- sample(rep(1:150, length.out = nrow(data)))
  labbeled_data <- data %>% add_column(Group = group)
  return(labbeled_data)
}

set.seed(4)
leafs_labeled <- label_func(leafs)
wood_labeled <- label_func(wood)

#Summing data: 
sum_func <- function(data) {
  for (i in c((1:59),150)){
    if (i == 1){
      group_data <- data %>% filter(Group == 1)
      new_data <- tibble(n = nrow(group_data), Sc = sum(group_data$Sc), 
                         Kgp = sum(group_data$Kgp))
    }
    else {
      group_data <- data %>% filter(Group == i)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp))
    }
  }
  for (i in (60:99)){
    if (i %% 2 == 0){
      group_data <- data %>% filter(Group == i | Group == i+1)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp))
    }
  }
  for (i in (100:129)){
    if (i %% 3 == 0){
      group_data <- data %>% filter(Group == i | Group == i-1 | Group == i-2)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp))
    }
  }
  for (i in (130:149)){
    if (i %% 4 == 0){
      group_data <- data %>% filter(Group == i-2 | Group == i-1 | Group == i | Group == i+1)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp))
    }
  }
  return(new_data)
}

sum_leafs <- sum_func(leafs_labeled)
sum_wood <- sum_func(wood_labeled)

#Plots of summed data

ggplot(sum_data1, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Foliage")

ggplot(sum_data2, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
  labs(title = "Wood")

################################################################################
## ----------------------- Summing prediction intervals --------------------- ##
################################################################################

#Summing prediction intervals: 
sum_pred_int_func <- function(data) {
  for (i in c((1:59),150)){
    if (i == 1){
      group_data <- data %>% filter(Group == 1)
      new_data <- tibble(n = nrow(group_data), Sc = sum(group_data$Sc), 
                         Kgp = sum(group_data$Kgp), Low = sum(group_data$Low),
                         High = sum(group_data$High), Fitted = sum(group_data$Fitted))
    }
    else {
      group_data <- data %>% filter(Group == i)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp), Low = sum(group_data$Low),
                                       High = sum(group_data$High), Fitted = sum(group_data$Fitted))
    }
  }
  for (i in (60:99)){
    if (i %% 2 == 0){
      group_data <- data %>% filter(Group == i | Group == i+1)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp), Low = sum(group_data$Low),
                                       High = sum(group_data$High), Fitted = sum(group_data$Fitted))
    }
  }
  for (i in (100:129)){
    if (i %% 3 == 0){
      group_data <- data %>% filter(Group == i | Group == i-1 | Group == i-2)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp), Low = sum(group_data$Low),
                                       High = sum(group_data$High), Fitted = sum(group_data$Fitted))
    }
  }
  for (i in (130:149)){
    if (i %% 4 == 0){
      group_data <- data %>% filter(Group == i-2 | Group == i-1 | Group == i | Group == i+1)
      new_data <- new_data %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp), Low = sum(group_data$Low),
                                       High = sum(group_data$High), Fitted = sum(group_data$Fitted))
    }
  }
  return(new_data)
}


#Split conformal score 2 logolsB on leafs and wood loo
set.seed(4)
loo_adj_l <- loo_pred_int(leafs, alpha = 0.2, s2_logolsb_conf) 
loo_adj_w <- loo_pred_int(wood, alpha = 0.2, s2_logolsb_conf) 

loo_adj_l[[1]] <- loo_adj_l[[1]] %>% add_column(Group = leafs_labeled$Group)
loo_adj_w[[1]] <- loo_adj_w[[1]] %>% add_column(Group = wood_labeled$Group)

sum_leafs_conf <- sum_pred_int_func(loo_adj_l[[1]])
sum_wood_conf <- sum_pred_int_func(loo_adj_w[[1]])

ggplot(sum_leafs_conf, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, shape = 3) + 
  geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, shape = 3) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Foliage")

ggplot(sum_wood_conf, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, shape = 3) + 
  geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, shape = 3) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")

mean(sum_leafs_conf$Low <= sum_leafs_conf$Kgp & sum_leafs_conf$Kgp <= sum_leafs_conf$High)
mean(sum_wood_conf$Low <= sum_wood_conf$Kgp & sum_wood_conf$Kgp <= sum_wood_conf$High)

#qrf


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

lower_l <- function(n,sc) qnorm(0.1, mean = hat_beta[1]*n + hat_alpha[1]*sc, sd = sqrt(n*var_hat[1]))
upper_l <- function(n,sc) qnorm(0.9, mean = hat_beta[1]*n + hat_alpha[1]*sc, sd = sqrt(n*var_hat[1]))
lower_w <- function(n,sc) qnorm(0.1, mean = hat_beta[2]*n + hat_alpha[2]*sc, sd = sqrt(n*var_hat[2]))
upper_w <- function(n,sc) qnorm(0.9, mean = hat_beta[2]*n + hat_alpha[2]*sc, sd = sqrt(n*var_hat[2]))

plot_data1 <- sum_data1 %>% mutate(Lower = lower_l(n,Sc), Upper = upper_l(n,Sc))
plot_data2 <- sum_data2 %>% mutate(Lower = lower_w(n,Sc), Upper = upper_w(n,Sc))

mean(plot_data1$Lower <= plot_data1$Kgp & plot_data1$Kgp <= plot_data1$Upper)
mean(plot_data2$Lower <= plot_data2$Kgp & plot_data2$Kgp <= plot_data2$Upper)

lower_l(sum_data1$n,sum_data1$Sc)

ggplot(sum_data1, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  geom_point(aes(x = Sc, y = lower_l(n,Sc)), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sc, y = upper_l(n,Sc)), color = "hotpink", size = 1, alpha = 0.7) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Foliage")

ggplot(sum_data2, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  geom_point(aes(x = Sc, y = lower_w(n,Sc)), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sc, y = upper_w(n,Sc)), color = "hotpink", size = 1, alpha = 0.7) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")

#Plot predikteret ifht. faktisk


# Tag evt. kvantiler af fordelingen ???


# LOO
# Summe pred intervaller
