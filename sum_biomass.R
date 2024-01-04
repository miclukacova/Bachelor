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
loo_adj_l_2 <- loo_pred_int(leafs, alpha = 0.2, s2_logolsb_conf) 
loo_adj_w_2 <- loo_pred_int(wood, alpha = 0.2, s2_logolsb_conf) 

loo_adj_l_2[[1]] <- loo_adj_l_2[[1]] %>% add_column(Group = leafs_labeled$Group)
loo_adj_w_2[[1]] <- loo_adj_w_2[[1]] %>% add_column(Group = wood_labeled$Group)

sum_leafs_conf2 <- sum_pred_int_func(loo_adj_l_2[[1]])
sum_wood_conf2 <- sum_pred_int_func(loo_adj_w_2[[1]])

#Split conformal score 1 logolsB on leafs and wood loo
loo_adj_l_1 <- loo_pred_int(leafs, alpha = 0.2, s1_logolsb_conf) 
loo_adj_w_1 <- loo_pred_int(wood, alpha = 0.2, s1_logolsb_conf) 

loo_adj_l_1[[1]] <- loo_adj_l_1[[1]] %>% add_column(Group = leafs_labeled$Group)
loo_adj_w_1[[1]] <- loo_adj_w_1[[1]] %>% add_column(Group = wood_labeled$Group)

sum_leafs_conf1 <- sum_pred_int_func(loo_adj_l_1[[1]])
sum_wood_conf1 <- sum_pred_int_func(loo_adj_w_1[[1]])

plot_func <- function(data,title) {
  ggplot(data, aes(x = Sc, y = Kgp)) + 
    geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
    geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, shape = 3) + 
    geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, shape = 3) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)
}

plot_func(sum_leafs_conf1, "Leafs")
plot_func(sum_wood_conf1, "Wood")

plot_func(sum_leafs_conf2, "Leafs")
plot_func(sum_wood_conf2, "Wood")

mean(sum_leafs_conf1$Low <= sum_leafs_conf1$Kgp & sum_leafs_conf1$Kgp <= sum_leafs_conf1$High)
mean(sum_wood_conf1$Low <= sum_wood_conf1$Kgp & sum_wood_conf1$Kgp <= sum_wood_conf1$High)

mean(sum_leafs_conf2$Low <= sum_leafs_conf2$Kgp & sum_leafs_conf2$Kgp <= sum_leafs_conf2$High)
mean(sum_wood_conf2$Low <= sum_wood_conf2$Kgp & sum_wood_conf2$Kgp <= sum_wood_conf2$High)

#qrf

leafs_pred <- read.csv("Data/QRF_intervals_leafs.csv")
wood_pred <- read.csv("Data/QRF_intervals_wood.csv")

leafs_pred <- leafs_pred %>% add_column(Group = leafs_labeled$Group)
wood_pred<- wood_pred %>% add_column(Group = wood_labeled$Group)

colnames(leafs_pred) <- c("Kgp", "Low", "High", "Sc", "Fitted", "nodesize", "Group")
colnames(wood_pred) <- c("Kgp", "Low", "High", "Sc", "Fitted", "nodesize", "Group")

sum_leafs_qrf <- sum_pred_int_func(leafs_pred)
sum_wood_qrf <- sum_pred_int_func(wood_pred)

plot_func(sum_leafs_qrf, "Leafs")
plot_func(sum_wood_qrf, "Wood")

mean(sum_leafs_qrf$Low <= sum_leafs_qrf$Kgp & sum_leafs_qrf$Kgp <= sum_leafs_qrf$High)
mean(sum_wood_qrf$Low <= sum_wood_qrf$Kgp & sum_wood_qrf$Kgp <= sum_wood_qrf$High)


#Udvikling af prediction intervals

agg_pred_int <- function(pred_int){
  agg_kgp <- c(); agg_sc <- c(); upp <- c(); down <- c(); agg_pred <- c()
  for (i in (1:nrow(pred_int))){
    agg_pred[i] <- sum(pred_int$Fitted[1:i])
    agg_kgp[i] <- sum(pred_int$Kgp[1:i])
    agg_sc[i] <- sum(pred_int$Sc[1:i])
    upp[i] <- sum(pred_int$High[1:i])
    down[i] <- sum(pred_int$Low[1:i])
  }
  return(tibble(Sc = agg_sc, Kgp = agg_kgp, High = upp, Low = down, Pred = agg_pred))
}

agg_conf_leafs2 <- agg_pred_int(loo_adj_l_2[[1]])
agg_conf_wood2 <- agg_pred_int(loo_adj_w_2[[1]])
agg_conf_leafs1 <- agg_pred_int(loo_adj_l_1[[1]])
agg_conf_wood1 <- agg_pred_int(loo_adj_w_1[[1]])
agg_qrf_leafs <- agg_pred_int(leafs_pred)
agg_qrf_wood <- agg_pred_int(wood_pred)

plot_data_leafs_agg <- tibble(High_s1 = agg_conf_leafs1$High, Low_s1 = agg_conf_leafs1$Low,
                              High_s2 = agg_conf_leafs2$High, Low_s2 = agg_conf_leafs2$Low,
                              High_qrf = agg_qrf_leafs$High, Low_qrf = agg_qrf_leafs$Low,
                              Kgp = agg_qrf_leafs$Kgp, Sc = agg_qrf_leafs$Sc)

plot_data_wood_agg <- tibble(High_s1 = agg_conf_wood1$High, Low_s1 = agg_conf_wood1$Low,
                              High_s2 = agg_conf_wood2$High, Low_s2 = agg_conf_wood2$Low,
                              High_qrf = agg_qrf_wood$High, Low_qrf = agg_qrf_wood$Low,
                              Kgp = agg_qrf_wood$Kgp, Sc = agg_qrf_wood$Sc)

cols <- c("darkolivegreen","black", "hotpink2", "hotpink4")

ggplot(plot_data_leafs_agg, aes(x = Sc, y = Kgp)) + 
  geom_line(aes(col = "Actual Kgp")) + 
  geom_line(aes(x = Sc, y = High_s2, col = "Conf score 2")) +
  geom_line(aes(x = Sc, y = High_s1, col = "Conf score 1")) + 
  geom_line(aes(x = Sc, y = High_qrf, col = "QRF")) +
  geom_line(aes(x = Sc, y = Low_s1, col = "Conf score 2")) +
  geom_line(aes(x = Sc, y = Low_s2, col = "Conf score 1")) +
  geom_line(aes(x = Sc, y = Low_qrf, col = "QRF")) +
  scale_colour_manual(values = cols)+
  theme_bw() +
  theme(plot.title = element_text(size = 17),
          axis.title = element_text(size = 13), 
        legend.background = element_rect(linetype = 'solid', color = 'black'),
        legend.position = c(0.15, 0.77))+
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")


ggplot(plot_data_wood_agg, aes(x = Sc, y = Kgp)) + 
  geom_line(aes(col = "Actual Kgp")) + 
  geom_line(aes(x = Sc, y = High_s2, col = "Conf score 2")) +
  geom_line(aes(x = Sc, y = High_s1, col = "Conf score 1")) + 
  geom_line(aes(x = Sc, y = High_qrf, col = "QRF")) +
  geom_line(aes(x = Sc, y = Low_s1, col = "Conf score 2")) +
  geom_line(aes(x = Sc, y = Low_s2, col = "Conf score 1")) +
  geom_line(aes(x = Sc, y = Low_qrf, col = "QRF")) +
  scale_colour_manual(values = cols)+
  theme_bw() +
  theme(plot.title = element_text(size = 17),
        axis.title = element_text(size = 13), 
        legend.background = element_rect(linetype = 'solid', color = 'black'),
        legend.position = c(0.15, 0.77))+
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")

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


# Wilkinson

#For 900 trees

B <- 3000
sum_kgp <- c()

for (i in (1:B)){
  new_kgp <- c()
  for (j in (1:nrow(leafs))){
    new_kgp[j] <- rlnorm(n = 1, log(leafs$Sc[j])*hat_alpha[1]+hat_beta[1], sqrt(var_hat[1]))
  }
  sum_kgp[i] <- sum(new_kgp)
}

mu_s <- sum(exp(log(leafs$Sc)*hat_alpha[1]+hat_beta[1])*exp(var_hat[1]/2))
v_s <- sum(exp(2*(log(leafs$Sc)*hat_alpha[1]+hat_beta[1])+var_hat[1])*(exp(var_hat[1])-1))

mu_s_n <- log(mu_s^2/(sqrt(mu_s^2+v_s)))
sigma_s_n <- log(1+v_s/mu_s^2)

density_logn_900 <- function(x) dlnorm(x, mu_s_n, sqrt(sigma_s_n))

density_logn_900(mu_s)

tibble(Kgp = sum_kgp) %>%
  ggplot() +
  geom_histogram(aes(x = Kgp, y = ..density..), color = "white", fill = "darkolivegreen3", bins = 50) +
  #xlim(c(0,2*10^8))+
  stat_function(fun = density_logn_900, color = "darkolivegreen", size = 1)+
  theme_bw()+
  labs(title = "Leafs")
