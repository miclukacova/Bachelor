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




##Det udkommenterede bruger vi ikke


################################################################################
## ------------------------- Creating sum data ------------------------------ ##
################################################################################

##Labelling data: 
#label_func <- function(data, num_groups = 150){
  group <- sample(rep(1:num_groups, length.out = nrow(data)))
  labbeled_data <- data %>% add_column(Group = group)
  return(labbeled_data)
}
#
#set.seed(4)
#leafs_labeled <- label_func(leafs)
#wood_labeled <- label_func(wood)
#roots_labeled <- label_func(roots, num_groups = 10)
#
##Summing data: 
#sum_func <- function(data) {
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
#
#sum_leafs <- sum_func(leafs_labeled)
#sum_wood <- sum_func(wood_labeled)
#
#for (i in c((1:26))){
    if (i == 1){
      group_data <- roots_labeled %>% filter(Group == 1)
      sum_roots <- tibble(n = nrow(group_data), Sc = sum(group_data$Sc), 
                         Kgp = sum(group_data$Kgp))
    }
    else {
      group_data <- roots_labeled %>% filter(Group == i)
      sum_roots <- sum_roots %>% add_row(n = nrow(group_data), Sc = sum(group_data$Sc), 
                                       Kgp = sum(group_data$Kgp))
  }
}
#
##Plots of summed data
##Det ser lineært ud
#
#ggplot(sum_leafs, aes(x = Sc, y = Kgp)) + 
#  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
#  theme_bw() +
#  xlab('Sc') + 
#  ylab('Kgp')+
#  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
#  labs(title = "Foliage")
#
#ggplot(sum_wood, aes(x = Sc, y = Kgp)) + 
#  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
#  theme_bw() +
#  xlab('Sc') + 
#  ylab('Kgp')+
#  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
#  labs(title = "Wood")
#
#ggplot(sum_roots, aes(x = Sc, y = Kgp)) + 
#  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
#  theme_bw() +
#  xlab('Sc') + 
#  ylab('Kgp')+
#  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'hotpink')+
#  labs(title = "Wood")
#
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
loo_adj_r_2 <- loo_pred_int(roots, alpha = 0.2, s2_logolsb_conf) 

#loo_adj_l_2[[1]] <- loo_adj_l_2[[1]] %>% add_column(Group = leafs_labeled$Group)
#loo_adj_w_2[[1]] <- loo_adj_w_2[[1]] %>% add_column(Group = wood_labeled$Group)
#loo_adj_r_2[[1]] <- loo_adj_r_2[[1]] %>% add_column(Group = roots_labeled$Group)

#sum_leafs_conf2 <- sum_pred_int_func(loo_adj_l_2[[1]])
#sum_wood_conf2 <- sum_pred_int_func(loo_adj_w_2[[1]])
#sum_roots_conf2 <- sum_pred_int_func(loo_adj_w_2[[1]])

#Split conformal score 1 logolsB on leafs and wood loo
loo_adj_l_1 <- loo_pred_int(leafs, alpha = 0.2, s1_logolsb_conf) 
loo_adj_w_1 <- loo_pred_int(wood, alpha = 0.2, s1_logolsb_conf) 
loo_adj_r_1 <- loo_pred_int(roots, alpha = 0.2, s1_logolsb_conf) 

#loo_adj_l_1[[1]] <- loo_adj_l_1[[1]] %>% add_column(Group = leafs_labeled$Group)
#loo_adj_w_1[[1]] <- loo_adj_w_1[[1]] %>% add_column(Group = wood_labeled$Group)
#loo_adj_r_1[[1]] <- loo_adj_r_1[[1]] %>% add_column(Group = roots_labeled$Group)

#sum_leafs_conf1 <- sum_pred_int_func(loo_adj_l_1[[1]])
#sum_wood_conf1 <- sum_pred_int_func(loo_adj_w_1[[1]])
#sum_roots_conf1 <- sum_pred_int_func(loo_adj_r_1[[1]])
#
#plot_func <- function(data,title) {
#  ggplot(data, aes(x = Sc, y = Kgp)) + 
#    geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
#    geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, shape = 3) + 
#    geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, shape = 3) +
#    theme_bw() +
#    xlab('Sc') + 
#    ylab('Kgp')+
#    labs(title = title)
#}
#
#plot_func(sum_leafs_conf1, "Leafs")
#plot_func(sum_wood_conf1, "Wood")
#plot_func(sum_roots_conf1, "Roots")
#
#plot_func(sum_leafs_conf2, "Leafs")
#plot_func(sum_wood_conf2, "Wood")
#plot_func(sum_roots_conf2, "Roots")
#
#mean(sum_leafs_conf1$Low <= sum_leafs_conf1$Kgp & sum_leafs_conf1$Kgp <= sum_leafs_conf1$High)
#mean(sum_wood_conf1$Low <= sum_wood_conf1$Kgp & sum_wood_conf1$Kgp <= sum_wood_conf1$High)
#mean(sum_roots_conf1$Low <= sum_roots_conf1$Kgp & sum_roots_conf1$Kgp <= sum_roots_conf1$High)
#
#mean(sum_leafs_conf2$Low <= sum_leafs_conf2$Kgp & sum_leafs_conf2$Kgp <= sum_leafs_conf2$High)
#mean(sum_wood_conf2$Low <= sum_wood_conf2$Kgp & sum_wood_conf2$Kgp <= sum_wood_conf2$High)
#mean(sum_roots_conf2$Low <= sum_roots_conf2$Kgp & sum_roots_conf2$Kgp <= sum_roots_conf2$High)

#qrf

leafs_pred <- read.csv("Data/QRF_intervals_leafs.csv")
wood_pred <- read.csv("Data/QRF_intervals_wood.csv")
roots_pred <- read.csv("Data/QRF_intervals_roots.csv")

#leafs_pred <- leafs_pred %>% add_column(Group = leafs_labeled$Group)
#wood_pred<- wood_pred %>% add_column(Group = wood_labeled$Group)
#roots_pred<- roots_pred %>% add_column(Group = roots_labeled$Group)
#
#colnames(leafs_pred) <- c("Kgp", "Low", "High", "Sc", "Fitted", "nodesize", "Group")
#colnames(wood_pred) <- c("Kgp", "Low", "High", "Sc", "Fitted", "nodesize", "Group")
#colnames(roots_pred) <- c("Kgp", "Low", "High", "Sc", "Fitted", "nodesize", "Group")
#
#sum_leafs_qrf <- sum_pred_int_func(leafs_pred)
#sum_wood_qrf <- sum_pred_int_func(wood_pred)
#sum_roots_qrf <- sum_pred_int_func(roots_pred)
#
#plot_func(sum_leafs_qrf, "Leafs")
#plot_func(sum_wood_qrf, "Wood")
#plot_func(sum_roots_qrf, "Roots")
#
#mean(sum_leafs_qrf$Low <= sum_leafs_qrf$Kgp & sum_leafs_qrf$Kgp <= sum_leafs_qrf$High)
#mean(sum_wood_qrf$Low <= sum_wood_qrf$Kgp & sum_wood_qrf$Kgp <= sum_wood_qrf$High)
#mean(sum_roots_qrf$Low <= sum_roots_qrf$Kgp & sum_roots_qrf$Kgp <= sum_roots_qrf$High)

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
agg_conf_roots2 <- agg_pred_int(loo_adj_r_2[[1]])
agg_conf_leafs1 <- agg_pred_int(loo_adj_l_1[[1]])
agg_conf_wood1 <- agg_pred_int(loo_adj_w_1[[1]])
agg_conf_roots1 <- agg_pred_int(loo_adj_r_1[[1]])
agg_qrf_leafs <- agg_pred_int(leafs_pred)
agg_qrf_wood <- agg_pred_int(wood_pred)
agg_qrf_roots <- agg_pred_int(roots_pred)

plot_data_leafs_agg <- tibble(High_s1 = agg_conf_leafs1$High, Low_s1 = agg_conf_leafs1$Low,
                              High_s2 = agg_conf_leafs2$High, Low_s2 = agg_conf_leafs2$Low,
                              High_qrf = agg_qrf_leafs$High, Low_qrf = agg_qrf_leafs$Low,
                              Kgp = agg_qrf_leafs$Kgp, Sc = agg_qrf_leafs$Sc)

plot_data_wood_agg <- tibble(High_s1 = agg_conf_wood1$High, Low_s1 = agg_conf_wood1$Low,
                              High_s2 = agg_conf_wood2$High, Low_s2 = agg_conf_wood2$Low,
                              High_qrf = agg_qrf_wood$High, Low_qrf = agg_qrf_wood$Low,
                              Kgp = agg_qrf_wood$Kgp, Sc = agg_qrf_wood$Sc)

plot_data_roots_agg <- tibble(High_s1 = agg_conf_roots1$High, Low_s1 = agg_conf_roots1$Low,
                             High_s2 = agg_conf_roots2$High, Low_s2 = agg_conf_roots2$Low,
                             High_qrf = agg_qrf_roots$High, Low_qrf = agg_qrf_roots$Low,
                             Kgp = agg_qrf_roots$Kgp, Sc = agg_qrf_roots$Sc)

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

ggplot(plot_data_roots_agg, aes(x = Sc, y = Kgp)) + 
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
  labs(title = "Roots")

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


#LOO Testing method

label_func <- function(data, num_groups = 50){
  group <- sample(rep(1:num_groups, length.out = nrow(data)))
  labbeled_data <- data %>% add_column(Group = group)
  return(labbeled_data)
}

pred_int_ols <- function(data, alpha, num_groups){
  
  #Grouping
  labeled_data <- label_func(data, num_groups = num_groups)
  
  for (i in (1:num_groups)){
    #Test/train split
    test <- filter(labeled_data, Group == i)
    train <- filter(labeled_data, Group != i)
    
    #Model fit
    lm_data <- lm(Kgp ~ Sc, data = train)
    alpha_hat = lm_data$coefficients[[2]]; beta_hat = lm_data$coefficients[[1]]; var_hat = var(lm_data$residuals)

    #Quantiles
    lower_l <- function(n,sc) qnorm(p = alpha/2, mean = beta_hat*n + alpha_hat*sc, sd = sqrt(n*var_hat))
    upper_l <- function(n,sc) qnorm(p = 1-alpha/2, mean = beta_hat*n + alpha_hat*sc, sd = sqrt(n*var_hat))
    
    if (i == 1){
      pred_int <- tibble(Up = upper_l(nrow(test), sum(test$Sc)), Down = lower_l(nrow(test), sum(test$Sc)), 
                         Sum_Sc = sum(test$Sc), Sum_Kgp = sum(test$Kgp))
    }
    else {
      pred_int <- pred_int %>% add_row(Up = upper_l(nrow(test), sum(test$Sc)), 
                                       Down = lower_l(nrow(test), sum(test$Sc)),
                                       Sum_Sc = sum(test$Sc), Sum_Kgp = sum(test$Kgp))
    }
  }
  return(pred_int)
}

set.seed(4)
ols_leafs <- pred_int_ols(leafs, 0.2, num_groups = 30)
ols_wood <- pred_int_ols(wood, 0.2, num_groups = 30)
ols_roots <- pred_int_ols(roots, 0.2, num_groups = 5)

mean(ols_leafs$Down <= ols_leafs$Sum_Kgp & ols_leafs$Sum_Kgp <= ols_leafs$Up)
mean(ols_wood$Down <= ols_wood$Sum_Kgp & ols_wood$Sum_Kgp <= ols_wood$Up)
mean(ols_roots$Down <= ols_roots$Sum_Kgp & ols_roots$Sum_Kgp <= ols_roots$Up)


ggplot(ols_leafs, aes(x = Sum_Sc, y = Sum_Kgp)) + 
  geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
               color = "hotpink", alpha = 0.4, lwd = 0.6) +
  geom_point(aes(x = Sum_Sc, y = Up), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sum_Sc, y = Down), color = "hotpink", size = 1, alpha = 0.7) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sum of Sc') + 
  ylab('Sum og Kgp')+
  labs(title = "Leafs")

ggplot(ols_wood, aes(x = Sum_Sc, y = Sum_Kgp)) + 
  geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
               color = "hotpink", alpha = 0.4, lwd = 0.6) +
  geom_point(aes(x = Sum_Sc, y = Up), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sum_Sc, y = Down), color = "hotpink", size = 1, alpha = 0.7) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sum of Sc') + 
  ylab('Sum og Kgp')+
  labs(title = "Wood")

ggplot(wilk_roots, aes(x = Sum_Sc, y = Sum_Kgp)) + 
  geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
               color = "hotpink", alpha = 0.4, lwd = 0.6) +
  geom_point(aes(x = Sum_Sc, y = Up), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sum_Sc, y = Down), color = "hotpink", size = 1, alpha = 0.7) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sum of Sc') + 
  ylab('Sum og Kgp')+
  labs(title = "Roots")



# Gaussiske pred. intervaller

f_hat <- function(x) beta_hat + x*alpha_hat

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



################################################################################
## ----------------------------- sum model logOLS --------------------------- ##
################################################################################

# Wilkinson
#For 900 trees

simul <- function(data, B = 3000, Title) {
  lm_data <- lm(log(Kgp) ~ log(Sc), data = data)
  alpha_hat = lm_data$coefficients[[2]]; beta_hat = lm_data$coefficients[[1]]; var_hat = var(lm_data$residuals)
  sum_kgp <- c()
  for (i in (1:B)){
    new_kgp <- c()
    for (j in (1:nrow(data))){
      new_kgp[j] <- rlnorm(n = 1, log(data$Sc[j])*alpha_hat+beta_hat, sqrt(var_hat))
    }
    sum_kgp[i] <- sum(new_kgp)
  }
  
  mu_s <- sum(exp(log(data$Sc)*alpha_hat+beta_hat)*exp(var_hat/2))
  v_s <- sum(exp(2*(log(data$Sc)*alpha_hat+beta_hat)+var_hat)*(exp(var_hat)-1))
  
  mu_s_n <- log(mu_s^2/(sqrt(mu_s^2+v_s)))
  sigma_s_n <- log(1+v_s/mu_s^2)
  
  density_logn <- function(x) dlnorm(x, mu_s_n, sqrt(sigma_s_n))
  
  g <- tibble(Kgp = sum_kgp) %>%
    ggplot() +
    geom_histogram(aes(x = Kgp, y = ..density..), color = "white", fill = "darkolivegreen3", bins = 50) +
    #xlim(c(0,2*10^8))+
    stat_function(fun = density_logn, color = "darkolivegreen", size = 1)+
    theme_bw()+
    labs(title = Title)
  
  return(list(sum_kgp,g))
}

set.seed(4)
leafs_full_sum <- simul(leafs, 3000, "Leafs")
wood_full_sum <- simul(wood, 3000, "Wood")
roots_full_sum <- simul(roots, 3000, "Roots")

leafs_full_sum[[2]]
wood_full_sum[[2]]
roots_full_sum[[2]]

#For 30 trees

set.seed(4)
leafs_30 <- sample_n(leafs, size = 30)
wood_25 <- sample_n(wood, 30, replace = TRUE)
roots_5 <- sample_n(roots, 5, replace = TRUE)

set.seed(4)
leafs_30_sum <- simul(leafs_30, 3000, "Leafs")
wood_25_sum <- simul(wood_25, 3000, "Wood")
roots_10_sum <- simul(roots_5, 3000, "Roots")

leafs_30_sum[[2]]
wood_25_sum[[2]]
roots_10_sum[[2]]


#qqplots

#Leafs

lm_data <- lm(log(Kgp) ~ log(Sc), data = leafs)
alpha_hat = lm_data$coefficients[[2]]; beta_hat = lm_data$coefficients[[1]]; var_hat = var(lm_data$residuals)
mu_s <- sum(exp(log(leafs$Sc)*alpha_hat+beta_hat)*exp(var_hat/2))
v_s <- sum(exp(2*(log(leafs$Sc)*alpha_hat+beta_hat)+var_hat)*(exp(var_hat)-1))

mu_s_n <- log(mu_s^2/(sqrt(mu_s^2+v_s)))
sigma_s_n <- log(1+v_s/mu_s^2)

ggplot(tibble(Kgp = leafs_full_sum[[1]]), aes(sample=Kgp))+
  stat_qq(distribution = qlnorm, dparams = c(mu_s_n,sqrt(sigma_s_n)))+
  stat_qq_line(distribution = qlnorm, dparams = c(mu_s_n,sqrt(sigma_s_n)))+
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Leafs")

#Wood

lm_data <- lm(log(Kgp) ~ log(Sc), data = wood)
alpha_hat = lm_data$coefficients[[2]]; beta_hat = lm_data$coefficients[[1]]; var_hat = var(lm_data$residuals)
mu_s <- sum(exp(log(wood$Sc)*alpha_hat+beta_hat)*exp(var_hat/2))
v_s <- sum(exp(2*(log(wood$Sc)*alpha_hat+beta_hat)+var_hat)*(exp(var_hat)-1))

mu_s_n <- log(mu_s^2/(sqrt(mu_s^2+v_s)))
sigma_s_n <- log(1+v_s/mu_s^2)

ggplot(tibble(Kgp = wood_full_sum[[1]]), aes(sample=Kgp))+
  stat_qq(distribution = qlnorm, dparams = c(mu_s_n,sqrt(sigma_s_n)))+
  stat_qq_line(distribution = qlnorm, dparams = c(mu_s_n,sqrt(sigma_s_n)))+
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Wood")

#Roots

lm_data <- lm(log(Kgp) ~ log(Sc), data = roots)
alpha_hat = lm_data$coefficients[[2]]; beta_hat = lm_data$coefficients[[1]]; var_hat = var(lm_data$residuals)
mu_s <- sum(exp(log(roots$Sc)*alpha_hat+beta_hat)*exp(var_hat/2))
v_s <- sum(exp(2*(log(roots$Sc)*alpha_hat+beta_hat)+var_hat)*(exp(var_hat)-1))

mu_s_n <- log(mu_s^2/(sqrt(mu_s^2+v_s)))
sigma_s_n <- log(1+v_s/mu_s^2)

ggplot(tibble(Kgp = roots_full_sum[[1]]), aes(sample=Kgp))+
  stat_qq(distribution = qlnorm, dparams = c(mu_s_n,sqrt(sigma_s_n)))+
  stat_qq_line(distribution = qlnorm, dparams = c(mu_s_n,sqrt(sigma_s_n)))+
  geom_abline(intercept = 0, slope = 1, color = "darkolivegreen", size = 0.9)+
  theme_bw()+
  labs(title = "Roots")


#Testing method

label_func <- function(data, num_groups = 50){
  group <- sample(rep(1:num_groups, length.out = nrow(data)))
  labbeled_data <- data %>% add_column(Group = group)
  return(labbeled_data)
}

pred_int_wilkinson <- function(data, alpha, num_groups){
  
  #Grouping
  labeled_data <- label_func(data, num_groups = num_groups)
  print(labeled_data)
  
  
  for (i in (1:num_groups)){
    #Test/train split
    test <- filter(labeled_data, Group == i)
    train <- filter(labeled_data, Group != i)
    
    #Approximation parameters
    lm_data <- lm(log(Kgp) ~ log(Sc), data = train)
    alpha_hat = lm_data$coefficients[[2]]; beta_hat = lm_data$coefficients[[1]]; var_hat = var(lm_data$residuals)
    
    #Approximation distribution
    mu_s <- sum(exp(log(test$Sc)*alpha_hat+beta_hat)*exp(var_hat/2))
    v_s <- sum(exp(2*(log(test$Sc)*alpha_hat+beta_hat)+var_hat)*(exp(var_hat)-1))
    mu_s_n <- log(mu_s^2/(sqrt(mu_s^2+v_s)))
    sigma_s_n <- log(1+v_s/mu_s^2)
    
    #Pred int.
    Up <- qlnorm(1-alpha/2, meanlog = mu_s_n, sdlog = sqrt(sigma_s_n))
    Down <- qlnorm(alpha/2, meanlog = mu_s_n, sdlog = sqrt(sigma_s_n))
    
    if (i == 1){
      pred_int <- tibble(Up = Up, Down = Down, Sum_Sc = sum(test$Sc), Sum_Kgp = sum(test$Kgp))
    }
    else {
      pred_int <- pred_int %>% add_row(Up = Up, Down = Down, Sum_Sc = sum(test$Sc), Sum_Kgp = sum(test$Kgp))
    }
  }
  return(pred_int)
}


set.seed(4)
wilk_leafs <- pred_int_wilkinson(leafs, 0.2, num_groups = 30)
wilk_wood <- pred_int_wilkinson(wood, 0.2, num_groups = 30)
wilk_roots <- pred_int_wilkinson(roots, 0.2, num_groups = 5)

mean(wilk_leafs$Down <= wilk_leafs$Sum_Kgp & wilk_leafs$Sum_Kgp <= wilk_leafs$Up)
mean(wilk_wood$Down <= wilk_wood$Sum_Kgp & wilk_wood$Sum_Kgp <= wilk_wood$Up)
mean(wilk_roots$Down <= wilk_roots$Sum_Kgp & wilk_roots$Sum_Kgp <= wilk_roots$Up)


ggplot(wilk_leafs, aes(x = Sum_Sc, y = Sum_Kgp)) + 
  geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
               color = "hotpink", alpha = 0.4, lwd = 0.6) +
  geom_point(aes(x = Sum_Sc, y = Up), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sum_Sc, y = Down), color = "hotpink", size = 1, alpha = 0.7) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sum of Sc') + 
  ylab('Sum og Kgp')+
  labs(title = "Leafs")

ggplot(wilk_wood, aes(x = Sum_Sc, y = Sum_Kgp)) + 
  geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
               color = "hotpink", alpha = 0.4, lwd = 0.6) +
  geom_point(aes(x = Sum_Sc, y = Up), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sum_Sc, y = Down), color = "hotpink", size = 1, alpha = 0.7) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sum of Sc') + 
  ylab('Sum og Kgp')+
  labs(title = "Wood")

ggplot(wilk_roots, aes(x = Sum_Sc, y = Sum_Kgp)) + 
  geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
               color = "hotpink", alpha = 0.4, lwd = 0.6) +
  geom_point(aes(x = Sum_Sc, y = Up), color = "hotpink", size = 1, alpha = 0.7) + 
  geom_point(aes(x = Sum_Sc, y = Down), color = "hotpink", size = 1, alpha = 0.7) +
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.6, shape = 21) + 
  theme_bw() +
  xlab('Sum of Sc') + 
  ylab('Sum og Kgp')+
  labs(title = "Roots")
  
  
# To spørgesmål
# hvor godt simulere metoden en lognormal fordeling? og hvor godt simulere den vores data?



################################################################################
## --------------------------------------- QRF ------------------------------ ##
################################################################################
library(randomForest)
library(quantregForest)


#---------------Fitting the QRF and calculating predictions + intervals:------

#Defining the nodegrids to be searched:
node_grid_l <- seq(2,30)
node_grid_w <- seq(2,30)
node_grid_r <-c(1,2,3,4,5,6)

leafs_pred <- loo_rf(leafs_qrf, node_grid_l)

#Function performing CV to choose minimal nodesize
node_size_choose <- function(data, k = 3, node_grid) {
  set.seed(4)
  MSE_ns <- c()
  group <- sample(rep(1:k, length.out = nrow(data)))
  for (j in node_grid){
    MSE <- c()
    for (i in (1:k)){
      #Fit model
      train_x <- data.frame(Sc = data[group != i,1])
      train_y<- data[group != i,]$Kgp
      qrf_ns <- quantregForest(x = train_x, y = train_y, nodesize = j)
      
      #MSE
      conditionalMean_ns <- predict(qrf_ns, data.frame(Sc = data[group == i,1]), what = mean)
      MSE[i] <- mean((conditionalMean_ns - data[group == i,2])^2)
    }
    MSE_ns <- append(MSE_ns, mean(MSE)) 
  }
  node_size <- node_grid[which.min(MSE_ns)]
  return(node_size)
}


#LOOCV-function that produces predictions for both mean and intervals:
loo_rf <- function(data, node_grid) {
  set.seed(777)
  pred <- c() ; obs <- c() ; q0.1 <- c() ; q0.9 <- c() ;mean_cv <- c(); node_size <- c()
  for (i in (1:nrow(data))){
    print(i)
    #choose nodesize
    nodesize <- node_size_choose(data = data, node_grid = node_grid)
    
    
    #Fit model
    train_x <- data.frame(Sc = data[-i,1])
    train_y <- data[-i,2]
    test_x <- data.frame(Sc=data[i,1])
    test_y <- data[i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = nodesize)
    
    #Getting quantiles
    conditionalQuantiles_cv <- predict(qrf_cv, test_x, what = c(0.1,0.9))
    conditionalMean_cv <- predict(qrf_cv, test_x, what = mean)
    
    #Creating vectors with observations and quantiles
    pred    <- append(pred, data[i,1])
    obs     <- append(obs, test_y)
    q0.1    <- append(q0.1, conditionalQuantiles_cv[,1])
    q0.9    <- append(q0.9, conditionalQuantiles_cv[,2])
    mean_cv <- append(mean_cv, conditionalMean_cv)
    node_size <- append(node_size, nodesize)
  }
  return("obs_int" = data.frame(Kgp = obs, quantile..0.1 = q0.1,quantile..0.9 = q0.9, Sc=pred, predicted_value = mean_cv, nodesize = nodesize))
}


#Labelling data: 
label_func <- function(data, num_groups = 30){
  group <- sample(rep(1:num_groups, length.out = nrow(data)))
  labbeled_data <- data %>% add_column(Group = group)
  return(labbeled_data)
}

set.seed(4)
leafs_labeled <- label_func(leafs)
wood_labeled <- label_func(wood)
roots_labeled <- label_func(roots, num_groups = 10)

#Summing data: 
sum_func <- function(data, num_groups) {
  group_data <- data %>% filter(Group == 1)
  new_data <- tibble(Sc = sum(group_data$Sc), 
                     Kgp = sum(group_data$Kgp))
  for (i in (2:num_groups)){
    group_data <- data %>% filter(Group == i)
    new_data <- new_data %>% add_row(Sc = sum(group_data$Sc), 
                                     Kgp = sum(group_data$Kgp))
  }
  return(new_data)
}

leafs_qrf <- sum_func(leafs_labeled, num_groups = 30)
wood_qrf <- sum_func(wood_labeled, num_groups = 30)
roots_qrf <- sum_func(roots_labeled, num_groups = 10)

leafs_pred <- loo_rf(leafs_qrf, node_grid_l)
wood_pred <- loo_rf(wood_qrf, node_grid_w)
roots_pred <- loo_rf(roots_qrf, node_grid_r)
