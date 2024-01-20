#Figures with layout changed for the presentation:
#---------------------------Indlæsning af pakker og data:----------------------
library(readODS)
library(tidyverse)
library(ggplot2)
library(ggpubr)
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
leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

#------EDA-----------------------------------------------------

#Uden log-log transformationer

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.8, shape = 21, size = 1.5) + 
  theme_bw() +
  xlab('Crown Size')+
  ylab('Biomass')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'brown4', alpha = 0.1)+
  labs(title = "Leafs")+
  theme(legend.position = "none", plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15))

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen4', alpha = 0.8, shape = 21, size = 1.5) + 
  theme_bw() +
  xlab('Crown Size')+
  ylab('Biomass')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'brown4', alpha = 0.8)+
  labs(title = "Roots")+
  theme(legend.position = "none", plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15))

ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.8, shape = 21, size = 1.5) + 
  theme_bw() +
  xlab('Crown Size')+
  ylab('Biomass')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'brown4', alpha = 0.8)+
  labs(title = "Wood")+
  theme(legend.position = "none", plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15))

#Med log-log transformation

ggplot(leafs_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen3', alpha = 0.8, shape = 21, size = 1.5) + 
  theme_bw() +
  xlab('log(Crown Size)')+
  ylab('log(Biomass)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'brown4', alpha = 0.8)+
  labs(title = "Leafs")+
  theme(legend.position = "none", plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15))

ggplot(roots_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen', fill = 'darkolivegreen4', alpha = 0.8, shape = 21, size = 1.5) + 
  theme_bw() +
  xlab('log(Crown Size)')+
  ylab('log(Biomass)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'brown4', alpha = 0.8, alpha  = 0.8)+
  labs(title = "Roots")+
  theme(legend.position = "none", plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15))

ggplot(wood_log, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'darkolivegreen',fill = 'darkolivegreen3', alpha = 0.8, shape = 21, size = 1.5) + 
  theme_bw() +
  xlab('log(Crown Size)')+
  ylab('log(Biomass)')+
  geom_smooth(method = lm, se = FALSE, formula = y ~ x, color = 'brown4', alpha = 0.8)+
  labs(title = "Wood")+
  theme(legend.position = "none", plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15))


#------------All Models-------------------------------
###################################################################################################

#Lineære modeller af log-log

lm_log_l <- lm(Kgp ~ Sc, data = leafs_log)
lm_log_w <- lm(Kgp ~ Sc, data = wood_log)
lm_log_r <- lm(Kgp ~ Sc, data = roots_log)

#Lineære modeller

lm_l <- lm(Kgp ~ Sc, data = leafs)
lm_w <- lm(Kgp ~ Sc, data = wood)
lm_r <- lm(Kgp ~ Sc, data = roots)

#Log log OLS 

ols_log_l <- function(x) exp(lm_log_l$coefficients[[1]] + lm_log_l$coefficients[[2]]*log(x))
ols_log_w <- function(x) exp(lm_log_w$coefficients[[1]] + lm_log_w$coefficients[[2]]*log(x))
ols_log_r <- function(x) exp(lm_log_r$coefficients[[1]] + lm_log_r$coefficients[[2]]*log(x))

ols_log_adj_l <- function(x) exp(lm_log_l$coefficients[[1]] + 
                                   lm_log_l$coefficients[[2]]*log(x))*exp(var(lm_log_l$residuals)/2)
ols_log_adj_w <- function(x) exp(lm_log_w$coefficients[[1]] +
                                   lm_log_w$coefficients[[2]]*log(x))*exp(var(lm_log_w$residuals)/2)
ols_log_adj_r <- function(x) exp(lm_log_r$coefficients[[1]] + 
                                   lm_log_r$coefficients[[2]]*log(x))*exp(var(lm_log_r$residuals)/2)

#OLS 

ols_l <- function(x) lm_l$coefficients[[1]] + lm_l$coefficients[[2]]*x
ols_w <- function(x) lm_w$coefficients[[1]] + lm_w$coefficients[[2]]*x
ols_r <- function(x) lm_r$coefficients[[1]] + lm_r$coefficients[[2]]*x

#NLR

nlr_l <- function(x) 0.5687030*x^0.7160185
nlr_w <- function(x) 6.9505933*x^0.9842457
nlr_r <- function(x) 0.1206275*x^1.7372176


###################################################################################################


#Plot på alt data real scale

cols <- c("darkolivegreen","black", "brown4", "hotpink4")

ggplot(leafs, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'black', fill = 'darkolivegreen2', alpha = 0.6, size = 1.5, shape = 21) + 
  theme_bw() +
  xlab('Crown Size') + 
  geom_function(fun = ols_l, aes(col = "OLS"), size = 1)+
  geom_function(fun = nlr_l, aes(col = "NLR"), size = 1)+
  geom_function(fun = ols_log_l, aes(col = "OLS log"), size = 1)+
  geom_function(fun = ols_log_adj_l, aes(col = "OLS log Bias adj."), size = 1)+
  ylab('Biomass')+
  labs(title = "Leafs")+
  scale_colour_manual(values = cols)+
  theme(legend.title = element_blank(),
        legend.position = c(0.21, 0.82), legend.background = element_rect(linetype = 'solid', color = 'black'), plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15), legend.text = element_text(size = 13))+
  ylim(c(0,35))


ggplot(wood, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'black', fill = 'darkolivegreen2', alpha = 0.6, size = 1.5, shape = 21) + 
  theme_bw() +
  xlab('Crown Size') + 
  ylab('Biomass')+
  geom_function(fun = ols_w, aes(col = "OLS"), size = 1)+
  geom_function(fun = nlr_w, aes(col = "NLR"), size = 0.7)+
  geom_function(fun = ols_log_w, aes(col = "OLS log"), size = 0.7)+
  geom_function(fun = ols_log_adj_w, aes(col = "OLS log Bias adj."), size = 1)+
  labs(title = "Wood")+
  scale_colour_manual(values = cols)+
  theme(legend.title = element_blank(),
        legend.position = c(0.23, 0.81), legend.background = element_rect(linetype = 'solid', color = 'black'), plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15), legend.text = element_text(size = 13))+
  ylim(c(0,1050))

ggplot(roots, aes(x = Sc, y = Kgp)) + 
  geom_point(color = 'black', fill = 'darkolivegreen', alpha = 0.6, size = 2., shape = 21) + 
  theme_bw() +
  xlab('Crown Size') + 
  ylab('Biomass')+
  geom_function(fun = ols_r, aes(col = "OLS"), size = 0.7)+
  geom_function(fun = nlr_r, aes(col = "NLR"), size = 0.7)+
  geom_function(fun = ols_log_r, aes(col = "OLS log"), size = 0.7)+
  geom_function(fun = ols_log_adj_r, aes(col = "OLS log Bias adj."), size = 0.7)+
  ggtitle( "Roots")+
  scale_colour_manual(values = cols)+
  theme(legend.title = element_blank(),
        legend.position = c(0.22, 0.82), legend.background = element_rect(linetype = 'solid', color = 'black'), plot.title = element_text(size = 21),
        axis.title = element_text(size = 17), axis.text = element_text(size = 15), legend.text = element_text(size = 13))


#------------------The RF----------------------------------------
leafs_pred <- read.csv("Data/QRF_intervals_leafs.csv")
wood_pred <- read.csv("Data/QRF_intervals_wood.csv")
roots_pred <- read.csv("Data/QRF_intervals_roots.csv")

#-----------------MODEL--------------------------------------------
#Defining function to plot the prediction of the mean:
Sc_plot <- function(data,title){
  plot_data <- data.frame(data)
  #Plot
  ggplot(plot_data, aes(x = Sc, y = Kgp)) +
    geom_point(color = 'darkolivegreen', fill = 'darkolivegreen2', alpha = 0.8, size = 1.5, shape = 21) +
    geom_point(aes(y=predicted_value), color = 'brown4', fill = 'brown4' ,size = 1.5, shape = 23, alpha = 0.6) +
    ggtitle(title)+
    xlab('Crown Size') + 
    ylab('Biomass')+
    theme_bw()+
    theme(legend.position = "none", plot.title = element_text(size = 21),
          axis.title = element_text(size = 17), axis.text = element_text(size = 15))
}

Sc_plot(leafs_pred, "Leafs")
Sc_plot(wood_pred, "Wood")
Sc_plot(roots_pred, "Roots")



#---------------Prediction intervals---------------------------

#Score funktion Absolute error / sd hat(Y) (VIRKER IKKE SÅ GODT)
loo_pred_int <- function(data, alpha = 0.2, pred_int) {
  low <- c()
  high <- c()
  fitted <- c()
  for (i in (1:nrow(data))){
    print(i)
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

plot_maker <- function(pred_int, title, fun , roots = F){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((Low <= Kgp)&(Kgp <= High),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  if(roots == T){
    ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
      geom_segment(aes(x = Sc, y = Low, xend = Sc, yend = High),
                   color = "brown4", alpha = 0.2, lwd = 0.6) +
      geom_point(aes(x = Sc, y = High), color = "brown4", size = 1, alpha = 0.5) + 
      geom_point(aes(x = Sc, y = Low), color = "brown4", size = 1, alpha = 0.5) +
      geom_function(fun = fun, color = 'brown2', size = 0.8)+
      geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1.7, alpha = 1) +
      theme_bw() +
      xlab('Crown Size') + 
      ylab('Biomass')+
      labs(title = title)+
      scale_color_manual(values = color)+
      theme_bw()+
      theme(legend.position = "none", plot.title = element_text(size = 21),
            axis.title = element_text(size = 17), axis.text = element_text(size = 15))
  }
  else{
    ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
      geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1, alpha = 1) + 
      geom_point(aes(x = Sc, y = High), color = "brown4", size = 1, alpha = 0.5) + 
      geom_point(aes(x = Sc, y = Low), color = "brown4", size = 1, alpha = 0.5) +
      geom_function(fun = fun, color = 'brown2', size = 0.8)+
      theme_bw() +
      xlab('Crown Size') + 
      ylab('Biomass')+
      labs(title = title)+
      scale_color_manual(values = color)+
      theme_bw()+
      theme(legend.position = "none", plot.title = element_text(size = 21),
            axis.title = element_text(size = 17), axis.text = element_text(size = 15))
  }
  
}

set.seed(4)
loo2_adj <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
plot_maker(loo2_adj[[1]], "Leafs", ols_log_adj_l)
loo2_adj_w <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
plot_maker(loo2_adj_w[[1]], "Wood", ols_log_adj_w)
loo2_adj_r <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2_adj) 
plot_maker(loo2_adj_r[[1]], "Roots", ols_log_adj_r, roots = T)


#The RF:

#Defining function for plotting prediction intervals:
plot_maker <- function(pred_int, title){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((quantile..0.1 <= Kgp)&(Kgp <= quantile..0.9),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen2")
  
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    geom_segment(aes(x = Sc, y = quantile..0.1, xend = Sc, yend = quantile..0.9),
                 color = "brown4", alpha = 0.2, lwd = 0.6)+
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1.7, alpha = 0.7, fill) + 
    geom_point(aes(x = Sc, y = quantile..0.9), color = "brown4", size = 1, alpha = 0.5) + 
    geom_point(aes(x = Sc, y = quantile..0.1), color = "brown4", size = 1,, alpha = 0.5) +
    theme_bw() +
    xlab('Crown Size') + 
    ylab('Biomass')+
    labs(title = title)+
    scale_color_manual(values = color)+
    theme_bw()+
    theme(legend.position = "none", plot.title = element_text(size = 21),
          axis.title = element_text(size = 17), axis.text = element_text(size = 15))
}

plot_maker(leafs_pred, "Leafs")
plot_maker(wood_pred, "Wood")
plot_maker(roots_pred, "Roots")


#----------------Model diagnostics:
#For the RF:
#Rolling coverage:
roll_cov <- function(pred_int, alpha = 0.2, bin_size = 50, title){
  roll_cov <- c()
  
  data_arr <- pred_int %>%
    arrange(Sc)
  
  for (i in seq(1,nrow(data_arr)-bin_size)){
    data_cov <- data_arr %>%
      slice(i:(i+bin_size))
    roll_cov[i] <-mean((data_cov$quantile..0.1 <= data_cov$Kgp) & (data_cov$quantile..0.9 >= data_cov$Kgp))
  }
  
  my_tib <- tibble("Bin" = seq(1,nrow(data_arr)-bin_size), "Roll_cov" = roll_cov)
  
  up_binom <- qbinom(alpha/2, bin_size, 1-alpha)/bin_size 
  down_binom <- qbinom(1-alpha/2,bin_size,1-alpha)/bin_size
  
  ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
    geom_line(linewidth = 0.6, aes(color = Roll_cov)) + 
    geom_hline(yintercept = 1-alpha, color = "purple")+
    geom_hline(yintercept = up_binom, color = "purple", linetype = "dashed", linewidth = 0.3)+
    geom_hline(yintercept = down_binom, color = "purple", linetype = "dashed", linewidth = 0.3)+
    theme_bw() +
    xlab('Bin') + 
    ylab('Coverage')+
    ylim(c(0,1))+
    labs(title = title)+
    scale_color_gradient2(low = 'blue', mid = 'purple', high = 'red', midpoint = 0.8, limits = c(0.6,1),
                          na.value = "blue")+
    theme(legend.position = "none", plot.title = element_text(size = 21),
          axis.title = element_text(size = 17), axis.text = element_text(size = 15))
  
}


set.seed(4)
roll_cov(leafs_pred, alpha = 0.2, bin_size = 50, "Leafs")
roll_cov(wood_pred, alpha = 0.2, bin_size = 50, "Wood")
roll_cov(roots_pred, alpha = 0.2, bin_size = 5, "Roots")


#For the split conformal:

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
    geom_line(linewidth = 0.6, aes(color = Roll_cov)) + 
    geom_hline(yintercept = 1-alpha, color = "purple")+
    geom_hline(yintercept = up_binom, color = "purple", linetype = "dashed", linewidth = 0.3)+
    geom_hline(yintercept = down_binom, color = "purple", linetype = "dashed", linewidth = 0.3)+
    theme_bw() +
    xlab('Bin') + 
    ylab('Coverage')+
    ylim(c(0,1))+
    labs(title = title)+
    scale_color_gradient2(low = 'blue', mid = 'purple', high = 'red', midpoint = 0.8, limits = c(0.6,1),
                          na.value = "blue")+
    theme(legend.position = "none", plot.title = element_text(size = 21),
          axis.title = element_text(size = 17), axis.text = element_text(size = 15))
}


set.seed(4)

leafs_pred_int <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2_adj)
wood_pred_int <- loo_pred_int(wood, alpha = 0.2, pred_int_log_ols_conf_2_adj)
roots_pred_int <- loo_pred_int(roots, alpha = 0.2, pred_int_log_ols_conf_2_adj)

roll_cov(pred_int = leafs_pred_int, title = "Leafs")
roll_cov(pred_int = wood_pred_int, title = "Wood")
roll_cov(pred_int = roots_pred_int, title = "Roots", bin_size = 5)


#---------------Sums of Biomass:--------------------------

#----------------------------the OLS--------------
lower_l <- function(n,sc) qnorm(0.1, mean = hat_beta[1]*n + hat_alpha[1]*sc, sd = sqrt(n*var_hat[1]))
upper_l <- function(n,sc) qnorm(0.9, mean = hat_beta[1]*n + hat_alpha[1]*sc, sd = sqrt(n*var_hat[1]))
lower_w <- function(n,sc) qnorm(0.1, mean = hat_beta[2]*n + hat_alpha[2]*sc, sd = sqrt(n*var_hat[2]))
upper_w <- function(n,sc) qnorm(0.9, mean = hat_beta[2]*n + hat_alpha[2]*sc, sd = sqrt(n*var_hat[2]))

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


sum_plot_maker <- function (pred_int, title){
  pred_int <- pred_int %>%
    mutate(Indicator = if_else((Down <= Sum_Kgp)&(Sum_Kgp <= Up),"in", "out"))
  
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(pred_int, aes(x = Sum_Sc, y = Sum_Kgp)) + 
    geom_segment(aes(x = Sum_Sc, y = Down, xend = Sum_Sc, yend = Up),
                 color = "brown4", alpha = 0.2, lwd = 0.6) +
    geom_point(aes(x = Sum_Sc, y = Up), color = "brown4", size = 1, alpha = 0.5) + 
    geom_point(aes(x = Sum_Sc, y = Down), color = "brown4", size = 1, alpha = 0.5) +
    geom_point(aes(color = Indicator), alpha = 0.8, size = 2) + 
    theme_bw() +
    xlab('Sum of Crown Size') + 
    ylab('Sum of Biomass')+
    labs(title = title)+
    scale_color_manual(values = color)+
    theme(legend.position = "none", plot.title = element_text(size = 21),
          axis.title = element_text(size = 17), axis.text = element_text(size = 15))
}


sum_plot_maker(ols_leafs, "Leafs")
sum_plot_maker(ols_wood, "Wood")
sum_plot_maker(ols_roots, "Roots")


#------------------Wilkinsons method---------------------------------

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

sum_plot_maker(wilk_leafs, "Leafs")
sum_plot_maker(wilk_wood, "Wood")
sum_plot_maker(wilk_roots, "Roots")


#--------------QRF---------------------------------------
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
      train_y<- data[group != i,2]
      #print(train_x)
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
    #nodesize <- 10
    
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
  new_data <- data.frame(Sc = sum(group_data$Sc), 
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

colnames(leafs_pred) <- c("Sum_Kgp", "Down", "Up", "Sum_Sc", "Fitted", "nodesize")
colnames(wood_pred) <- c("Sum_Kgp", "Down", "Up", "Sum_Sc", "Fitted", "nodesize")
colnames(roots_pred) <- c("Sum_Kgp", "Down", "Up", "Sum_Sc", "Fitted", "nodesize")


mean(leafs_pred$Down <= leafs_pred$Sum_Kgp & leafs_pred$Sum_Kgp <= leafs_pred$Up)
mean(wood_pred$Down <= wood_pred$Sum_Kgp & wood_pred$Sum_Kgp <= wood_pred$Up)
mean(roots_pred$Down <= roots_pred$Sum_Kgp & roots_pred$Sum_Kgp <= roots_pred$Up)

sum_plot_maker(leafs_pred, "Leafs")
sum_plot_maker(wood_pred, "Wood")
sum_plot_maker(roots_pred, "Roots")


#------------------------------Simulation---------
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
    stat_function(fun = density_logn, color = "darkolivegreen", size = 1)+
    theme_bw()+
    labs(title = Title)+
    xlab("Biomass")+
    theme(legend.position = "none", plot.title = element_text(size = 21),
          axis.title = element_text(size = 17), axis.text = element_text(size = 15))
  
  return(list(sum_kgp,g))
}

set.seed(4)
leafs_full_sum <- simul(leafs, 3000, "Leafs")
wood_full_sum <- simul(wood, 3000, "Wood")
roots_full_sum <- simul(roots, 3000, "Roots")

leafs_full_sum[[2]]
wood_full_sum[[2]]
roots_full_sum[[2]]

