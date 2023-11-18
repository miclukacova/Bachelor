#################################--Indlæsning af pakker og data---###############################

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

library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)
###################################################################################################

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = train_leafs_log)
lm_roots_log <- lm(Kgp ~ Sc, data = train_roots_log)
lm_wood_log <- lm(Kgp ~ Sc, data = train_wood_log)


#Estimater

hat_beta <- c(lm_leafs_log$coefficients[[1]],
              lm_roots_log$coefficients[[1]],
              lm_wood_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
               lm_roots_log$coefficients[[2]],
               lm_wood_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_roots_log$residuals),
             var(lm_wood_log$residuals))


#log_hat estimater uden bias correction:

f_hat_leafs <- function(x) hat_beta[1] + hat_alpha[1]*x
f_hat_roots <- function(x) hat_beta[2] + hat_alpha[2]*x
f_hat_wood <- function(x) hat_beta[3] + hat_alpha[3]*x

#Y_hat estimater med bias correction: 
f_hat_leafs_exp_adj <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)*exp(var_hat[1]/2)
f_hat_roots_exp_adj <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)*exp(var_hat[2]/2)
f_hat_wood_exp_adj <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)*exp(var_hat[3]/2)


#Prediction intervals log scale

alpha <- 0.1

upper_leafs <- function(x) {
  f_hat_leafs(x) - qt(alpha/2, nrow(train_leafs_log)-2)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sqrt(var_hat[1])
}
lower_leafs <- function(x) {
  f_hat_leafs(x) - qt(1-alpha/2, nrow(train_leafs_log)-2)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sqrt(var_hat[1])
}
upper_wood <- function(x) {
  f_hat_wood(x) - qt(alpha/2, nrow(train_wood_log)-2)*sqrt(x^2/sum(train_wood_log$Sc^2)+1)*sqrt(var_hat[3])
}
lower_wood <- function(x) {
  f_hat_wood(x) - qt(1-alpha/2, nrow(train_wood_log)-2)*sqrt(x^2/sum(train_wood_log$Sc^2)+1)*sqrt(var_hat[3])
}
upper_roots <- function(x) {
  f_hat_roots(x) - qt(alpha/2, nrow(train_roots_log)-2)*sqrt(x^2/sum(train_roots_log$Sc^2)+1)*sqrt(var_hat[3])
}
lower_roots <- function(x) {
  f_hat_roots(x) - qt(1-alpha/2, nrow(train_roots_log)-2)*sqrt(x^2/sum(train_roots_log$Sc^2)+1)*sqrt(var_hat[3])
}


#Plot with prediction intervals log scale

test_leafs_log_plot <- test_leafs_log %>%
  mutate(Indicator = if_else((lower_leafs(test_leafs_log$Sc) <= test_leafs_log$Kgp)&
                               (test_leafs_log$Kgp <= upper_leafs(test_leafs_log$Sc)),"in", "out"))

test_roots_log_plot <- test_roots_log %>%
  mutate(Indicator = if_else((lower_roots(test_roots_log$Sc) <= test_roots_log$Kgp)&
                               (test_roots_log$Kgp <= upper_roots(test_roots_log$Sc)),"in", "out"))

test_wood_log_plot <- test_wood_log %>%
  mutate(Indicator = if_else((lower_wood(test_wood_log$Sc) <= test_wood_log$Kgp)&
                               (test_wood_log$Kgp <= upper_wood(test_wood_log$Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_leafs, colour = "hotpink1") +
  geom_function(fun = upper_leafs, colour = "hotpink4") +
  geom_function(fun = lower_leafs, colour = "hotpink4") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_roots_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_roots, colour = "hotpink1") +
  geom_function(fun = upper_roots, colour = "hotpink4") +
  geom_function(fun = lower_roots, colour = "hotpink4") +
  labs(title = "roots")+
  scale_color_manual(values = color)

ggplot(test_wood_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_wood, colour = "hotpink1") +
  geom_function(fun = upper_wood, colour = "hotpink4") +
  geom_function(fun = lower_wood, colour = "hotpink4") +
  labs(title = "wood")+
  scale_color_manual(values = color)

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp &upper(data$Sc) >= data$Kgp)
}

a <- coverage(test_leafs_log, upper_leafs, lower_leafs)
b <- coverage(test_wood_log, upper_wood, lower_wood)
c <- coverage(test_roots_log, upper_roots, lower_roots)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)

#Plot with prediction intervals real scale

#Y_hat estimater uden bias correction: 
f_hat_leafs_exp <- function(x) exp(hat_beta[1])*x^hat_alpha[1]
f_hat_roots_exp <- function(x) exp(hat_beta[2])*x^hat_alpha[2]
f_hat_wood_exp <- function(x) exp(hat_beta[3])*x^hat_alpha[3]

upper_leafs_exp <- function(x) exp(upper_leafs(log(x)))
lower_leafs_exp <- function(x) exp(lower_leafs(log(x)))
upper_wood_exp <- function(x) exp(upper_wood(log(x)))
lower_wood_exp <- function(x) exp(lower_wood(log(x)))
upper_roots_exp <- function(x) exp(upper_roots(log(x)))
lower_roots_exp <- function(x) exp(lower_roots(log(x)))

test_leafs_plot <- test_leafs %>%
  mutate(Indicator = if_else((lower_leafs_exp(Sc) <= Kgp)&
                               (Kgp <= upper_leafs_exp(Sc)),"in", "out"))

test_wood_plot <- test_wood %>%
  mutate(Indicator = if_else((lower_wood_exp(Sc) <= Kgp)&
                               (Kgp <= upper_wood_exp(Sc)),"in", "out"))

test_roots_plot <- test_roots %>%
  mutate(Indicator = if_else((lower_roots_exp(Sc) <= Kgp)&
                               (Kgp <= upper_roots_exp(Sc)),"in", "out"))


color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_leafs_exp, colour = "hotpink1") +
  geom_function(fun = upper_leafs_exp, colour = "hotpink4") +
  geom_function(fun = lower_leafs_exp, colour = "hotpink4") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_roots_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_roots_exp, colour = "hotpink1") +
  geom_function(fun = upper_roots_exp, colour = "hotpink4") +
  geom_function(fun = lower_roots_exp, colour = "hotpink4") +
  labs(title = "Roots")+
  scale_color_manual(values = color)

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_wood_exp, colour = "hotpink1") +
  geom_function(fun = upper_wood_exp, colour = "hotpink4") +
  geom_function(fun = lower_wood_exp, colour = "hotpink4") +
  labs(title = "Wood")+
  scale_color_manual(values = color)

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp & upper(data$Sc) >= data$Kgp)
}

a <- coverage(test_leafs, upper_leafs_exp, lower_leafs_exp)
b <- coverage(test_wood, upper_wood_exp, lower_wood_exp)
c <- coverage(test_roots, upper_roots_exp, lower_roots_exp)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)


#Random split to assess coverage: 

rs_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit model
    lm_rs <- lm(Kgp ~ Sc, data = train_rs)
    sd_hat <- sqrt(sum(lm_rs$residuals^2)/(sample_size-1))
    f_hat <- function(x) lm_rs$coefficients[[2]]*x + lm_rs$coefficients[[1]] 
    
    # Quantiles
    upper <- function(x) {
      f_hat(x) - qt(alpha/2, sample_size-2)*sqrt(x^2/sum(train_rs$Sc^2)+1)*sd_hat
    }
    
    lower <- function(x) {
      f_hat(x) - qt(1-alpha/2, sample_size-2)*sqrt(x^2/sum(train_rs$Sc^2)+1)*sd_hat
    }
    
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                   &upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

#Checking for correct coverage

set.seed(4)
a <- rs_cov(leafs_log, 30, 0.1)
b <- rs_cov(wood_log, 30, 0.1)
c <- rs_cov(roots_log, 30,0.1)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(a$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0,1)+
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

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0,1.1))+
  labs(title = "Roots")

#------------------Rolling coverage---------------------------------------------

#Leafs

bin_size <- 50
roll_cov <- c()

leafs_log_arr <- leafs_log_test %>%
  arrange(Sc)

for (i in seq(1,nrow(leafs_log_test)-bin_size)){
  data_cov <- leafs_log_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- coverage(data_cov, upper_leafs, lower_leafs)
}

my_tib <- tibble("Bin" = leafs_log_arr[1:(nrow(leafs_log_test)-bin_size),1], "Roll_cov" = roll_cov)

#Mangler lige lidt color coding, men ellers er den god
  
ggplot(my_tib, aes(x = exp(Bin), y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-alpha, color = "purple")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')

#Wood

bin_size <- 50
roll_cov <- c()

wood_log_arr <- wood_log_test %>%
  arrange(Sc)

for (i in seq(1,nrow(wood_log_test)-bin_size)){
  data_cov <- wood_log_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- coverage(data_cov, upper_wood, lower_wood)
}

my_tib <- tibble("Bin" = wood_log_arr[1:(nrow(wood_log_test)-bin_size),1], "Roll_cov" = roll_cov)


ggplot(my_tib, aes(x = exp(Bin), y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-alpha, color = "purple")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_gradient(low = 'blue', high = 'red')

#------------------Rolling coverage part 2--------------------------------------

cov_window <- function(binsize = 49, data, alpha = 0.1) {
  roll_cov <- c()
  n <- nrow(data)
  
  #Arrange data
  data <- data %>%
    arrange(Sc)
  
  for (i in seq(1,n-binsize)){
    
    #"Test" data
    data_cov <- data %>%
      slice(i:(i+binsize))
    
    #Model fit data
    data_fit <- data %>%
      slice(-(i:(i+binsize)))
    n_fit <- nrow(data_fit)
    
    # Fit model
    lm_rw <- lm(Kgp ~ Sc, data = data_fit)
    sd_hat <- sqrt(var(lm_rw$residuals))
    f_hat <- function(x) lm_rw$coefficients[[2]]*x + lm_rw$coefficients[[1]] 
    
    # Quantiles
    upper <- function(x) {
      f_hat(x) - qt(alpha/2, n_fit-2)*sqrt(x^2/sum(data_fit$Sc^2)+1)*sd_hat
    }
    
    lower <- function(x) {
      f_hat(x) - qt(1-alpha/2, n_fit-2)*sqrt(x^2/sum(data_fit$Sc^2)+1)*sd_hat
    }
    
    roll_cov[i] <- coverage(data_cov, upper, lower)
  }
  return(roll_cov)
}

#Leafs

roll_cov_leafs <- cov_window(data = leafs_log)

my_tib <- tibble("Bin" = seq(1,nrow(leafs_log)-49), "Roll_cov" = roll_cov_leafs)


ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-alpha, color = "purple")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')


#Wood

roll_cov_wood <- cov_window(data = wood_log)

my_tib <- tibble("Bin" = seq(1,nrow(wood_log)-49), "Roll_cov" = roll_cov_wood)


ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-alpha, color = "purple")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "wood")+
  scale_color_gradient(low = 'blue', high = 'red')

#Checking coverage for different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()
cov_alpha_r <- c()

for (i in (1:4)){
  alpha <- alphas[i]
  upper_leafs <- upper_leafs
  lower_leafs <- lower_leafs
  cov_alpha_l[i] <- coverage(leafs_log_test, upper_leafs, lower_leafs)
}

for (i in (1:4)){
  alpha <- alphas[i]
  upper_wood <- upper_wood
  lower_wood <- lower_wood
  cov_alpha_w[i] <- coverage(wood_log_test, upper_wood, lower_wood)
}

for (i in (1:4)){
  alpha <- alphas[i]
  upper_roots <- upper_roots
  lower_roots <- lower_roots
  cov_alpha_r[i] <- coverage(roots_log_test, upper_roots, lower_roots)
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))



#Checking for conditional coverage in bins--------------------------------------

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp & upper(data$Sc) >= data$Kgp)
}
cond_cov <- function(data, upper, lower, num_bins){
  #Bins
  bins <- sort(data$Sc)[seq(1,num_bins)*floor(nrow(data)/num_bins)]
  indi <- c()
  
  for (i in (1:nrow(data))){
    x <- data$Sc[i]
    j_0 <- - Inf
    for (j in (1:num_bins)){
      if (j_0 < x & x <= bins[j]){
        indi[i] <- j
        j_0 <- bins[j]
      }
    }
  }
  cond_cov_data <- cbind(data, indi = indi)
  
  #Conditional Coverage
  
  cond_cov_vec <- c()
  
  for (i in cond_cov_data$indi){
    zz <- cond_cov_data %>%
      filter(indi == i)
    
    cond_cov_vec[i] <- coverage(zz, upper, lower)
  }
  
  j_0 <- - Inf
  bins2 <- c()
  for (i in (1:num_bins)){
    bins2[i] <- paste("[", j_0, ",", round(bins[i],2), "]")
    j_0 <- round(bins[i],2)
  }
  
  return(tibble(Bin = bins2, "Conditional coverage" = cond_cov_vec))
}

a <- cond_cov(leafs_log, upper_leafs, lower_leafs, 5)
b <- cond_cov(wood_log, upper_wood, lower_wood, 5)

xtable(cbind(a,b), type = latex)
xtable(b, type = latex)

bins <- sort(leafs_log$Sc)[seq(1,5)*floor(nrow(leafs_log)/5)]

test_leafs_log_plot <- leafs_log %>%
  mutate(Indicator = if_else((lower_leafs(Sc) <= Kgp)&
                               (Kgp <= upper_leafs(Sc)),"in", "out"))

ggplot(test_leafs_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator), alpha = 0.4) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_leafs, colour = "hotpink1") +
  geom_function(fun = upper_leafs, colour = "hotpink4") +
  geom_function(fun = lower_leafs, colour = "hotpink4") +
  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

bins <- sort(wood_log$Sc)[seq(1,5)*floor(nrow(wood_log)/5)]

ggplot(test_wood_log_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Kgp)')+
  geom_function(fun = f_hat_wood, colour = "hotpink1") +
  geom_function(fun = upper_wood, colour = "hotpink4") +
  geom_function(fun = lower_wood, colour = "hotpink4") +
  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
  labs(title = "Wood")+
  scale_color_manual(values = color)




