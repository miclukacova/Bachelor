### Prediction intervals for lognormal variables

#################################--Indlæsning af pakker og data---###############################

leafs_log_train <- read.csv('Data/train_leafs_log.csv')
roots_log_train <- read.csv('Data/train_roots_log.csv')
wood_log_train<- read.csv('Data/train_wood_log.csv')

leafs_log_test <- read.csv('Data/test_leafs_log.csv')
roots_log_test <- read.csv('Data/test_roots_log.csv')
wood_log_test<- read.csv('Data/test_wood_log.csv')

leafs_test <- read.csv('Data/test_leafs.csv')
roots_test <- read.csv('Data/test_roots.csv')
wood_test <- read.csv('Data/test_wood.csv')

library(tidyverse)
library(readr)
library(infer)
################################################################################################

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log_train)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log_train)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log_train)

#Estimater fra lm

hat_beta <- c(lm_leafs_log$coefficients[[1]],
              lm_roots_log$coefficients[[1]],
              lm_wood_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
               lm_roots_log$coefficients[[2]],
               lm_wood_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_roots_log$residuals),
             var(lm_wood_log$residuals))

#Mean of the distribution on log scale

mean_l_log <- function(x) lm_leafs_log$coefficients[[1]]+ lm_leafs_log$coefficients[[2]]*log(x)
mean_w_log <- function(x) lm_wood_log$coefficients[[1]]+ lm_wood_log$coefficients[[2]]*log(x)
mean_r_log <- function(x) lm_roots_log$coefficients[[1]]+ lm_roots_log$coefficients[[2]]*log(x)
mean_l <- function(x) exp(lm_leafs_log$coefficients[[1]])*x^lm_leafs_log$coefficients[[2]]
mean_w <- function(x) exp(lm_wood_log$coefficients[[1]])*x^lm_wood_log$coefficients[[2]]
mean_r <- function(x) exp(lm_roots_log$coefficients[[1]])*x^lm_roots_log$coefficients[[2]]

# Quantiles

quant_low_l <- function(x) qlnorm(0.05, meanlog = mean_l_log(x), sdlog = var_hat[1]) 
quant_up_l <- function(x) qlnorm(0.95, meanlog = mean_l_log(x), sdlog = var_hat[1]) 

quant_low_w <- function(x) qlnorm(0.05, meanlog = mean_w_log(x), sdlog = var_hat[2]) 
quant_up_w <- function(x) qlnorm(0.95, meanlog = mean_w_log(x), sdlog = var_hat[2])

quant_low_r <- function(x) qlnorm(0.05, meanlog = mean_r_log(x), sdlog = var_hat[3]) 
quant_up_r <- function(x) qlnorm(0.95, meanlog = mean_r_log(x), sdlog = var_hat[3])

#On test set 


test_leafs_plot <- leafs_test %>%
  mutate(Indicator = if_else((quant_low_l(Sc) <= Kgp)&
                               (Kgp <= quant_up_l(Sc)),"in", "out"))

test_wood_plot <- wood_test %>%
  mutate(Indicator = if_else((quant_low_w(Sc) <= Kgp)&
                               (Kgp <= quant_up_w(Sc)),"in", "out"))

test_roots_plot <- roots_test %>%
  mutate(Indicator = if_else((quant_low_l(Sc) <= Kgp)&
                               (Kgp <= quant_up_l(Sc)),"in", "out"))

#Coverage

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp &upper(data$Sc) >= data$Kgp)
}

a <- coverage(leafs_test, quant_up_l, quant_low_l)
b <- coverage(wood_test, quant_up_w, quant_low_w)
c <- coverage(roots_test, quant_up_r, quant_low_r)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)

#Plot with prediction intervals real scale

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = mean_l, colour = "hotpink1") +
  geom_function(fun = quant_low_l, colour = "hotpink4") +
  geom_function(fun = quant_up_l, colour = "hotpink4") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = mean_w, colour = "hotpink1") +
  geom_function(fun = quant_low_w, colour = "hotpink4") +
  geom_function(fun = quant_up_w, colour = "hotpink4") +
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(test_roots_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = mean_r, colour = "hotpink1") +
  geom_function(fun = quant_low_r, colour = "hotpink4") +
  geom_function(fun = quant_up_r, colour = "hotpink4") +
  labs(title = "Roots")+
  scale_color_manual(values = color)


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
      f_hat(x) - qt(alpha/2, sample_size-1)*sqrt(x^2/sum(train_rs$Sc^2)+1)*sd_hat
    }
    
    lower <- function(x) {
      f_hat(x) - qt(1-alpha/2, sample_size-1)*sqrt(x^2/sum(train_rs$Sc^2)+1)*sd_hat
    }
    
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                   &upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

##Checking for correct coverage
#
#set.seed(4)
#a <- rs_cov(leafs_log, 30, 0.1)
#b <- rs_cov(wood_log, 30, 0.1)
#c <- rs_cov(roots_log, 30,0.1)
#
##Mean coverage:
#mean_a <- mean(a$Coverage)
#mean_b <- mean(b$Coverage)
#mean_c <- mean(c$Coverage) 
#
#xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
#              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)
#
#a %>%
#  ggplot() +
#  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
#                 fill = "darkolivegreen3", bins = 30)+
#  geom_vline(xintercept = 0.9, color = "hotpink") +
#  xlim(0,1)+
#  theme_bw()+
#  labs(title = "Foliage")
#
#b %>%
#  ggplot() +
#  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
#                 fill = "darkolivegreen3", bins = 30)+
#  geom_vline(xintercept = 0.9, color = "hotpink") +
#  theme_bw()+
#  xlim(c(0.5,1))+
#  labs(title = "Wood")
#
#c %>%
#  ggplot() +
#  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
#                 fill = "darkolivegreen3", bins = 30)+
#  geom_vline(xintercept = 0.9, color = "hotpink") +
#  theme_bw()+
#  xlim(c(0,1))+
#  labs(title = "Roots")
#
#
##Checking for conditional coverage----------------------------------------------
#
#coverage <- function(data, upper, lower){
#  mean(lower(data$Sc) <= data$Kgp & upper(data$Sc) >= data$Kgp)
#}
#
#cond_cov <- function(data, upper, lower, num_bins){
#  #Bins
#  bins <- sort(data$Sc)[seq(1,num_bins)*floor(nrow(data)/num_bins)]
#  indi <- c()
#  
#  for (i in (1:nrow(data))){
#    x <- data$Sc[i]
#    j_0 <- - Inf
#    for (j in (1:num_bins)){
#      if (j_0 < x & x <= bins[j]){
#        indi[i] <- j
#        j_0 <- bins[j]
#      }
#    }
#  }
#  cond_cov_data <- cbind(data, indi = indi)
#  
#  #Conditional Coverage
#  
#  cond_cov_vec <- c()
#  
#  for (i in cond_cov_data$indi){
#    zz <- cond_cov_data %>%
#      filter(indi == i)
#    
#    cond_cov_vec[i] <- coverage(zz, upper, lower)
#  }
#  
#  j_0 <- - Inf
#  bins2 <- c()
#  for (i in (1:num_bins)){
#    bins2[i] <- paste("[", j_0, ",", round(bins[i],2), "]")
#    j_0 <- round(bins[i],2)
#  }
#  
#  return(tibble(Bin = bins2, "Conditional coverage" = cond_cov_vec))
#}
#
#a <- cond_cov(test_leafs_log, upper_leafs, lower_leafs, 5)
#b <- cond_cov(test_wood_log, upper_wood, lower_wood, 5)
#
#nrow(test_wood_log)
#
#xtable(cbind(a,b), type = latex)
#xtable(b, type = latex)
#
#bins <- sort(test_leafs_log$Sc)[seq(1,5)*floor(nrow(test_leafs_log)/5)]
#
#ggplot(test_leafs_log_plot, aes(x = Sc, y = Kgp)) + 
#  geom_point(aes(color = Indicator)) + 
#  theme_bw() +
#  xlab('log(Sc)') + 
#  ylab('log(Kgp)')+
#  geom_function(fun = f_hat_leafs, colour = "hotpink1") +
#  geom_function(fun = upper_leafs, colour = "hotpink4") +
#  geom_function(fun = lower_leafs, colour = "hotpink4") +
#  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  labs(title = "Leafs")+
#  scale_color_manual(values = color)
#
#bins <- sort(test_wood_log$Sc)[seq(1,5)*floor(nrow(test_wood_log)/5)]
#
#ggplot(test_wood_log_plot, aes(x = Sc, y = Kgp)) + 
#  geom_point(aes(color = Indicator)) + 
#  theme_bw() +
#  xlab('log(Sc)') + 
#  ylab('log(Kgp)')+
#  geom_function(fun = f_hat_wood, colour = "hotpink1") +
#  geom_function(fun = upper_wood, colour = "hotpink4") +
#  geom_function(fun = lower_wood, colour = "hotpink4") +
#  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.3, color = "hotpink3")+
#  labs(title = "Wood")+
#  scale_color_manual(values = color)


## Min metode - skal rettes til 


#Estimeret middelværdi af Y

hat_y_leafs <- function(x) exp(hat_beta[1])* x^hat_alpha[1]*exp(var_hat[1]/2)
hat_y_roots <- function(x) exp(hat_beta[2])* x^hat_alpha[2]*exp(var_hat[2]/2)
hat_y_wood <- function(x) exp(hat_beta[3])* x^hat_alpha[3]*exp(var_hat[3]/2)

#Estimeret varians af Y

var_y_hat_leafs <- function(x) exp(2*hat_y_leafs(x) + var_hat[1])*(exp(var_hat[1])-1) 
var_y_y_hat_wood <- function(x) exp(2*hat_y_wood(x) + var_hat[2])*(exp(var_hat[2])-1)  
var_y_y_hat_roots <- function(x) exp(2*hat_y_roots(x) + var_hat[3])*(exp(var_hat[3])-1)  

# hat log Y som funktion af log x

hat_log_y_leafs <- function(x) hat_beta[1] + hat_alpha[1]*x
hat_log_y_roots <- function(x) hat_beta[2] + hat_alpha[2]*x
hat_log_y_wood <- function(x) hat_beta[3] + hat_alpha[3]*x

# hat Y som funktion af x

hat_y_leafs <- function(x) exp(hat_beta[1])* x^hat_alpha[1]*exp(var_hat[1]/2)
hat_y_roots <- function(x) exp(hat_beta[2])* x^hat_alpha[2]*exp(var_hat[2]/2)
hat_y_wood <- function(x) exp(hat_beta[3])* x^hat_alpha[3]*exp(var_hat[3]/2)

# hat varians af log Y - hat log Y som funktion af log x

var_y_y_hat_leafs <- function(x) x*var_hat[1]/(sum(leafs_log_train^2))+var_hat[1] 
var_y_y_hat_wood <- function(x) x*var_hat[2]/(sum(wood_log_train^2))+var_hat[2] 
var_y_y_hat_roots <- function(x) x*var_hat[3]/(sum(roots_log_train^2))+var_hat[3] 

# Middelværdi og varians af exp(logY - hat log Y) som funktion af log x

mean_exp_y_y_hat_leafs <- function(x) exp(var_y_y_hat_leafs(x)/2) 
mean_exp_y_y_hat_wood <- function(x) exp(var_y_y_hat_wood(x)/2) 
mean_exp_y_y_hat_roots <- function(x) exp(var_y_y_hat_roots(x)/2) 

var_exp_y_y_hat_leafs <- function(x) exp(var_y_y_hat_leafs(x))*(exp(var_y_y_hat_leafs(x))-1) 
var_exp_y_y_hat_wood <- function(x) exp(var_y_y_hat_wood(x))*(exp(var_y_y_hat_wood(x))-1) 
var_exp_y_y_hat_roots <- function(x) exp(var_y_y_hat_roots(x))*(exp(var_y_y_hat_roots(x))-1) 

#Kvantil som funktion af log x

kvant_low <- function(x) qlnorm(0.2, meanlog = mean_exp_y_y_hat_leafs(x), sdlog = sqrt(var_exp_y_y_hat_leafs(x)))
kvant_up <- function(x) qlnorm(0.7, meanlog = mean_exp_y_y_hat_leafs(x), sdlog = sqrt(var_exp_y_y_hat_leafs(x)))

#Prediktionsinterval som funktion af log x

pred_int_low <- function(x) kvant_low(x) * mean_hat_y_leafs(x)
pred_int_up <- function(x) kvant_up(x) * mean_hat_y_leafs(x)

#Prediktionsinterval som funktion af x

pred_int_low_log <- function(x) pred_int_low(log(x))
pred_int_up_log <- function(x) pred_int_up(log(x))

mean(pred_int_low_log(leafs_test$Sc)<= leafs_test$Sc & pred_int_up_log(leafs_test$Sc) >= leafs_test$Sc)

l <- pred_int_low_log(leafs_test$Sc)
u <- pred_int_up_log(leafs_test$Sc)

color <- c("darkolivegreen1", "darkolivegreen4")
#aes(color = Indicator)


