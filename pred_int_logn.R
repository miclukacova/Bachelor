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

leafs <- read.csv('Data/leafs.csv')
roots <- read.csv('Data/roots.csv')
wood <- read.csv('Data/wood.csv')

library(tidyverse)
library(readr)
library(infer)
library(xtable)
################################################################################################

#Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = leafs_log_train)
lm_roots_log <- lm(Kgp ~ Sc, data = roots_log_train)
lm_wood_log <- lm(Kgp ~ Sc, data = wood_log_train)

#Estimater fra lm

sd_hat <- c(sqrt(var(lm_leafs_log$residuals)),
             sqrt(var(lm_roots_log$residuals)),
             sqrt(var(lm_wood_log$residuals)))

#Mean of the distribution on log scale

mean_l_log <- function(x) lm_leafs_log$coefficients[[1]]+ lm_leafs_log$coefficients[[2]]*log(x)
mean_w_log <- function(x) lm_wood_log$coefficients[[1]]+ lm_wood_log$coefficients[[2]]*log(x)
mean_r_log <- function(x) lm_roots_log$coefficients[[1]]+ lm_roots_log$coefficients[[2]]*log(x)
mean_l <- function(x) exp(lm_leafs_log$coefficients[[1]])*x^lm_leafs_log$coefficients[[2]]
mean_w <- function(x) exp(lm_wood_log$coefficients[[1]])*x^lm_wood_log$coefficients[[2]]
mean_r <- function(x) exp(lm_roots_log$coefficients[[1]])*x^lm_roots_log$coefficients[[2]]

# Quantiles

alpha <- 0.1

quant_low_l <- function(x) qlnorm(alpha/2, meanlog = mean_l_log(x), sdlog = sd_hat[1]) 
quant_up_l <- function(x) qlnorm(1-alpha/2, meanlog = mean_l_log(x), sdlog = sd_hat[1]) 

quant_low_w <- function(x) qlnorm(alpha/2, meanlog = mean_w_log(x), sdlog = sd_hat[2]) 
quant_up_w <- function(x) qlnorm(1 - alpha/2, meanlog = mean_w_log(x), sdlog = sd_hat[2])

quant_low_r <- function(x) qlnorm(alpha/2, meanlog = mean_r_log(x), sdlog = sd_hat[3]) 
quant_up_r <- function(x) qlnorm(1 - alpha/2, meanlog = mean_r_log(x), sdlog = sd_hat[3])

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


#Checking coverage for different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()
cov_alpha_r <- c()

for (i in (1:4)){
  alpha <- alphas[i]
  quant_low_l <- quant_low_l
  quant_up_l <- quant_up_l
  cov_alpha_l[i] <- coverage(leafs_test, quant_up_l, quant_low_l)
}

for (i in (1:4)){
  alpha <- alphas[i]
  quant_low_w <- quant_low_w
  quant_up_w <- quant_up_w
  cov_alpha_w[i] <- coverage(wood_test, quant_up_w, quant_low_w)
}

for (i in (1:4)){
  alpha <- alphas[i]
  quant_low_r <- quant_low_r
  quant_up_r <- quant_up_r
  cov_alpha_r[i] <- coverage(roots_test, quant_up_r, quant_low_r)
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))

#Checking for correct coverage

#Random split to assess coverage: 

rs_cov <- function(data, k, alpha) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    train_rs <- train_rs %>% mutate(Sc = log(Sc), Kgp = log(Kgp))
    test_rs = data[-picked_rs,]
    
    # Fit model
    lm_rs <- lm(Kgp ~ Sc, data = train_rs)
    sd_hat <- sqrt(var(lm_leafs_log$residuals))
    f_hat_log <- function(x) lm_rs$coefficients[[1]]+ lm_rs$coefficients[[2]]*log(x)
    
    # Quantiles
    low <- function(x) qlnorm(0.05, meanlog = f_hat_log(x), sdlog = sd_hat)
    up <- function(x) qlnorm(0.95, meanlog = f_hat_log(x), sdlog = sd_hat)
    
    #Definere
    cov[i] <- mean(low(test_rs$Sc) <= test_rs$Kgp 
                   &up(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

set.seed(7)
a <- rs_cov(leafs, 30, 0.1)
b <- rs_cov(wood, 30, 0.1)
c <- rs_cov(roots, 30,0.1)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

a %>%
  ggplot(aes(x = Coverage, y = ..density..)) +
  geom_histogram(color = "white", 
                 fill = "darkolivegreen3", bins = 50)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1.01)+
  xlab('Coverage')+  
  theme_bw()+
  labs(title = "Foliage")

median(c$Coverage)


b %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 40)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlab('Coverage')+ 
  xlim(c(0.5,1.01))+
  labs(title = "Wood")

#Det her er lidt mærkeligt

c %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 50)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  xlim(c(0.5,1.01))+
  labs(title = "Roots")


#Checking for conditional coverage----------------------------------------------

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

a <- cond_cov(leafs_test, quant_up_l, quant_low_l, 5)
b <- cond_cov(wood_test, quant_up_w, quant_low_w, 5)


xtable(cbind(a,b), type = latex)

bins <- sort(leafs_test$Sc)[seq(1,5)*floor(nrow(leafs_test)/5)]

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = mean_l, colour = "hotpink1") +
  geom_function(fun = quant_low_l, colour = "hotpink4") +
  geom_function(fun = quant_up_l, colour = "hotpink4") +
  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

bins <- sort(wood_test$Sc)[seq(1,5)*floor(nrow(wood_test)/5)]

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = mean_w, colour = "hotpink1") +
  geom_function(fun = quant_low_w, colour = "hotpink4") +
  geom_function(fun = quant_up_w, colour = "hotpink4") +
  geom_vline(xintercept = bins[1], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[2], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[3], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[4], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  geom_vline(xintercept = bins[5], linetype = "dotted", linewidth = 0.5, color = "hotpink3")+
  labs(title = "Wood")+
  scale_color_manual(values = color)



#------------------Rolling coverage---------------------------------------------

#Leafs

alpha <- 0.1
bin_size <- 50
roll_cov <- c()

leafs_arr <- leafs_test %>%
  arrange(Sc)

for (i in seq(1,nrow(leafs_test)-bin_size)){
  data_cov <- leafs_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- coverage(data_cov, quant_up_l, quant_low_l)
}

my_tib <- tibble("Bin" = leafs_arr[1:(nrow(leafs_test)-bin_size),1], "Roll_cov" = roll_cov)

#Mangler lige lidt color coding, men ellers er den god

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.3, color = "darkolivegreen") + 
  geom_hline(yintercept = 1-alpha, color = "hotpink")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

#Wood

alpha <- 0.1
bin_size <- 50
roll_cov <- c()

wood_arr <- wood_test %>%
  arrange(Sc)

for (i in seq(1,nrow(wood_test)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- coverage(data_cov, quant_up_w, quant_low_w)
}

my_tib <- tibble("Bin" = wood_arr[1:(nrow(wood_test)-bin_size),1], "Roll_cov" = roll_cov)

#Mangler lige lidt color coding, men ellers er den god

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.3, color = "darkolivegreen") + 
  geom_hline(yintercept = 1-alpha, color = "hotpink")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_manual(values = color)



#Jeg kan ikke huske hvad det her er????
#Slettes?
#Varians af y- y_hat

var_yy_l <- function(x) x^2*sd_hat[1]^2/(sum(leafs_log_train^2)) + sd_hat[1]^2

quant_yy_exp_l_down <- function(x) qlnorm(0.05, meanlog = 0, sdlog = sqrt(var_yy_l(log(x))))
quant_yy_exp_l_up <- function(x) qlnorm(0.95, meanlog = 0, sdlog = sqrt(var_yy_l(log(x))))

pred_up <- function(x) quant_yy_exp_l_up(x)*mean_l(x)
pred_down <- function(x) quant_yy_exp_l_down(x)*mean_l(x)


mean(quant_yy_exp_l_down(leafs_test$Sc) <= leafs_test$Kgp / mean_l(leafs_test$Sc) &
       quant_yy_exp_l_up(leafs_test$Sc) >= leafs_test$Kgp / mean_l(leafs_test$Sc))

mean(pred_down(leafs_test$Sc)<= leafs_test$Kgp & pred_up(leafs_test$Sc) >= leafs_test$Kgp)

