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

###################################################################################################
#Log-log OLS
###################################################################################################

lm_leafs_log <- lm(Kgp ~ Sc, data = train_leafs_log)
lm_roots_log <- lm(Kgp ~ Sc, data = train_roots_log)
lm_wood_log <- lm(Kgp ~ Sc, data = train_wood_log)

##Bootstrap som på stackexchange:
#(koden er taget herfra: https://stats.stackexchange.com/questions/226565/bootstrap-prediction-interval)

set.seed(72)
boot <- function(model, data_train, data_test, B) {
  #Obtaining standardized residuals 
  Y_p <- coef(model)["(Intercept)"] + coef(model)["Sc"]*data_test$Sc
  leverage <- influence(model)$hat
  s_res <- residuals(model)/sqrt(1-leverage)
  my_s_res <- s_res - mean(s_res)
  
  rep_e_n1 <- function(lm, residuals, X_n_1){
    # Make bootstrap residuals
    e_star <- sample(residuals, size = length(lm$residuals), replace = TRUE)
    
    # Make bootstrap Y
    y_star <- fitted(lm)+e_star
    
    # Do bootstrap regression
    x <- model.frame(lm)[,2]
    new_model <- lm(y_star~x)
    
    # Create bootstrapped adjusted residuals
    bs_lev <- influence(new_model)$hat
    bs_res   <- residuals(new_model)/sqrt(1-bs_lev)
    bs_res   <- bs_res - mean(bs_res)
    
    # Calculate draw on prediction error
    beta_beta <- c(coef(model)["(Intercept)"] - coef(new_model)["(Intercept)"],
                   (coef(model)["Sc"] - coef(new_model)["x"]))
    e_N_1 <- t(beta_beta)%*%c(1,X_n_1) + sample(bs_res,size=1)
    
    return(unname(e_N_1))
  }
  
  #Replicate
  
  ep <- matrix(nrow = B, ncol = nrow(data_test))
  
  for (i in (1:nrow(data_test))){
    for (j in (1:B)){
      ep[j,i] <- rep_e_n1(model, my_s_res, data_test[i,1])
    }
  }
  return(list(ep, Y_p))
}

#Performing the bootstrap

leafs_boot <- boot(lm_leafs_log, train_leafs_log, test_leafs_log, 100)
wood_boot <- boot(lm_wood_log, train_wood_log, test_wood_log, 100)
roots_boot <- boot(lm_roots_log, train_roots_log, test_roots_log, 100)

#Creating the prediction intervals

pred_int <- function(boot, data,alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] + quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] + quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                            "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(Sc = exp(Sc), Kgp = exp(Kgp), down = exp(down), up = exp(up))%>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}

#Plot

pred_leafs <- function(x) exp(coef(lm_leafs_log)["(Intercept)"])*
  x^coef(lm_leafs_log)["Sc"]*exp(var(lm_leafs_log$residuals)/2)
pred_wood <- function(x) exp(coef(lm_wood_log)["(Intercept)"])*
  x^coef(lm_wood_log)["Sc"]*exp(var(lm_wood_log$residuals)/2)
pred_roots <- function(x) exp(coef(lm_roots_log)["(Intercept)"])*
  x^coef(lm_roots_log)["Sc"]*exp(var(lm_roots_log$residuals)/2)

pred_int_leafs <- pred_int(leafs_boot, leafs_log_test, 0.1) 
pred_int_wood <- pred_int(wood_boot, wood_log_test, 0.1)
pred_int_roots <- pred_int(roots_boot, roots_log_test, 0.1)

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(pred_int_leafs) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_leafs, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)


ggplot(pred_int_wood) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_wood, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_roots, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Coverage

coverage <- function(pred_int){
  mean(pred_int$down <= pred_int$Kgp & pred_int$Kgp <= pred_int$up)
}

coverage(pred_int_leafs)
coverage(pred_int_wood)
coverage(pred_int_roots)

#For different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()
cov_alpha_r <- c()

for (i in (1:4)){
  pred_int_alpha <- pred_int(leafs_boot, leafs_log_test, alphas[i]) 
  cov_alpha_l[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int(wood_boot, wood_log_test, alphas[i]) 
  cov_alpha_w[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int(roots_boot, roots_log_test, alphas[i]) 
  cov_alpha_r[i] <- coverage(pred_int_alpha)
}


###################################################################################################
#OLS
###################################################################################################

lm_leafs <- lm(Kgp ~ Sc, data = leafs_train)
lm_roots <- lm(Kgp ~ Sc, data = roots_train)
lm_wood <- lm(Kgp ~ Sc, data = wood_train)

#Performing the bootstrap

leafs_boot2 <- boot(lm_leafs, leafs_train, test_leafs, 100)
wood_boot2 <- boot(lm_wood, wood_train, test_wood, 100)
roots_boot2 <- boot(lm_roots, roots_train, test_roots, 100)

#Creating the prediction intervals

pred_int2 <- function(boot, data, alpha) {
  down <- c()
  up <- c()
  
  for (i in ncol(boot[[1]])){
    down <- boot[[2]] + quantile(boot[[1]][,i], probs = alpha/2)
    up <- boot[[2]] + quantile(boot[[1]][,i], probs = 1-alpha/2)
  } 
  
  plot_data <- tibble("Sc" = data$Sc, "Kgp" = data$Kgp,
                      "Pred" = boot[[2]], "down" = down, "up" = up) %>%
    mutate(indicator = if_else((down <= Kgp)&(Kgp <= up),"in", "out"))
  
  return(plot_data)
}

pred_int_leafs2 <- pred_int2(leafs_boot2, test_leafs, 0.1) 
pred_int_wood2 <- pred_int2(wood_boot2, test_wood, 0.1)
pred_int_roots2 <- pred_int2(roots_boot2, test_roots, 0.1)

#Plot

pred_leafs2 <- function(x) coef(lm_leafs)["(Intercept)"] + x*coef(lm_leafs)["Sc"]
pred_wood2 <- function(x) coef(lm_wood)["(Intercept)"] + x*coef(lm_wood)["Sc"]
pred_roots2 <- function(x) coef(lm_roots)["(Intercept)"] +x*coef(lm_roots)["Sc"]

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(pred_int_leafs2) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_leafs2, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)


ggplot(pred_int_wood2) + 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_wood2, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

ggplot(pred_int_roots2)+ 
  geom_point(aes(x = Sc, y = down), color = 'hotpink') +
  geom_point(aes(x = Sc, y = up), color = 'hotpink') +
  geom_function(fun = pred_roots2, color = "hotpink4") +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

#Coverage

coverage <- function(pred_int){
  mean(pred_int$down <= pred_int$Kgp & pred_int$Kgp <= pred_int$up)
}

coverage(pred_int_leafs2)
coverage(pred_int_wood2)
coverage(pred_int_roots2)

#For different alphas

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l2 <- c()
cov_alpha_w2 <- c()
cov_alpha_r2 <- c()

for (i in (1:4)){
  pred_int_alpha <- pred_int2(leafs_boot2, leafs_test, alphas[i]) 
  cov_alpha_l2[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int2(wood_boot2, wood_test, alphas[i]) 
  cov_alpha_w2[i] <- coverage(pred_int_alpha)
}

for (i in (1:4)){
  pred_int_alpha <- pred_int2(roots_boot2, roots_test, alphas[i]) 
  cov_alpha_r2[i] <- coverage(pred_int_alpha)
}


#Checking for correct coverage

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
    
    #Perform bootstrap
    boot_rs <- boot(lm_leafs, leafs_train, test_leafs, 100)
    
    # Quantiles
    pred_int_rs <- pred_int2(boot_rs, test_rs, alpha) 
    
    #Definere
    cov[i] <- coverage(pred_int_rs)
  }
  return(tibble("Coverage" = cov))
}

#Det her tager ægte lang tid, jeg ved ikke om det går godt...

set.seed(4)
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
  xlim(c(0,1))+
  labs(title = "Roots")




##Bootstrap som i matstat - konfidensintervaller

B <- 100

boot_leafs <- leafs_log %>% rep_sample_n(replace = TRUE, reps = B, size = nrow(leafs_log))
boot_roots <- roots_log %>% rep_sample_n(replace = TRUE, reps = B, size = nrow(roots_log))
boot_wood <- wood_log %>% rep_sample_n(replace = TRUE, reps = B, size = nrow(wood_log))

## Beregening af parametre ved OLS

lm_leafs_coefs <- boot_leafs %>%
  group_by(replicate) %>%
  summarize(a = lm(Kgp ~ Sc)$coefficients[1], b = lm(Kgp ~ Sc)$coefficients[2])

lm_roots_coefs <- boot_roots %>%
  group_by(replicate) %>%
  summarize(a = lm(Kgp ~ Sc)$coefficients[1], b = lm(Kgp ~ Sc)$coefficients[2])

lm_wood_coefs <- boot_wood %>%
  group_by(replicate) %>%
  summarize(a = lm(Kgp ~ Sc)$coefficients[1], b = lm(Kgp ~ Sc)$coefficients[2])

##Fordeling af estimater

ggplot(data = lm_wood_coefs)+
  geom_histogram(aes(x=a, y=..density..), color = "white")+
  labs(title = "Histogram over the a coefficient")

ggplot(data = lm_wood_coefs)+
  geom_histogram(aes(x=b, y=..density..), color = "white")+
  labs(title = "Histogram over the b coefficient")

##Konfidens intervaller - percentil:

#a

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(quantile(lm_leafs_coefs$a, 0.025), quantile(lm_roots_coefs$a, 0.025), quantile(lm_wood_coefs$a, 0.025)),
  Upper = c(quantile(lm_leafs_coefs$a, 0.975), quantile(lm_roots_coefs$a, 0.975), quantile(lm_wood_coefs$a, 0.975)),
)


#b

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(quantile(lm_leafs_coefs$b, 0.025), quantile(lm_roots_coefs$b, 0.025), quantile(lm_wood_coefs$b, 0.025)),
  Upper = c(quantile(lm_leafs_coefs$b, 0.975), quantile(lm_roots_coefs$b, 0.975), quantile(lm_wood_coefs$b, 0.975)),
)

##Det giver rigtig god mening baseret på antallet af observerede data punkter:

nrow(roots_log)
nrow(wood_log)
nrow(leafs_log)

##Konfidens intervaller - standard error (bare for leafs):

theta_hat_a <- c(lm(Kgp ~ Sc, leafs_log)$coefficients[1],
                 lm(Kgp ~ Sc, roots_log)$coefficients[1],
                 lm(Kgp ~ Sc, wood_log)$coefficients[1])


theta_hat_b <-  c(lm(Kgp ~ Sc, leafs_log)$coefficients[2],
                  lm(Kgp ~ Sc, roots_log)$coefficients[2],
                  lm(Kgp ~ Sc, wood_log)$coefficients[2])

se_b_a <- c(sqrt(1/(B-1)*(sum(theta_hat_a[1] - lm_leafs_coefs$a)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_a[2] - lm_roots_coefs$a)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_a[3] - lm_wood_coefs$a)^2)))


se_b_b <- c(sqrt(1/(B-1)*(sum(theta_hat_b[1] - lm_leafs_coefs$b)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_b[2] - lm_roots_coefs$b)^2)),
            sqrt(1/(B-1)*(sum(theta_hat_b[3] - lm_wood_coefs$b)^2)))

upper_a <- theta_hat_a-qt(0.025, nrow(leafs_log)-1)*se_b_a
upper_b <- theta_hat_b-qt(0.025, nrow(leafs_log)-1)*se_b_b

lower_a <- theta_hat_a+qt(0.025, nrow(leafs_log)-1)*se_b_a
lower_b <- theta_hat_b+qt(0.025, nrow(leafs_log)-1)*se_b_b

#a

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(lower_a[1], lower_a[2], lower_a[3]),
  Upper = c(upper_a[1], upper_a[2], upper_a[3]),
)

#b

tibble(
  Data = c("Leafs", "Roots", "Wood"), 
  Lower = c(lower_b[1], lower_b[2], lower_b[3]),
  Upper = c(upper_b[1], upper_b[2], upper_b[3]))



