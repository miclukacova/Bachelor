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



#########################################################################
#------------------------------- Gammelt --------------------------------

pred_int_l <- full_conformal(leafs_train, 0.1, ols_alg, test_leafs$Sc)

test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((pred_int_l$low <= Kgp)&(Kgp <= pred_int_l$high),"in", "out"))%>%
  add_column(low = pred_int_l$low, high = pred_int_l$high)



ggplot(test_leafs_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_l, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

cov_l <- mean((pred_int_l$low <= test_leafs$Kgp)&(test_leafs$Kgp <= pred_int_l$high))

#Wood

pred_int_w <- full_conformal(wood_train, 0.1, ols_alg, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

ols_w <- lm(Kgp ~ Sc, wood_train)
f_hat_w <- function(x) ols_w$coef[[1]] + ols_w$coef[[2]]*x

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_w, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Wood

pred_int_r <- full_conformal(roots_train, 0.1, ols_alg, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

ols_r <- lm(Kgp ~ Sc, roots_train)
f_hat_r <- function(x) ols_r$coef[[1]] + ols_r$coef[[2]]*x

ggplot(test_roots_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_r, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

cov_r <- mean((pred_int_r$low <= test_roots$Kgp)&(test_roots$Kgp <= pred_int_r$high))

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), "Coverage" = c(cov_l, cov_w, cov_r)))

#--------------------------- Log-log OLS (bias adj.) ---------------------------

log_ols_alg <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  return(f_hat)
}

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")


#Leafs

pred_int_l <- full_conformal(leafs_train, 0.1, log_ols_alg, test_leafs$Sc)


test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((pred_int_l$low <= Kgp)&(Kgp <= pred_int_l$high),"in", "out"))%>%
  add_column(low = pred_int_l$low, high = pred_int_l$high)

model <- lm(log(Kgp) ~ log(Sc), leafs_train)
f_hat_l <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_leafs_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_l, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

cov_l <- mean((pred_int_l$low <= test_leafs$Kgp)&(test_leafs$Kgp <= pred_int_l$high))

#Wood

pred_int_w <- full_conformal(wood_train, 0.1, log_ols_alg, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

model <- lm(log(Kgp) ~ log(Sc), wood_train)
f_hat_w <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.8, alpha = 0.8) +
  geom_function(fun = f_hat_w, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Roots

#Der går måske noget galt her

pred_int_r <- full_conformal(roots_train, 0.1, log_ols_alg, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

model <- lm(log(Kgp) ~ log(Sc), roots_train)
f_hat_r <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)

ggplot(test_roots_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.8, alpha = 0.8) +
  geom_function(fun = f_hat_r, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

cov_r <- mean((pred_int_r$low <= test_roots$Kgp)&(test_roots$Kgp <= pred_int_r$high))

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), "Coverage" = c(cov_l, cov_w, cov_r)))



#--------------------------------------- NLR ----------------------------------- 

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

nlr_alg <- function(data, start_point){
  mod <- optim(par = start_point, fn = MSE_NLR, data = data)
  f_hat <- function(x) mod$par[[1]]*x^mod$par[[2]]
  return(f_hat)
}

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")


#Leafs

nlr_alg_l <- function(data) nlr_alg(data, c(0.61,0.81))

pred_int_l <- full_conformal(leafs_train, 0.1, nlr_alg_l, test_leafs$Sc)


test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((pred_int_l$low <= Kgp)&(Kgp <= pred_int_l$high),"in", "out"))%>%
  add_column(low = pred_int_l$low, high = pred_int_l$high)

f_hat_l <- nlr_alg_l(leafs_train)

ggplot(test_leafs_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_l, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

cov_l <- mean((pred_int_l$low <= test_leafs$Kgp)&(test_leafs$Kgp <= pred_int_l$high))

#Wood

nlr_alg_w <- function(data) nlr_alg(data, c(5.51,0.73))

pred_int_w <- full_conformal(wood_train, 0.1, nlr_alg_w, test_wood$Sc)

test_wood_plot <- test_wood %>%
  mutate(indicator = if_else((pred_int_w$low <= Kgp)&(Kgp <= pred_int_w$high),"in", "out"))%>%
  add_column(low = pred_int_w$low, high = pred_int_w$high)

f_hat_w <- nlr_alg_w(wood_train)

ggplot(test_wood_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_w, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)

cov_w <- mean((pred_int_w$low <= test_wood$Kgp)&(test_wood$Kgp <= pred_int_w$high))

#Roots

nlr_alg_r <- function(data) nlr_alg(data, c(3.66,0.1))
pred_int_r <- full_conformal(roots_train, 0.1, nlr_alg_r, test_roots$Sc)

test_roots_plot <- test_roots %>%
  mutate(indicator = if_else((pred_int_r$low <= Kgp)&(Kgp <= pred_int_r$high),"in", "out"))%>%
  add_column(low = pred_int_r$low, high = pred_int_r$high)

f_hat_r <- nlr_alg_r(roots_train)

ggplot(test_roots_plot) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator), size = 0.8, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = high), color = "hotpink", size = 0.9, alpha = 0.8) + 
  geom_point(aes(x = Sc, y = low), color = "hotpink", size = 0.9, alpha = 0.8) +
  geom_function(fun = f_hat_r, colour = "hotpink4", linetype = "dashed") +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

cov_r <- mean((pred_int_r$low <= test_roots$Kgp)&(test_roots$Kgp <= pred_int_r$high))

xtable(tibble(" " = c("Leafs", "Wood", "Roots"), "Coverage" = c(cov_l, cov_w, cov_r)))








#------------------------------ Investigation of coverage ----------------------
#------------------------------ For different alphas ---------------------------
alphas <- c(0.01, 0.05, 0.1, 0.2)

set.seed(2)
conf_diff_alphas <- function(data_train, reg_alg, new_data){
  cov_alpha <- c()
  for (i in (1:4)){
    a <- full_conformal(data_train, alpha = alphas[i], reg_alg, new_data$Sc)
    cov_alpha[i] <- mean(a$low <= new_data$Kgp & new_data$Kgp  <= a$high)
  }
  return(cov_alpha)
}


cov_alpha_l <- conf_diff_alphas(leafs_train, log_ols_alg, test_leafs)
cov_alpha_w <- conf_diff_alphas(wood_train, log_ols_alg, test_wood)
cov_alpha_r <- conf_diff_alphas(roots_train, log_ols_alg, test_roots)


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w, "Roots" = cov_alpha_r))




#----------------------------Distribution of coverage by resampling-----------------------------------------

rs_cov <- function(data, k, alpha, reg_alg) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    if (i = 15){
      print("Halfway!")
    }
    # Test and train
    
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    #Model fit
    
    full_conf <- full_conformal(train_rs, alpha, reg_alg, test_rs$Sc)
    
    #Definere
    cov[i] <- mean(full_conf$low <= test_rs$Kgp 
                   & full_conf$high >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

set.seed(4)
a <- rs_cov(leafs, 30, 0.1, log_ols_alg)
b <- rs_cov(wood, 30, 0.1, log_ols_alg)
c <- rs_cov(roots, 30, 0.1, log_ols_alg)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1)+
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
  xlim(c(0.5,1.1))+
  labs(title = "Roots")

#Ret jævnt fordelt - plot evt. den teoretiske fordeling


#----------------------------Rolling coverage---------------------------------------------

set.seed(7)
conf_int <- full_conformal(leafs_train, 0.1, log_ols_alg, test_leafs$Sc)

roll_cov <- function(bin_size = 50, test_data, conf_int){
  roll_cov <- c()
  data_arr <- test_data %>%
    arrange(Sc)
  for (i in seq(1,nrow(test_data)-bin_size)){
    data_cov <- leafs_arr %>%
      slice(i:(i+bin_size))
    roll_cov[i] <- mean(conf_int$low <= data_cov$Kgp & data_cov$Kgp  <= conf_int$high)
  }
  my_tib <- tibble("Bin" = seq(1,nrow(test_leafs)-bin_size), "Roll_cov" = roll_cov)
  return
  
  my_plot <- ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
    geom_point(size = 0.6, aes(color = Roll_cov)) + 
    geom_hline(yintercept = 1-0.1, color = "purple")+
    geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
    geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
    theme_bw() +
    xlab('Sc') + 
    ylab('Coverage')+
    labs(title = "Leafs")+
    scale_color_gradient(low = 'blue', high = 'red')
  
  return(my_plot)
}

roll_cov(test_leafs, test_leafs, conf_int)









#Forsøg på at bruge pakke#######################################################

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




#Distribution of coverage by resampling

rs_cov <- function(data, k, alpha, node_size = 100) {
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
    qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
    
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
    cov[i] <- mean(t_05(test$Sc) - q_hat <= test$Kgp &
                     test$Kgp <= t_95(test$Sc) + q_hat)
  }
  return(tibble("Coverage" = cov))
}s

set.seed(4)
a <- rs_cov(leafs, 1, 0.1, 100)
b <- rs_cov(wood, 30, 0.1, 70)

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


#----------------------------Checking coverage for different alphas-------------

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()

set.seed(2)
for (i in (1:4)){
  a <- pred_int_making(leafs_train, node_size = 100, alpha = alphas[i])
  cov_alpha_l[i] <- mean(a[[1]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[2]](test_leafs$Sc))
}

for (i in (1:4)){
  b <- pred_int_making(wood_train, alpha = alphas[i])
  cov_alpha_w[i] <- mean(b[[1]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[2]](test_wood$Sc))
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))

#----------------------------Rolling coverage---------------------------------------------

#Leafs

set.seed(7)
a <- pred_int_making(leafs_train, node_size = 100)

bin_size <- 50
roll_cov <- c()

leafs_arr <- test_leafs %>%
  arrange(Sc)

for (i in seq(1,nrow(test_leafs)-bin_size)){
  data_cov <- leafs_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[1]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[2]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_leafs)-bin_size), "Roll_cov" = roll_cov)

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-0.1, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')


#Wood

set.seed(7)
a <- pred_int_making(wood_train, node_size = 70)

bin_size <- 50
roll_cov <- c()

wood_arr <- test_wood %>%
  arrange(Sc)

for (i in seq(1,nrow(test_wood)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[1]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[2]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_wood)-bin_size), "Roll_cov" = roll_cov)


ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 0.9, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_gradient(low = 'blue', high = 'red')



rs_cov <- function(data, k, alpha, node_size = 100) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked <- sample(n,size = sample_size)
    train_data = data[picked,]
    test_rs = data[-picked,]
    
    # Train and cali
    picked <- sample(seq(1, nrow(train_data)), 0.5*nrow(train_data))
    train <- train_data[picked,]
    cali <- train_data[-picked,]
    train_x <- data.frame(Sc = train[,1])
    train_y <- train[,2]
    qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
    
    #Functions
    qrf_func <- function(x) {
      if (is.atomic(x)){
        x <- data.frame(Sc = x)
        return(predict(qrf, x, what = mean))
      }
      return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
    }
    score <- sort(abs(qrf_func(cali) - cali$Kgp))
    quanti <- ceiling((nrow(cali)+1)*(1-0.1))
    q_hat <- score[quanti]
    
    upper <- function(x) qrf_func(x) + q_hat
    lower <- function(x) qrf_func(x) - q_hat
    
    #Definere
    cov[i] <- mean(lower(test_rs$Sc) <= test_rs$Kgp 
                   &upper(test_rs$Sc) >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}


set.seed(4)
a <- rs_cov(leafs, 30, 0.1, 100)
b <- rs_cov(wood, 30, 0.1, 70)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b)), type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 40)+
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

#Evt. optimer train cali split, og også måske noget med et usikkerhedsmål
pred_int_making <- function(train_data, model, alpha = 0.1) {
  #Test and calibration
  picked <- sample(seq(1, nrow(train_data)), 0.8*nrow(train_data))
  train <- train_data[picked,]
  cali <- train_data[-picked,]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(model(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
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
var_a <- var(a$Coverage)
var_b <- var(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood"), 
              "Mean coverage" =c(mean_a, mean_b),
              "Variance of coverage" =c(var_a, var_b)),
       type = latex)

a %>%
  ggplot() +
  geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1)+
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

#Ret jævnt fordelt
#----------------------------Checking coverage for different alphas-------------------

alphas <- c(0.01, 0.05, 0.1, 0.2)
cov_alpha_l <- c()
cov_alpha_w <- c()

set.seed(2)
for (i in (1:4)){
  a <- pred_int_making(leafs_train, f_hat_l, alpha = alphas[i])
  cov_alpha_l[i] <- mean(a[[1]](test_leafs$Sc) <= test_leafs$Kgp & test_leafs$Kgp  <= a[[2]](test_leafs$Sc))
}

for (i in (1:4)){
  b <- pred_int_making(wood_train, f_hat_w, alpha = alphas[i])
  cov_alpha_w[i] <- mean(b[[1]](test_wood$Sc) <= test_wood$Kgp & test_wood$Kgp  <= b[[2]](test_wood$Sc))
}


xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Rolling coverage-----------------------------------

#Leafs

set.seed(7)
a <- pred_int_making(leafs_train, f_hat_l, alpha = 0.1)

bin_size <- 50
roll_cov <- c()

leafs_arr <- test_leafs %>%
  arrange(Sc)

for (i in seq(1,nrow(test_leafs)-bin_size)){
  data_cov <- leafs_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[1]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[2]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_leafs)-bin_size), "Roll_cov" = roll_cov)

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-0.1, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')


#Wood

set.seed(7)
a <- pred_int_making(wood_train, f_hat_w, alpha = 0.1)

bin_size <- 50
roll_cov <- c()

wood_arr <- test_wood %>%
  arrange(Sc)

for (i in seq(1,nrow(test_wood)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[1]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[2]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_wood)-bin_size), "Roll_cov" = roll_cov)


ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 0.9, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_gradient(low = 'blue', high = 'red')




#Leafs

set.seed(7)
a <- pred_int_making_1(train_leafs_log, alpha = 0.1)

bin_size <- 50
roll_cov <- c()

leafs_arr <- test_leafs %>%
  arrange(Sc)

for (i in seq(1,nrow(test_leafs)-bin_size)){
  data_cov <- leafs_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[5]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[6]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_leafs)-bin_size), "Roll_cov" = roll_cov)

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-0.1, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')


#Wood

set.seed(7)
a <- pred_int_making_1(train_wood_log, alpha = 0.1)

bin_size <- 50
roll_cov <- c()

wood_arr <- test_wood %>%
  arrange(Sc)

for (i in seq(1,nrow(test_wood)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- mean(a[[5]](data_cov$Sc) <= data_cov$Kgp & data_cov$Kgp  <= a[[6]](data_cov$Sc))
}

my_tib <- tibble("Bin" = seq(1,nrow(test_wood)-bin_size), "Roll_cov" = roll_cov)


ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.6, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 0.9, color = "purple")+
  geom_hline(yintercept = qbinom(0.05,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  geom_hline(yintercept = qbinom(0.95,50,0.9)/50, color = "purple", linetype = "dashed", size = 0.3)+
  theme_bw() +
  xlab('') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_gradient(low = 'blue', high = 'red')


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
                 fill = "darkolivegreen3", bins = 30)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  xlim(0.5,1)+
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

#Ret jævnt fordelt - plot evt. den teoretiske fordeling


#Plots

test_leafs_plot <- test_leafs %>%
  mutate(indicator = if_else((a[[5]](Sc) <= Kgp)&(Kgp <= a[[6]](Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = a[[1]], colour = "hotpink4") +
  geom_function(fun = a[[5]], colour = "hotpink") +
  geom_function(fun = a[[6]], colour = "hotpink") +
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
  mutate(indicator = if_else((c[[6]](Sc) <= Kgp)&(Kgp <= c[[5]](Sc)),"in", "out"))

test_wood_plot1 <- test_wood %>%
  mutate(indicator = if_else((d[[6]](Sc) <= Kgp)&(Kgp <= d[[5]](Sc)),"in", "out"))

ggplot(test_leafs_plot1, aes(x = Sc, y = Kgp)) +
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = c[[1]], colour = "hotpink4") +
  geom_function(fun = c[[5]], colour = "hotpink") +
  geom_function(fun = c[[6]], colour = "hotpink") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_wood_plot1, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(x = Sc, y = Kgp, color = indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = d[[1]], colour = "hotpink4") +
  geom_function(fun = d[[5]], colour = "hotpink") +
  geom_function(fun = d[[6]], colour = "hotpink") +
  labs(title = "Wood")+
  scale_color_manual(values = color)


##################################Absolute error score##########################
##---------------------------På OLS log log, naiv og bias adjusted
#----------------------------Pred intervaller--------------------------------

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

my_tib <- tibble("Bin" = seq(1,nrow(leafs_test)-bin_size), "Roll_cov" = roll_cov)

#Mangler lige lidt color coding, men ellers er den god

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.7, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-alpha, color = "purple")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Leafs")+
  scale_color_gradient(low = 'blue', high = 'red')

#Wood

alpha <- 0.2
bin_size <- 50
roll_cov <- c()

wood_arr <- wood_test %>%
  arrange(Sc)

for (i in seq(1,nrow(wood_test)-bin_size)){
  data_cov <- wood_arr %>%
    slice(i:(i+bin_size))
  roll_cov[i] <- coverage(data_cov, quant_up_w, quant_low_w)
}

my_tib <- tibble("Bin" = seq(1,nrow(wood_test)-bin_size), "Roll_cov" = roll_cov)

#Mangler lige lidt color coding, men ellers er den god

ggplot(my_tib, aes(x = Bin, y = Roll_cov)) + 
  geom_point(size = 0.7, aes(color = Roll_cov)) + 
  geom_hline(yintercept = 1-alpha, color = "purple")+
  theme_bw() +
  xlab('Sc') + 
  ylab('Coverage')+
  labs(title = "Wood")+
  scale_color_gradient(low = 'blue', high = 'red')






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

mean_l <- function(x) exp(lm_leafs_log$coefficients[[1]])*x^lm_leafs_log$coefficients[[2]]*exp(sd_hat[1]^2/2)
mean_w <- function(x) exp(lm_wood_log$coefficients[[1]])*x^lm_wood_log$coefficients[[2]]*exp(sd_hat[2]^2/2)
mean_r <- function(x) exp(lm_roots_log$coefficients[[1]])*x^lm_roots_log$coefficients[[2]]*exp(sd_hat[3]^2/2)

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





#gamle måde - Lineære modeller af log-log

lm_leafs_log <- lm(Kgp ~ Sc, data = train_leafs_log)
lm_wood_log <- lm(Kgp ~ Sc, data = train_wood_log)
lm_roots_log <- lm(Kgp ~ Sc, data = train_roots_log)

#Estimater

hat_beta <- c(lm_leafs_log$coefficients[[1]],
              lm_wood_log$coefficients[[1]],
              lm_roots_log$coefficients[[1]])

hat_alpha <- c(lm_leafs_log$coefficients[[2]],
               lm_wood_log$coefficients[[2]],
               lm_roots_log$coefficients[[2]])

var_hat <- c(var(lm_leafs_log$residuals),
             var(lm_wood_log$residuals),
             var(lm_roots_log$residuals))


#log_hat estimater uden bias correction:

f_hat_leafs <- function(x) hat_beta[1] + hat_alpha[1]*x
f_hat_wood <- function(x) hat_beta[2] + hat_alpha[2]*x
f_hat_roots <- function(x) hat_beta[3] + hat_alpha[3]*x

#Y_hat estimater med bias correction: 
f_hat_leafs_exp_adj <- function(x) exp(hat_beta[1] + hat_alpha[1]*x)*exp(var_hat[1]/2)
f_hat_wood_exp_adj <- function(x) exp(hat_beta[2] + hat_alpha[2]*x)*exp(var_hat[2]/2)
f_hat_roots_exp_adj <- function(x) exp(hat_beta[3] + hat_alpha[3]*x)*exp(var_hat[3]/2)


#Prediction intervals log scale

alpha <- 0.1

P_l <- solve((t(model.matrix(lm_leafs_log)) %*% model.matrix(lm_leafs_log)))
P_w <- solve((t(model.matrix(lm_wood_log)) %*% model.matrix(lm_wood_log)))
P_r <- solve((t(model.matrix(lm_roots_log)) %*% model.matrix(lm_roots_log)))

upper_leafs <- function(x) {
  (f_hat_leafs(x) - qt(alpha/2, nrow(train_leafs_log)-2)*
     sqrt(P_l[1,1] + (P_l[2,1] + P_l[1,2])*x+ P_l[2,2]*x^2+ 1)*sqrt(var_hat[1]))
}

lower_leafs <- function(x) {
  (f_hat_leafs(x) - qt(1-alpha/2, nrow(train_leafs_log)-2)*
     sqrt(P_l[1,1] + (P_l[2,1] + P_l[1,2])*x+ P_l[2,2]*x^2+ 1)*sqrt(var_hat[1]))
}

upper_wood <- function(x) {
  (f_hat_wood(x) - qt(alpha/2, nrow(train_wood_log)-2)*
     sqrt(P_w[1,1] + (P_w[2,1] + P_w[1,2])*x+ P_w[2,2]*x^2+ 1)*sqrt(var_hat[2]))
}
lower_wood <- function(x) {
  (f_hat_wood(x) - qt(1-alpha/2, nrow(train_wood_log)-2)*
     sqrt(P_w[1,1] + (P_w[2,1] + P_w[1,2])*x+ P_w[2,2]*x^2+ 1)*sqrt(var_hat[2]))
}
upper_roots <- function(x) {
  (f_hat_roots(x) - qt(alpha/2, nrow(train_roots_log)-2)*
     sqrt(P_r[1,1] + (P_r[2,1] + P_r[1,2])*x+ P_r[2,2]*x^2+ 1)*sqrt(var_hat[3]))
}
lower_roots <- function(x) {
  (f_hat_roots(x) - qt(1-alpha/2, nrow(train_roots_log)-2)*
     sqrt(P_r[1,1] + (P_r[2,1] + P_r[1,2])*x+ P_r[2,2]*x^2+ 1)*sqrt(var_hat[3]))
}

#Plot with prediction intervals log scale

test_leafs_log_plot <- test_leafs_log %>%
  mutate(Indicator = if_else((lower_leafs(Sc) <= Kgp)&(Kgp <= upper_leafs(Sc)),"in", "out"))

test_wood_log_plot <- test_wood_log %>%
  mutate(Indicator = if_else((lower_wood(Sc) <= Kgp)&(Kgp <= upper_wood(Sc)),"in", "out"))

test_roots_log_plot <- test_roots_log %>%
  mutate(Indicator = if_else((lower_roots(Sc) <= Kgp)&(Kgp <= upper_roots(Sc)),"in", "out"))

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
f_hat_wood_exp <- function(x) exp(hat_beta[2])*x^hat_alpha[2]
f_hat_roots_exp <- function(x) exp(hat_beta[3])*x^hat_alpha[3]

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

coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp & upper(data$Sc) >= data$Kgp)
}

a <- coverage(test_leafs, upper_leafs_exp, lower_leafs_exp)
b <- coverage(test_wood, upper_wood_exp, lower_wood_exp)
c <- coverage(test_roots, upper_roots_exp, lower_roots_exp)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)




#Lineære modeller af log-log

lm_leafs <- lm(Kgp ~ Sc, data = leafs_train)
lm_wood <- lm(Kgp ~ Sc, data = wood_train)
lm_roots <- lm(Kgp ~ Sc, data = roots_train)

#Y_hat estimater:

f_hat_leafs <- function(x) lm_leafs$coefficients[1] + lm_leafs$coefficients[2]*x
f_hat_wood <- function(x) lm_wood$coefficients[1] + lm_wood$coefficients[2]*x
f_hat_roots <- function(x) lm_roots$coefficients[1] + lm_roots$coefficients[2]*x

var_hat <- c(var(lm_leafs$residuals), var(lm_wood$residuals), var(lm_roots$residuals))


#Prediction intervals

alpha <- 0.1

P_l <- solve((t(model.matrix(lm_leafs)) %*% model.matrix(lm_leafs)))
P_w <- solve((t(model.matrix(lm_wood)) %*% model.matrix(lm_wood)))
P_r <- solve((t(model.matrix(lm_roots)) %*% model.matrix(lm_roots)))

upper_leafs <- function(x) {
  (f_hat_leafs(x) - qt(alpha/2, nrow(leafs_train)-2)*
     sqrt(P_l[1,1] + P_l[2,1] + P_l[1,2]*x+ P_l[2,2]*x^2+1)*sqrt(var_hat[1]))
}

lower_leafs <- function(x) {
  (f_hat_leafs(x) - qt(1-alpha/2, nrow(leafs_train)-2)*
     sqrt(P_l[1,1] + P_l[2,1] + P_l[1,2]*x+ P_l[2,2]*x^2+ 1)*sqrt(var_hat[1]))
}

upper_wood <- function(x) {
  (f_hat_wood(x) - qt(alpha/2, nrow(wood_train)-2)*
     sqrt(P_w[1,1] + (P_w[2,1] + P_w[1,2])*x+ P_w[2,2]*x^2+ 1)*sqrt(var_hat[2]))
}
lower_wood <- function(x) {
  (f_hat_wood(x) - qt(1-alpha/2, nrow(wood_train)-2)*
     sqrt(P_w[1,1] + (P_w[2,1] + P_w[1,2])*x+ P_w[2,2]*x^2+ 1)*sqrt(var_hat[2]))
}
upper_roots <- function(x) {
  (f_hat_roots(x) - qt(alpha/2, nrow(roots_train)-2)*
     sqrt(P_l[1,1] + (P_r[2,1] + P_r[1,2])*x+ P_r[2,2]*x^2+ 1)*sqrt(var_hat[3]))
}
lower_roots <- function(x) {
  (f_hat_roots(x) - qt(1-alpha/2, nrow(roots_train)-2)*
     sqrt(P_r[1,1] + (P_r[2,1] + P_r[1,2])*x+ P_r[2,2]*x^2+ 1)*sqrt(var_hat[3]))
}
#Plots

test_leafs_plot <- test_leafs %>%
  mutate(Indicator = if_else((lower_leafs(Sc) <= Kgp)&(Kgp <= upper_leafs(Sc)),"in", "out"))

test_roots_plot <- test_roots %>%
  mutate(Indicator = if_else((lower_roots(Sc) <= Kgp)&(Kgp <= upper_roots(Sc)),"in", "out"))

test_wood_plot <- test_wood %>%
  mutate(Indicator = if_else((lower_wood(Sc) <= Kgp)&(Kgp <= upper_wood(Sc)),"in", "out"))

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(test_leafs_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_leafs, colour = "hotpink1") +
  geom_function(fun = upper_leafs, colour = "hotpink4") +
  geom_function(fun = lower_leafs, colour = "hotpink4") +
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(test_wood_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_wood, colour = "hotpink1") +
  geom_function(fun = upper_wood, colour = "hotpink4") +
  geom_function(fun = lower_wood, colour = "hotpink4") +
  labs(title = "wood")+
  scale_color_manual(values = color)

ggplot(test_roots_plot, aes(x = Sc, y = Kgp)) + 
  geom_point(aes(color = Indicator)) + 
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  geom_function(fun = f_hat_roots, colour = "hotpink1") +
  geom_function(fun = upper_roots, colour = "hotpink4") +
  geom_function(fun = lower_roots, colour = "hotpink4") +
  labs(title = "roots")+
  scale_color_manual(values = color)


coverage <- function(data, upper, lower){
  mean(lower(data$Sc) <= data$Kgp &upper(data$Sc) >= data$Kgp)
}

a <- coverage(test_leafs, upper_leafs, lower_leafs)
b <- coverage(test_wood, upper_wood, lower_wood)
c <- coverage(test_roots, upper_roots, lower_roots)

xtable(tibble("Data" = c("Leafs", "Wood", "Roots"), "Coverage" = c(a,b,c)), type = latex)