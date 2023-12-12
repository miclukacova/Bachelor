################################################################################
#Bootstrap functions
################################################################################

#Model skal spytte et alpha og et beta estimat ud.

model_logols <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  return(list(beta = exp(model$coef[[1]]), alpha = model$coef[[2]]))
}

model_ols <- function(data){
  model <- lm(Kgp ~ Sc, data)
  return(list(beta = model$coef[[1]], alpha = model$coef[[2]]))
}

model_logolsB <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  bias_term <- exp(var(model$residuals)/2)
  model$coef[[1]] <- exp(model$coef[[1]])*bias_term
  return(list(beta = model$coef[[1]], alpha = model$coef[[2]]))
}

MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}

model_NLR <- function(data, starting_point){
  model <- optim(par = starting_point, fn = MSE_NLR, data = data)
  return(list(beta = model$par[1], alpha = model$par[2]))
}

bootstrap_loo <- function(model, data, B, alpha) {
  up <- c() ; down <- c() ; pred <- c()
  for (i in 1:nrow(data)) {
    print(i)
    #Fitting model on training-data:
    fit_mod <- model(data[-i,])
    
    #Computing residuals:
    e <- data$Kgp[-i] / (fit_mod$beta * data$Sc[-i]^fit_mod$alpha)
    pred <- append(pred, fit_mod$beta * data$Sc[i]^fit_mod$alpha)
    
    #Bootstrapping:
    y_star_n1 <- c()
    for (b in 1:B) {
      y.star <- fit_mod$beta*data$Sc[-i]^fit_mod$alpha*sample(e, replace = TRUE)
      boot_data <- data.frame(Sc = data$Sc[-i], Kgp = y.star)
      boot_mod <- model(boot_data)
      e_boot <- y.star / (boot_mod$beta * data$Sc[-i]^boot_mod$alpha)
      y_star_n1 <- append(y_star_n1, boot_mod$beta*data$Sc[i]^boot_mod$alpha*sample(e_boot,1))
    }
    up <- append(up, quantile(y_star_n1,1-alpha/2)) ; down <- append(down, quantile(y_star_n1,alpha/2))
  }
  return(tibble(High = up, Low = down, Kgp = data$Kgp, Sc = data$Sc, Fitted = pred))
}

coverage <- function(pred_int) {
  mean((pred_int$Low <= pred_int$Kgp) & (pred_int$Kgp <= pred_int$High))
}

#rs_cov_boot <- function(data, k, alpha, model, B = 300) {
  cov <- c(); n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    print(i)
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    #Model fit
    model_rs <- model(train_rs)
    e <- train_rs$Kgp / (model_rs$beta * train_rs$Sc^model_rs$alpha)
    pred <- model_rs$beta * test_rs$Sc^model_rs$alpha
    
    #Bootstrapping:
    for (j in 1:nrow(test_rs)) {
      up <- c() ; down <- c()
      y_star_n1 <- c()
      for (b in 1:B) {
        y.star <- model_rs$beta*train_rs$Sc^model_rs$alpha*sample(e)
        boot_data <- data.frame(Sc = train_rs$Sc, Kgp = y.star)
        boot_mod <- model(boot_data)
        e_boot <- y.star / (boot_mod$beta * train_rs$Sc^boot_mod$alpha)
        y_star_n1 <- append(y_star_n1, boot_mod$beta*test_rs$Sc[j]^boot_mod$alpha*sample(e_boot,1))
      }
      up <- append(up, quantile(y_star_n1,1-alpha/2)) ; down <- append(down, quantile(y_star_n1, alpha/2))
    }
    
    #Definere
    cov[i] <- mean((down <= test_rs$Kgp) & (up >= test_rs$Kgp))
  }
  return(tibble("Coverage" = cov))
}

rs_cov_boot <- function(data, k, alpha, model, B = 1000) {
  cov <- c(); n <- nrow(data) ; sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    print(i)
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    #Model fit
    model_rs <- model(train_rs)
    e <- train_rs$Kgp / (model_rs$beta * train_rs$Sc^model_rs$alpha)
    pred <- model_rs$beta * test_rs$Sc^model_rs$alpha
    
    #Bootstrapping:
    for (b in 1:B) {
      y.star <- model_rs$beta*train_rs$Sc^model_rs$alpha*sample(e, replace = TRUE)
      boot_data <- data.frame(Sc = train_rs$Sc, Kgp = y.star)
      boot_mod <- model(boot_data)
      e_boot <- y.star / (boot_mod$beta * train_rs$Sc^boot_mod$alpha)
      if (b == 1){
        y_star_n1 <- tibble(obs = c(1:nrow(test_rs)), 
                            y_star_n1 = boot_mod$beta*test_rs$Sc^boot_mod$alpha*
                              sample(e_boot, nrow(test_rs), replace = TRUE))
      }
      else {
        y_star_n1 <- y_star_n1 %>% add_row(obs = c(1:nrow(test_rs)), 
                                           y_star_n1 = boot_mod$beta*test_rs$Sc^boot_mod$alpha*
                                             sample(e_boot, nrow(test_rs), replace = TRUE))
      }
    }
    up <- c() ; down <- c()
    for (j in (1:nrow(test_rs))){
      obs_i <- y_star_n1 %>% filter(obs == j)
      up <- append(up, quantile(obs_i$y_star_n1,1-alpha/2)) ; down <- append(down, quantile(obs_i$y_star_n1, alpha/2))
    }
    
    #Definere
    cov[i] <- mean((down <= test_rs$Kgp) & (test_rs$Kgp <= up))
  }
  return(tibble("Coverage" = cov))
}

roll_cov_boot <- function(pred_int, alpha = 0.2, bin_size = 50, title){
  roll_cov <- c()
  
  data_arr <- pred_int %>%
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
    xlab('Sc') + 
    ylab('Coverage')+
    labs(title = title)+
    scale_color_gradient(low = 'blue', high = 'red')
}

diff_alohas <- function(data, model, B = 150, alpha = 0.2){
  alphas <- c(0.05, 0.1, 0.2, 0.3)
  cov_alpha <- c()
  for (i in (1:4)){
    alpha <- alphas[i]
    pred <- bootstrap_loo(data = data, model = model, alpha = alpha, B = B)
    cov_alpha[i] <- coverage(pred)
  }
  return(cov_alpha)
}

################################################################################
# Til Gaussiske og lognormale kvantiler
################################################################################


#Creation of prediction intervals

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

plot_maker <- function(pred_int, title){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((Low <= Kgp)&(Kgp <= High),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 0.8, alpha = 0.7) + 
    geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, alpha = 0.7) + 
    geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, alpha = 0.7) +
    geom_line(aes(x = Sc, y = Fitted), color = "hotpink4", size = 1, alpha = 0.7) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    scale_color_manual(values = color)+ 
    theme(legend.position = "none")
}

#Distribution of coverage

rs_cov <- function(data, k, alpha, pred_int_maker) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit model
    model_rs <- pred_int_maker(data = train_rs, alpha = alpha)
    low <- model_rs[[3]](test_rs$Sc)
    high <- model_rs[[2]](test_rs$Sc)
    
    #Definere
    cov[i] <- mean(low <= test_rs$Kgp & high >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

rs_plot_maker <- function(rs_cov, title, alpha, conformal = FALSE, n=0){
  plot <- rs_cov %>%
    ggplot() +
    geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                   fill = "darkolivegreen3", bins = 40)+
    geom_vline(xintercept = 1-alpha, color = "hotpink4") +
    xlim(0,1.1)+
    theme_bw()+
    labs(title = title)
  if (conformal == TRUE){
    n_b <- n*0.4
    beta <- function(x) dbeta(x, n_b+1-floor((n_b+1)*alpha),
                                  floor((n_b+1)*alpha))
    plot <- plot + geom_function(fun = beta , colour = "darkolivegreen", alpha = 0.8)
  }
  return(plot)
}


#Rolling coverage

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
    xlab('Sc') + 
    ylab('Coverage')+
    labs(title = title)+
    scale_color_gradient(low = 'blue', high = 'red')
}

#Checking coverage for different alphas

diff_alohas <- function(data, pred_int){
  alphas <- c(0.05, 0.1, 0.2, 0.3)
  cov_alpha <- c()
  for (i in (1:4)){
    alpha <- alphas[i]
    pred <- loo_pred_int(data = data, pred_int = pred_int, alpha = alpha)
    cov_alpha[i] <- pred[[2]]
  }
  return(cov_alpha)
}

