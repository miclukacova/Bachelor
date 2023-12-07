#Bootstrap functions

coverage <- function(pred_int) {
  mean((pred_int$Low <= pred_int$Kgp) & (pred_int$Kgp <= pred_int$High))
}

model_logolsB <- function(data){
  model <- lm(log(Kgp) ~ log(Sc), data)
  return(list(beta = model$coef[[1]], alpha = model$coef[[2]]))
}

bootstrap_loo <- function(model, data, B, alpha) {
  up <- c() ; down <- c() ; pred <- c()
  for (i in 1:nrow(data)) {
    print(i)
    #Fitting model on training-data:
    fit_mod <- model(data[-i,])
    
    #Computing residuals:
    log_e <- log(data$Kgp[-i]) - fit_mod$beta - log(data$Sc[-i])*fit_mod$alpha
    pred <- append(pred, exp(fit_mod$beta + log(data$Sc[i])*fit_mod$alpha)*exp(var(log_e)/2))
    
    #Bootstrapping:
    y_star_n1 <- c()
    for (b in 1:B) {
      y.star <- exp(fit_mod$beta)*data$Sc[-i]^fit_mod$alpha*exp(sample(log_e))
      boot_data <- data.frame(Sc = data$Sc[-i], Kgp = y.star)
      boot_mod <- model(boot_data)
      e_boot <- log(y.star) - boot_mod$beta - log(data$Sc[-i])*boot_mod$alpha
      y_star_n1 <- append(y_star_n1, exp(boot_mod$beta)*data$Sc[i]^boot_mod$alpha*exp(sample(e_boot,1)))
    }
    up <- append(up, quantile(y_star_n1,1-alpha/2)) ; down <- append(down, quantile(y_star_n1,alpha/2))
  }
  return(tibble(High = up, Low = down, Kgp = data$Kgp, Sc = data$Sc, Fitted = pred))
}


#Snak med Dina
bootstrap <- function(model, train_data, B, alpha, test_data) {
  up <- c() ; down <- c() ; pred <- c()
  for (i in 1:nrow(data)) {
    print(i)
    #Fitting model on training-data:
    fit_mod <- model(data[-i,])
    
    #Computing residuals:
    log_e <- log(data$Kgp[-i]) - fit_mod$beta - log(data$Sc[-i])*fit_mod$alpha
    pred <- append(pred, exp(fit_mod$beta + log(data$Sc[i])*fit_mod$alpha)*exp(var(log_e)/2))
    
    #Bootstrapping:
    y_star_n1 <- c()
    for (b in 1:B) {
      y.star <- exp(fit_mod$beta)*data$Sc[-i]^fit_mod$alpha*exp(sample(log_e))
      boot_data <- data.frame(Sc = data$Sc[-i], Kgp = y.star)
      boot_mod <- model(boot_data)
      e_boot <- log(y.star) - boot_mod$beta - log(data$Sc[-i])*boot_mod$alpha
      y_star_n1 <- append(y_star_n1, exp(boot_mod$beta)*data$Sc[i]^boot_mod$alpha*exp(sample(e_boot,1)))
    }
    up <- append(up, quantile(y_star_n1,1-alpha/2)) ; down <- append(down, quantile(y_star_n1,alpha/2))
  }
  return(tibble(High = up, Low = down, Kgp = data$Kgp, Sc = data$Sc, Fitted = pred))
}

rs_cov_boot <- function(data, k, alpha, pred_int_maker, model, B = 100) {
  cov <- c()
  n <- nrow(data)
  sample_size <- floor(0.8*n)
  
  for (i in (1:k)){
    # Test and train
    set.seed(7)
    picked_rs <- sample(n,size = sample_size)
    train_rs = data[picked_rs,]
    test_rs = data[-picked_rs,]
    
    # Fit model
    model_rs <- pred_int_maker(model, data = train_rs, alpha = alpha, B = B)
    low <- model_rs[[3]](test_rs$Sc)
    high <- model_rs[[2]](test_rs$Sc)
    
    #Definere
    cov[i] <- mean(low <= test_rs$Kgp & high >= test_rs$Kgp)
  }
  return(tibble("Coverage" = cov))
}

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
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 2, alpha = 1) + 
    geom_line(aes(x = Sc, y = High), color = "hotpink", size = 1, alpha = 1) + 
    geom_line(aes(x = Sc, y = Low), color = "hotpink", size = 1, alpha = 1) +
    geom_line(aes(x = Sc, y = Fitted), color = "hotpink4", size = 1, alpha = 1) +
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
    set.seed(7)
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

