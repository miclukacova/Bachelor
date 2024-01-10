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
  with(data, sum((Kgp-par[1]*Sc^par[2])^2)/nrow(data))
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
    xlab('Bin') + 
    ylab('Coverage')+
    ylim(c(0,1))+
    labs(title = title)+
    scale_color_gradient2(low = 'blue', mid = 'purple', high = 'red', midpoint = 0.8, limits = c(0.6,1),
                          na.value = "blue")+
    theme(legend.position = "none", plot.title = element_text(size = 17),
          axis.title = element_text(size = 13))
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

plot_maker <- function(pred_int, title, fun, roots = F){
  
  pred_plot <- pred_int %>%
    mutate(Indicator = if_else((Low <= Kgp)&(Kgp <= High),"in", "out"))
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  if(roots == T){
  ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
    geom_segment(aes(x = Sc, y = Low, xend = Sc, yend = High),
                color = "hotpink", alpha = 0.4, lwd = 0.6) +
    geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, alpha = 0.7) + 
    geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, alpha = 0.7) +
    geom_function(fun = fun, color = 'hotpink4', size = 0.8)+
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1.7, alpha = 1) +
    theme_bw() +
    xlab('Crown Size') + 
    ylab('Biomass')+
    labs(title = title)+
    scale_color_manual(values = color)+
      theme( legend.title = element_blank(),
             legend.position = "none", legend.background = element_rect(linetype = 'solid', color = 'black'),
             plot.title = element_text(size = 19),
             axis.title = element_text(size = 15), legend.text = element_text(size = 13),
             text = element_text(family = "serif"), axis.text = element_text(size = 13))
  }
  else{
    ggplot(pred_plot, aes(x = Sc, y = Kgp)) +
      geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1, alpha = 1) + 
      geom_point(aes(x = Sc, y = High), color = "hotpink", size = 1, alpha = 0.7) + 
      geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 1, alpha = 0.7) +
      geom_function(fun = fun, color = 'hotpink4', size = 0.8)+
      theme_bw() +
      xlab('Crown Size') + 
      ylab('Biomass')+
      labs(title = title)+
      scale_color_manual(values = color)+
      theme( legend.title = element_blank(),
             legend.position = "none", legend.background = element_rect(linetype = 'solid', color = 'black'),
             plot.title = element_text(size = 19),
             axis.title = element_text(size = 15), legend.text = element_text(size = 13),
             text = element_text(family = "serif"), axis.text = element_text(size = 13))
  }
  
}

#Distribution of coverage

rs_cov <- function(data, k = 50, alpha, pred_int_maker) {
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
    geom_vline(xintercept = 1-alpha, color = "hotpink2", lwd = 0.9) +
    xlim(0,1.1)+
    theme_bw()+
    labs(title = title)   +
    theme( legend.title = element_blank(),
           legend.position = "none", legend.background = element_rect(linetype = 'solid', color = 'black'),
           plot.title = element_text(size = 19),
          axis.title = element_text(size = 15), legend.text = element_text(size = 13),
          text = element_text(family = "serif"), axis.text = element_text(size = 13))
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
    xlab('Bin') + 
    ylab('Coverage')+
    ylim(c(0,1))+
    labs(title = title)+
    scale_color_gradient2(low = 'blue', mid = 'purple', high = 'red', midpoint = 0.8, limits = c(0.6,1),
                          na.value = "blue")+
    theme( legend.title = element_blank(),
           legend.position = "none", legend.background = element_rect(linetype = 'solid', color = 'black'),
           plot.title = element_text(size = 19),
           axis.title = element_text(size = 15), legend.text = element_text(size = 13),
           text = element_text(family = "serif"), axis.text = element_text(size = 13))
}

#Checking coverage for different alphas

diff_alohas <- function(data, pred_int, k){
  alphas <- c(0.05, 0.1, 0.2, 0.3)
  cov_alpha <- c()
  for (i in (1:4)){
    print(i)
    cov <- c(); group <- sample(rep(1:k, length.out = nrow(data))); alpha <- alphas[i]
    
    for (j in (1:k)){
      # Test and train
      train_data = data[group != j,]
      test_data = data[group == j,]
      pred_model <- pred_int(data = train_data, alpha = alpha)
      cov[j] <- mean(pred_model[[3]](test_data$Sc) <= test_data$Kgp 
                  & pred_model[[2]](test_data$Sc) >= test_data$Kgp)
    }
    
    cov_alpha[i] <- mean(cov)
  }
  return(cov_alpha)
}




#--------K-fold for bootstrap----------------------------------------------------------:
kfold_boot <- function(data, k, alpha, model, B = 1000) {
  cov <- c(); group <- sample(rep(1:k, length.out = nrow(data)))
  
  for (i in (1:k)){
    print(i)
    # Test and train
    train_data = data[group != i,]
    test_data = data[group == i,]
    fit_mod <- model(train_data)
    
    #Residuals and predicted values
    e <- train_data$Kgp / (fit_mod$beta * train_data$Sc^fit_mod$alpha)
    pred <- fit_mod$beta * test_data$Sc^fit_mod$alpha
    
    #Bootstrapping:
    for (b in 1:B) {
      y.star <- fit_mod$beta*train_data$Sc^fit_mod$alpha*sample(e, replace = TRUE)
      boot_data <- data.frame(Sc = train_data$Sc, Kgp = y.star)
      boot_mod <- model(boot_data)
      e_boot <- y.star / (boot_mod$beta * train_data$Sc^boot_mod$alpha)
      if (b == 1){
        y_star_n1 <- tibble(obs = c(1:nrow(test_data)), 
                            y_star_n1 = boot_mod$beta*test_data$Sc^boot_mod$alpha*
                              sample(e_boot, nrow(test_data), replace = TRUE))
      }
      else {
        y_star_n1 <- y_star_n1 %>% add_row(obs = c(1:nrow(test_data)), 
                                           y_star_n1 = boot_mod$beta*test_data$Sc^boot_mod$alpha*
                                             sample(e_boot, nrow(test_data), replace = TRUE))
      }
    }
    up <- c() ; down <- c()
    for (j in (1:nrow(test_data))){
      obs_i <- y_star_n1 %>% filter(obs == j)
      up <- append(up, quantile(obs_i$y_star_n1,1-alpha/2)) ; down <- append(down, quantile(obs_i$y_star_n1, alpha/2))
    }
    
    #Definere
    cov[i] <- mean((down <= test_data$Kgp) & (test_data$Kgp <= up))
  }
  return(mean(cov))
}

diff_alohas_boot <- function(data, model, B = 300, k = 5){
  alphas <- c(0.05, 0.1, 0.2, 0.3)
  cov_alpha <- c()
  for (i in (1:4)){
    alpha <- alphas[i]
    cov_alpha[i] <- kfold_boot(data = data, model = model, alpha = alpha, B = B, k = k)
  }
  return(cov_alpha)
}


###############################################################################
#Conformal prediction
###############################################################################

#logols

#Score funktion Absolute error
s2_logolsb_conf <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat_adj <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]*exp(var_hat/2)
  
  # Heuristic notion of uncertainty
  score_adj <- sort(abs(f_hat_adj(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat_adj <- score_adj[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat_adj(x) + q_hat_adj
  lower <- function(x) f_hat_adj(x) - q_hat_adj
  
  return(list(f_hat_adj, upper,lower))
}
s2_logols_conf <- function(data, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper, lower))
}

#Score funktion Absolute error / sd hat(Y) (VIRKER IKKE SÅ GODT)
s1_logolsb_conf <- function(data, alpha = 0.2) {
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


#nlr

MSE_NLR <- function(par, data){
  with(data, sqrt(sum((Kgp-par[1]*Sc^par[2])^2)/nrow(data)))
}

nlr_alg <- function(data, start_point){
  mod <- optim(par = start_point, fn = MSE_NLR, data = data)
  f_hat <- function(x) mod$par[[1]]*x^mod$par[[2]]
  return(f_hat)
}

pred_int_nlr <- function(data, alpha = 0.2, starting_points) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  f_hat <- nlr_alg(data, starting_points)
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper,lower))
}

pred_int_nlr_l <- function(data, alpha = 0.2) pred_int_nlr(data, alpha, c(0.2693082, 0.9441130))
pred_int_nlr_w <- function(data, alpha = 0.2) pred_int_nlr(data, alpha, c(3.944818, 1.106841))
pred_int_nlr_r <- function(data, alpha = 0.2) pred_int_nlr(data, alpha = 0.2, c(0.8339087, 1.1730237))

#RF

pred_int_making <- function(data, node_size = 70, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  f_hat <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali) - cali$Kgp))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) f_hat(x) + q_hat
  lower <- function(x) f_hat(x) - q_hat
  
  return(list(f_hat, upper, lower ))
}

pred_int_rf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = alpha)
pred_int_rf_w <- function(data, alpha = 0.2) pred_int_making(data, node_size = 70, alpha = alpha)
pred_int_rf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = alpha, node_size = 26)

#Conformalised quantile regression forest

pred_int_making <- function(data, node_size = 70, alpha = 0.2) {
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.5*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  train_x <- data.frame(Sc = train[,1])
  train_y <- train[,2]
  qrf <- quantregForest(x = train_x, y = train_y, nodesize = node_size)
  
  f_hat <- function(x) {
    if (is.atomic(x)){
      x <- data.frame(Sc = x)
      return(predict(qrf, x, what = mean))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = mean))
  }
  t_down <- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = alpha/2))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = alpha/2))
  }
  t_up<- function(x) {
    if (is.atomic(x)){
      return(predict(qrf, data.frame(Sc = x), what = 1-alpha/2))
    }
    return(predict(qrf, data.frame(Sc = x$Sc), what = 1-alpha/2))
  }
  
  # Heuristic notion of uncertainty
  score <- c()
  for (i in (1:nrow(cali))){
    score[i] <- max((t_down(cali[i,]) - cali[i,]$Kgp), (cali[i,]$Kgp- t_up(cali[i,])))
  }
  score <- sort(score)
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction interval functions
  
  upper <- function(x) t_up(x) + q_hat
  lower <- function(x) t_down(x) - q_hat
  
  return(list(f_hat, upper, lower ))
}

pred_int_qrf_l <- function(data, alpha = 0.2) pred_int_making(data, node_size = 100, alpha = 0.2)
pred_int_qrf_w <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2)
pred_int_qrf_r <- function(data, alpha = 0.2) pred_int_making(data, alpha = 0.2, node_size = 26)

