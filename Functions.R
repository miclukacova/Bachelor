#Creation of prediction intervals

loo_pred_int <- function(data, alpha = 0.2, pred_int) {
  low <- c()
  high <- c()
  fitted <- c()
  for (i in (1:nrow(data))){
    if (i == nrow(data)/2){
      print("halfway!")
    }
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
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 0.8, alpha = 0.5) + 
    geom_point(aes(x = Sc, y = High), color = "hotpink", size = 0.6, alpha = 0.5) + 
    geom_point(aes(x = Sc, y = Low), color = "hotpink", size = 0.6, alpha = 0.5) +
    geom_point(aes(x = Sc, y = Fitted), color = "hotpink4", size = 0.6, alpha = 0.5) +
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

rs_plot_maker <- function(rs_cov, title, alpha){
  rs_cov %>%
    ggplot() +
    geom_histogram(aes(x = Coverage, y = ..density..), color = "white", 
                   fill = "darkolivegreen3", bins = 40)+
    geom_vline(xintercept = 1-alpha, color = "hotpink") +
    xlim(0,1.1)+
    theme_bw()+
    labs(title = title)
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

