pred_int_log_ols_conf_2 <- function(data, alpha = 0.2) {
  set.seed(1)
  #Test and calibration
  picked <- sample(seq(1, nrow(data)), 0.6*nrow(data))
  train <- data[picked,]
  cali <- data[-picked,]
  
  #Linear model
  lm <- lm(log(Kgp) ~ log(Sc), train)
  var_hat <- sum(lm$residuals^2)/(nrow(train)-1)
  f_hat <- function(x) exp(lm$coefficients[[1]])*x^lm$coefficients[[2]]
  sd_y_hat <- function(x) sqrt(exp(2*(lm$coefficients[[1]]+log(x)*lm$coefficients[[2]])+var_hat)*(exp(var_hat)-1))
  
  # Heuristic notion of uncertainty
  score <- sort(abs(f_hat(cali$Sc) - cali$Kgp)/sd_y_hat(cali$Sc))
  quanti <- ceiling((nrow(cali)+1)*(1-alpha))
  q_hat <- score[quanti]
  
  #Prediction intervals
  upper <- function(x) f_hat(x) + q_hat*sd_y_hat(x)
  lower <- function(x) f_hat(x) - q_hat*sd_y_hat(x)
  
  return(list(f_hat, upper, lower))
}


loo2 <- loo_pred_int(leafs, alpha = 0.2, pred_int_log_ols_conf_2) 
plot_maker(loo2[[1]], "Leafs")


#----------------------------Checking coverage for different alphas-----------------------------------------
alphas <- c(0.05, 0.1, 0.2, 0.3)

set.seed(2)
cov_alpha_l <- diff_alohas(leafs, pred_int_nlr_l)
cov_alpha_w <- diff_alohas(wood, pred_int_nlr_w)

xtable(tibble("Signif. level" = alphas, "Leafs" = cov_alpha_l, 
              "Wood" = cov_alpha_w))


#----------------------------Distribution of coverage by resampling-----------------------------------------

#Checking for correct coverage

set.seed(4)
a <- rs_cov(data = leafs, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_l)
b <- rs_cov(data = wood, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_w)
c <- rs_cov(data = roots, k = 50, alpha = 0.2, pred_int_maker = pred_int_nlr_r)

#Mean coverage:
mean_a <- mean(a$Coverage)
mean_b <- mean(b$Coverage)
mean_c <- mean(c$Coverage) 

median(b$Coverage)

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean_a, mean_b, mean_c)), type = latex)

rs_plot_maker(a, "Leafs", 0.2)
rs_plot_maker(b, "Wood", 0.2)
rs_plot_maker(c, "Roots", 0.2)


#----------------------------Rolling coverage---------------------------------------------

roll_cov(pred_int = loo_l, title = "Leafs")
roll_cov(pred_int = loo_w, title = "Wood")
roll_cov(pred_int = loo_r, title = "Roots", bin_size = 5)