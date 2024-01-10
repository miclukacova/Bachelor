#Getting data
leafs <- read.csv('Data/leafs.csv')
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")


#Comparison of lognormal quantile and the Gaussian prediction interval
 #Defining the lognormal-quantile prediction interval:
pred_int_quant <- function(alpha = 0.2, data){
  
  #model fit
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  f_hat_log <- function(x) model$coef[[1]] + log(x)*model$coef[[2]]
  
  #creation of prediction interval
  
  upper <- function(x) qlnorm(1-alpha/2, meanlog = f_hat_log(x), sdlog = sqrt(var(model$residuals))) 
  lower <- function(x)  qlnorm(alpha/2, meanlog = f_hat_log(x), sdlog = sqrt(var(model$residuals)))
  
  return(list(f_hat, upper, lower))
}

#Creating a function to plot them alongside eachother:

logn_gauss <- function(data, title) {
  pred_int_gauss <- loo_pred_int(data = data, pred_int = pred_int_log_ols, alpha = 0.2)
  pred_int_logn <- loo_pred_int(data = data, pred_int = pred_int_quant, alpha = 0.2)
  
  
  pred_plot_gauss <- pred_int_gauss[[1]] %>%
    mutate(Indicator = if_else((Low <= Kgp)&(Kgp <= High),"in", "out"))
  
  
  plot_tibble <- tibble(Logn_low = pred_int_logn[[1]]$Low, Logn_high = pred_int_logn[[1]]$High,
                        Gauss_low = pred_plot_gauss$Low, Gauss_high = pred_plot_gauss$High,
                        Sc = pred_plot_gauss$Sc, Kgp = pred_plot_gauss$Kgp,
                        Indicator = pred_plot_gauss$Indicator)
  
  
  color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")
  
  ggplot(plot_tibble, aes(x = Sc, y = Kgp)) +
    geom_point(aes(x = Sc, y = Kgp, color = Indicator), size = 1.7, alpha = 0.7) + 
    geom_line(aes(x = Sc, y = Logn_high), color = "green4", size = 1, alpha = 1) + 
    geom_line(aes(x = Sc, y = Logn_low), color = "green4", size = 1, alpha = 1) +
    geom_line(aes(x = Sc, y = Gauss_high), color = "hotpink", size = 1, alpha = 1) + 
    geom_line(aes(x = Sc, y = Gauss_low), color = "hotpink", size = 1, alpha = 1) +
    theme_bw() +
    xlab('Sc') + 
    ylab('Kgp')+
    labs(title = title)+
    
    scale_color_manual(values = color)+
    theme( legend.title = element_blank(),
           legend.position = "none", legend.background = element_rect(linetype = 'solid', color = 'black'),
           plot.title = element_text(size = 19),
           axis.title = element_text(size = 15), legend.text = element_text(size = 13),
           text = element_text(family = "serif"), axis.text = element_text(size = 13))
}

set.seed(4)
logn_gauss(leafs, "Leafs")
logn_gauss(wood, "Wood")
logn_gauss(roots, "Roots")



diff_term <- function(alpha = 0.2, data){
  #model fit
  model <- lm(log(Kgp) ~ log(Sc), data)
  f_hat <- function(x) exp(model$coef[[1]])*x^model$coef[[2]]*exp(var(model$residuals)/2)
  f_hat_log <- function(x) model$coef[[1]] + x*model$coef[[2]]
  
  #Lognormal quantile prediction intervals
  output1 <- function(x) 1
  
  #creation of prediction interval
  P <- solve((t(model.matrix(model)) %*% model.matrix(model)))
  output2 <- function(x) {sqrt(P[1,1] + (P[2,1] + P[1,2])*x+ P[2,2]*x^2+ 1)}
  return(list(output1,output2))
}

a <- diff_term(data = leafs)
b <- diff_term(data = wood)
c <- diff_term(data = roots)

xtable(tibble("Leafs" = mean(a[[2]](log(leafs$Sc))), "Wood" = mean(b[[2]](log(wood$Sc))),
       "Roots" = mean(c[[2]](log(roots$Sc)))), digits = 4)

a1 <- 
b1 <- diff_term(data = wood)
c1 <- diff_term(data = roots)

#The quantiles are the same up to 4 digits

xtable(tibble(" "= c("q_t(0.1)","q_t(0.9)","q_n(0.1)","q_n(0.9)"), 
"Leafs" =c(qt(0.1, df = nrow(leafs)-2) , qt(0.9, df = nrow(leafs)-2),
           qnorm(0.1), qnorm(0.9)),
"Wood" = c(qt(0.1, df = nrow(wood)-2),qt(0.9, df = nrow(wood)-2),
           qnorm(0.1), qnorm(0.9)),
"Roots" = c(qt(0.1, df = nrow(roots)-2),qt(0.9, df = nrow(roots)-2),
            qnorm(0.1), qnorm(0.9))), digits = 4)







?xtable




