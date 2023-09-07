leafs_log <- read.csv('Data/leafs_log.csv')
roots_log <- read.csv('Data/roots_log.csv')
wood_log <- read.csv('Data/wood_log.csv')

# Split Data 
sample_size = floor(0.8*nrow(leafs_log))
set.seed(777)

# Randomly split data
picked = sample(seq_len(nrow(leafs_log)),size = sample_size1)
train_leafs_log = leafs_log[picked,]
test_leafs_log = leafs_log[-picked,]

nrow(train_leafs_log)
nrow(test_leafs_log)

# The linear model

lm <- lm(Bfkg ~ Sc, train_leafs_log)

sd_hat <- sqrt(sum(lm$residuals^2)/(nrow(train_leafs_log)-1))

f_hat <- function(x) lm$coefficients[[2]]*x + lm$coefficients[[1]] 

alpha <- 0.1

upper <- function(x) {
  f_hat(x) - qt(alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sd_hat
}

lower <- function(x) {
  f_hat(x) - qt(1-alpha/2, nrow(train_leafs_log)-1)*sqrt(x^2/sum(train_leafs_log$Sc^2)+1)*sd_hat
}

#Plot with prediction intervals

ggplot(test_leafs_log, aes(x = Sc, y = Bfkg)) + 
  geom_point() + 
  theme_bw() +
  xlab('log(Sc)') + 
  ylab('log(Bfkg)')+
  geom_function(fun = f_hat, colour = "red") +
  geom_function(fun = upper, colour = "blue") +
  geom_function(fun = lower, colour = "blue") +
  labs(title = "Bfkg as function of Sc with Standard Gaussian prediction intervals")

