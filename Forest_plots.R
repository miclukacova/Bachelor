library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)

leafs <- read.csv('Data/leafs.csv')
sample_size_leafs = floor(0.8*nrow(leafs))
set.seed(777)
picked_leafs = sample(seq_len(nrow(leafs)),size = sample_size_leafs)

train_leafs_x = data.frame(Sc = leafs[picked_leafs,1])
train_leafs_y = leafs[picked_leafs,2]
test_leafs_x = data.frame(Sc=leafs[-picked_leafs,1])
test_leafs_y = leafs[-picked_leafs,2]

#Quantile regression forest

qrf <- quantregForest(x = train_leafs_x, y =train_leafs_y, nodesize = 30)
plot(qrf)

conditionalQuantiles <- predict(qrf, test_leafs_x)
conditionalMean <-  predict(qrf, test_leafs_x, what = mean)
conditionalMedian <-  predict(qrf, test_leafs_x, what = median)
head(conditionalMedian)

obs_fitted <- data.frame(observed = test_leafs_y, fitted = conditionalMedian)

#Observed plotted against fitted values
ggplot(data = obs_fitted, aes(x=fitted, y = observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

#Arranging prediction intervals
obs_intervals <- data.frame(observed = test_leafs_y, conditionalQuantiles[,c(1,3)], 
                                interval = conditionalQuantiles[,3]-conditionalQuantiles[,1])
obs_intervals <- arrange(obs_intervals, interval)
head(obs_intervals_cent)

q_mean <- apply(obs_intervals[2:3], MARGIN = 1, FUN = mean)
obs_intervals_cent <- obs_intervals-q_mean


obs_intervals_cent$indx <- as.numeric(row.names(obs_intervals))


ggplot(data = obs_intervals_cent, aes(x = indx)) +
  geom_point(aes(y=quantile..0.1), color = 'hotpink',
             fill = 'hotpink', size = 0.8, shape = 24) +
  geom_point(aes(y=quantile..0.9), color = 'hotpink', 
             fill = 'hotpink', size = 0.8, shape = 25)+
  geom_point(aes(y=observed),color = 'darkolivegreen', size = 0.8)+
  geom_segment(aes(x = indx, y = quantile..0.1, xend = indx, yend = quantile..0.9),
               color = "hotpink", alpha = 0.4, lwd = 0.4) +
  theme_bw()

obs_intervals_cent <- obs_intervals_cent %>%
  mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))

count <- obs_intervals_cent %>%
  count(indicator)

coverage <- count[1,2]/sum(count[,2])
coverage



 #in relation to predictor-variables
plot_data <- data.frame(test_leafs_x, conditionalQuantiles)
head(plot_data)

plot_data <- arrange(plot_data, Sc)
?order

colors <- c("0.1" = "hotpink", "0.5" = "darkolivegreen", "0.9" = "darkolivegreen2")

ggplot(plot_data, aes(x = Sc)) +
  geom_point(aes(y=quantile..0.1, color = "0.1")) +
  geom_point(aes(y=quantile..0.5, color = "0.5")) +
  geom_point(aes(y=quantile..0.9, color = "0.9" )) +
  geom_smooth(aes(y=quantile..0.1, color = "0.1"), se = F)+
  geom_smooth(aes(y=quantile..0.5, color = "0.5"), se = F)+
  geom_smooth(aes(y=quantile..0.9, color = "0.9"), se = F)+
  labs(color = "quantile",
       y = "Quantiles",
       x = "Crown size")+
  theme_bw()+
  scale_color_manual(values = colors)


#Cross-validation to get a plot for all observed values:

matrix(ncol = 3, nrow = nrow(test_leafs_x))

cv <- function(data, k) {
  n <- nrow(data)
  group <- sample(rep(1:k, length.out = n))
  for (i in (1:k)){
    #Fit model
    train_x <- data.frame(Sc = data[group != i,1])
    train_y <- data[group != i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = 20)
    
    #Create matrix
    obs_int[i] <- matrix(ncol = 3, nrow = )
    
    #MSE
    conditionalQuantiles_cv <- predict(qrf_cv, data.frame(Sc = data[group == i,1]), what = c(0.1,0.9))
    obs_int[1,i] <- data[group == i,2]
    obs_int[2,i] <- conditionalQuantiles_cv[,1]
    onb_int[3,i] <- conditionalQuantiles_cv[,2]
  }
  return(tibble("obs_int" = obs_int))
}

cv(leafs,5)
