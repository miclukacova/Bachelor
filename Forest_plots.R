library(randomForest)
library(quantregForest)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(xtable)

set.seed(777)
leafs_train <- read.csv('Data/train_leafs.csv')
roots_train <- read.csv('Data/train_roots.csv')
wood_train<- read.csv('Data/train_wood.csv')

test_leafs <- read.csv('Data/test_leafs.csv')
test_roots <- read.csv('Data/test_roots.csv')
test_wood <- read.csv('Data/test_wood.csv')

train_leafs_x = data.frame(Sc = leafs_train[,1])
train_leafs_y = leafs_train[,2]
test_leafs_x = data.frame(Sc=test_leafs[,1])
test_leafs_y = test_leafs[,2]

train_wood_x = data.frame(Sc = wood_train[,1])
train_wood_y = wood_train[,2]
test_wood_x = data.frame(Sc=test_wood[,1])
test_wood_y = test_wood[,2]

train_roots_x = data.frame(Sc = roots_train[,1])
train_roots_y = roots_train[,2]
test_roots_x = data.frame(Sc=test_roots[,1])
test_roots_y = test_roots[,2]

#Quantile regression forest for leafs

set.seed(4)
qrf <- quantregForest(x = train_leafs_x, y =train_leafs_y, nodesize = 100)
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

plot_data <- data.frame(obs_intervals, pred = test_leafs_x)
head(plot_data)

plot_data <- plot_data %>%
  mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))

head(obs_intervals)
colors <- c("Quantiles" = "darkolivegreen", "Observed" = "hotpink")

ggplot(plot_data, aes(x = Sc)) +
  geom_point(aes(y=quantile..0.9, color = "Quantiles"), fill = "darkolivegreen", shape = 25, alpha = 0.7) +
  geom_point(aes(y=quantile..0.1, color = "Quantiles"), fill = "darkolivegreen", shape = 24, alpha = 0.7) +
  geom_segment(aes(x = Sc, y = quantile..0.1, xend = Sc, yend = quantile..0.9),
               color = "darkolivegreen3", alpha = 0.2, lwd = 0.9)+
  geom_point(aes(y=observed, color = "Observed"), alpha=0.5, fill = "hotpink3") +
  labs(title = "Foliage")+
  xlab(bquote('Crown area'~(m^2/plant)))+
  ylab('Foliage dry mass (kg/plant)')+
  theme_bw()+
  scale_color_manual(values = colors)

#in relation to predictor-variables
plot_data <- data.frame(test_leafs_x, conditionalQuantiles, pred = test_leafs_x)
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
cv <- function(data, k) {
  set.seed(777)
  n <- nrow(data)
  group <- sample(rep(1: k, length.out = n))
  pred <- c()
  obs <- c()
  q0.1 <- c()
  q0.9 <- c()
  for (i in (1:k)){
    #Fit model
    train_x <- data.frame(Sc = data[group != i,1])
    train_y <- data[group != i,2]
    test_x <- data.frame(Sc=data[group == i,1])
    test_y <- data[group == i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = 76)
    
    #Getting quantiles
    conditionalQuantiles_cv <- predict(qrf_cv, test_x, what = c(0.05,0.95))
    
    #Creating vectors with observations and quantiles
    if (i == 1) {
      pred <- c(data[group == i,1])
      obs <- c(test_y)
      q0.1 <- c(conditionalQuantiles_cv[,1])
      q0.9 <- c(conditionalQuantiles_cv[,2])
    }
    else {
    pred <- c(pred, data[group == i,1])
    obs <- c(obs, test_y)
    q0.1 <- c(q0.1, conditionalQuantiles_cv[,1])
    q0.9 <- c(q0.9, conditionalQuantiles_cv[,2])
    }
    }
  return("obs_int" = data.frame(observed = obs, quantile..0.1 = q0.1,quantile..0.9 = q0.9, pred=pred))
}

cv_intervals_leafs <- cv(leafs,10)
cv_intervals_wood <- cv(wood, 10)
cv_intervals_roots <- cv(roots, 10)

cv_int <- function(x){
  cv_intervals <- data.frame(x, 
                             interval = x$quantile..0.9-x$quantile..0.1)
  cv_intervals <- arrange(cv_intervals, interval)
  return(cv_intervals)
}

cv_intervals_leafs <- cv_int(cv_intervals_leafs)
cv_intervals_wood <- cv_int(cv_intervals_wood)
cv_intervals_roots <- cv_int(cv_intervals_roots)


centered_plot <- function(data, title, y){
  q_mean <- apply(data[2:3], MARGIN = 1, FUN = mean)
  cv_intervals_cent <- data-q_mean
  cv_intervals_cent$indx <- as.numeric(row.names(cv_intervals_cent))
  cv_intervals_cent <- cv_intervals_cent %>%
    mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))
  colors <- c("in" = "hotpink", "out" = "hotpink4")
  ggplot(data = cv_intervals_cent, aes(x = indx)) +
    geom_segment(aes(x = indx, y = quantile..0.1, xend = indx, yend = quantile..0.9),
                 color = "darkolivegreen3", alpha = 0.2, lwd = 0.4) +
    geom_point(aes(y=quantile..0.1), color = 'darkolivegreen3',
               fill = 'darkolivegreen3', size = 0.8, shape = 24, alpha = 0.4) +
    geom_point(aes(y=quantile..0.9), color = 'darkolivegreen3', 
               fill = 'darkolivegreen3', size = 0.8, shape = 25, alpha = 0.4)+
    geom_point(aes(y=observed, color = indicator), size = 0.7)+
    scale_color_manual(values = colors)+
    ylab(y)+ ggtitle(title)+
    xlab('Ordered Samples')+
    theme_bw()
}

#Plots for leafs and wood
centered_plot(cv_intervals_leafs,'Leafs','Leaf dry mass (kg/plant)')
centered_plot(cv_intervals_wood,'Wood','Wood dry mass (kg/plant)')

#plot for roots
centered_plot_roots <- function(data){
  q_mean <- apply(data[2:3], MARGIN = 1, FUN = mean)
  cv_intervals_cent <- data-q_mean
  cv_intervals_cent$indx <- as.numeric(row.names(cv_intervals_cent))
  cv_intervals_cent <- cv_intervals_cent %>%
    mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))
  colors <- c("in" = "hotpink", "out" = "hotpink4")
  ggplot(data = cv_intervals_cent, aes(x = indx)) +
    geom_segment(aes(x = indx, y = quantile..0.1, xend = indx, yend = quantile..0.9),
                 color = "darkolivegreen3", alpha = 0.5, lwd = 0.9) +
    geom_point(aes(y=quantile..0.1), color = 'darkolivegreen3',
               fill = 'darkolivegreen3', size = 0.9, shape = 24, alpha = 0.9) +
    geom_point(aes(y=quantile..0.9), color = 'darkolivegreen3', 
               fill = 'darkolivegreen3', size = 0.9, shape = 25, alpha = 0.9)+
    geom_point(aes(y=observed, color = indicator))+
    scale_color_manual(values = colors)+
    ylab('Root dry mass (kg/plant)')+ ggtitle('Roots')+
    xlab('Ordered Samples')+
    theme_bw()
}

centered_plot_roots(cv_intervals_roots)



count <- cv_intervals_cent %>%
  count(indicator)

coverage <- count[1,2]/sum(count[,2])
coverage

#In relation to predictor variables
Sc_plot <- function(data,title, y){
  plot_data <- data.frame(data)
  plot_data <- plot_data %>%
    mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))
  #Plot
  ggplot(plot_data, aes(x = pred)) +
    geom_point(aes(y=quantile..0.9), color = 'darkolivegreen', fill = "darkolivegreen", shape = 24, alpha = 0.7) +
    geom_point(aes(y=quantile..0.1), color = 'darkolivegreen', fill = "darkolivegreen", shape = 24, alpha = 0.7) +
    geom_segment(aes(x = pred, y = quantile..0.1, xend = pred, yend = quantile..0.9),
                 color = "darkolivegreen3", alpha = 0.2, lwd = 0.9)+
    geom_point(aes(y=observed), color = "hotpink", alpha=0.5, fill = "hotpink3") +
    labs(title = title)+
    xlab(bquote('Crown area'~(m^2/plant)))+
    ylab(y)+
    theme_bw()
}

Sc_plot(cv_intervals_leafs,"Foliage",'Foliage dry mass (kg/plant)')
Sc_plot(cv_intervals_wood,"Wood",'Wood dry mass (kg/plant)')
Sc_plot(cv_intervals_roots,"Roots",'Root dry mass (kg/plant)')



#Tjek af, hvilke prediktionsintervaller der fucker med mine plots:

nul_int <- cv_intervals_leafs
nul_int$indx <- as.numeric(row.names(cv_intervals_leafs))
nul_int <- data.frame(nul_int, score = abs(nul_int$quantile..0.9-nul_int$observed))
nul_int["score" == 0,1]

nul_int <- arrange(nul_int, score)
head(nul_int)

#Så der er små forskelle altid. 

#PLots og check af coverage
 #Cross validation til coverage:
cv_cov <- function(data, k) {
  set.seed(777)
  n <- nrow(data)
  group <- sample(rep(1: k, length.out = n))
  pred <- c()
  obs <- c()
  q0.1 <- c()
  q0.9 <- c()
  cov <- c()
  indi <- c()
  for (i in (1:k)){
    #Fit model
    train_x <- data.frame(Sc = data[group != i,1])
    train_y <- data[group != i,2]
    test_x <- data.frame(Sc=data[group == i,1])
    test_y <- data[group == i,2]
    qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = 76)
    
    #Getting quantiles
    conditionalQuantiles_cv <- predict(qrf_cv, test_x, what = c(0.05,0.95))
    
    #Creating vectors with observations and quantiles
    if (i == 1) {
      pred <- c(data[group == i,1])
      obs <- c(test_y)
      q0.05 <- c(conditionalQuantiles_cv[,1])
      q0.95 <- c(conditionalQuantiles_cv[,2])
      cov[i] <- c(mean(q0.05 <= obs& obs <=q0.95))
    }
    else {
      pred <- c(pred, data[group == i,1])
      obs <- c(obs, test_y)
      q0.05 <- c(q0.05, conditionalQuantiles_cv[,1])
      q0.95 <- c(q0.95, conditionalQuantiles_cv[,2])
      cov[i] <- c(mean(q0.05 <= obs& obs <=q0.95))
    }
  }
  return("cov" = data.frame(coverage = cov))
}

cov_leafs <- cv_cov(leafs, 10)

#Plots and table:
a <- rbind(cv_cov(leafs, 10), cv_cov(leafs, 10), cv_cov(leafs, 10))
b <- rbind(cv_cov(wood, 10), cv_cov(wood, 10), cv_cov(wood, 10))
c <- rbind(cv_cov(roots, 10), cv_cov(roots, 10),cv_cov(roots, 10))

a %>%
  ggplot() +
  geom_histogram(aes(x = coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins=10)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins=20)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", binwidth = 0.01)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Roots")

#Mean coverage:

xtable(tibble(Data = c("Leafs", "Wood", "Roots"), 
              "Mean coverage" =c(mean(a$coverage), mean(b$coverage), mean(c$coverage))), type = latex)


#Plots with smoothed intervals. Probably not really relevant or what?:
Sc_plot_smoothed <- function(data,title, y){
  plot_data <- data.frame(data)
  plot_data <- plot_data %>%
    mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))
  #Plot
  ggplot(plot_data, aes(x = pred)) +
    geom_point(aes(y=observed), color = "hotpink", alpha=0.5, fill = "hotpink3") +
    geom_smooth(aes(y=quantile..0.9), color = 'darkolivegreen', se=F) +
    geom_smooth(aes(y=quantile..0.1), color = 'darkolivegreen', se=F) +
    labs(title = title)+
    xlab(bquote('Crown area'~(m^2/plant)))+
    ylab(y)+
    theme_bw()
}
Sc_plot_smoothed(cv_intervals_leafs,"Foliage",'Foliage dry mass (kg/plant)')
Sc_plot_smoothed(cv_intervals_wood,"Wood",'Wood dry mass (kg/plant)')
Sc_plot_smoothed(cv_intervals_roots,"Roots",'Root dry mass (kg/plant)')





#Quantile regression forest for wood

set.seed(4)
qrf <- quantregForest(x = train_wood_x, y =train_wood_y, nodesize = 70)
plot(qrf)

conditionalQuantiles <- predict(qrf, test_wood_x)
conditionalMean <-  predict(qrf, test_wood_x, what = mean)
conditionalMedian <-  predict(qrf, test_wood_x, what = median)

obs_fitted <- data.frame(observed = test_wood_y, fitted = conditionalMedian)

#Arranging prediction intervals
obs_intervals <- data.frame(observed = test_wood_y, conditionalQuantiles[,c(1,3)], 
                            interval = conditionalQuantiles[,3]-conditionalQuantiles[,1])
obs_intervals <- arrange(obs_intervals, interval)

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

plot_data <- data.frame(obs_intervals, pred = test_wood_x)
head(plot_data)

plot_data <- plot_data %>%
  mutate(indicator = if_else((quantile..0.1 <= observed)&(observed <= quantile..0.9),"in", "out"))

head(obs_intervals)
colors <- c("Quantiles" = "darkolivegreen", "Observed" = "hotpink")

ggplot(plot_data, aes(x = Sc)) +
  geom_point(aes(y=quantile..0.9, color = "Quantiles"), fill = "darkolivegreen", shape = 25, alpha = 0.7) +
  geom_point(aes(y=quantile..0.1, color = "Quantiles"), fill = "darkolivegreen", shape = 24, alpha = 0.7) +
  geom_segment(aes(x = Sc, y = quantile..0.1, xend = Sc, yend = quantile..0.9),
               color = "darkolivegreen3", alpha = 0.2, lwd = 0.9)+
  geom_point(aes(y=observed, color = "Observed"), alpha=0.5, fill = "hotpink3") +
  labs(title = "Wood")+
  xlab(bquote('Crown area'~(m^2/plant)))+
  ylab('Foliage dry mass (kg/plant)')+
  theme_bw()+
  scale_color_manual(values = colors)

#in relation to predictor-variables
plot_data <- data.frame(test_wood_x, conditionalQuantiles, pred = test_wood_x)
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


###############################################################################################################
#Den rigtige version af dette her:

#Vi starter med at træne modellerne på træningssættet:
leafs_qrf <- quantregForest(x = train_leafs_x, y =train_leafs_y, nodesize = 100)
wood_qrf <- quantregForest(x = train_wood_x, y =train_wood_y, nodesize = 70)
roots_qrf <- quantregForest(x = train_roots_x, y =train_roots_y, nodesize = 5)

#Predicted quantiles
leafs_pred <- predict(leafs_qrf, test_leafs_x, what = c(0.05,0.95))
leafs_pred_mean <- predict(leafs_qrf, test_leafs_x, what = mean)
wood_pred <- predict(wood_qrf, test_wood_x, what = c(0.05,0.95))
wood_pred_mean <- predict(wood_qrf, test_wood_x, what = mean)
roots_pred <- predict(roots_qrf, test_roots_x, what = c(0.05,0.95))
roots_pred_mean <- predict(roots_qrf, test_roots_x, what = mean)


plot_data <- function(int, obs, Sc, mean){
  plot_data <- data.frame(int, observed = obs, Sc = Sc, mean = mean)
  plot_data <- plot_data %>%
    mutate(indicator = if_else((quantile..0.05 <= observed)&(observed <= quantile..0.95),"in", "out"))
}

plot_leafs <- plot_data(leafs_pred, test_leafs_y, test_leafs_x, leafs_pred_mean)
plot_wood <- plot_data(wood_pred, test_wood_y, test_wood_x, wood_pred_mean)
plot_roots <- plot_data(roots_pred, test_roots_y, test_roots_x, roots_pred_mean)

head(plot_leafs)
nrow(plot_leafs)

color <- c("in" = "darkolivegreen", "out" = "darkolivegreen3")

ggplot(plot_leafs) + 
  geom_point(aes(x = Sc, y = quantile..0.05), color = 'hotpink', size = 1.5) +
  geom_point(aes(x = Sc, y = quantile..0.95), color = 'hotpink', size = 1.5) +
  geom_point(aes(x = Sc, y = observed, color = indicator), size = 1.5) +
  geom_point(aes(x = Sc, y = mean), color = 'hotpink4', size = 1.5) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Leafs")+
  scale_color_manual(values = color)

ggplot(plot_wood) + 
  geom_point(aes(x = Sc, y = quantile..0.05), color = 'hotpink', size = 1.5) +
  geom_point(aes(x = Sc, y = quantile..0.95), color = 'hotpink', size = 1.5) +
  geom_point(aes(x = Sc, y = observed, color = indicator), size = 1.5) +
  geom_point(aes(x = Sc, y = mean), color = 'hotpink4', size = 1.5) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Wood")+
  scale_color_manual(values = color)


ggplot(plot_roots) + 
  geom_point(aes(x = Sc, y = quantile..0.05), color = 'hotpink', size = 2.5) +
  geom_point(aes(x = Sc, y = quantile..0.95), color = 'hotpink', size = 2.5) +
  geom_point(aes(x = Sc, y = observed, color = indicator), size = 2.5) +
  geom_point(aes(x = Sc, y = mean), color = 'hotpink4', size = 2.5) +
  theme_bw() +
  xlab('Sc') + 
  ylab('Kgp')+
  labs(title = "Roots")+
  scale_color_manual(values = color)

####Coverage:
#Indlæser data:
leafs <- read.csv('Data/leafs.csv')
wood <- read.csv('Data/wood.csv')
roots <- read.csv('Data/roots.csv')


#Cross validation til coverage:


cv_cov <- function(data, k, nodesize) {
    cov <- c()
    n <- nrow(data)
    sample_size <- floor(0.8*n)
    
    for (i in (1:k)){
      # Test and train
      picked_rs <- sample(n,size = sample_size)
      train_rs = data[picked_rs,]
      test_rs = data[-picked_rs,]
    
  
      #Fit model
      train_x <- data.frame(train_rs$Sc)
      train_y <- train_rs$Kgp
      test_x <- data.frame(test_rs$Sc)
      test_y <- test_rs$Kgp
      qrf_cv <- quantregForest(x = train_x, y =train_y, nodesize = nodesize)
      
    #Getting quantiles
    conditionalQuantiles_cv <- predict(qrf_cv, newdata = test_x, what = c(0.05,0.95))
    
    #Creating vectors with observations and quantiles
    if (i == 1) {
      pred <- c(data[group == i,1])
      obs <- c(test_y)
      q0.05 <- c(conditionalQuantiles_cv[,1])
      q0.95 <- c(conditionalQuantiles_cv[,2])
      cov[i] <- c(mean(q0.05 <= obs& obs <=q0.95))
    }
    else {
      pred <- c(pred, data[group == i,1])
      obs <- c(obs, test_y)
      q0.05 <- c(q0.05, conditionalQuantiles_cv[,1])
      q0.95 <- c(q0.95, conditionalQuantiles_cv[,2])
      cov[i] <- c(mean(q0.05 <= obs& obs <=q0.95))
    }
  }
  return("cov" = data.frame(coverage = cov))
}

cov_leafs <- cv_cov(leafs,10,100)
cov_wood <- cv_cov(wood,10)
cov_roots <- cv_cov(roots,10)









a %>%
  ggplot() +
  geom_histogram(aes(x = coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins=10)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Foliage")

b %>%
  ggplot() +
  geom_histogram(aes(x = coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", bins=20)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Wood")

c %>%
  ggplot() +
  geom_histogram(aes(x = coverage, y = ..density..), color = "white", 
                 fill = "darkolivegreen3", binwidth = 0.01)+
  geom_vline(xintercept = 0.9, color = "hotpink") +
  theme_bw()+
  labs(title = "Roots")
