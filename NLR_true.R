#Packages
library(tidyverse)
library(readr)
library(infer)
library(foreign)
library(xtable)
library(stargazer)


#Getting data
leafs <- read.csv('Data/leafs.csv')
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")

#OLS estimates
par_ols_l <- c(0.2693082, 0.9441130)
par_ols_w <- c(3.944818, 1.106841)
par_ols_r <- c(0.8339087, 1.1730237)

#Loss function
MSE_NLR <- function(par, data){
  with(data, sqrt(sum((Kgp-par[1]*Sc^par[2])^2)/nrow(data)))
}

optim(par = par_ols_l, fn = MSE_NLR, data = leafs)
optim(par = par_ols_w, fn = MSE_NLR, data = wood)
optim(par = par_ols_r, fn = MSE_NLR, data = roots)


#c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent")


#------------------------------------Heatmap------------------------------------

#Function for creating grid

creating_grid <- function(data, par_ols) {
  hej <- TRUE
  for (i in seq(0, par_ols[1]*7, length.out = 50)){
      if (hej){
        grid <- tibble(b = rep(i,50), a = seq(0.2, par_ols[2]*1.7, length.out = 50))
        hej <- FALSE
      }
      else {
        for (j in seq(0.2, par_ols[2]*1.7, length.out = 50)){
          grid <- add_row(grid, b = i, a = j)
        }
      }
    }
  return(grid)
}

heat_map <- function(data, par_ols, Title){
  my_grid <- creating_grid(data, par_ols)
  
  #Calculating MSE along grid
  mse <- c()
  for (i in (1:nrow(my_grid))){
    mse <- append(mse, MSE_NLR(data = data, par = c(my_grid$b[i], my_grid$a[i])))
  }
  
  #Finding breaks
  breaks_data <- sort(mse)[c(seq(1,length(mse)/3, length.out = 7),seq(length(mse)/3,length(mse), length.out = 2))]
  
  #Transforming data
  plot_data <- data.frame(a = my_grid$a, b = my_grid$b, mse = mse)
  
  #return(plot_data)
  
  #Plot
  p <- ggplot(data = plot_data, aes(x = a, y = b, z = mse)) +
    geom_point(aes(x=par_ols[2], y = par_ols[1]), color = "red", size = 3)+
    stat_contour_filled(breaks = breaks_data) +
    theme_bw()+
    labs(title = Title)
  
  return(p)
}


heat_map(leafs, par_ols_l, "Leafs")
heat_map(wood, par_ols_w, "Wood")
heat_map(roots, par_ols_r, "Roots")

#------------------------------------Nelson-Mead optimizer----------------------

grid_l <- creating_grid(leafs, par_ols_l)
grid_w <- creating_grid(wood, par_ols_w)
grid_r <- creating_grid(roots, par_ols_r)

grid_values <- function(data, grid, title) {
  hej <- TRUE
  for (i in (1:nrow(grid))){
    c <- c(grid[i,1], grid[i,2])
    if (hej){
      NLR_loop <- optim(par = c, fn = MSE_NLR, data = data)
      par_grid <- data.frame(b = grid[i,1],
                             a = grid[i,2],
                             MSE = NLR_loop$value)
      hej <- FALSE
    }
    else{
      NLR_loop <- optim(par = c, fn = MSE_NLR, data = data)
      par_grid <- par_grid %>% add_row(b = as.numeric(grid[i,1]),
                                       a = as.numeric(grid[i,2]),
                                       MSE = NLR_loop$value)
    }
  }
  
  #Finding breaks
  breaks_data <- sort(par_grid$MSE)[c(seq(1,nrow(par_grid)*999/1000, length.out = 2), 
                                      seq(nrow(par_grid)*999/1000, nrow(par_grid), length.out = 5))]
  
  #Plot
  p <- ggplot(data = par_grid, aes(x = a, y = b, z = MSE)) +
    stat_contour_filled(breaks = breaks_data) +
    theme_bw()+
    labs(title = title)
  return(p)
}

a <- grid_values(leafs, grid_l, "Leafs")
b <- grid_values(wood, grid_w, "Wood")
c <- grid_values(roots, grid_r, "Roots")


a
b
c







#Usuccesfuldt forsøg med sejere heat maps

b <- heat_map(leafs, par_ols_l, "Leafs")
d <-  heat_map(leafs, par_ols_l, "Leafs")

breaks_data <- sort(d$mse)[c(seq(1,nrow(d)/3, length.out = 10),seq(nrow(d)/2,nrow(d), length.out = 2))]

ggplot(data = d, aes(x = a, y = b, z = mse))  + 
  geom_raster(aes(fill = mse)) +
  geom_contour(colour = "white", breaks = breaks_data)

?geom_contour
+
  theme_bw()+
  geom_raster(aes(fill = density)) +
  geom_contour(colour = "white")

heat_map(wood, par_ols_w, "Wood")
heat_map(roots, par_ols_r, "Roots")

?stat_contour_filled

#Appendix plots


ggplot(creating_grid(leafs,par_ols_l), aes(x = a, y = b)) +
  geom_point(size = 0.9)+
  geom_point(aes(x=par_ols_l[2], y = par_ols_l[1]), color = "red", size = 4)+
  geom_text(aes(x=par_ols_l[2], y = par_ols_l[1]), label = "OLS", hjust = -.5, color = "red", size = 5)+
  theme_bw()+
  labs(title = "Leafs")

ggplot(creating_grid(wood,par_ols_w), aes(x = a, y = b)) +
  geom_point(size = 0.9)+
  geom_point(aes(x=par_ols_w[2], y = par_ols_w[1]), color = "red", size = 4)+
  geom_text(aes(x=par_ols_w[2], y = par_ols_w[1]), label = "OLS", hjust = -.5, color = "red", size = 5)+
  theme_bw()+
  labs(title = "Wood")


ggplot(creating_grid(roots,par_ols_w), aes(x = a, y = b)) +
  geom_point(size = 0.9)+
  geom_point(aes(x=par_ols_r[2], y = par_ols_r[1]), color = "red", size = 4)+
  geom_text(aes(x=par_ols_r[2], y = par_ols_r[1]), label = "OLS", hjust = -.5, color = "red", size = 5)+
  theme_bw()+
  labs(title = "Roots")
