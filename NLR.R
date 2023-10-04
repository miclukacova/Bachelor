#Packages


#Getting data
leafs <- read.csv('Data/leafs.csv')
wood <- read.csv("Data/wood.csv")
roots <- read.csv("Data/roots.csv")

#Creating function to be minimized
MSE_NLR <- function(par, data){
  with(data, sum((Kgp-par[1]*Sc^par[2])^2))
}



#Testing function:
MSE_NLR(c(1,1),leafs)
sum((leafs$Kgp-1*leafs$Sc^1)^2)


#Trying minimization out with initial parameters 1,1

optim(par = c(1,1), fn = MSE_NLR, data = leafs)
optim(par = c(1,1), fn = MSE_NLR, data = wood)$par
optim(par = c(1,1), fn = MSE_NLR, data = roots)$par

#Trying out different methods
optim(par = c(1,1), fn = MSE_NLR, data = leafs, method = "BFGS")$par
optim(par = c(1,1), fn = MSE_NLR, data = wood, method = "BFGS")$par
optim(par = c(1,1), fn = MSE_NLR, data = roots, method = "BFGS")$par

optim(par = c(1,1), fn = MSE_NLR, data = leafs, method = "CG")$par
optim(par = c(1,1), fn = MSE_NLR, data = wood, method = "CG")$par
optim(par = c(1,1), fn = MSE_NLR, data = roots, method = "CG")$par


#Trying different starting points
par_art_l <- c(0.3668,0.9441)
par_art_w <- c(5.6658,1.1068)
par_art_r <- c(1.1921,1.1730)

optim(par = par_art_l, fn = MSE_NLR, data = leafs, method = "BFGS")$par
optim(par = par_art_w, fn = MSE_NLR, data = wood, method = "BFGS")$par
optim(par = par_art_l, fn = MSE_NLR, data = roots, method = "BFGS")$par




