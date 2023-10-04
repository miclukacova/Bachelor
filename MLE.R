leafs_train <- read.csv('Data/train_leafs.csv')

logxi <- log(leafs_train$Sc)
logyi <- log(leafs_train$Kgp)
n <- nrow(leafs_train)


alpha1 <- sum(logxi*logyi-1/n*sum(logyi)*logxi)/sum(logxi^2-1/(n)*sum(logxi)*logxi)
log_beta <- exp(1/n * sum(logyi-alpha1*logxi))
