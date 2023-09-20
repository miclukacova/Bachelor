n <- seq(10, 1500, length.out = 6)
l <- floor((n+1)*0.1)

colors = c('deeppink4','darkcyan','midnightblue', 'darkolivegreen',
           'lightslateblue','palegreen4','mediumorchid4')

curve(dbeta(x, n[6]+1-l[6],l[6]), 
      from = 0.7, to = 1.01, 
      col =  colors[6],
      main = "Beta distribution",
      ylab = "Density",
      lwd = 2)

names <- c()

for (i in 1:6){
  curve(dbeta(x, n[i]+1-l[i],l[i]), from = 0.7, to = 1.01, col = colors[i], add = TRUE, lwd = 2)
  names[i] <- c(paste("n = ", n[i]))
}

legend(1, 95, legend=names, col=c("red", "blue"))

