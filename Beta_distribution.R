n <- seq(10, 1500, length.out = 6)
l <- floor((n+1)*0.1)

colors = c('darkolivegreen1','darkolivegreen2','darkolivegreen3','darkolivegreen4'
          ,'darkolivegreen','hotpink3')

curve(dbeta(x, n[6]+1-l[6],l[6]), 
      from = 0.7, to = 1.01, 
      col =  colors[6],
      main = "Beta Distribution",
      ylab = "Density",
      lwd = 2)

names <- c()

for (i in 1:6){
  curve(dbeta(x, n[i]+1-l[i],l[i]), from = 0.7, to = 1.01, col = colors[i], add = TRUE, lwd = 1)
  names[i] <- c(paste("n = ", n[i]))
}

legend("topleft", legend=names, col=colors, lty = 1, cex = 0.8, lwd = 2)
