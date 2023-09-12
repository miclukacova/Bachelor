n <- seq(10, 1210,200)
l <- floor((n+1)*0.1)

length(n)

curve(dbeta(x, n[7]+1-l[7],l[7]), 
      from = 0.5, to = 1.1, 
      col = 3,
      main = "Beta distribution",
      ylab = "Density")

for (i in 1:6){
  curve(dbeta(x, n[i]+1-l[i],l[i]), from = 0.5, to = 1.1, col = 3, add = TRUE)
}

?curve

