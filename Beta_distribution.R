n <- seq(10, 1500, length.out = 6)
l <- floor((n+1)*0.1)

names <- c()

for (i in 1:6){
  names[i] <- c(paste("n = ", n[i]))
}


n <- seq(10, 1500, length.out = 6)
l <- floor((n+1)*0.1)

func_list <- list()

func_list[[1]] <- function(x) dbeta(x, n[1]+1-l[1],l[1]) 
func_list[[2]] <- function(x) dbeta(x, n[2]+1-l[2],l[2]) 
func_list[[3]] <- function(x) dbeta(x, n[3]+1-l[3],l[3]) 
func_list[[4]] <- function(x) dbeta(x, n[4]+1-l[4],l[4]) 
func_list[[5]] <- function(x) dbeta(x, n[5]+1-l[5],l[5]) 
func_list[[6]] <- function(x) dbeta(x, n[6]+1-l[6],l[6]) 

ggplot(test_roots_plot) +
  geom_function(fun = func_list[[1]], aes(color = "n =  10"), linewidth = 1.1)+
  geom_function(fun = func_list[[2]], aes(color = "n =  308"), linewidth = 1.1)+
  geom_function(fun = func_list[[3]], aes(color = "n =  606"), linewidth = 1.1)+
  geom_function(fun = func_list[[4]], aes(color = "n =  904"), linewidth = 1.1)+
  geom_function(fun = func_list[[5]], aes(color = "n =  1202"), linewidth = 1.1)+
  geom_function(fun = func_list[[6]], aes(color = "n =  1500"), linewidth = 1.1)+
  xlim(c(0.8,1.01))+
  theme_bw()+
  scale_color_manual(values = colors, breaks=c("n =  10", "n =  308", "n =  606", "n =  904", "n =  1202", "n =  1500"))

?scale_
colors = c("n =  10" = "darkolivegreen1",
           "n =  308" ="darkolivegreen2",
           "n =  606" = "darkolivegreen3",
           "n =  904" = "darkolivegreen4",
           "n =  1202" = "green4", 
           "n =  1500" = "darkgreen")


levels(colors)
  
  