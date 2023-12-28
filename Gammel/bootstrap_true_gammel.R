
#----------------------logOLSB---------------------------

#set.seed(4)
#
#boot_leafs <- bootstrap_loo(model_logolsB, leafs, 1000, alpha = 0.2)
#boot_wood <- bootstrap_loo(model_logolsB, wood, 1000, alpha = 0.2)
#boot_roots <- bootstrap_loo(model_logolsB, roots, 1000, alpha = 0.2) 
#
#write.csv(boot_leafs, "/Users/michaelalukacova/Bachelor1/Data/boot_leafs_logolsB.csv", row.names=F)
#write.csv(boot_wood, "/Users/michaelalukacova/Bachelor1/Data/boot_wood_logolsB.csv", row.names=F)
#write.csv(boot_roots, "/Users/michaelalukacova/Bachelor1/Data/boot_roots_logolsB.csv", row.names=F)

#boot_leafs <- read.csv('Data/boot_leafs_logolsB.csv')
#boot_wood <- read.csv('Data/boot_wood_logolsB.csv')
#boot_roots <- read.csv('Data/boot_roots_logolsB.csv')
#
#plot_maker(boot_leafs, "Leafs")
#plot_maker(boot_wood, "Wood")
#plot_maker(boot_roots, "Roots")
#
#coverage(boot_leafs)
#coverage(boot_wood)
#coverage(boot_roots)
#
##Distribution of coverage
#
#set.seed(4)
#
#boot_leafs_rs <- rs_cov_boot(data = leafs, k = 50, alpha = 0.2, model = model_logolsB)
#boot_wood_rs <- rs_cov_boot(data = wood, k = 50, alpha = 0.2, model = model_logolsB)
#boot_roots_rs <- rs_cov_boot(data = roots, k = 50, alpha = 0.2, model = model_logolsB)
#
#rs_plot_maker(boot_leafs_rs, "Leafs", alpha = 0.2)
#rs_plot_maker(boot_wood_rs, "Wood", alpha = 0.2)
#rs_plot_maker(boot_roots_rs, "Roots", alpha = 0.2)
#
##Conditional coverage
#
#roll_cov(boot_leafs, alpha = 0.2, bin_size = 50, "Leafs")
#roll_cov(boot_leafs, alpha = 0.2, bin_size = 50, "Leafs")
#