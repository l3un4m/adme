#install.packages('Ecdat')
library('Ecdat')
data(package = "Ecdat")
dat = as.data.frame(BudgetItaly)
dat <- subset(dat, select = -4)
dat_73 <- dat[73,]
dat_sum <- summary(dat)
dat_sum

