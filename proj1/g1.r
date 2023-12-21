library('Ecdat')
library(rrcov)
library(plotrix)
library(stats)
library(WRS2)
library(psych)
library(DescTools)
library(MASS)

####1####
###All###
dat = as.data.frame(BudgetItaly)
dat <- subset(dat, select = -4)
dat_noyear <- subset(dat, select = -7)
dat_sum <- summary(dat)
dat

#3D exploded pie chart with all the years
pie3D(as.vector(table(BudgetItaly$year)),labels=names(table(BudgetItaly$year)),
      explode=0.1, main="BudgetItaly years",labelcex=1.0)

###Year73###
dat_73 <- subset(dat, dat$year == "73")
dat_73 <- subset(dat_73, select = -7)

##Boxplot of all the variables pf year 73 except pfood
# Create boxplots for each variable in the data frame
pdf("boxplots55.pdf", width = 10, height = 5)
par(mfrow = c(1, ncol(dat_73)))
for (col_name in names(dat_73)) {
  boxplot(
    dat_73[[col_name]],
    main = col_name, col = "lightpink",
    border = "black",
    notch = TRUE
  )
  rm(col_name)
}
dev.off()

##Mean#
dat_73_mean <- colMeans(dat_73)

#Trimmed Mean#
dat_73_trim_wfood = mean(dat_73$wfood, trim = 0.2)
dat_73_trim_whouse = mean(dat_73$whouse, trim = 0.2)
dat_73_trim_wmisc = mean(dat_73$wmisc, trim = 0.2)
dat_73_trim_phouse = mean(dat_73$phouse, trim = 0.2)
dat_73_trim_pmisc = mean(dat_73$pmisc, trim = 0.2)
dat_73_trim_tt = mean(dat_73$totexp, trim = 0.2)
dat_73_trim_income = mean(dat_73$income, trim = 0.2)
dat_73_trim_size = mean(dat_73$size, trim = 0.2)
dat_73_trim_pct = mean(dat_73$pct, trim = 0.2)

#Winsorized Mean#
dat_73_win_wfood = winmean(dat_73$wfood, trim = 0.2)
dat_73_win_whouse = winmean(dat_73$whouse, trim = 0.2)
dat_73_win_wmisc = winmean(dat_73$wmisc, trim = 0.2)
dat_73_win_phouse = winmean(dat_73$phouse, trim = 0.2)
dat_73_win_pmisc = winmean(dat_73$pmisc, trim = 0.2)
dat_73_win_tt = winmean(dat_73$totexp, trim = 0.2)
dat_73_win_year = winmean(dat_73$year, trim = 0.2)
dat_73_win_income = winmean(dat_73$income, trim = 0.2)
dat_73_win_size = winmean(dat_73$size, trim = 0.2)
dat_73_win_pct = winmean(dat_73$pct, trim = 0.2)
#Median#quando distribuiçao e simetrica media e mediana coincide
dat_73_median <- sapply(dat_73, median)

#Variance#
dat_73_var = apply(dat_73, 2, var)
dat_73_vartot = sum(dat_73_var)
dat_73_vartot#referir no relatorio 
#Covariance#
dat_73_cov = round(cov(dat_73), digits = 9)
dat_73_gen_var = det(dat_73_var/dat_73_cov)

#mad variable#
dat_73_mad_wfood = mad(dat_73$wfood)
dat_73_mad_whouse = mad(dat_73$whouse)
dat_73_mad_wmisc = mad(dat_73$wmisc)
dat_73_mad_phouse = mad(dat_73$phouse)
dat_73_mad_pmisc = mad(dat_73$pmisc)
dat_73_mad_totexp = mad(dat_73$totexp)
dat_73_mad_year = mad(dat_73$year)
dat_73_mad_income = mad(dat_73$income)
dat_73_mad_size = mad(dat_73$size)
dat_73_mad_pct = mad(dat_73$pct)

#Mahalabonis Distance#
dat_73_maha = mahalanobis(dat_73, dat_73_mean, dat_73_cov)
#Boxplot of Mahalanobis Distance
pdf("mahalanobis_distances3.pdf", width = 8, height = 5)
plot(
  dat_73_maha,
  pch = 16,
  col = "purple",
  main = "Mahalanobis Distances",
  xlab = "Observation",
  ylab = "Distance"
)
abline(
  h = sqrt(qchisq(0.975, df = ncol(dat_73))),
  col = "blue",
  lty = 2
)
dev.off()

###Year73###




####2####
#Original Scale#Classical Sample Covariance Estimate#
dat_73_budget.pca <- prcomp(dat_73)

#Standardized Variables#
dat_73_stand <- scale(dat_73)
dat_73_pca_stand <- prcomp(dat_73_stand)

##teste b adicionar graf cotovelo
summary(dat_73_pca_stand)
summary(dat_73_budget.pca)
##biplot
teste <- dat_73_budget.pca$x
abline(h=mean(teste[,2]), col = "blue")
abline(h=mean(teste[,1]), col = "blue")
biplot(dat_73_budget.pca, scale = 0)
biplot(dat_73_pca_stand, scale = 0)

####3####
##ALL##
dat_p3 <- dat_73
#Function to multiply the values of the 5 first rows by 0.01 as it's asked
for (row_index in 1:5){
  for (col_index in 1:9){
    dat_p3[row_index, col_index] <- dat_p3[row_index, col_index] * 0.01
  }
}
#Classical PCA#
dat_p3.pca <- prcomp(dat_p3)

##Year 73##
dat_73_p3 <- dat_73
#Function to multiply the values of the 5 first rows by 0.01 as it's asked
for (row_index in 1:5) {
  for (col_index in 1:9) {
    dat_73_p3[row_index, col_index] <- dat_73_p3[row_index, col_index] * 0.01
  }
}
#Classical PCA#
dat_73_p3.pca <- prcomp(dat_73_p3)
summary(dat_73_p3.pca)
##Robust PCA on the MCD estimate, terminar
dat_73.mcd_pca <- eigen(cov.rob(dat_73_p3)$cov)
dat_73.mcd_pca
