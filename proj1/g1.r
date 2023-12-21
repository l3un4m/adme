library('Ecdat')
library(rrcov)
library(plotrix)
library(stats)
library(WRS2)
library(psych)
library(DescTools)
library(MASS)

####1####
dat = as.data.frame(BudgetItaly)
dat <- subset(dat, select = -4)

###Year73###
dat_73 <- subset(dat, dat$year == "73")
dat_73 <- subset(dat_73, select = -7)

###Resume of data###
describe(dat_73)

#Winsorized Mean#
dat_73_win_wfood = round(winmean(dat_73$wfood, trim = 0.2), digits = 4)
dat_73_win_whouse = round(winmean(dat_73$whouse, trim = 0.2), digits = 4)
dat_73_win_wmisc = round(winmean(dat_73$wmisc, trim = 0.2), digits = 4)
dat_73_win_phouse = round(winmean(dat_73$phouse, trim = 0.2), digits = 4)
dat_73_win_pmisc = round(winmean(dat_73$pmisc, trim = 0.2), digits = 4)
dat_73_win_tt = round(winmean(dat_73$totexp, trim = 0.2), digits = 4)
dat_73_win_income = round(winmean(dat_73$income, trim = 0.2), digits = 4)
dat_73_win_size = round(winmean(dat_73$size, trim = 0.2), digits = 4)
dat_73_win_pct = round(winmean(dat_73$pct, trim = 0.2), digits = 4)

#Variance#
dat_73_var = round(apply(dat_73, 2, var), digits = 4)
dat_73_vartot = round(sum(dat_73_var), digits = 4)

#Covariance#
dat_73_cov = round(cov(dat_73), digits = 4)

#mad variable#
dat_73_mad_wfood = round(mad(dat_73$wfood), digits = 4)
dat_73_mad_whouse = round(mad(dat_73$whouse), digits = 4)
dat_73_mad_wmisc = round(mad(dat_73$wmisc), digits = 4)
dat_73_mad_phouse = round(mad(dat_73$phouse), digits = 4)
dat_73_mad_pmisc = round(mad(dat_73$pmisc), digits = 4)
dat_73_mad_totexp = round(mad(dat_73$totexp), digits = 4)
dat_73_mad_income = round(mad(dat_73$income), digits = 4)
dat_73_mad_size = round(mad(dat_73$size), digits = 4)
dat_73_mad_pct = round(mad(dat_73$pct), digits = 4)

#Mahalabonis Distance#
dat_73_maha = round(mahalanobis(dat_73, dat_73_mean, dat_73_cov), digits = 4)

###Tables###
year73_mat <- data.frame(
  Insorized_Mean = c(dat_73_win_wfood, dat_73_win_whouse, dat_73_win_wmisc, 
                     dat_73_win_phouse, dat_73_win_pmisc, dat_73_win_tt, 
                     dat_73_win_income, dat_73_win_size, dat_73_win_pct),
  Variance = cbind(data.matrix(dat_73_var)),
  Mad = c(dat_73_mad_wfood, dat_73_mad_whouse, dat_73_mad_wmisc,
          dat_73_mad_phouse, dat_73_mad_pmisc, dat_73_mad_totexp,
          dat_73_mad_income, dat_73_mad_size, dat_73_mad_pct))
year73_mat

###Graphics###

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

###
pairs.panels(dat_73)

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
####2####
#Original Scale#Classical Sample Covariance Estimate#
dat_73_budget.pca <- prcomp(dat_73)

#Standardized Variables#
dat_73_stand <- scale(dat_73)
dat_73_pca_stand <- prcomp(dat_73_stand)

##teste b adicionar graf cotovelo
summary(dat_73_budget.pca)
summary(dat_73_pca_stand)

##biplot
#A#
score <- dat_73_budget.pca$x
plot(score[,1], score[,2], xlab = "PC1", ylab = "PC2", type = "n", xlim = c(min(score[,1]),
                                                                            max(score[,1])), ylim = c(min(score[,2]), max(score[,2])))
text(score[,1],score[,2], rownames(score),
     col="blue",cex=0.7)
abline(h=mean(score[,2]),col="green")
abline(v=mean(score[,1]),col="green")

#B#
score_std <- dat_73_pca_stand$x
plot(score_std[,1], score_std[,2], xlab = "PC1", ylab = "PC2", type = "n", xlim = c(min(score_std[,1]),
                                                                                    max(score_std[,1])), ylim = c(min(score_std[,2]), max(score_std[,2])))
text(score_std[,1],score_std[,2], rownames(score_std),
     col="blue",cex=0.7)
abline(h=mean(score_std),col="green")
abline(v=mean(score_std[,1]),col="green")

####3####
##ALL##
dat_p3 <- dat_73
#Function to multiply the values of the 5 first rows by 0.01 as it's asked
for (row_index in 1:5){
  for (col_index in 1:9){
    dat_p3[row_index, col_index] <- dat_p3[row_index, col_index] * 0.01
  }
}

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
summary(dat_73_budget.pca)
summary(dat_73_p3.pca)
#3 - biplots
score_p3.pca <- dat_73_p3.pca$x
plot(score_p3.pca[,1], score_p3.pca[,2], xlab = "PC1", ylab = "PC2", type = "n", xlim = c(min(score_p3.pca[,1]),
  max(score_p3.pca[,1])), ylim = c(min(score_p3.pca[,2]), max(score_p3.pca[,2])))
text(score_p3.pca[,1],score_p3.pca[,2], rownames(score_p3.pca),
  col="blue",cex=0.7)
abline(h=mean(score_p3.pca[,2]),col="green")
abline(v=mean(score_p3.pca[,1]),col="green")

##Robust PCA on the MCD estimate, terminar
dat_pca_robust <- PcaCov(dat_73_p3)
dat_pca_robust
biplot(dat_pca_robust, scale = 0, cex = 0.6)
