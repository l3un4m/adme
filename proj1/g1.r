#install.packages('Ecdat')
library('Ecdat')
data(package = "Ecdat")
###General###
dat = as.data.frame(BudgetItaly)
dat <- subset(dat, select = -4)
dat_sum <- summary(dat)
dat_sum

##Mean
dat_mean <- colMeans(dat)
dat_mean
#Trimmed Mean
dat_trim_wfood = mean(dat$wfood, trim = 0.2)
dat_trim_whouse = mean(dat$whouse, trim = 0.2)
dat_trim_wmisc = mean(dat$wmisc, trim = 0.2)
dat_trim_phouse = mean(dat$phouse, trim = 0.2)
dat_trim_pmisc = mean(dat$pmisc, trim = 0.2)
dat_trim_tt = mean(dat$totexp, trim = 0.2)
dat_trim_year = mean(dat$year, trim = 0.2)
dat_trim_income = mean(dat$income, trim = 0.2)
dat_trim_size = mean(dat$size, trim = 0.2)
dat_trim_pct = mean(dat$pct, trim = 0.2)
#Winsorized Mean
library(WRS2)
dat_win_wfood = winmean(dat$wfood, trim = 0.2)
dat_win_whouse = winmean(dat$whouse, trim = 0.2)
dat_win_wmisc = winmean(dat$wmisc, trim = 0.2)
dat_win_phouse = winmean(dat$phouse, trim = 0.2)
dat_win_pmisc = winmean(dat$pmisc, trim = 0.2)
dat_win_tt = winmean(dat$totexp, trim = 0.2)
dat_win_year = winmean(dat$year, trim = 0.2)
dat_win_income = winmean(dat$income, trim = 0.2)
dat_win_size = winmean(dat$size, trim = 0.2)
dat_win_pct = winmean(dat$pct, trim = 0.2)
#Median
dat_median <- sapply(dat, median)
dat_median
#Variance
dat_var = apply(dat, 2, var)
dat_var
dat_vartot = sum(dat_var)
dat_vartot
#Covariance
dat_cov = round(cov(dat), digits = 9)
dat_gen_var = det(dat_var/dat_cov)
dat_gen_var
#mad variable
library(stats)
dat_mad_wfood = mad(dat$wfood)
dat_mad_whouse = mad(dat$whouse)
dat_mad_wmisc = mad(dat$wmisc)
dat_mad_phouse = mad(dat$phouse)
dat_mad_pmisc = mad(dat$pmisc)
dat_mad_totexp = mad(dat$totexp)
dat_mad_year = mad(dat$year)
dat_mad_income = mad(dat$income)
dat_mad_size = mad(dat$size)
dat_mad_pct = mad(dat$pct)
#Mahalabonis Distance
dat_maha = mahalanobis(dat, dat_mean, dat_cov)
dat_maha

###Year73###
dat_73 = subset(dat, dat$year == "73")
dat_73 <- subset(dat, select = -7)
dat_73
##Mean
dat_73_mean <- colMeans(dat_73)
dat_73_mean
#Trimmed Mean
dat_73_trim_wfood = mean(dat_73$wfood, trim = 0.2)
dat_73_trim_whouse = mean(dat_73$whouse, trim = 0.2)
dat_73_trim_wmisc = mean(dat_73$wmisc, trim = 0.2)
dat_73_trim_phouse = mean(dat_73$phouse, trim = 0.2)
dat_73_trim_pmisc = mean(dat_73$pmisc, trim = 0.2)
dat_73_trim_tt = mean(dat_73$totexp, trim = 0.2)
dat_73_trim_year = mean(dat_73$year, trim = 0.2)
dat_73_trim_income = mean(dat_73$income, trim = 0.2)
dat_73_trim_size = mean(dat_73$size, trim = 0.2)
dat_73_trim_pct = mean(dat_73$pct, trim = 0.2)
#Winsorized Mean
library(WRS2)
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
#Median
dat_73_median <- sapply(dat_73, median)
dat_73_median
#Variance
dat_73_var = apply(dat_73, 2, var)
dat_73_var
dat_73_vartot = sum(dat_73_var)
dat_73_vartot
#Covariance
dat_73_cov = round(cov(dat_73), digits = 9)
dat_73_gen_var = det(dat_73_var/dat_73_cov)
dat_73_gen_var
#mad variable
library(stats)
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
#Mahalabonis Distance
dat_73_maha = mahalanobis(dat_73, dat_73_mean, dat_73_cov)
dat_73_maha




