library(Ecdat)
BudgetItaly
BudgetItaly$year
year73_with_pfood = subset(BudgetItaly, BudgetItaly$year == "73")
year73 = subset(year73_with_pfood, select = -c(pfood))
year73

#3D exploded pie chart
library(plotrix)
pie3D(as.vector(table(BudgetItaly$year)),labels=names(table(BudgetItaly$year)),explode=0.1,
      main="BudgetItaly years",labelcex=1.0)

# Pie Chart with Percentages
slices <- as.vector(table(BudgetItaly$year))
lbls <- names(table(BudgetItaly$year))
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # add % to labels
pie3D(slices,labels = lbls, col=rainbow(length(lbls)),
      main="BudgetItaly years", labelcex=0.8)

#sort
sort(year73$wfood)
sort(year73$whouse)
sort(year73$wmisc)
sort(year73$phouse)
sort(year73$pmisc)
sort(year73$totexp)
sort(year73$year)
sort(year73$income)
sort(year73$size)
sort(year73$pct)

#mean
year73_mean = apply(year73, 2, mean)
year73_mean

#median and variance
library(stats)
year73_median = apply(year73, 2, median)
year73_median
year73_var = apply(year73, 2, var)
year73_var
#total variance - The problem with total variation is that it does not
#take into account correlations among the variables
year73_ttvar = sum(year73_var)
year73_ttvar

#trimmed means
year73_trim_wfood = mean(year73$wfood, trim = 0.2)
year73_trim_whouse = mean(year73$whouse, trim = 0.2)
year73_trim_wmisc = mean(year73$wmisc, trim = 0.2)
year73_trim_phouse = mean(year73$phouse, trim = 0.2)
year73_trim_pmisc = mean(year73$pmisc, trim = 0.2)
year73_trim_tt = mean(year73$totexp, trim = 0.2)
year73_trim_year = mean(year73$year, trim = 0.2)
year73_trim_income = mean(year73$income, trim = 0.2)
year73_trim_size = mean(year73$size, trim = 0.2)
year73_trim_pct = mean(year73$pct, trim = 0.2)

#winsorized means
library(WRS2)
year73_wins_wfood = winmean(year73$wfood, trim = 0.2)
year73_wins_whouse = winmean(year73$whouse, trim = 0.2)
year73_wins_wmisc = winmean(year73$wmisc, trim = 0.2)
year73_wins_phouse = winmean(year73$phouse, trim = 0.2)

#mad variable
library(stats)
mad_year73_wfood= mad(year73$wfood)
mad_year73_wfood

#covariance
y73 = round (cov(year73), digits = 9)
y73

#generalized var - determinante da matrix var/cov
year73_var/y73

#mahalanobis distance - mahalanobis (matrix of data, mean vector of the distribution, covariance matrix of the distribution)
year73_maha = mahalanobis(year73, year73_mean, y73)
year73_maha


#PCA