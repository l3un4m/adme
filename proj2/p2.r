# Load necessary library
library(rrcov)
library(plotrix)
library(stats)
library(WRS2)
library(psych)
library(DescTools)
library(MASS)
library(ggplot2)
library(car)

# Load Auto dataset
data("Auto")

# Select subset from observation 1 to 50
auto_subset <- Auto[1:50, 1:8]

# Resume statistics
tab_describe <-describe(auto_subset)

# Mean
auto_mean <- apply(auto_subset, 2, mean)

#Winsorized Mean#
auto_win_mpg = round(winmean(auto_subset$mpg, trim = 0.2), digits = 4)
auto_win_cylinders = round(winmean(auto_subset$cylinders, trim = 0.2), digits = 4)
auto_win_displacement = round(winmean(auto_subset$displacement, trim = 0.2), digits = 4)
auto_win_horsepower = round(winmean(auto_subset$horsepower, trim = 0.2), digits = 4)
auto_win_weight = round(winmean(auto_subset$weight, trim = 0.2), digits = 4)
auto_win_acceleration= round(winmean(auto_subset$acceleration, trim = 0.2), digits = 4)
auto_win_year = round(winmean(auto_subset$cylinders, trim = 0.2), digits = 4)
auto_win_origin = round(winmean(auto_subset$origin, trim = 0.2), digits = 4)

#Variance#
auto_var = round(apply(auto_subset, 2, var), digits = 4)
auto_vartot = round(sum(auto_subset), digits = 4)

#Covariance#
auto_cov = round(cov(auto_subset), digits = 4)

#Mahalabonis Distance#
auto_maha = round(mahalanobis(auto_subset, auto_mean, auto_cov), digits = 4)

###Tables###
colunas <- c("vars", "n", "sd", "trimmed", "mad", "min", "max", "range", "skew", "kurtosis", "se")
describe_nosso <- tab_describe[,colunas]

auto_mat <- cbind(describe_nosso,
                  Winsorized_Mean = c(auto_win_mpg, auto_win_cylinders, auto_win_displacement, 
                                      auto_win_horsepower, auto_win_weight, auto_win_acceleration,
                                      auto_win_year, auto_win_origin),
                  Variance = cbind(data.matrix(auto_var)))

###Graphics###

##Boxplot of all the variables of auto except name
# Create boxplots for each variable in the data frame
pdf("boxplots2.pdf", width = 10, height = 5)
par(mfrow = c(1, ncol(auto_subset)))
for (col_name in names(auto_subset)) {
  boxplot(
    auto_subset[[col_name]],
    main = col_name, col = "lightpink",
    border = "black",
    notch = TRUE
  )
  rm(col_name)
}
dev.off()

###
pairs.panels(auto_subset)

#Boxplot of Mahalanobis Distance
pdf("mahalanobis_distances2.pdf", width = 8, height = 5)
plot(
  auto_maha,
  pch = 16,
  col = "purple",
  main = "Mahalanobis Distances",
  xlab = "Observation",
  ylab = "Distance"
)
abline(
  h = sqrt(qchisq(0.975, df = ncol(auto_subset))),
  col = "blue",
  lty = 2
)
dev.off()


#2A
#COMEÇA AQUI, pedir para Manuel explicar no relatório como ajustado diminuiu pouco preferimos ficar com menos
reg1 = lm(mpg~cylinders + displacement + horsepower + weight + acceleration + year + origin, data=auto_subset)
summary(reg1)
reg2 = lm(mpg~cylinders  + horsepower + weight + acceleration + year + origin, data=auto_subset)
summary(reg2)
reg3 = lm(mpg~cylinders  + horsepower + weight + acceleration + origin, data=auto_subset)
summary(reg3)
reg4 = lm(mpg~cylinders  + horsepower + weight + acceleration , data=auto_subset)
summary(reg4)
reg5 = lm(mpg~cylinders  + weight + acceleration , data=auto_subset)
summary(reg5)
#Fazendo este comando e retirando o valor mais alto que n tem * até chegarmos a um output só com asterisco vemos que cylinders  + weight + acceleration sao os selecionados
reg_mpg <- lm(log(mpg) ~ log(cylinders) + log(weight) + log(acceleration), data = auto_subset)
summary(reg_mpg)
#Percentage of variability of the response variable that is explained by the regression model: r²=0.8911, r²adj=0.884 
multiple_r_squared <- summary(reg_mpg)$r.squared
adjusted_r_squared <- summary(reg_mpg)$adj.r.squared


#2B
# Hat's values - leverage points

p <- length(coef(reg5)) - 1
n <- nrow(auto_subset)

hM=hatvalues(reg5)
hMlev=hM[hM>2*p/n]
# Cook's distances - influential observations
cM=cooks.distance(reg5)
cMinfl=cM[cM>4/(n-p)]
cMinfl_R=cM[cM>4*mean(cM)] #R rule

# Influential plots
influenceIndexPlot(reg5)
influencePlot(reg5)
#2C
obs_14 <- auto_subset[14,]
obs_31 <- auto_subset[31,]

##Confidence Interval##
#14
p_conf_14 <- predict(reg_mpg, interval='confidence', newdata = obs_14, 0.975)
p_conf_14
p_conf_14_lwr_value <- p_conf_14$fit[, "lwr"]
p_conf_14_upr_value <- p_conf_14$fit[, "upr"]
vec_p_conf_14 = paste(']' , p_conf_14_lwr_value ,  ' , ' , p_conf_14_upr_value , '[')

#31
p_conf_31 <- predict(reg_mpg, interval='confidence', newdata = obs_31, 0.975)
p_conf_31
p_conf_31_lwr_value <- p_conf_31$fit[, "lwr"]
p_conf_31_upr_value <- p_conf_31$fit[, "upr"]
vec_p_conf_31 = paste(']' , p_conf_31_lwr_value ,  ' , ' , p_conf_31_upr_value , '[')

##Prediction Interval##
#14
p_pred_14 <- predict(reg_mpg, interval='prediction', newdata = obs_14, 0.975)
p_pred_14
p_pred_14_lwr_value <- p_pred_14$fit[, "lwr"]
p_pred_14_upr_value <- p_pred_14$fit[, "upr"]
vec_p_pred_14 = paste(']' , p_pred_14_lwr_value ,  ' , ' , p_pred_14_upr_value , '[')
p_pred_14
vec_p_pred_14

#31
p_pred_31 <- predict(reg_mpg, interval='prediction', newdata = obs_31, 0.975)
p_pred_31
p_pred_31_lwr_value <- p_pred_31$fit[, "lwr"]
p_pred_31_upr_value <- p_pred_31$fit[, "upr"]
vec_p_pred_31 = paste(']' , p_pred_31_lwr_value ,  ' , ' , p_pred_31_upr_value , '[')
p_pred_31
vec_p_pred_31

res_obs_14 <- c(vec_p_conf_14, vec_p_pred_14)
res_obs_31 <- c(vec_p_conf_31, vec_p_pred_31)
res_2c <- data.frame(Obs_14 = res_obs_14, Obs_31 = res_obs_31)
rownames(res_2c) <- c('CI', 'PI')


res <- paste(' RESPOSTA 2A:' , '\n', 'r² =', multiple_r_squared, '\n','r²adj =', adjusted_r_squared, '\n','RESPOSTA 2C:')
cat(res)
res_2c