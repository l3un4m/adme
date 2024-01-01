# Load necessary library
library(ISLR)

# Load Auto dataset
data("Auto")

# Select subset from observation 1 to 50
auto_subset <- Auto[1:50, 1:8]

# Summary statistics
summary(auto_subset)

# Pairwise scatterplot matrix
pairs(auto_subset)
# Fit a multiple linear regression model
lm_model <- lm(mpg ~ ., data = auto_subset)

# Summary of the regression
summary(lm_model)
# Residuals vs Fitted plot
plot(lm_model, which = 1)

# Normal Q-Q plot
plot(lm_model, which = 2)

# Cook's distance plot
plot(lm_model, which = 4)

# Identify influential observations
influential_obs <- which( cooks.distance(lm_model) > 4 / length(auto_subset$mpg) )

# Print influential observations
print(influential_obs)
# Confidence intervals for expected values
conf_int_14 <- predict(lm_model, newdata = auto_subset[14, ], interval = "confidence", level = 0.975)
conf_int_31 <- predict(lm_model, newdata = auto_subset[31, ], interval = "confidence", level = 0.975)

# Prediction intervals
pred_int_14 <- predict(lm_model, newdata = auto_subset[14, ], interval = "prediction", level = 0.975)
pred_int_31 <- predict(lm_model, newdata = auto_subset[31, ], interval = "prediction", level = 0.975)

# Print results
print(conf_int_14)
print(conf_int_31)
print(pred_int_14)
print(pred_int_31)

