# Get the working directory
getwd()

# Importing required packages and dataset
library(tidyverse)
library(ggpubr)
library(broom)
library(ggfortify)

mpg <- read.csv('mpg.csv', header = T, sep = ',')

## inspect dataset
head(mpg)
dim(mpg) # Should be 234 rows and 12 variables for now

#Check the internal structure of the data frame
str(mpg)

# Count missing values in the variables
sum(is.na(mpg))
sapply(mpg, function(x) sum(is.na(x)))

colnames(mpg) # column names for the data frame

# Drop the first column (X column) of the data frame
mpg <- mpg[, -1]
colnames(mpg)

# Data Visualizations
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  stat_smooth()

# Find the correlation between the chosen variables
cor(mpg$cty, mpg$hwy)

# Model Building: linear regression model
model <- lm(hwy ~ cty, data = mpg) # hwy = 0.8982 + 1.337cty 
model

# Plot the regression line for the model
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point() +
  stat_smooth(method = lm) # se = False to remove confidence interval

# Model Assessment I
summary(model)

# Calculate the confidence interval for the coefficients
confint(model) # p-value is highly significant therefore reject the null hypothesis

# Calculate the prediction error of the fitted model
sigma(model)*100/mean(mpg$hwy)

# Model Prediction
# Find the fitted values of the simple regression model
fitted <- predict.lm(model)
head(fitted, 3)

# Find the fitted values of the simple regression model
model_diag_metrics <- augment(model)
head(model_diag_metrics)

# Visualize the residual error of the fitted model
ggplot(model_diag_metrics, aes(cty, hwy)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = cty, yend = .fitted), color = 'red', size = 0.5)

# Predicting new values using the model
predict(
  object = model, 
  newdata = data.frame(cty = c(21, 27, 14))
)

# Assumptions Check: Diagnostic Plots

# Plotting the fitted model
par(mfrow = c(2, 2))   ## This plots the figures in a 2 x 2
plot(model)

# Better Version using autoplot
autoplot(model)

## 9.2: Return par back to default
dev.off()

## or
par(mfrow = c(1, 1))

# Return the first diagnostic plot for the model
plot(model, 1)

# Build another regression model using non-linear transformation of the predictor
model_1 <- lm(hwy ~ sqrt(cty), data = mpg)
plot(model_1, 1)

# Return the second diagnostic plot for the model
plot(model, 2)

# Return the third diagnostic plot for the model
plot(model, 3) # potentially use a log transformation on the predictor variable o prove assumption

#  Multiple Regression
multiple_reg <- lm(hwy ~ cty + cyl, data = mpg)

multiple_reg

# Check the summary of the multiple regression model
summary(multiple_reg)

# Plot the fitted multiple regression model
autoplot(multiple_reg)