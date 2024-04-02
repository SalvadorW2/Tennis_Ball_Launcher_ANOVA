### Loading Packages ###

library(tidyverse)
library(ggpubr)
library(rstatix)
library(nortest)


# Importing data
data = read.csv("C:\\Users\\scwag\\OneDrive\\Desktop\\R_Workspace\\Statistics\\tennis_ball_data.csv")
head(data)

# Setting seed
set.seed(99)

# Converting to factors
data$angle = as.factor(data$angle)
levels(data$angle)

data$psi = as.factor(data$psi)
levels(data$psi)


# Boxplot of data
box_plot = ggplot(data = data, aes(x = angle, y = distance, color = psi)) + 
  geom_boxplot()

box_plot

# 2-way ANOVA
anova_1 = aov(distance ~ psi + angle + angle * psi, data = data)
anova_summary = summary(anova_1)
anova_summary

## Getting standardized residuals
data$std_residuals = rstandard(anova_1)
head(data$std_residuals)

## Getting adjusted mean squared error
adj_mse = anova_summary[[1]][["Mean Sq"]][4]
adj_mse

## Calculating estimated standard deviation
est_sd = sqrt(adj_mse)
est_sd

# Checking ANOVA assumptions

# Normality

## QQ plot
ggqqplot(residuals(anova_1))

## Histogram of residuals
ggplot(data = data, aes(x = anova_1$residuals)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")

## Anderson-Darling Test
ad.test(residuals(anova_1))

## Homogeneity of variance

### Standardized residuals by fitted value (distance)
ggplot(data = data, aes(x = distance, y = std_residuals)) +
  geom_point() +
  labs(title = "Standardized Residuals by Fitted Value",
       x = "Distance",
       y = "Standardized Residuals")

### Standardized residuals by PSI
ggplot(data = data, aes(x = as.numeric(psi), y = std_residuals)) +
  geom_point() +
  xlim(min(as.numeric(data$psi)), max(as.numeric(data$psi))) +
  labs(title = "Standardized Residuals by PSI",
       x = "PSI",
       y = "Standardized Residuals")


### Standardized residuals by angle
ggplot(data = data, aes(x = as.numeric(angle), y = std_residuals)) +
  geom_point() +
  xlim(min(as.numeric(data$angle)), max(as.numeric(data$angle))) +
  labs(title = "Standardized Residuals by Angle",
       x = "Angle",
       y = "Standardized Residuals")

## No outliers

### Standardized residuals by leverage
ggplot(data = data, aes(x = std_residuals)) +
  geom_histogram(bins = 15, fill = "steelblue", color = "black") +
  labs(title = "Standardized Residuals by Frequency",
       x = "Standardized Residuals",
       y = "Frequency")

### Cook's distance
plot(anova_1, 4)

# Independence of residuals
ggplot(data = data, aes(x = seq_along(std_residuals), y = std_residuals)) +
  geom_point() +
  labs(title = "Standardized Residuals by Order",
       x = "Order of Observations",
       y = "Standardized Residuals")

# Post-hoc Tests

## PSI
psi_tukey = TukeyHSD(x = anova_1, which = "psi")
psi_tukey

plot(psi_tukey, las = 1)

## Angle
angle_tukey = TukeyHSD(x = anova_1, which = "angle")
angle_tukey

angle_tukey = TukeyHSD(x = anova_1, which = "angle")
plot(angle_tukey, las = 1)

# Angle group means
angle_means = data %>%
  group_by(angle) %>%
  dplyr::summarize(Mean = mean(distance, na.rm = TRUE))
angle_means

(143.466 - 138.3013) / est_sd
(138.3013 - 123.0760) / est_sd

# PSI group means
psi_means = data %>%
  group_by(psi) %>%
  dplyr::summarize(Mean = mean(distance, na.rm = TRUE))
psi_means

(199.976 - 143.58867) / est_sd
(143.58867 - 72.27867) / est_sd
