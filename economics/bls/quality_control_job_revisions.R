library(dplyr)
library(readr)
library(qcc)
library(forecast)
library(ggplot2)
library(mclust)
library(tidyverse)
library(zoo)


df <- read_csv('job_revisions_1_mo.csv')

# You can jump down to Article 2 for the mixture modeling

# Article 1 ------

revision_chart <- qcc(data = df$revision,
                      type = "xbar.one",
                      labels = df$revision_report_date,
                      title = "Individuals Chart for Monthly Job Revisions (in thousands)",
                      xlab = "Month",
                      ylab = "Revision Delta (in thousands)")


full_start_date <- c(1955, 3)
revision_ts <- ts(df$revision, 
                  frequency = 12, 
                  start = full_start_date)

arima_model <- auto.arima(revision_ts)

print(arima_model)

checkresiduals(arima_model)

revision_forecast <- forecast(arima_model, h = 24)

plot(revision_forecast)

# Post 1980 ----
df_post1980 <- df %>%
  filter(revision_report_date >= as.Date("1980-01-01"))

revision_chart <- qcc(data = df_post1980$revision,
                      type = "xbar.one",
                      labels = df_post1980$revision_report_date,
                      title = "Individuals Chart for Monthly Job Revisions (in thousands)",
                      xlab = "Revision Report Date", # Adjusted to match the 'labels' data
                      ylab = "Revision Delta (in thousands)")

# Plot the newly defined chart
plot(revision_chart_corrected)

# Article 2 ------
# Back at it for some mixture modeling -------------

df_post1990 <- df %>%
  filter(revision_report_date >= as.Date("1990-01-01"))

# Assuming your data frame is named 'df'
revisions <- df_post1990$revision

revision_chart <- qcc(data = revisions,
                      type = "xbar.one",
                      labels = df_post1990$revision_report_date,
                      title = "BLS Jobs Revisions Since 1990",
                      xlab = "Month",
                      ylab = "Revision")


# Fit a GMM with 2 components
# V indicates variance can vary between components
# E indicates the means can be different
# The model="V" is for a 1-D case allowing unequal variance.
gmm_model <- Mclust(revisions, G = 2, modelNames = "V")

summary(gmm_model)
gmm_model$parameters

df_post1990$classification <- gmm_model$classification

# See which points are "surprises" (Component 2)
surprises <- df_post1990 %>% filter(classification == 2)
print(head(surprises))

# Visualize the result
 ggplot(df_post1990, aes(x = revision)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "gray", alpha = 0.6) +
  stat_function(fun = function(x) {
    gmm_model$parameters$pro[1] * dnorm(x, gmm_model$parameters$mean[1], sqrt(gmm_model$parameters$variance$sigmasq[1]))
  }, color = "blue", size = 1) +
  stat_function(fun = function(x) {
    gmm_model$parameters$pro[2] * dnorm(x, gmm_model$parameters$mean[2], sqrt(gmm_model$parameters$variance$sigmasq[2]))
  }, color = "red", size = 1) +
  labs(title = "Mixture Model of BLS 1-Month Jobs Revisions",
       subtitle = "Blue = 'Normal' | Red = 'Surprise'",
       x = "Revision Amount", y = "Density") +
  theme_minimal() +
  coord_cartesian(xlim = c(-1000, 1000))

# https://gemini.google.com/app/76bd42734ed1fd0e

# 1. Extract parameters from the model
params <- gmm_model$parameters
pi_k <- params$pro
mu_k <- params$mean
sigma_k <- sqrt(params$variance$sigmasq)

# Calculate the CDF value for each data point
x <- revisions 
F_y <- pi_k[1] * pnorm(x, mean = mu_k[1], sd = sigma_k[1]) +
       pi_k[2] * pnorm(x, mean = mu_k[2], sd = sigma_k[2])

# Transform to standard normal residuals using the inverse CDF (qnorm)
quantile_residuals <- qnorm(F_y)

# Visualize the residuals to check for normality

# A Q-Q plot is the best tool for this. Points should follow the line.
qqnorm(quantile_residuals, main = "Q-Q Plot of Quantile Residuals")
qqline(quantile_residuals, col = "red", lwd = 2)


resid_qcc <- qcc(data = quantile_residuals,
    type = "xbar.one",
    labels = df_post1990$revision_report_date,
    title = "Quantile Residuals over Time",
    xlab = "Month",
    ylab = "Residual")

# Identify which component has the larger variance (this is the "surprise" state)
variances <- gmm_model$parameters$variance$sigmasq
surprise_component_index <- which.max(variances)
print(paste("The 'Surprise' component is index:", surprise_component_index))

prob_surprise <- gmm_model$z[, surprise_component_index]

df_post1990$prob_surprise <- prob_surprise


ggplot(df_post1990, aes(x = revision_report_date, y = revision)) +
  # Use a line at zero for a clear baseline
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  # Plot revisions as points, with color mapped to the surprise probability
  geom_point(aes(color = prob_surprise), size = 2.5, alpha = 0.8) +
  
  # Use a color scale that makes high probabilities stand out
  scale_color_gradient(
    low = "dodgerblue", 
    high = "firebrick", 
    name = "Surprise\nProbability"
  ) +
  
  # Add titles and theme
  labs(
    title = "Forecast Revisions Colored by Surprise Probability",
    x = "Date",
    y = "Revision Amount"
  ) +
  theme_minimal()


# Calculate a 12-month rolling average of the surprise probability
df_post1990 <- df_post1990 %>%
  mutate(prob_surprise_avg = rollmean(prob_surprise, k = 12, fill = NA, align = "right"))

# Plot the original series with the rolling average overlaid
ggplot(df_post1990, aes(x = revision_report_date)) +
  geom_line(aes(y = prob_surprise), color = "gray80") +
  geom_line(aes(y = prob_surprise_avg), color = "firebrick", size = 1.2) +
  labs(
    title = "12-Month Rolling Average of Surprise Probability",
    subtitle = "Highlights sustained periods of higher uncertainty",
    x = "Date",
    y = "Probability of Surprise"
  ) +
  theme_minimal()
