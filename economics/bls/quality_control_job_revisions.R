library(dplyr)
library(readr)
library(qcc)
library(forecast)

df <- read_csv('job_revisions_1_mo.csv')

revision_chart <- qcc(data = df$revision,
                      type = "xbar.one",
                      labels = df$revision_report_date,
                      title = "Individuals Chart for Monthly Job Revisions (in thousands)",
                      xlab = "Month",
                      ylab = "Revision Delta (in thousands)")

summary(revision_chart)

plot(revision_chart)


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
