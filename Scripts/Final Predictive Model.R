# Install required packages if not already installed
install.packages("dplyr")
install.packages("lubridate")

library(dplyr)
library(lubridate)

# Parameters for dataset generation
set.seed(42)
n_customers = 10000  # Number of customers
n_weeks = 52  # Number of weeks (1 year)

# Create a sequence of weekly dates starting from January 1st
start_date <- as.Date("2024-01-01")
week_dates <- seq.Date(from = start_date, by = "week", length.out = n_weeks)

# Generate weekly data
data <- data.frame(
  CustomerID = rep(1:n_customers, each = n_weeks),
  Date = rep(week_dates, n_customers),  # Assign weekly dates to each customer
  LoginsPerWeek = sample(1:40, n_customers * n_weeks, replace = TRUE),
  SessionDurationPerWeek = sample(1:300, n_customers * n_weeks, replace = TRUE),
  FeatureInteractionsPerWeek = sample(1:50, n_customers * n_weeks, replace = TRUE),
  TimeSinceLastLogin = sample(0:7, n_customers * n_weeks, replace = TRUE),
  LifecycleStage = sample(c("onboarding", "active", "at risk", "churned"),
                          n_customers * n_weeks, replace = TRUE, prob = c(0.15, 0.5, 0.25, 0.1))
)

# Save the dataset to CSV
write.csv(data, "weekly_saas_dataset_with_dates.csv", row.names = FALSE)

# Display the first few rows of the dataset
head(data)
str(data)


# Print summary of the dataset
summary(data)

# 1. Check for missing values across the dataset
missing_values <- colSums(is.na(data))
print(missing_values)

# 2. Check if the Date column is in the correct format
str(data$Date)  # Ensure this is a "Date" class

# 3. Verify numeric columns (LoginsPerWeek, SessionDurationPerWeek, etc.)
str(data)


# Ensure these columns are numeric and have valid ranges
summary(data$LoginsPerWeek)
summary(data$SessionDurationPerWeek)
summary(data$FeatureInteractionsPerWeek)
summary(data$TimeSinceLastLogin)

# Plot Logins per month Over Time

# Load necessary packages
library(dplyr)

# 1. Group the data by month and aggregate logins and session duration
data_by_month <- data %>%
  mutate(Month = format(Date, "%Y-%m")) %>%  # Extract month from the Date column
  group_by(Month) %>%
  summarise(
    Avg_LoginsPerMonth = mean(LoginsPerWeek),
    Avg_SessionDurationPerMonth = mean(SessionDurationPerWeek)
  )
str(data_by_month)

data_by_month$Month <- as.Date(paste0(data_by_month$Month, "-01"))
str(data_by_month$Month)

# 2. Plot aggregated logins per month

library(ggplot2)
ggplot(data_by_month, aes(x = as.Date(paste0(Month, "-01")), y = Avg_LoginsPerMonth)) +
  geom_line(color = "blue") +
  labs(title = "Average Logins Per Month", x = "Month", y = "Average Logins Per Month")

# 3. Plot aggregated session duration per month
ggplot(data_by_month, aes(x = as.Date(paste0(Month, "-01")), y = Avg_SessionDurationPerMonth)) +
  geom_line(color = "green") +
  labs(title = "Average Session Duration Per Month", x = "Month", y = "Average Session Duration (minutes)")


#density plot

# Density plot for Logins Per Month
ggplot(data_by_month, aes(x = Avg_LoginsPerMonth)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Average Logins Per Month", x = "Average Logins Per Month", y = "Density")

# Density plot for Session Duration Per Month
ggplot(data_by_month, aes(x = Avg_SessionDurationPerMonth)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot of Average Session Duration Per Month", x = "Average Session Duration (minutes)", y = "Density")

# Summary statistics for Logins Per Month
summary(data_by_month$Avg_LoginsPerMonth)

# Summary statistics for Session Duration Per Month
summary(data_by_month$Avg_SessionDurationPerMonth)

#  ACF and PACF for Average Logins Per Month
install.packages("forecast")
library(forecast)

acf(data_by_month$Avg_LoginsPerMonth, main="ACF of Average Logins Per Month")
pacf(data_by_month$Avg_LoginsPerMonth, main="PACF of Average Logins Per Month")

#  ACF and PACF for Average Session Duration Per Month
acf(data_by_month$Avg_SessionDurationPerMonth, main="ACF of Average Session Duration Per Month")
pacf(data_by_month$Avg_SessionDurationPerMonth, main="PACF of Average Session Duration Per Month")


# Fitting an ARIMA model for Average Logins Per Month
arima_logins <- auto.arima(data_by_month$Avg_LoginsPerMonth)
summary(arima_logins)

# Fitting an ARIMA model for Average Session Duration Per Month
arima_sessions <- auto.arima(data_by_month$Avg_SessionDurationPerMonth)
summary(arima_sessions)

# Check residuals
# Residual diagnostics for Average Logins Per Month
checkresiduals(arima_logins)

# Residual diagnostics for Average Session Duration Per Month
checkresiduals(arima_sessions)

# Ljung-Box Test for checking the independence of residuals
Box.test(residuals(arima_logins), lag=10, type="Ljung-Box")
Box.test(residuals(arima_sessions), lag=10, type="Ljung-Box")

# Shapiro-Wilk Test for checking normality of residuals
shapiro.test(residuals(arima_logins))
shapiro.test(residuals(arima_sessions))

#Forecast
# Forecast the next 12 months for Average Logins Per Month

library(forecast)
logins_forecast <- forecast(arima_logins, h=12)
plot(logins_forecast)

# Forecast the next 12 months for Average Session Duration Per Month
sessions_forecast <- forecast(arima_sessions, h=12)
plot(sessions_forecast)

# Test
# Step 1: Train-Test Split (80% Train, 20% Test)
train_logins <- head(data_by_month$Avg_LoginsPerMonth, round(length(data_by_month$Avg_LoginsPerMonth) * 0.8))
test_logins <- tail(data_by_month$Avg_LoginsPerMonth, length(data_by_month$Avg_LoginsPerMonth) - length(train_logins))

train_sessions <- head(data_by_month$Avg_SessionDurationPerMonth, round(length(data_by_month$Avg_SessionDurationPerMonth) * 0.8))
test_sessions <- tail(data_by_month$Avg_SessionDurationPerMonth, length(data_by_month$Avg_SessionDurationPerMonth) - length(train_sessions))

# Step 2: Refit the ARIMA Model on Training Data
arima_logins_train <- auto.arima(train_logins)
arima_sessions_train <- auto.arima(train_sessions)

# Step 3: Predict on Test Data
logins_forecast_test <- forecast(arima_logins_train, h=length(test_logins))
sessions_forecast_test <- forecast(arima_sessions_train, h=length(test_sessions))

# Step 4: Compare Predictions to Actual Test Data
logins_comparison <- data.frame(Actual = test_logins, Predicted = logins_forecast_test$mean)
sessions_comparison <- data.frame(Actual = test_sessions, Predicted = sessions_forecast_test$mean)

# Print comparison for logins
print(logins_comparison)

# Print comparison for sessions
print(sessions_comparison)

# Step 5: Calculate Error Metrics (MAE, RMSE)
logins_mae <- mean(abs(logins_comparison$Actual - logins_comparison$Predicted))
logins_rmse <- sqrt(mean((logins_comparison$Actual - logins_comparison$Predicted)^2))

sessions_mae <- mean(abs(sessions_comparison$Actual - sessions_comparison$Predicted))
sessions_rmse <- sqrt(mean((sessions_comparison$Actual - sessions_comparison$Predicted)^2))

cat("Logins MAE:", logins_mae, "\n")
cat("Logins RMSE:", logins_rmse, "\n")
cat("Sessions MAE:", sessions_mae, "\n")
cat("Sessions RMSE:", sessions_rmse, "\n")

# Visualize Actual vs Predicted for Logins
library(ggplot2)
plot(test_logins, type="l", col="blue", lwd=2, xlab="Time", ylab="Logins", main="Actual vs Predicted Logins")
lines(logins_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)

# Visualize Actual vs Predicted for Session Duration
plot(test_sessions, type="l", col="blue", lwd=2, xlab="Time", ylab="Session Duration", main="Actual vs Predicted Session Duration")
lines(sessions_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)


length(test_logins)
length(test_logins)

length(test_sessions)
length(test_sessions)

length(test_logins)
length(logins_forecast_test$mean)

length(test_sessions)
length(sessions_forecast_test$mean)


# Generate forecast for the same length as the test set
logins_forecast_test <- forecast(arima_logins_train, h=length(test_logins))
sessions_forecast_test <- forecast(arima_sessions_train, h=length(test_sessions))

# Visualize Actual vs Predicted for Logins with Correct Index
library(ggplot2)
plot(test_logins, type="l", col="blue", lwd=2, xlab="Time", ylab="Logins", main="Actual vs Predicted Logins")
lines(1:length(logins_forecast_test$mean), logins_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)

# Visualize Actual vs Predicted for Session Duration with Correct Index
plot(test_sessions, type="l", col="blue", lwd=2, xlab="Time", ylab="Session Duration", main="Actual vs Predicted Session Duration")
lines(1:length(sessions_forecast_test$mean), sessions_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)


# Print first few rows of forecasted and actual values for logins
print(head(logins_forecast_test$mean))
print(head(test_logins))

str(logins_forecast_test$mean)
str(test_logins)
str(sessions_forecast_test$mean)
str(test_sessions)
# Print first few rows of forecasted and actual values for sessions
print(head(sessions_forecast_test$mean))
print(head(test_sessions))

# New Train-Test Split for no reason
# Step 1: Train-Test Split (Increase Test Set Length)
# Use a larger portion of the data for testing, e.g., the last 4 months
train_logins <- head(data_by_month$Avg_LoginsPerMonth, round(length(data_by_month$Avg_LoginsPerMonth) * 0.75))  # 75% for training
test_logins <- tail(data_by_month$Avg_LoginsPerMonth, length(data_by_month$Avg_LoginsPerMonth) - length(train_logins))  # 25% for testing

train_sessions <- head(data_by_month$Avg_SessionDurationPerMonth, round(length(data_by_month$Avg_SessionDurationPerMonth) * 0.75))  # 75% for training
test_sessions <- tail(data_by_month$Avg_SessionDurationPerMonth, length(data_by_month$Avg_SessionDurationPerMonth) - length(train_sessions))  # 25% for testing

# Step 2: Refit the ARIMA Model on the Longer Training Data
arima_logins_train <- auto.arima(train_logins)
arima_sessions_train <- auto.arima(train_sessions)

# Step 3: Generate Forecast for the Test Data Period
logins_forecast_test <- forecast(arima_logins_train, h=length(test_logins))
sessions_forecast_test <- forecast(arima_sessions_train, h=length(test_sessions))

# Step 4: Visualize Actual vs Predicted with the Longer Test Set
plot(test_logins, type="l", col="blue", lwd=2, xlab="Time", ylab="Logins", main="Actual vs Predicted Logins")
lines(1:length(logins_forecast_test$mean), logins_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)

plot(test_sessions, type="l", col="blue", lwd=2, xlab="Time", ylab="Session Duration", main="Actual vs Predicted Session Duration")
lines(1:length(sessions_forecast_test$mean), sessions_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)


# Ensure correct time index for plotting
time_index <- 1:length(test_logins)  # Create a time index that matches the test data

# Plot Actual vs Predicted for Logins
plot(time_index, test_logins, type="l", col="blue", lwd=2, xlab="Time", ylab="Logins", main="Actual vs Predicted Logins")
lines(time_index, logins_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)

# Plot Actual vs Predicted for Session Duration
plot(time_index, test_sessions, type="l", col="blue", lwd=2, xlab="Time", ylab="Session Duration", main="Actual vs Predicted Session Duration")
lines(time_index, sessions_forecast_test$mean, col="red", lwd=2)
legend("topright", legend=c("Actual", "Predicted"), col=c("blue", "red"), lty=1, lwd=2)

arima_logins_train <- auto.arima(train_logins)
arima_sessions_train <- auto.arima(train_sessions)

logins_forecast_test <- forecast(arima_logins_train, h=length(test_logins))
sessions_forecast_test <- forecast(arima_sessions_train, h=length(test_sessions))
