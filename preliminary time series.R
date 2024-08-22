# Load the data
data("EuStockMarkets")
?EuStockMarkets

# Extract a specific index (e.g., DAX) for analysis
tsData <- EuStockMarkets[, "DAX"]

# Plot the time series data
plot(tsData, main = "DAX Index Over Time", ylab = "Index Value", xlab = "Time")

# Exploratory Analysis for time series components
# Decompose the time series to check for Trend + Seasonality + Cyclicity
# Note: Decompose assumes an additive model and requires a seasonal component.
# Since the DAX data is daily without a clear seasonal pattern, this may not be suitable.

# If you still want to decompose for trend:
components.ts <- decompose(tsData, type = "multiplicative")
plot(components.ts)

# Check stationarity with the KPSS Test
library(urca)
kpss_test <- ur.kpss(tsData, type = 'tau')
summary(kpss_test)

# Differencing to achieve stationarity if KPSS indicates non-stationarity
if(kpss_test@teststat > kpss_test@cval[1]) {
  ts_stationary <- diff(tsData)
  plot(ts_stationary, main = "Differenced DAX Index", ylab = "Differenced Value", xlab = "Time")
} else {
  ts_stationary <- tsData
}

# Seasonal Adjustment (if applicable)
# Subtract the seasonal component if the decomposition indicated seasonality
if (!is.null(components.ts$seasonal)) {
  ts_stationary <- ts_stationary - components.ts$seasonal
  plot(ts_stationary, main = "Seasonally Adjusted DAX Index", ylab = "Adjusted Value", xlab = "Time")
}

# Plot ACF and PACF to determine the order of ARIMA model
acf(ts_stationary, main = "ACF of Stationary DAX Index", lag.max = 40)
pacf(ts_stationary, main = "PACF of Stationary DAX Index", lag.max = 40)

# Fit an ARIMA model
library(forecast)
model <- auto.arima(tsData, trace = TRUE)

# Summarize the model
summary(model)

# Plot the residuals of the model
plot(model$residuals, main = "Residuals of ARIMA Model", ylab = "Residuals", xlab = "Time")

# Check the residuals for autocorrelation to validate the model fit
acf(model$residuals, main = "ACF of Model Residuals")

# Forecasting using the fitted ARIMA model
forecasted_values <- forecast(model, h = 200, level = c(95))
plot(forecasted_values, main = "Forecast of DAX Index", ylab = "Index Value", xlab = "Time")

# Evaluate forecast accuracy (Optional)
accuracy(forecasted_values)
