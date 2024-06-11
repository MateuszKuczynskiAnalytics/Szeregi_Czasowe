library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

raw_data <- read.csv("Dane/PJME_hourly.csv")

raw_data <- raw_data %>%
  mutate(datetime = as.POSIXct(Datetime, format = "%Y-%m-%d %H:%M:%S"))

ggplot(raw_data, aes(x = datetime, y = PJME_MW)) +
  geom_point(size = 0.3) +
  labs(title = "Time Series Plot",
       x = "Datetime",
       y = "PJME_MW") +
  theme_minimal()

# Create the time series object with frequency = 24 for hourly data
ts_data <- ts(raw_data$PJME_MW, frequency = 24)

ndiffs(ts_data)


adf_test <- adf.test(ts_data)
print(adf_test)

kpss_test <- kpss.test(ts_data)
print(kpss_test)

autoplot(ts_data) + ggtitle("Original Series")

# Plot the differenced series
diff_ts_data <- diff(ts_data, differences = 1)
autoplot(diff_ts_data) + ggtitle("Differenced Series")

ts_data_seasonal_diff <- diff(ts_data, lag = frequency(ts_data))
autoplot(ts_data_seasonal_diff) + ggtitle("Seasonally Differenced Series")

decomposition <- stl(ts_data, s.window = "periodic")

# Plot the decomposed components
autoplot(decomposition) + ggtitle("Decomposed Time Series")

seasonal_diff <- diff(ts_data, lag = frequency(ts_data))
autoplot(seasonal_diff) + ggtitle("Seasonally Differenced Series")

adf_test_seasonal_diff <- adf.test(seasonal_diff)
print(adf_test_seasonal_diff)

# Perform KPSS test on seasonally differenced data
kpss_test_seasonal_diff <- kpss.test(seasonal_diff)
print(kpss_test_seasonal_diff)