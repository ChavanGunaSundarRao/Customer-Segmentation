install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("forecast")
library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
uber_data <- read.csv(file.choose(), stringsAsFactors = FALSE)
head(uber_data)
colnames(uber_data)
uber_data$START_DATE <- parse_date_time(
  uber_data$START_DATE,
  orders = c("mdy HM", "m/d/Y H:M", "m/d/y H:M")
)

uber_data <- uber_data %>%
  mutate(
    Date = as.Date(START_DATE),
    Day = day(START_DATE),
    Month = month(START_DATE, label = TRUE),
    Year = year(START_DATE),
    Hour = hour(START_DATE),
    Weekday = wday(START_DATE, label = TRUE)
  )

ggplot(uber_data, aes(x = Hour)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Trips by Hour of Day",
       x = "Hour",
       y = "Number of Trips")
ggplot(uber_data, aes(x = Weekday)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Trips by Day of the Week",
       x = "Weekday",
       y = "Number of Trips")
daily_trips <- uber_data %>%
  group_by(Date) %>%
  summarise(Total_Trips = n())

ggplot(daily_trips, aes(x = Date, y = Total_Trips)) +
  geom_line(color = "tomato") +
  labs(title = "Total Daily Trips Over Time",
       x = "Date",
       y = "Number of Trips")
ggplot(uber_data, aes(x = CATEGORY)) +
  geom_bar(fill = "purple") +
  labs(title = "Trip Category Breakdown",
       x = "Category",
       y = "Total Trips")
ggplot(uber_data, aes(x = PURPOSE)) +
  geom_bar(fill = "orange") +
  labs(title = "Trip Purpose Distribution",
       x = "Purpose",
       y = "Total Trips") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
uber_data %>%
  group_by(PURPOSE) %>%
  summarise(Total_Miles = sum(MILES, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(PURPOSE, -Total_Miles), y = Total_Miles)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Miles by Trip Purpose",
       x = "Purpose",
       y = "Total Miles") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ts_data <- ts(daily_trips$Total_Trips, frequency = 7)
model <- auto.arima(ts_data)
future_30 <- forecast(model, h = 30)



plot(future_30,
     main = "Forecast of Daily Uber Trips (Next 14 Days)",
     xlab = "Time",
     ylab = "Trip Count")
future_30 <- forecast(model, h = 30)

last_date <- max(daily_trips$Date, na.rm = TRUE)

future_dates <- seq(from = last_date + 1,
                    by = "day",
                    length.out = 30)

forecast_df <- data.frame(
  Date = future_dates,
  Predicted_Trips = as.numeric(future_30$mean),
  Lower = as.numeric(future_30$lower[,2]),
  Upper = as.numeric(future_30$upper[,2])
)
p <- ggplot() +
  geom_line(data = daily_trips,
            aes(x = Date, y = Total_Trips),
            color = "blue") +
  geom_line(data = forecast_df,
            aes(x = Date, y = Predicted_Trips),
            color = "red",
            linetype = "dashed") +
  geom_ribbon(data = forecast_df,
              aes(x = Date, ymin = Lower, ymax = Upper),
              fill = "pink",
              alpha = 0.3) +
  labs(title = "Forecast of Uber Trips for Next 30 Days",
       x = "Date",
       y = "Trip Count") +
  theme_minimal()

print(p)
