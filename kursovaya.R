if (!require("tidyverse")) install.packages("tidyverse")
if (!require("forecast")) install.packages("forecast")
if (!require("tseries")) install.packages("tseries")

library(tidyverse)
library(forecast)
library(tseries)

data <- read.csv("energyProduction.csv", sep=";", fileEncoding = "Latin1", stringsAsFactors = FALSE)
colnames(data) <- c("COUNTRY", "CODE_TIME", "TIME", "YEAR", "MONTH", "MONTH_NAME", "PRODUCT", "VALUE", "DISPLAY_ORDER", "yearToDate", "previousYearToDate", "share")
data$VALUE <- as.numeric(data$VALUE)

#расчет годового производства
annual_data <- data %>%
  group_by(COUNTRY, PRODUCT, YEAR) %>%
  summarize(ANNUAL_VALUE = sum(VALUE, na.rm = TRUE)) %>%
  ungroup()

total_production_by_country <- annual_data %>%
  group_by(COUNTRY) %>%
  summarize(TOTAL_PRODUCTION = sum(ANNUAL_VALUE))

#топ стран по производству электроэнергии
top_countries <- total_production_by_country %>%
  arrange(desc(TOTAL_PRODUCTION))


top_countries_10 <- top_countries[4:10, ]

barplot(top_countries_10$TOTAL_PRODUCTION, 
        names.arg = top_countries_10$COUNTRY,
        main = "Топ стран по производству электроэнергии",
        xlab = "Страна",
        ylab = "Производство электроэнергии (GWh)",
        col = "skyblue")

us_annual_data <- annual_data %>%
  filter(COUNTRY == "United States" & PRODUCT == "Electricity supplied") %>%
  select(YEAR, ANNUAL_VALUE)

ts_annual_data <- ts(us_annual_data$ANNUAL_VALUE, start = min(us_annual_data$YEAR), frequency = 1)

plot(ts_annual_data, main = "Годовое производство электроэнергии в США",
     ylab = "Производство (GWh)", xlab = "Год")

data$TIME <- as.Date(paste(data$YEAR, data$MONTH, "01", sep = "-"))

data_ts <- data %>%
  filter(COUNTRY == "United States" & PRODUCT == "Electricity supplied") %>%
  select(TIME, VALUE)

plot(data_ts$TIME, data_ts$VALUE, type = "l", 
     main = "Производство электроэнергии в США", 
     xlab = "Дата", ylab = "Производство (GWh)", 
     col = "blue", xaxt = "n")

axis(side = 1, at = seq(min(data_ts$TIME), max(data_ts$TIME), by = "years"), 
     labels = format(seq(min(data_ts$TIME), max(data_ts$TIME), by = "years"), "%Y"))  


ts_data <- ts(data_ts$VALUE, start = c(year(min(data_ts$TIME)), month(min(data_ts$TIME))), frequency = 12)

# тест Льюнга-Бокса
ljung_box_test <- Box.test(ts_data, lag = 12, type = "Ljung-Box")
ljung_box_test

# тест Дики-Фуллераь
adf_test_result <- adf.test(ts_data)
adf_test_result



decomposed_ts <- decompose(ts_data)
plot(decomposed_ts)


# уравнение тренда
trend_model <- tslm(ts_data ~ trend)

summary(trend_model)

intercept <- coef(trend_model)["(Intercept)"]
slope <- coef(trend_model)["trend"]

plot(ts_data, main = "График производства электроэнергии в США с уравнением тренда", xlab = "Год", ylab = "Производство (GWh)")
lines(fitted(trend_model), col = "red")  

# прогноз на 5 лет
forecast_model <- auto.arima(ts_data, d = 0)
forecast_values <- forecast(forecast_model, h = 12*5)
summary(forecast_model)


forecast_data <- as.data.frame(forecast_values)


forecast_data$Year <- paste0("20", substr(rownames(forecast_data), 8, 10))

sum_by_year <- forecast_data %>%
  group_by(Year) %>%
  summarise(
    Total_Point_Forecast = sum(`Point Forecast`),
    Total_Lo_80 = sum(`Lo 80`),
    Total_Hi_80 = sum(`Hi 80`),
    Total_Lo_95 = sum(`Lo 95`),
    Total_Hi_95 = sum(`Hi 95`)
  )

sum_by_year


plot(us_annual_data$YEAR, us_annual_data$ANNUAL_VALUE, type = "l", col = "blue", lwd = 2,
     xlim = c(min(us_annual_data$YEAR), max(c(max(us_annual_data$YEAR), as.integer(sum_by_year$Year)))), 
     ylim = c(min(sum_by_year$Total_Lo_95), max(sum_by_year$Total_Hi_95)), 
     xlab = "Год", ylab = "Производство (GWh)", main = "Годовое производство электроэнергии в США")

lines(as.integer(sum_by_year$Year), sum_by_year$Total_Point_Forecast, col = "red", lty = "dashed", lwd = 2)
lines(as.integer(sum_by_year$Year), sum_by_year$Total_Lo_80, col = "green", lty = "dotted", lwd = 2)
lines(as.integer(sum_by_year$Year), sum_by_year$Total_Hi_80, col = "green", lty = "dotted", lwd = 2)
lines(as.integer(sum_by_year$Year), sum_by_year$Total_Lo_95, col = "orange", lty = "dotted", lwd = 2)
lines(as.integer(sum_by_year$Year), sum_by_year$Total_Hi_95, col = "orange", lty = "dotted", lwd = 2)

lines(c(max(us_annual_data$YEAR), as.integer(sum_by_year$Year[1])), c(us_annual_data$ANNUAL_VALUE[nrow(us_annual_data)], sum_by_year$Total_Point_Forecast[1]), col = "blue", lwd = 2)
legend("topleft", legend = c("Прошлые значения", "Прогноз", "80% доверительный интервал", "95% доверительный интервал"), 
       col = c("blue", "red", "green", "orange"), lty = c(1, 2, 3, 3), lwd = 2)

axis(1, at = seq(min(us_annual_data$YEAR), max(as.integer(sum_by_year$Year)), by = 1))
grid()


# то же самое для Японии

jp_data <- data %>%
  filter(COUNTRY == "Japan" & PRODUCT == "Electricity supplied") %>%
  select(TIME, VALUE)

ts_jp_data <- ts(jp_data$VALUE, start = c(year(min(jp_data$TIME)), month(min(jp_data$TIME))), frequency = 12)

plot(ts_jp_data, main = "Ежемесячное производство электроэнергии В Японии",
     ylab = "Производство (GWh)", xlab = "Год")

jp_annual_data <- annual_data %>%
  filter(COUNTRY == "Japan" & PRODUCT == "Electricity supplied") %>%
  select(YEAR, ANNUAL_VALUE)

decomposed_jp_ts <- decompose(ts_jp_data)
plot(decomposed_jp_ts)

jp_trend_model <- tslm(ts_jp_data ~ trend)

summary(jp_trend_model)

intercept <- coef(jp_trend_model)["(Intercept)"]
slope <- coef(jp_trend_model)["trend"]

plot(ts_jp_data, main = "График прозиводства электроэнергии в Японии с линией тренда", xlab = "Год", ylab = "Производство (GWh)")
lines(fitted(jp_trend_model), col = "red")
adf_test_jp_result <- adf.test(ts_jp_data)
adf_test_jp_result

ljung_box_test <- Box.test(ts_jp_data, lag = 12, type = "Ljung-Box")
ljung_box_test

jp_forecast_model <- auto.arima(ts_jp_data, d = 0)
jp_forecast_values <- forecast(jp_forecast_model, h = 12*5) # Прогноз на 5 лет вперед
summary(jp_forecast_model)

jp_forecast_values
jp_forecast_data <- as.data.frame(jp_forecast_values)
jp_forecast_data$Year <- substr(rownames(jp_forecast_data), 5, 8)

sum_jp_by_year <- jp_forecast_data %>%
  group_by(Year) %>%
  summarise(
    Total_Point_Forecast = sum(`Point Forecast`),
    Total_Lo_80 = sum(`Lo 80`),
    Total_Hi_80 = sum(`Hi 80`),
    Total_Lo_95 = sum(`Lo 95`),
    Total_Hi_95 = sum(`Hi 95`)
  )

sum_jp_by_year

plot(jp_annual_data$YEAR, jp_annual_data$ANNUAL_VALUE, type = "l", col = "blue", lwd = 2,
     xlim = c(min(jp_annual_data$YEAR), max(as.integer(sum_jp_by_year$Year))), 
     ylim = c(min(sum_jp_by_year$Total_Lo_95), max(jp_annual_data$ANNUAL_VALUE)), 
     xlab = "Год", ylab = "Производство (GWh)", main = "Годовое производство электроэнергии в Японии")

lines(as.integer(sum_jp_by_year$Year), sum_jp_by_year$Total_Point_Forecast, col = "red", lty = "dashed", lwd = 2)
lines(as.integer(sum_jp_by_year$Year), sum_jp_by_year$Total_Lo_80, col = "green", lty = "dotted", lwd = 2)
lines(as.integer(sum_jp_by_year$Year), sum_jp_by_year$Total_Hi_80, col = "green", lty = "dotted", lwd = 2)
lines(as.integer(sum_jp_by_year$Year), sum_jp_by_year$Total_Lo_95, col = "orange", lty = "dotted", lwd = 2)
lines(as.integer(sum_jp_by_year$Year), sum_jp_by_year$Total_Hi_95, col = "orange", lty = "dotted", lwd = 2)

lines(c(max(jp_annual_data$YEAR), as.integer(sum_jp_by_year$Year[1])), c(jp_annual_data$ANNUAL_VALUE[nrow(jp_annual_data)], sum_jp_by_year$Total_Point_Forecast[1]), col = "blue", lwd = 2)

legend("bottomleft", legend = c("Прошлые значения", "Прогноз", "80% доверительный интервал", "95% доверительный интервал"), 
       col = c("blue", "red", "green", "orange"), lty = c(1, 2, 3, 3), lwd = 2)

axis(1, at = seq(min(jp_annual_data$YEAR), max(as.integer(sum_jp_by_year$Year)), by = 1))
grid()



# то же самое лдя Индии
ind_data <- data %>%
  filter(COUNTRY == "India" & PRODUCT == "Electricity supplied") %>%
  select(TIME, VALUE)

ts_ind_data <- ts(ind_data$VALUE, start = c(year(min(ind_data$TIME)), month(min(ind_data$TIME))), frequency = 12)

plot(ts_ind_data, main = "Ежемесячное производство электроэнергии в Индии",
     ylab = "Производство (GWh)", xlab = "Год")

decomposed_ind_ts <- decompose(ts_ind_data)
plot(decomposed_ind_ts)

ind_trend_model <- tslm(ts_ind_data ~ trend)
summary(ind_trend_model)

intercept <- coef(ind_trend_model)["(Intercept)"]
slope <- coef(ind_trend_model)["trend"]

plot(ts_ind_data, main = "График с уравнением тренда", xlab = "Год", ylab = "Производство (GWh)")
lines(fitted(ind_trend_model), col = "red")  

adf_test_ind_result <- adf.test(ts_ind_data)
adf_test_ind_result

ljung_box_test <- Box.test(ts_ind_data, lag = 12, type = "Ljung-Box")
ljung_box_test

ind_forecast_model <- auto.arima(ts_ind_data, d = 0)
ind_forecast_values <- forecast(ind_forecast_model, h = 12*5) 
summary(ind_forecast_model)

plot(ind_forecast_values, main = "Прогноз производства электроэнергии в Индии на 5 лет")


ind_forecast_data <- as.data.frame(ind_forecast_values)
ind_forecast_data$Year <- substr(rownames(jp_forecast_data), 5, 8)

ind_annual_data <- annual_data %>%
  filter(COUNTRY == "India" & PRODUCT == "Electricity supplied") %>%
  select(YEAR, ANNUAL_VALUE)

sum_ind_by_year <- ind_forecast_data %>%
  group_by(Year) %>%
  summarise(
    Total_Point_Forecast = sum(`Point Forecast`),
    Total_Lo_80 = sum(`Lo 80`),
    Total_Hi_80 = sum(`Hi 80`),
    Total_Lo_95 = sum(`Lo 95`),
    Total_Hi_95 = sum(`Hi 95`)
  )


plot(ind_annual_data$YEAR, ind_annual_data$ANNUAL_VALUE, type = "l", col = "blue", lwd = 2,
     xlim = c(min(ind_annual_data$YEAR), max(as.integer(sum_ind_by_year$Year))), 
     ylim = c(min(ind_annual_data$ANNUAL_VALUE), max(sum_ind_by_year$Total_Hi_95)), 
     xlab = "Год", ylab = "Производство электроэнергии (GWh)", main = "Годовое производство электроэнергии в Индии")

lines(as.integer(sum_ind_by_year$Year), sum_ind_by_year$Total_Point_Forecast, col = "red", lty = "dashed", lwd = 2)
lines(as.integer(sum_ind_by_year$Year), sum_ind_by_year$Total_Lo_80, col = "green", lty = "dotted", lwd = 2)
lines(as.integer(sum_ind_by_year$Year), sum_ind_by_year$Total_Hi_80, col = "green", lty = "dotted", lwd = 2)
lines(as.integer(sum_ind_by_year$Year), sum_ind_by_year$Total_Lo_95, col = "orange", lty = "dotted", lwd = 2)
lines(as.integer(sum_ind_by_year$Year), sum_ind_by_year$Total_Hi_95, col = "orange", lty = "dotted", lwd = 2)

lines(c(max(ind_annual_data$YEAR), as.integer(sum_ind_by_year$Year[1])), c(ind_annual_data$ANNUAL_VALUE[nrow(ind_annual_data)], sum_ind_by_year$Total_Point_Forecast[1]), col = "blue", lwd = 2)

legend("topleft", legend = c("Прошлые значения", "Прогноз", "80% доверительный интервал", "95% доверительный интервал"), 
       col = c("blue", "red", "green", "orange"), lty = c(1, 2, 3, 3), lwd = 2)

axis(1, at = seq(min(ind_annual_data$YEAR), max(as.integer(sum_ind_by_year$Year)), by = 1))
grid()

