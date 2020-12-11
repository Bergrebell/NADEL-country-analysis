# Source: https://www.jakeruss.com/cheatsheets/stargazer/

library("dplyr")
library("nycflights13")
library("AER") # Applied Econometrics with R
library("stargazer")

daily <- flights %>%
  filter(origin == "EWR") %>%
  group_by(year, month, day) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))

daily_weather <- weather %>%
  filter(origin == "EWR") %>%
  group_by(year, month, day) %>%
  summarise(temp   = mean(temp, na.rm = TRUE),
            wind   = mean(wind_speed, na.rm = TRUE),
            precip = sum(precip, na.rm = TRUE))

# Merge flights with weather data frames
both <- daily %>%
  inner_join(y = daily_weather, by = c("year", "month", "day")) %>% 
  data.frame()  # Temporary fix

# Create an indicator for quarter
both$quarter <- cut(both$month, breaks = c(0, 3, 6, 9, 12), 
                    labels = c("1", "2", "3", "4"))

# Create a vector of class logical
both$hot <- as.logical(both$temp > 85)

head(both)

stargazer(t05, type = "text")

stargazer(as.data.frame(t05[c("year","Y_his")]), type = "text", digits=1,flip = TRUE)


stargazer(as.data.frame(t05[c("c_age","Y_his")]), 
          type = 'text', min.max=TRUE, mean.sd = TRUE, 
          nobs = FALSE, median = FALSE, iqr = FALSE,
          digits=1, align=T,
          summary.stat = c("min", "mean","max", "median"),
          title = "Summary Statistics",
          out = 'test.html')

