library(readxl)
library(lubridate)
library(ggplot2)
library(hms)
library(dplyr)

w_t_logger <- read_excel("Data/Water_Temperature_Logger_2024.xlsx", col_types = c("text", "numeric"))

# Переводим китайское время в московское
w_t_logger$Date_Time <-
parse_date_time(w_t_logger$Date_Time, "Y-m-d H:M") - hours(5)



w_t_logger %>%
  filter(hour(Date_Time) %in% c(0,6, 12, 18)) %>%
  mutate(H = hour(Date_Time), H_M = as_hms(Date_Time)) %>%
  group_by(H) %>%
  filter(H_M == min(H_M)) %>%
  select(-H_M) ->w_t_logger_short


############################

hydr_long <- read_excel("data/Hydrology_monitoring_Youzhnzya_inlet_2007_2023.xlsx", na = "NA")

str(hydr_long)

df<-
hydr %>%
  select(Date_Time, Year, Air_T_logger, Water_T_logger, S) %>%
  rename(Water_T = Water_T_logger, Air_T = Air_T_logger)

str(df)

df$Date_Time <- format(df$Date_Time, "%d.%m.%Y %H:%M")


library(writexl)
hydr_long <-
rbind(hydr_long, df)

write_xlsx(hydr_long, "data/Hydrology_monitoring_Youzhnzya_inlet_2007_2024.xlsx")


