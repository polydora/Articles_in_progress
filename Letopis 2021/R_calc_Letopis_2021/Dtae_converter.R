


hydr <- read.csv("data/Hydrology_monitoring_Youzhnzya_inlet_2007_2020.csv")

hydr$Date_Time <- paste(format(hydr$date, format = "%d-%m-%Y"), format(hydr$time, format = "%H"))

hydr$Date2 <- as.POSIXct(hydr$Date_Time, tz = "Europe/Moscow", format = "%m/%d/%Y %H")

hydr$Date_Time2 <- format(hydr$Date2, "%d.%m.%Y %H:00")

hydr$Date_day <- format(hydr$Date2, "%d-%m")

library(dplyr)
hydr <- hydr %>% select(Date_Time2, Year, Air_T, Water_T,  S )


write.csv(hydr, "Hydrology_monitoring_Youzhnzya_inlet_2007_2020.csv", row.names = F)
