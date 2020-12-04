library(ggplot2)
library(lubridate)
library(readxl)

tide <- read.csv("Data/tides.csv", header = F)
tide <- tide[-c(1:144),-1]

names(tide) <- c("H", "Time", "Day_part", "Date")

tide$Date[1:144] <- "25/07/2009"
tide$Date[145:nrow(tide)] <- "26/07/2009"


tide$Date2 <- paste(tide$Time, tide$Day_part, tide$Date )

tide$Date3 <- parse_date_time(tide$Date2, '%I:%M %p %d/%m%Y', tz = "Europe/Moscow")

min_time <- as.POSIXct(strptime("17:00 25.07.2009", format = "%H:%M %d.%m.%Y", tz = "Europe/Moscow"))

max_time <- as.POSIXct(strptime("13:00 26.07.2009", format = "%H:%M %d.%m.%Y", tz = "Europe/Moscow"))

tide <- tide[tide$Date3 < max_time & tide$Date3 > min_time, ]




ggplot(tide, aes(x = Date3, y = H)) + geom_line()


sal_tuv <- read_excel("Data/TuMyt_2009_2010.xlsx", sheet = "sal-temp")

sal_tuv$Dat_text <- paste(sal_tuv$Time, sal_tuv$Date)


sal_tuv$Date_true <- as.POSIXct(strptime(sal_tuv$Dat_text, '%H_%M %d_%m_%Y', tz = "Europe/Moscow"))


sal_tuv$H <- abs(sal_tuv$Height)

ggplot(tide, aes(x = Date3, y = H)) + geom_line() + geom_point(data = sal_tuv, aes(x = Date_true, y = abs(Height), size = Salinity)) + facet_grid(Transect ~ Depth) + geom_hline(yintercept = c(2, 1.5, 1, 0.5, -0.5, -1.5, -3.5))

ggplot(sal_tuv, aes(x = abs(Height), y = Salinity)) + geom_point()


Mod_tide <- lm(Salinity ~ H*Distance, data = sal_tuv)

summary(Mod_tide)


tuv <- read_excel("Data/TuMyt_2009_2010.xlsx", sheet = "для многомерки" )

tuv$Mean_Salinity_predicted <- NA
tuv$Min_Salinity_predicted <- NA


library(dplyr)

for(i in 1:nrow(tuv)){
  d <- tide %>% filter(H >= tuv$Depth[i])
  d$Distance <- tuv$Distance[i]
  Sal_pred <- predict(Mod_tide, newdata = d)
  tuv$Mean_Salinity_predicted[i] <- mean(Sal_pred)
  tuv$Min_Salinity_predicted[i] <- min(Sal_pred)
}



tuv$Min_Salinity_predicted

ggplot(tuv, aes(x = Distance, y = Min_Salinity_predicted)) + geom_point()
ggplot(tuv, aes(x = Distance, y = Min_Salinity_predicted)) + geom_text(aes(label = Depth), position = position_jitter(width = 0))


tide_salinity$Predict_sal <- predict(Mod_tide, newdata = tide_salinity)

ggplot(tide_salinity, aes(x = Date3, y = H, size = Predict_sal)) + geom_point() + geom_hline(yintercept = 1.5)


tuv_print <- tuv %>% select(Sample_ID, Min_Salinity_predicted, Mean_Salinity_predicted)

write.table(tuv_print, "Data/Salinity_predicted.csv", sep = ";", row.names = F)

