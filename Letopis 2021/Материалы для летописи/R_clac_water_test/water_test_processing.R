library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)


wtest <- read_excel("Data/all_data_finished.xlsx", na = "NA")

wtest$Date_begin <- as.POSIXct(as.numeric(wtest$Date) - wtest$Day*24*60*60, origin = "1970-01-01 00:00.00 UTC" )

wtest$DOY <- as.numeric(strftime(wtest$Date_begin, format = "%j"))

wtest$Year <- year(wtest$Date) 

wtest_2020 <- wtest %>% filter(Year == 2020)


median(wtest$DOY, na.rm = T)


wtest %>% group_by(Region) %>% summarise(DOY = median(DOY, na.rm = T))

ggplot(wtest, aes(x = factor(Year), DOY)) + geom_boxplot() + facet_wrap(~Region)

ggplot(wtest, aes(x = Region, DOY)) + geom_boxplot() + geom_hline(yintercept = mean(wtest$DOY, na.rm = T), linetype = 2) + labs(x = "Регион",  y = "День в году")



ggplot(wtest, aes(x = factor(Region), DOY)) + geom_boxplot() + facet_wrap(~Year)

ggplot(wtest, aes(y = N_eggs, x = DOY)) + geom_point()+ geom_smooth(method = "lm") 

ggplot(wtest_2020, aes(x = Site, y = DOY)) + geom_boxplot() + facet_wrap(~Region)


table(wtest$Site, wtest$Year)
