library(ggplot2)
library(lubridate)
library(readxl)
library(dplyr)


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



ggplot(sal_tuv, aes(x =Depth, y = Salinity)) + geom_point() + geom_smooth()

sal_tuv %>% filter(Depth == -2) %>% summarize(Sal = mean(Salinity))%>% 
  ggplot(., aes(x = Distance, y = Salinity)) + geom_point()


nrow(sal_tuv)

sal_tuv$Time

ggplot(tide, aes(x = Date3, y = H)) + geom_line() + geom_point(data = sal_tuv, aes(x = Date_true, y = abs(Height), size = Salinity)) + facet_grid(Transect ~ Depth) + geom_hline(yintercept = c(2, 1.5, 1, 0.5, -0.5, -1.5, -3.5))

ggplot(sal_tuv, aes(x = (Height), y = Salinity)) + geom_point() +  geom_smooth(method = "lm", formula = y ~poly(x, 2))


Mod_tide <- lm(Salinity ~ H*Distance, data = sal_tuv) 

summary(Mod_tide)


tuv <- read_excel("Data/TuMyt_2009_2010.xlsx", sheet = "для многомерки" )

tuv$Mean_Salinity_predicted <- NA
tuv$Min_Salinity_predicted <- NA


library(dplyr)
i=9
for(i in 1:nrow(tuv)){
  d <- tide %>% filter(H >= tuv$Depth[i])
  d$Distance <- tuv$Distance[i]
  Sal_pred <- predict(Mod_tide, newdata = d)
  tuv$Mean_Salinity_predicted[i] <- mean(Sal_pred)
  tuv$Min_Salinity_predicted[i] <- min(Sal_pred)
}


tide_transect_R <- tide
tide_transect_R$Transect <- "R"
tide_transect_R$Distance <- 0


tide_transect_B <- tide
tide_transect_B$Transect <- "B"
tide_transect_B$Distance <- 250


tide_transect_Mid <- tide
tide_transect_Mid$Transect <- "Mid"
tide_transect_Mid$Distance <- 1750


tide_transect_Mon <- tide
tide_transect_Mon$Transect <- "Mon"
tide_transect_Mon$Distance <- 2550

tide_transect <- rbind(tide_transect_R, tide_transect_B, tide_transect_Mid, tide_transect_Mon)

tide_transect$Salinity_predicted <- predict(Mod_tide, newdata = tide_transect)

tide_transect$Transect <- factor(tide_transect$Transect, levels = c("R", "B", "Mid", "Mon"))
sal_tuv$Transect <- factor(sal_tuv$Transect, levels = c("R", "B", "Mid", "Mon"))


levels_salinity_0.5 <- tide_transect %>% filter(H>=0.5) %>% group_by(Transect) %>% summarise(Level = 0.5, Salinity_predicted = mean(Salinity_predicted))
  
levels_salinity_1 <- tide_transect %>% filter(H>=1) %>% group_by(Transect) %>% summarise(Level = 1, Salinity_predicted = mean(Salinity_predicted))

levels_salinity_1.5 <- tide_transect %>% filter(H>=1.5) %>% group_by(Transect) %>% summarise(Level = 1.5, Salinity_predicted = mean(Salinity_predicted))

levels_salinity_2 <- tide_transect %>% filter(H>=2) %>% group_by(Transect) %>% summarise(Level = 2, Salinity_predicted = mean(Salinity_predicted))



levels_salinity_subtidal <- tide_transect %>% filter(H>=0) %>% group_by(Transect) %>% summarise(Level = "subtidal", Salinity_predicted = mean(Salinity_predicted))



mean_salinity <- tide_transect %>% group_by(Transect) %>% summarise(Salinity_predicted = mean(Salinity_predicted))



ggplot(tide_transect, aes(x = Date3, y = Salinity_predicted)) + 
  geom_line() + 
  facet_wrap(~Transect, nrow = 1) + 
  geom_point(data = sal_tuv, aes(x= Date_true, y = Salinity, color = Depth), size = 2) + 
  geom_hline(data = levels_salinity_0.5, aes(yintercept = Salinity_predicted)) +
  geom_text(data = levels_salinity_0.5, aes(y = Salinity_predicted - 1, x = mean(tide_transect$Date3), label = "+0.5 m")) +
  geom_hline(data = levels_salinity_0.5, aes(yintercept = Salinity_predicted))+
  geom_hline(data = levels_salinity_1, aes(yintercept = Salinity_predicted)) +
  geom_hline(data = levels_salinity_1.5, aes(yintercept = Salinity_predicted)) + 
  geom_hline(data = levels_salinity_2, aes(yintercept = Salinity_predicted)) + 
  geom_hline(data = levels_salinity_subtidal, aes(yintercept = Salinity_predicted)) + 
  geom_text(data = levels_salinity_2, aes(y = Salinity_predicted + 1, x = mean(tide_transect$Date3), label = "+2 m")) +
  theme(axis.text.x = element_text(angle = 90))
  
write.table(tide_transect, "salinity_prediction.csv", sep = ",", row.names = F)
write.table(sal_tuv, "observed_salinity.csv", sep = ",", row.names = F)




ggplot(tide_transect, aes(x = Date3, y = Salinity_predicted)) + 
  geom_line(size = 1) + 
  facet_wrap(~Transect, nrow = 1)+
  theme_bw() + 
  geom_point(data = sal_tuv, aes(x= Date_true, y = Salinity, color = Depth), size = 3) + 
  geom_hline(data = mean_salinity, aes(yintercept = Salinity_predicted)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(color = "Water sampling \ndepth", x = "Date and Time", y = "Salinity") +
  geom_line(data = tide_transect, aes(y = H)) + geom_text(aes(x = mean(Date3), y = -1, label = "Tide level"), size = 4)
  





plot(Sal_pred)


tuv$Min_Salinity_predicted

ggplot(tuv, aes(x = Distance, y = Min_Salinity_predicted)) + geom_point()
ggplot(tuv, aes(x = Distance, y = Min_Salinity_predicted)) + geom_text(aes(label = Depth), position = position_jitter(width = 0))


tide_salinity$Predict_sal <- predict(Mod_tide, newdata = tide_salinity)

ggplot(tide_salinity, aes(x = Date3, y = H, size = Predict_sal)) + geom_point() + geom_hline(yintercept = 1.5)


tuv_print <- tuv %>% select(Sample_ID, Min_Salinity_predicted, Mean_Salinity_predicted)

write.table(tuv_print, "Data/Salinity_predicted.csv", sep = ";", row.names = F)

