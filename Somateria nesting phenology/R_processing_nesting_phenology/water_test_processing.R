library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)


# Калькулятор даты откладки яиц по данным водного теста

df = wtest

wt_calculator <- function(df){
  require(readxl)
  require(dplyr)
  wt_calc <- read_excel("Data/water_test_calculator.xlsx")
  wt_calc$Value <- as.numeric(wt_calc$Value)
  wt_calc_angle <- wt_calc %>% filter(Type == "Angle")
  wt_calc_diam <- wt_calc %>% filter(Type == "Diametr" & Value !=0)
  Mod_angle <- lm(Day ~ Value, data = wt_calc_angle)
  Mod_diam <- lm(Day ~ Value, data =  wt_calc_diam)
  df <- 
  df %>% mutate(Day = case_when(
    Type == "Angle" ~ round(predict(Mod_angle, newdata = df)),
    Type == "Diametr" & Value <=5 ~ 11,
    Type == "Diametr" & Value > 5 ~ round(predict(Mod_diam, newdata = df)),
    Type == "Pierced" ~ 25))
  df$Day
}




###

wtest <- read_excel("Data/water_test_data_base.xlsx", na = "NA")


# Вычисление дня откладки яйца

wtest$Day <- as.numeric(wt_calculator(wtest))

# Проверка соответствия названий данным по координатам

islands <- read_excel("Data/Kandalaksha_Bay_Islands.xlsx")

wtest %>% filter(!(wtest$Name %in% islands$Name)) %>% select(Name) %>% unique(.)

# Проверка соответствия названий базе данных по островам

islands_db <- read_excel("Data/Islands_data_base.xlsx")

wtest %>% filter(!(wtest$Name %in% islands_db$Name)) %>% select(Name) %>% unique(.)


# Комбинирую данные по водному тесту и координатам

wtest_coord <- merge(wtest,islands[, c("Name", "Lat", "Long")], all.x = TRUE )


ggplot(wtest_coord, aes(x = Long, y = Lat)) + geom_point()




area <- islands_db %>% filter(!is.na(islands_db$Area))

area$Area2 <- area$Area 
area$Area2[area$Area2 == "< 0,1"] <- 0.05
area$Area2 <- as.numeric(area$Area2)


#Вычисление даты начала кладки


wtest_coord$Date_begin <- as.POSIXct(as.numeric(wtest_coord$Date) - wtest_coord$Day*24*60*60, origin = "1970-01-01 00:00.00 UTC" )

wtest_coord$DOY <- as.numeric(strftime(wtest_coord$Date_begin, format = "%j"))

wtest_coord$Year <- year(wtest_coord$Date) 


# Отклонение от многолетнего срденего значения 

Total_median <- median(wtest_coord$DOY, na.rm = T)

wtest_coord$Anomalia <- wtest_coord$DOY - Total_median 


wtest_generalized <- wtest_coord %>% group_by(Name) %>% summarise(Anomalia = median(Anomalia, na.rm = T), Sd_DOY = sd(DOY, na.rm = T), DOY = median(DOY, na.rm = T),   Lat = mean(Lat), Long = mean(Long))





load("Data/Kand_upper_map.RData")

ggmap(Kand_map, darken=0, base_layer=ggplot(aes(x=Long, y=Lat), data=wtest_generalized)) + geom_point(aes(color = Anomalia), size = 4)  + scale_color_gradient(low = "white", high = "red") + labs(color = "Отклонение \nот многолетнего \nсреднего (дни)")




