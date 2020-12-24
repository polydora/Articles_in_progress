library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(tidyr)


theme_set(theme_bw())

# Калькулятор даты откладки яиц по данным водного теста
df =  read_excel("Data/water_test_calculator.xlsx")

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


str(islands)


islands$Lat <- as.numeric(islands$Lat)

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


## Средение даты начала откладки яиц

Total_median <- median(wtest_coord$DOY, na.rm = T)
as.Date(Total_median, origin = "2020-01-01")





# Отклонение от многолетнего срденего значения 



wtest_coord$Anomalia <- wtest_coord$DOY - Total_median 

nrow(wtest_coord)


wtest_generalized <- wtest_coord %>% group_by(Name) %>% summarise(Anomalia = median(Anomalia, na.rm = T), Sd_DOY = sd(DOY, na.rm = T), DOY = median(DOY, na.rm = T),   Lat = mean(Lat), Long = mean(Long))

nrow(wtest_generalized)



load("Data/Kand_upper_map.RData")

ggmap(Kand_map, darken=0, base_layer=ggplot(aes(x=Long, y=Lat), data=wtest_generalized)) + geom_point(aes(color = Anomalia), size = 4)  + scale_color_gradient(low = "white", high = "red", breaks = c(-5, 0, 5, 10)) + labs(color = "Отклонение \nот многолетнего \nсреднего (дни)")



# Даты начала откладки яиц на островах в разных регионах

  
# Отбираю острова, где всегда были наблюдения

N_eggs_year <- wtest_coord %>% group_by(Name, Year) %>% summarize(N = n()) %>% spread(Year, N,  fill = 0) %>% as.data.frame() %>%  mutate(N_missed = rowSums(.==0), N_obs = (rowSums(.>0)-1) )

island_const <- N_eggs_year$Name[N_eggs_year$N_missed == 0]


wtest_short <- wtest_coord %>% filter(Name %in% island_const)

wtest_short$Region <- factor(wtest_short$Region, levels = c("Северный архипелаг", "Лувеньгский архипелаг", "Незаповедная территория" ))





# Расположение островов, где были постоянные наблюдения


wtest_short_generalized <- wtest_short %>% group_by(Name) %>% summarise(Anomalia = median(Anomalia, na.rm = T), Sd_DOY = sd(DOY, na.rm = T), DOY = median(DOY, na.rm = T),   Lat = mean(Lat), Long = mean(Long))

ggmap(Kand_map, darken=0, base_layer=ggplot(aes(x=Long, y=Lat), data=wtest_short_generalized)) + geom_point(aes(color = Anomalia), size = 4)  + scale_color_gradient(low = "white", high = "red", breaks = c(-5, 0, 5, 10)) + labs(color = "Отклонение \nот многолетнего \nсреднего (дни)")






# Распределине сроков откладки яиц по регионам по материалам константных островов

ggplot(wtest_short, aes(x = factor(Year), y = DOY)) + geom_boxplot(aes(fill = Region)) + facet_wrap(~Region) + guides(fill = "none") + labs(y = "День года", x = "Годы") + geom_hline(yintercept = Total_median, linetype = 2)



