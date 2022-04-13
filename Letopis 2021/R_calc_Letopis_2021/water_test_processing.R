library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggmap)
library(tidyr)
library(reshape2)


theme_set(theme_bw())

# Калькулятор даты откладки яиц по данным водного теста
df =  read_excel("data/water_test_calculator.xlsx")

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

wtest <- read_excel("Data/water_test_data_base_2014_2021.xlsx", na = "NA")


# Приведение к стандарту данных 2021 года от Н. С. Бойко

# boy <- read_excel("Data/Бойко_водный_тест_гага_2021.xlsx", na = "NA", col_names = T)
# 
# boy2 <- melt(boy, id.vars = c("ID", "Species", "Region",   "Name",  "Date", "N_eggs"))
# 
# boy2 <- boy2 %>% filter(!is.na(value))
# 
# boy2$variable <- as.character(boy2$variable)
# 
# 
# boy2$variable[startsWith(boy2$variable, "Angle")] <- "Angle"
# boy2$variable[startsWith(boy2$variable, "Diam")] <- "Diametr"
# unique(boy2$variable)
# 
# 
# boy2$value[boy2$variable == "Pierced"] <- NA
# 
# names(boy2)[7] <- "Type"
# names(boy2)[8] <- "Value"
# 
# 
# boy2 <- boy2 %>%  select(ID,	Species,	Date,	Region,	Name,	N_eggs,		Value,	Type)
# 
# 
# boy2 <- boy2 %>% mutate(W_test = case_when(Type == "Angle" ~ paste("A_", Value, sep = ""),
#                                            Type == "Diametr" ~ paste("D_", Value, sep = "")))
# 
# boy2 <- boy2 %>%  select(ID,	Species,	Date,	Region,	Name,	N_eggs, W_test,		Value,	Type)
# 
# write.csv(boy2, "Boyko_2021_long.csv",row.names = F, fileEncoding = "UTF-8")




# Вычисление дня откладки яйца

wtest$Day <- as.numeric(wt_calculator(wtest))



tail(wtest)


# Проверка соответствия названий данным по координатам

islands <- read_excel("Data/Kandalaksha_Bay_Islands_2021.xlsx")


str(islands)


islands$Lat <- as.numeric(islands$Lat)
islands$Long <- as.numeric(islands$Long)



wtest %>% filter(!(wtest$Name %in% islands$Name)) %>% select(Name) %>% unique(.)

# Проверка соответствия названий базе данных по островам

islands_db <- read_excel("Data/Islands_area.xlsx")

wtest %>% filter(!(wtest$Name %in% islands_db$Name)) %>% select(Name) %>% unique(.)


# Комбинирую данные по водному тесту и координатам

wtest_coord <- merge(wtest,islands[, c("Name", "Lat", "Long")], all.x = TRUE )




wtest_coord$Date_begin <- as.POSIXct(as.numeric(wtest_coord$Date) - wtest_coord$Day*24*60*60, origin = "1970-01-01 00:00.00 UTC" )

wtest_coord$DOY <- as.numeric(strftime(wtest_coord$Date_begin, format = "%j"))

wtest_coord$Year <- year(wtest_coord$Date) 





ggplot(wtest_coord, aes(x = Long, y = Lat)) + geom_point()




wtest_coord <- merge(wtest_coord, islands_db)

wtest$Area <- as.numeric(wtest$Area) 

str(wtest_coord)

wtest_coord$Area <- as.numeric(wtest_coord$Area)

#Вычисление даты начала кладки

ggplot(wtest_coord, aes(x = log(Area), y = DOY)) + geom_point()  + geom_smooth(method = "lm")





ggplot(wtest_coord, aes(x= Year, y = DOY, group = Year)) + geom_boxplot()


wtest_coord_21 <- wtest_coord %>% filter(Year == 2021)

wtest_coord_21_mean <- wtest_coord_21 %>% group_by(Name) %>% summarise(med_DOY = median(DOY), Lat = mean(Lat), Long = mean(Long))


load("Data/Kand_upper_map.RData")

ggmap(Kand_map, darken=0, base_layer=ggplot(aes(x=Long, y=Lat), data=wtest_coord_21_mean)) + geom_point(aes(color = med_DOY), size = 4)  + scale_color_gradient(low = "white", high = "red", breaks = c(-5, 0, 5, 10)) 











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



