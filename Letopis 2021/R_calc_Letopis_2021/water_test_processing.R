library(reshape2)
library(knitr)
library(pander)
library(readxl)
library(dplyr)
library(ggplot2)
library(cowplot)
library(lubridate)
library(ggmap)
library(stringr)






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

wtest <- read_excel("Data/water_test_data_base_2014_2021.xlsx", na = "NA")


# Вычисление дня откладки яйца

wtest$Day <- as.numeric(wt_calculator(wtest))

wtest$Day[wtest$Type %in% c("Pierced","Hatching" )] <- 45

wtest <- wtest %>% mutate(Year = year(wtest$Date))

# wtest <-  wtest %>% filter(Year == 2021)  


# Проверка соответствия названий данным по координатам

islands <- read_excel("Data/Kandalaksha_Bay_Islands_2021.xlsx", na = "NA")
islands$Lat <- as.numeric(islands$Lat)
islands$Long <- as.numeric(islands$Long)



islands_area <- read_excel("Data/Kandalaksha_islands_area.xlsx")


islands_coord <- islands %>% select(Name, Lat, Long)

islands_coord <- islands_coord[order(islands_coord$Name), ]


islands_coord <- merge(islands_coord, islands_area) 

islands_coord <- islands_coord %>% filter(Name %in% unique(wtest$Name))

wtest_coord <- merge(wtest,islands_coord[, c("Name", "Lat", "Long", "Area_Ga")], all.x = TRUE )

wtest_coord$Date_begin <- as.POSIXct(as.numeric(wtest_coord$Date) - wtest_coord$Day*24*60*60, origin = "1970-01-01 00:00.00 UTC")


wtest_coord$Date_begin <- date(wtest_coord$Date_begin)


wtest_coord$DOY <- as.numeric(strftime(wtest_coord$Date_begin, format = "%j"))

wtest_coord$Year <- year(wtest_coord$Date) 


Total_median <- median(wtest_coord$DOY, na.rm = T)
# as.Date(Total_median, origin = "2020-01-01")
Total_mean <- mean(wtest_coord$DOY, na.rm = T)


# Отклонение от многолетнего срденего значения 

wtest_coord$Anomalia <- wtest_coord$DOY - Total_median 



wtest_generalized <- wtest_coord %>% group_by(Name, Year) %>% summarise(Anomalia = mean(Anomalia, na.rm = T), Sd_DOY = sd(DOY, na.rm = T), DOY = mean(DOY, na.rm = T), N = n(), N_nest = length(unique(ID)),  Lat = mean(Lat), Long = mean(Long), Area_Ga = mean(Area_Ga))



hist(wtest_generalized$N_nest)



wtest_generalized %>% arrange(desc(N_nest))

wtest_coord <- merge(wtest_coord, wtest_generalized %>% select(Name, N_nest, N))



# 
# wtest_generalized2 <- wtest_coord %>% group_by(Name, Year) %>% summarise(Anomalia = median(Anomalia, na.rm = T), Sd_DOY = sd(DOY, na.rm = T), DOY = median(DOY, na.rm = T), N = n(), N_nest = length(unique(ID)),  Lat = mean(Lat), Long = mean(Long), Area_Ga = mean(Area_Ga))
# 





samp <- data.frame(n_samp = NULL, Year = NULL, Name = NULL, ID = NULL, DOY = NULL)
i<-0
sample_size <- c(10, 20, 30, 40, 50, 60)

# 
# year <- 2019
# name <- "Натальина Луда"

for(j in 1:1) for(samp_siz in sample_size) for(year in unique(wtest_coord$Year)){
  wt <- wtest_coord %>% filter(Year == year)
  for(name in unique(wt$Name)){
    wt2 <- wt %>% filter(Name == name)
    if(nrow(wt2) > samp_siz){
      nests <- wt2 %>% filter(ID %in% sample(.$ID, samp_siz))
      df <- nests %>% select(Year, Name, ID, DOY)
      df$n_samp = samp_siz
      samp <- rbind(samp, df)
      i <- i +1
      print(i)
      
    }
    
  }
  print(year)
}


sam_generalized <- samp %>% group_by(n_samp, Year, Name) %>% summarise(Samp_DOY = mean(DOY, na.rm = T), Samp_Sd_DOY = sd(DOY, na.rm = T))


samp_wtest <- merge(sam_generalized, wtest_generalized, all.x = T)

samp_wtest$Dif_DOY <- samp_wtest$Samp_DOY - samp_wtest$DOY

ggplot(samp_wtest, aes(x = Dif_DOY)) + geom_histogram() + facet_wrap(~n_samp)


nests <- wtest_coord %>% group_by(Year, Name, ID) %>%  summarise(Mean_DOY = mean(DOY, na.rm = T))

CI <- as.vector(t.test(nests$Mean_DOY)$"conf.int")


ggplot(samp_wtest, aes(x = n_samp, y = Samp_DOY)) + geom_point(position = position_jitter(width = 0.1)) + geom_smooth(method = "loess") +  geom_hline(yintercept = CI, linetype = 2)






nests <- wtest_coord %>% group_by(Year, Name, ID) %>%  summarise(Mean_DOY = mean(DOY, na.rm = T))

nrow(nests)

Mean_DOY <- mean(nests$Mean_DOY, na.rm = T)
Sd_DOY <- sd(nests$Mean_DOY, na.rm = T)

CI <- as.vector(t.test(nests$Mean_DOY)$"conf.int")

CI_width <- CI[2]-CI[1]



install.packages("presize")
library(presize)

prec_mean(mean = Mean_DOY, sd = Sd_DOY, conf.width = CI_width)  

prec_mean(mean = Mean_DOY, sd = Sd_DOY, n = 40)  



