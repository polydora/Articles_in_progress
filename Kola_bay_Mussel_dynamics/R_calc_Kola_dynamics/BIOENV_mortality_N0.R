# избыточное количество пакетов перекочевало с полной версии кода для статьи
library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)
library(car)
library(gratia)
library(reshape2)
library(broom)
library(ggpubr)
library(DHARMa)

####### Данные по численностям мидий
Ya <- read_excel("Data/Ya_lit_initial_data_ITOG.xlsx")

# делаем матрицу с количеством изученных мидий
Ya_count <- dcast(Year + Sample + Part_studied + Frame_area  ~ Age, data = Ya)

# делаем матрицу численностей на квадратный метр
Ya_count[ , 5 : ncol(Ya_count)] <- Ya_count[ , 5 : ncol(Ya_count)] / Ya_count$Part_studied / Ya_count$Frame_area

Ya_count <-
  Ya_count %>%
  group_by(Year, Sample) %>%
  summarise_at(vars(- c(Part_studied,  Frame_area)),.funs = sum)

# возвращаемся к "длинному" построчному формату
Ya_N <- melt(Ya_count, id.vars = c("Year", "Sample"), variable.name = "Age", value.name = "N") 

# добавляем столбик с генерацией
Ya_N <- 
  Ya_N %>% 
  mutate(Generation = Year - as.numeric(Age))

# считаем среднюю численность генерации для каждого года
Ya_gen <- 
  Ya_N %>% 
  group_by(Year, Generation, Age) %>% 
  summarise(mean_N = mean(N, na.rm = TRUE))
# write.table("clipboard", sep = "\t")

# считаем среднюю численность вообще
Ya_N_sum <- 
  Ya_N %>% 
  group_by(Year, Sample) %>% 
  summarise(sum_N = sum(N, na.rm = TRUE)) 

Ya_N_mean <- 
  Ya_N_sum %>% 
  group_by(Year) %>% 
  summarise(mean_N = mean(sum_N, na.rm = TRUE))
# write.table("clipboard", sep = "\t")

Ya_gen %>% 
  filter(mean_N > 0) %>%
  filter(Generation >= 1995 & Generation <= 2014) %>% 
  mutate(Age = as.numeric(Age)) -> df


df_sample_size <- 
  df %>%  
  group_by(Generation) %>% 
  summarise(N = n())

df %>% 
  merge(., df_sample_size) %>% 
  group_by(Generation) %>% 
  filter(Age > 1) %>% #
  filter(N > 3) %>%
  filter(Generation %in% c(1995:2014)) %>% 
  group_modify( ~ tidy(lm(log(mean_N) ~ Age, data = .))) -> df_lm_filtered 

df_lm_filtered %>%
  filter(term == "(Intercept)") %>%
  dplyr::  select(Generation, estimate) %>%
  mutate(Anundance_0 = exp(estimate)) ->
  N0


# Вытаскиваем данные по угловым коэфициентам
df_lm_filtered %>%
  filter(term == "Age") %>% 
  dplyr::  select(Generation, estimate) -> 
  mortality

# В какие годы встречались особи каждой  генерации

df %>% 
  group_by(Generation) %>% 
  summarise(min_Year = min(Year), max_Year = max(Year)) ->
  generation_limits

# Показатели обилия популяции мидий в годы, когда жила соотвествующиая генерация

Ya_B_mean <- read_excel("Data/Myt_YaL_B.xlsx")

# Этот код можно использовать если брать показатели за те годы, когда генерация была описана

#######################################
# generation_limits$B <- NA
# 
# for(i in 1:nrow(generation_limits)){
#   years <- generation_limits$min_Year[i]:generation_limits$max_Year[i]
#   generation_limits$B[i] <- 
#     Ya_B_mean %>% 
#     filter(Year %in% years) %>% 
#     pull(B) %>% 
#     mean()
# }
########################################


###### Данные по климатическим параметрам
clim <- read_excel("Data/climate_Murman.xlsx", sheet = "data_transposed", na = "NA")

# переводим в "длинный" формат 
clim_long <- 
  melt(clim, id.vars = c("Param",	"Param_Type",	"Month"), variable.name = "Year") %>% 
  filter(complete.cases(.)) %>% 
  mutate(Year = as.numeric(as.character(Year)))

# убираем солёность, тк она лажа
clim_long <- 
  clim_long %>% 
  filter(Param_Type != "Sal_mean")

# выделяем сезоны, данные по ноябрю и декабрю взяты за предыдущий календ. год, они закодированы как -11 и -12 
clim_long <- 
  clim_long %>% 
  mutate(Season = case_when(Month %in% 7:8 ~ "Summer",
                            Month %in% 9:10 ~ "Autumn",
                            Month %in% c(-11,-12, 1:4) ~ "Winter",
                            Month %in% 5:6 ~ "Spring")) %>% 
  filter(complete.cases(.))

clim_long <- 
  clim_long %>% 
  filter(Param_Type %in% c("Tw_mean", "Ta_mean", "Wind_mean", "Waves_mean"))

# лето июль-август
# осень сентябрь-октябрь - по совету Малавенды убрала ноябрь в зиму! и зимний ветер исчез)
# зима ноябрь-апрель
# весна май-июнь


# Присоединяю данные по среднесезонной температуре ВОДЫ к описанию условий существования генераций

generation_limits$Winter_Tw_begin <- NA
generation_limits$Spring_Tw_begin <- NA
generation_limits$Summer_Tw_begin <- NA
generation_limits$Autumn_Tw_begin <- NA

generation_limits$Winter_Ta_begin <- NA
generation_limits$Spring_Ta_begin <- NA
generation_limits$Summer_Ta_begin <- NA
generation_limits$Autumn_Ta_begin <- NA

generation_limits$Winter_Waves_begin <- NA
generation_limits$Spring_Waves_begin <- NA
generation_limits$Summer_Waves_begin <- NA
generation_limits$Autumn_Waves_begin <- NA

generation_limits$Winter_Wind_begin <- NA
generation_limits$Spring_Wind_begin <- NA
generation_limits$Summer_Wind_begin <- NA
generation_limits$Autumn_Wind_begin <- NA


generation_limits$Winter_Tw_lifespan <- NA
generation_limits$Spring_Tw_lifespan <- NA
generation_limits$Summer_Tw_lifespan <- NA
generation_limits$Autumn_Tw_lifespan <- NA

generation_limits$Winter_Ta_lifespan <- NA
generation_limits$Spring_Ta_lifespan <- NA
generation_limits$Summer_Ta_lifespan <- NA
generation_limits$Autumn_Ta_lifespan <- NA


# generation_limits$Mean_T <- NA

generation_limits$N_lifespan <- NA
generation_limits$B_lifespan <- NA


for(i in 1:nrow(generation_limits)){

  # Парамтеры для года формирования генерации 
  
  years <- generation_limits$Generation[i]
  
  generation_limits$Winter_Tw_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean" & Year %in% years & Season == "Winter") %>% 
    pull(value) %>% 
    mean
  
  
  generation_limits$Spring_Tw_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean" & Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Summer_Tw_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean" & Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Autumn_Tw_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean" & Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean
  
###############################
  generation_limits$Winter_Ta_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean" & Year %in% years & Season == "Winter") %>% 
    pull(value) %>% 
    mean
  
  
  generation_limits$Spring_Ta_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean" & Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Summer_Ta_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean" & Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Autumn_Ta_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean" & Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean
  
############################


  generation_limits$Winter_Wind_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Wind_mean" & Year %in% years & Season == "Winter") %>% 
    pull(value) %>% 
    mean
  
  
  generation_limits$Spring_Wind_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Wind_mean" & Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Summer_Wind_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Wind_mean" & Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Autumn_Wind_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Wind_mean" & Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean
  
###############################  

  
  generation_limits$Winter_Waves_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Waves_mean" & Year %in% years & Season == "Winter") %>% 
    pull(value) %>% 
    mean
  
  
  generation_limits$Spring_Waves_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Waves_mean" & Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Summer_Waves_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Waves_mean" & Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean
  
  generation_limits$Autumn_Waves_begin[i] <- 
    clim_long %>% 
    filter(Param_Type == "Waves_mean" & Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean
  
  ###############################  
  
  
    
  # years <- generation_limits$min_Year[i]:generation_limits$max_Year[i]
  
  years <- generation_limits$Generation[i]:generation_limits$max_Year[i] 
  
  generation_limits$B_lifespan[i] <-
    Ya_B_mean %>%
    filter(Year %in% years) %>%
    pull(B) %>%
    mean()
  
  
  generation_limits$N_lifespan[i] <- 
    Ya_N_mean %>% 
    filter(Year %in% years) %>% 
    pull(mean_N) %>% 
    mean(na.rm = T)
  
  # Вот это самый тонкий момент. Если считать погоду только в те годы, когда описана генерация, то все получается через жопу
  
  # years <- generation_limits$Generation[i]:(generation_limits$Generation[i] + 5)
  
  # years <- generation_limits$min_Year[i]:generation_limits$max_Year[i]
  
  # years <- generation_limits$Generation[i]:generation_limits$max_Year[i]   
  
#######################################
    generation_limits$Winter_Tw_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean", Year %in% years &  Season == "Winter") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Spring_Tw_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean", Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Summer_Tw_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean", Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Autumn_Tw_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Tw_mean", Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean(na.rm = T)  
#######################################

  generation_limits$Winter_Ta_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean", Year %in% years &  Season == "Winter") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Spring_Ta_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean", Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Summer_Ta_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean", Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Autumn_Ta_lifespan[i] <- 
    clim_long %>% 
    filter(Param_Type == "Ta_mean", Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean(na.rm = T)  
  #######################################
  
    
  # 
  # generation_limits$Mean_T[i] <- 
  #   clim_long %>% 
  #   filter(Year %in% years) %>% 
  #   pull(value) %>% 
  #   mean(na.rm = T)  
  # 
  
}

