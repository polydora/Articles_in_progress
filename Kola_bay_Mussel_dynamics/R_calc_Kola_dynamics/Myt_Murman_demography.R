# избыточное количество пакетов перекочевало с полной версии кода для статьи
library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)
library(car)
library(nortest)
library(emmeans)
library(multcomp)
library(gratia)
library(vegan)
library(reshape2)
library(broom)
library(ggnewscale)
library(ggpubr)
library(DHARMa)
library(lmPerm)
library(AID)
library(gamm4)

####### Данные по численностям мидий
Ya <- read_excel("Data/Ya_lit_initial_data_ITOG.xlsx")

str(Ya)

unique(Ya$Frame_area)

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

str(Ya_N)

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

# df_lm_filtered %>%
#   filter(term == "(Intercept)") %>% 
#   dplyr::  select(Generation, estimate) %>% 
#   mutate(Anundance_0 = exp(estimate)) -> 
#   recruitment




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

generation_limits$B <- NA

for(i in 1:nrow(generation_limits)){
  years <- generation_limits$min_Year[i]:generation_limits$max_Year[i]
  generation_limits$B[i] <- 
    Ya_B_mean %>% 
    filter(Year %in% years) %>% 
    pull(B) %>% 
    mean()
}


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
  filter(Param_Type == "Tw_mean")

# лето июль-август
# осень сентябрь-октябрь - по совету Малавенды убрала ноябрь в зиму! и зимний ветер исчез)
# зима ноябрь-апрель
# весна май-июнь


# Присоединяю данные по среднесезонной температуре ВОДЫ к описанию условий существования генераций


generation_limits$Winter_T <- NA
generation_limits$Spring_T <- NA
generation_limits$Summer_T <- NA
generation_limits$Autumn_T <- NA
generation_limits$Mean_T <- NA



for(i in 1:nrow(generation_limits)){
  years <- generation_limits$Generation[i]:(generation_limits$Generation[i] + 6)
  generation_limits$Winter_T[i] <- 
    clim_long %>% 
    filter(Year %in% years & Season == "Winter") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Spring_T[i] <- 
    clim_long %>% 
    filter(Year %in% years & Season == "Spring") %>% 
    pull(value) %>% 
    mean(na.rm = T)
  
  generation_limits$Summer_T[i] <- 
    clim_long %>% 
    filter(Year %in% years & Season == "Summer") %>% 
    pull(value) %>% 
    mean(na.rm = T)

  generation_limits$Autumn_T[i] <- 
    clim_long %>% 
    filter(Year %in% years & Season == "Autumn") %>% 
    pull(value) %>% 
    mean(na.rm = T)  
  
  generation_limits$Mean_T[i] <- 
    clim_long %>% 
    filter(Year %in% years) %>% 
    pull(value) %>% 
    mean(na.rm = T)  
  
  
}


generation_limits


generation_mortality <- merge(generation_limits, mortality)

foo_mod <- lm(estimate ~ B + Winter_T +  Summer_T   , data = generation_mortality)

vif(foo_mod)


mod_mortality <- gam(estimate ~ B + s(Winter_T, bs = "cs") +  s(Summer_T, bs = "cs"), data = generation_mortality)

appraise(mod_mortality)

simulateResiduals(mod_mortality, plot = T)

# vif(mod_mortality)


summary(mod_mortality)

draw(mod_mortality, parametric = T) 


#########################################

## Подбираем оптимальную модель методом BioEnv
# переводим обратно в широкий формат, создаём датафрейм предикторов для сезонов
clim_long %>% 
  group_by(Year, Season, Param_Type) %>% 
  summarise(Mean_value = mean(value, na.rm = T)) %>% 
  dcast(Year ~ Param_Type + Season) ->
  predictors

# берём те годы, для которых рассчитано N0 
predictors %>%
  filter(Year %in% c(1995:2014)) ->
  predictors_reduced

# удаляем данные по Кольскому меридиану
predictors_reduced <- 
  predictors_reduced %>% 
  dplyr:: select(-c(Tw_KM_Autumn, Tw_KM_Spring, Tw_KM_Summer, Tw_KM_Winter))

# представляем временной ряд в виде матрицы эвклидовых расстояний 
recruitment_dist <- vegdist(recruitment[,2], method = "euclidean")

# процедура bioenv, которая позволяет подобрать наиболее КОНГРУЭНТНУЮ матрицу предиктов, которая будет максимально подобна матрице эвклидовых расстояний между N0
bioenv_results <- bioenv(recruitment_dist, predictors_reduced[, -1],  metric = "euclidean")

### Визуализация связи N0 с предикторами из оптимальной модели     

recruitment2 <- data.frame(Year = recruitment$Generation, variable = "Recruitment", value = recruitment$Anundance_0)

# рисуем первичные данные
predictors_reduced %>% 
  dplyr::select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring) %>% melt(., id.vars = "Year") %>% rbind(., recruitment2) %>% 
  ggplot(., aes(x = Year, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") 

# рисуем зависимоть значений logNo от температуры воды весной
predictors_reduced %>% 
  dplyr::select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Tw_mean_Spring, y = log(Recruitment))) +
  geom_text(aes(label = Year)) +
  geom_smooth(method = "lm")

# рисуем зависимоть значений logNo от температуры воздуха весной
predictors_reduced %>% 
  dplyr::select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Ta_mean_Spring, y = log(Recruitment))) +
  geom_text(aes(label = Year)) +
  geom_smooth(method = "lm")

# рисуем зависимоть значений logNo от скорости ветра весной
predictors_reduced %>% 
  dplyr::select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Wind_mean_Spring, y = log(Recruitment))) +
  geom_text(aes(label = Year))+
  geom_smooth(method = "lm")

# # оцениваем значимость мантеловской корреляции
perm_binv <- c(1:1000)
varespec <- recruitment[,2]
varechem <- predictors_reduced[,-1]

for (i in 1:999)
{
  perm_num <- sample(1:nrow(varespec))
  perm_com <- varespec[perm_num,]
  perm_i <- bioenv(perm_com, varechem)

  manteltop <- length(perm_i$models)
  est <- c(1:manteltop)
  for (j in 1:ncol(varechem)) est[j] <- perm_i$models[[j]]$est
  perm_binv[i] <- max(est)
  cat("interation", i, "\n")
}

# кладём результаты исходного анализа в переменную BioEnv
BioEnv <- bioenv_results

manteltop <- length(BioEnv$models)

estim <- c(1:manteltop)
for (j in 1:ncol(varechem)) estim[j] <- BioEnv$models[[j]]$est
perm_binv[1000] <- max(estim)

perm_binv <- data.frame(perm_i = perm_binv)

p = length(perm_binv[perm_binv[,1] >= perm_binv[1000,1],1]) /
  nrow(perm_binv)

# пи-хакинг
# делаю датафрейм, содержащий N0 и предикторы
all_data_lm <- merge(predictors_reduced, recruitment2)

# делаем полную модель
lm <- lm(log(value) ~ Wind_mean_Spring + Tw_mean_Spring + Ta_mean_Spring, data = all_data_lm)
summary(lm)

vif(lm)

# пермутационная оценка модели (F-критерия) "вручную"
perm_lm <- c(1:10000)

for (i in 1:9999)
{
  value_perm <- all_data_lm$value[sample(1:nrow(all_data_lm))] 
  mod_perm <- lm(log(value_perm) ~ Tw_mean_Spring + Ta_mean_Spring + Wind_mean_Spring, data = all_data_lm)
  perm_lm[i] <- summary(mod_perm)$fstatistic[1] 
  
}

mod_real <- lm(log(value) ~ Tw_mean_Spring + Ta_mean_Spring + Wind_mean_Spring, data = all_data_lm)

perm_lm[10000] <- summary(mod_real)$fstatistic[1] 

mean(perm_lm >= perm_lm[10000])
