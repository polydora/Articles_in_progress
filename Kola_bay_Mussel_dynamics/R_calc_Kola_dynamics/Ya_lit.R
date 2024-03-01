library(readxl)
library(dplyr)
library(reshape2)
library(broom)
library(ggplot2)


Ya <- read_excel("Data/Ya_lit_initial_data.xlsx")

str(Ya)

unique(Ya$Frame_area)


# делаем матрицу с количеством изученных мидий
Ya_count <- dcast(Year + Sample + Part_studied + Frame_area + Include  ~ Age, data = Ya)

# делаем матрицу численностей на квадратный метр
Ya_count[ , 6 : ncol(Ya_count)] <- Ya_count[ , 6 : ncol(Ya_count)] / Ya_count$Part_studied / Ya_count$Frame_area

# убираем мидий из 2002 года
Ya_count <- Ya_count %>% 
  filter(Include == 1)

Ya_count[Ya_count$Year == 2002 & Ya_count$Sample %in% c(2, 4), 6 : 8] <- NA

Ya_count[Ya_count$Year == 2002 & Ya_count$Sample %in% c(3, 5), 10 : 12] <- NA

Ya_count <- 
Ya_count %>% 
  group_by(Year, Sample) %>% 
  summarise_at(vars(- c(Part_studied,  Frame_area, Include)),.funs = sum)

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


  
# round(dcast(Generation ~ Year, data = Ya_gen))
  


Ya_gen %>% 
  filter(mean_N > 0) %>%
  filter(Generation >= 1995 & Generation <= 2014) %>% 
  mutate(Age = as.numeric(Age)) -> df


# 
# df %>% 
#   filter(Generation == 2014)
# 


df_sample_size <- 
  df %>%  
  group_by(Generation) %>% 
  summarise(N = n())


  df %>% 
  merge(., df_sample_size) %>% 
  group_by(Generation) %>% 
  filter(Age>1) %>% #
  filter(N > 3) %>% 
  group_modify( ~ tidy(lm(log(mean_N + 1) ~ Age, data = .))) -> df_lm_filtered
  
  
  df_lm_filtered %>%
    filter(term == "(Intercept)") %>% 
    select(Generation, estimate) %>% 
    mutate(Anundance_0 = exp(estimate)) -> 
    recruitment

# добавляем возрастную структуру
Ya_count %>% 
  group_by(Year) %>% 
  summarise_at(vars(- c(Sample)),.funs = function(x)mean(x, na.rm = TRUE)) ->
  Age_str

# делаем датафрейм с биомассой 
YaB <- read_excel("Data/Ya_lit_initial_data_B.xlsx")
  
YaB %>% 
  select(Year, Biomass) %>% 
  group_by(Year) %>% 
  summarise(Biomass=mean(Biomass))->
  Biomass

# для merge переименовывыем generation в Year и убираем ln
recruitment %>% 
  select(-estimate) %>% 
  rename(Year=Generation) ->
  No
  
Myt_pop <- merge(Age_str, No, all.x = TRUE)

Myt_pop2 <- merge(Myt_pop, Biomass, all.x = TRUE)

# делаем ординацию





###### Данные по климатическим параметрам
clim <- read_excel("Data/climate_Murman.xlsx", sheet = "data_transposed", na = "NA")

# переводим в "длинный! формат 
clim_long <- 
  melt(clim, id.vars = c("Param",	"Param_Type",	"Month"), variable.name = "Year") %>% 
  filter(complete.cases(.)) %>% 
  mutate(Year = as.numeric(as.character(Year)))

# убираем солёность, тк она лажа
clim_long <- 
  clim_long %>% 
  filter(Param_Type != "Sal_mean")

# выделяем сезоны, данные по декабрю взяты за предыдущий календ. год, они закодированы как -12 
clim_long <- 
  clim_long %>% 
  mutate(Season = case_when(Month %in% 7:8 ~ "Summer",
                            Month %in% 9:11 ~ "Autumn",
                            Month %in% c(-12, 1:4) ~ "Winter",
                            Month %in% 5:6 ~ "Spring")) %>% 
  filter(complete.cases(.))

# лето июль-август
# Осень сентябрь-ноябрь
# зима декабрь-апрель
# весна май-июнь



# Динамика среднегоовых значений параметров - график
clim_long %>% 
  group_by(Year, Param_Type) %>% 
  summarise(Mean_value = mean(value, na.rm = T)) %>% 
  ggplot(., aes(x= Year, y =  Mean_value)) +
  geom_line() +
  facet_wrap(~Param_Type, scales = "free_y")

  
## Подбираем оптимальную модель методом BioEnv

library(vegan)

# переводим обратно в широкий формат, создаём датафрейм предикторов
clim_long %>% 
  group_by(Year, Season, Param_Type) %>% 
  summarise(Mean_value = mean(value, na.rm = T)) %>% 
  dcast(Year ~ Param_Type + Season) ->
  predictors

# берём те годы, для которых рассчитано N0 
predictors %>% 
  filter(Year %in% Myt_pop2$Year) ->
  predictors_reduced

# удаляем данные по Кольскому меридиану
predictors_reduced <- 
  predictors_reduced %>% 
  select(-c(Tw_KM_Autumn, Tw_KM_Spring, Tw_KM_Summer, Tw_KM_Winter))

Myt_pop3 <- Myt_pop2 %>% select(-Anundance_0)

# Myt_pop3 <- Myt_pop2 %>% filter(complete.cases(.))


# рисуем популяционную ординацию

clim_pca <- rda(predictors_reduced[, -1])

summary(clim_pca)

plot(clim_pca)

PC_scores <- as.data.frame(scores(clim_pca)$sites)



pop_ord <- cca(Myt_pop3[,-1] ~ ., data = PC_scores)

plot(pop_ord)

anova(pop_ord)

env_fit <- envfit(pop_ord ~ ., data = PC_scores)

plot(env_fit)



# представляем временной ряд в виде матрицы эвклидовых расстояний 
recruitment_dist <- vegdist(recruitment[,2], method = "euclidean")

# процедура bioenv, которая позволяет подобрать наиболее КОНГРУЭНТНУЮ матрицу предиктов, которая будет максимально подобна матрице эвклидовых расстояний между N0
bioenv_results <- bioenv(recruitment_dist, predictors_reduced[, -1],  metric = "euclidean")



### Визуализация связи с предикторами из оптимальной модели     

recruitment2 <- data.frame(Year = recruitment$Generation, variable = "Recruitment", value = recruitment$Anundance_0)

# рисуем первичные данные
predictors_reduced %>% 
  select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>% rbind(., recruitment2) %>% 
  ggplot(., aes(x = Year, y = value)) +
  geom_line() +
  facet_wrap(~variable, scales = "free_y") 

# рисуем зависимоть значений logNo от температуры воды весной
predictors_reduced %>% 
  select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Tw_mean_Spring, y = log(Recruitment))) +
  geom_text(aes(label = Year))

# рисуем зависимоть значений logNo от температуры воздуха весной
predictors_reduced %>% 
  select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Ta_mean_Spring, y = log(Recruitment))) +
  geom_text(aes(label = Year))

# рисуем зависимоть значений logNo от скорости ветра весной
predictors_reduced %>% 
  select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Wind_mean_Spring, y = log(Recruitment))) +
  geom_text(aes(label = Year))

# рисуем зависимоть значений logNo от скорости ветра зимой
predictors_reduced %>% 
  select(Year, Ta_mean_Spring, Tw_mean_Spring, Wind_mean_Spring, Wind_mean_Winter) %>% melt(., id.vars = "Year") %>%
  rbind(., recruitment2) %>% 
  dcast(Year ~ variable) %>%  
  ggplot(., aes(x = Wind_mean_Winter, y = log(Recruitment))) +
  geom_text(aes(label = Year))

# оцениваем значимость мантеловской корреляции
perm_binv <- c(1:100)
varespec <- recruitment[,2]
varechem <- predictors_reduced[,-1]

for (i in 1:99)
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
perm_binv[100] <- max(estim)

perm_binv <- data.frame(perm_i = perm_binv)

p = length(perm_binv[perm_binv[,1] >= perm_binv[100,1],1]) /
  nrow(perm_binv)



