# В этом скрипте анализируются выборки мидий из Кольского залива

library(readxl)
library(vegan)
library(dplyr)
library(tidyr)
library(writexl)



myt <- read_excel("Data/BTN_ecology_KolaBay_sizeClasses_030926.xlsx")

myt <-
  myt %>% 
  mutate(Size_class = case_when(Length <= 5 ~ "L3",
                                Length > 5 & Length <= 10 ~ "L8",
                                Length > 10 & Length <= 15 ~ "L13",
                                Length > 15 & Length <= 20 ~ "L18",
                                Length > 20 & Length <= 25 ~ "L23",
                                Length > 25 & Length <= 30 ~ "L28",
                                Length > 30 & Length <= 35 ~ "L33",
                                Length > 35 & Length <= 40 ~ "L38",
                                Length > 40 ~ "L43"))




myt %>% 
  group_by(Year,Date, Site_code, Latitude, Longitude, Sample, Frame_area,Salinity, Depth, PT, Prevalence_BTN, Size_class) %>% 
  summarise(N = n()) ->
  site_abundance

site_abundance <- 
  site_abundance %>% 
  mutate(Abund = N/Frame_area)

site_abundance %>% 
  group_by(Year, Date, Site_code, Latitude, Longitude, Salinity, Depth, PT, Prevalence_BTN, Size_class) %>% 
  summarise(Mean_Abund = mean(Abund), .groups = "drop") %>%  
  pivot_wider(
    id_cols = c(Year, Date, Site_code, Latitude, Longitude, Salinity, Depth, PT, Prevalence_BTN),
    names_from = Size_class,
    values_from = Mean_Abund,
    values_fill = 0
  ) ->
  site_Size_class_abundance 

scam <- 
  site_Size_class_abundance %>% 
  select(-c(1:9))

## PCA

log_scam <- decostand(scam, method = "log")
  
pca_result <- rda(log_scam, scale = TRUE)



PC_scores <- as.data.frame(scores(pca_result)$sites)

site_Size_class_abundance <- cbind(site_Size_class_abundance, PC_scores) 


# Интерпретация PC

plot(pca_result, display =  "species")

# PC1 - отражает просто обилие. Чем больше PC1 тем меньше обилие. Те сайты, где PC1 отрицательная там высокое обилие, те сайты, где PC1 положительная - низкое обилие. Возможно, надо в PCA включить биомассу!

# PC2 - отражает соотношение молодых и старых мидий максимальное значение PC2 - доминирует молодь, минимальное занчение - доминируют старые


site_Size_class_abundance %>% 
  ggplot(aes(x = PC2, y = L3)) +
  geom_point() 


site_Size_class_abundance %>% 
  ggplot(aes(x = PC2, y = L8)) +
  geom_point() 


site_Size_class_abundance %>% 
  ggplot(aes(x = PC2, y = L38)) +
  geom_point() 






# Визуализация размерной структуры. Графики упорядочены в соотвтетствии с PC2 (от самых старперских к самым молодым)

site_order <- site_Size_class_abundance %>%
  select(Site_code, PC2) %>%
  distinct() %>%  # убираем дубликаты, если они есть
  arrange(PC2) %>%  
  pull(Site_code)   

myt$Site_code <- factor(myt$Site_code, levels = site_order)


myt %>% 
  ggplot(aes(Length)) +
  geom_histogram() +
  facet_wrap(~Site_code, ncol = 4, dir = "v", scales = "free_y")



# Size class abundance and PCA results

write_xlsx(x = site_Size_class_abundance, path = "Data/Kola_site_size_structure.xlsx")




# Модель связи BTN с предикторами НО! без расстояния до Белокаменки 

myt_btn <- read_excel("Data/sites_info_KolaBay_2026.xlsx")

myt_btn <-
myt_btn %>% 
  mutate(Fi_BTN_prevalrnce = 2*asin(sqrt(Prevalence_BTN))*180/pi)

library(mgcv)
library(gratia)


mod <- gam(Fi_BTN_prevalrnce ~ s(PT, bs = "cs", k = 5) + s(Depth, bs = "cs", k = 5) + s(PC1, bs = "cs", k = 5) + s(PC2, bs = "cs", k = 5) + s(Fetch, bs = "cs", k = 5), data = myt_btn)

summary(mod)

draw(mod)

# Если включеине расстояния до Белокаменки не насрет в модель, то прикольно. Получается, что рака мноо там где обилие высокое и там, где много молоди.Ну и на литораль, похоже раки не любят ползти.

library(DHARMa)

simulateResiduals(mod, plot = T)
