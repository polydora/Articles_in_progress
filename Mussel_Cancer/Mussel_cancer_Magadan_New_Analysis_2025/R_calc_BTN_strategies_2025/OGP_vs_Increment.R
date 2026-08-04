library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(tidyr)
library(purrr)
library(minpack.lm)  # для нелинейного МНК
library(mgcv)
library(gratia)
library(cowplot)
library(patchwork)




# myt_ind <- read_excel("Data/ЕДИНСТВЕННЫЙ ФАЙЛ.xlsx", na = "NA")

myt_ind <- read_excel("Data/ЕДИНСТВЕННЫЙ ФАЙЛ 20.07.2026.xlsx", na = "NA")

# myt_ind <-
#   myt_ind %>% 
#   filter(Year == 2023)

str(myt_ind)


# Далее работаем только с данными 2023

# myt_ind$ID2 <- paste(myt_ind$Site_code, myt_ind$Sample, myt_ind$ID, sep = "_")

unique(myt_ind$ID2)

myt_ind %>%
  mutate(Prop_Increment = Increment/L) ->
  myt_ind


sum(is.na(myt_ind$Prop_Increment))

## Переводим в длинный формат

myt_ind %>% 
  select(ID2, ring1, ring2, ring3, ring4, ring5, ring6, ring7, ring8, ring9, ring10, ring11, ring12, ring13) %>%
  melt(id.vars = "ID2") %>% 
  rename(Ring = variable, L = value) %>% 
  filter_out(is.na(L)) ->
  myt_rings
  
unique(myt_rings$ID2)

myt_rings <-
  myt_rings %>% 
  mutate(Year = as.numeric(gsub("ring", "", Ring))) %>% 
  arrange(ID2) 


myt_rings %>% 
  group_by(ID2) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  hist()
  
  
unique(myt_rings$ID2)

### Вычисляем OGP #####

# Уравнение Берталанфи: L = Linf * (1 - exp(-k * (t - t0)))
# где:
# L - размер в возрасте t (Year)
# Linf - асимптотический максимальный размер
# k - коэффициент скорости роста
# t0 - гипотетический возраст, когда размер равен 0

# Функция для подбора параметров для одной особи
fit_von_bertalanffy <- function(data) {
  # Проверяем, достаточно ли данных (минимум 3 точки для 3 параметров)
  if (nrow(data) < 3) {
    return(tibble(Linf = NA, k = NA, t0 = NA, 
                  R2 = NA, converged = FALSE, n_obs = nrow(data)))
  }
  
  # Начальные приближения
  Linf_init <- 90
  k_init <- 0.1  # типичное начальное значение
  t0_init <- 0   # начальное предположение

  
  # Linf_init <- max(data$L, na.rm = TRUE) * 1.05  # чуть больше максимального размера
  # k_init <- 0.3  # типичное начальное значение
  # t0_init <- 0   # начальное предположение
  # 
    
  tryCatch({
    # Используем nlsLM (более устойчив к плохим начальным значениям)
    model <- nlsLM(L ~ Linf * (1 - exp(-k * (Year - t0))),
                   data = data,
                   start = list(Linf = Linf_init, k = k_init, t0 = t0_init),
                   lower = c(Linf = 0, k = 0, t0 = -10),
                   upper = c(Linf = Inf, k = 5, t0 = min(data$Year)),
                   control = nls.lm.control(maxiter = 500))
    
    # Псевдо R-квадрат для нелинейной регрессии
    residuals <- residuals(model)
    ss_res <- sum(residuals^2)
    ss_tot <- sum((data$L - mean(data$L))^2)
    r_squared <- 1 - (ss_res / ss_tot)
    
    # Извлекаем параметры
    params <- coef(model)
    
    return(tibble(Linf = params["Linf"],
                  k = params["k"],
                  t0 = params["t0"],
                  R2 = r_squared,
                  converged = TRUE,
                  n_obs = nrow(data)))
    
  }, error = function(e) {
    # В случае ошибки подбора возвращаем NA
    return(tibble(Linf = NA, k = NA, t0 = NA, 
                  R2 = NA, converged = FALSE, n_obs = nrow(data)))
  })
}

# Применяем функцию для каждой особи (по ID2)
results <- myt_rings %>%
  group_by(ID2) %>%
  nest() %>%
  mutate(fit = map(data, fit_von_bertalanffy)) %>%
  unnest(fit, keep_empty = TRUE) %>%
  select(-data)  

# Фильтрация особей с успешно подобранными параметрами
# successful_fits <- results %>%
#   filter(converged == TRUE, !is.na(Linf))


## At! Это точка вычислеиня самого OGP.Этот показатель может считаться и по альтренативной формуле.

results <-
  results %>% 
  mutate(OGP = log10(k * Linf), OGP_L3 = log10(k * Linf^3))


# results %>% 
#   filter(ID2 == "BATCC32")


# Важно! Надо проверить на выборке мидий совпадают ли параметры уравнения Берталанфи с парамтерами, подобранными через эксель.

######


myt_ind$Fi_Increment <- 2*asin(sqrt(myt_ind$Prop_Increment)) * 180/pi

myt_ind %>% 
  select(-c(Ecology, Site,Date, N, ID,  Ploidy_of_aneuploid_cells)) ->
  myt_ind


myt_ind_ogp <- merge(myt_ind, results)

myt_ind_ogp$BTN <- factor(myt_ind_ogp$BTN)
myt_ind_ogp$Site_code <- factor(myt_ind_ogp$Site_code)

myt_ind_ogp %>% 
  filter(is.na(BTN_genotype)) %>%
  group_by(Site_code) %>% 
  summarise(N = n()) %>% 
  pull(N) %>% 
  range()






myt_ind_ogp <- 
  myt_ind_ogp %>% 
  mutate(BTN = case_when(BTN_genotype == "BTN2.2" ~ "BTN2",
                         BTN_genotype == "BTN2.1" ~ "BTN2",
                         BTN_genotype == "BTN1" ~ "BTN1",
                         BTN_genotype == "BTN1_Am" ~ "BTN1",
                         BTN_genotype == "BTN1/BTN2.1" ~ "BTN2",
                         BTN_genotype == "BTN2.1/BTN1" ~ "BTN2", 
                         BTN_genotype == "BTN2.2/BTN1" ~ "BTN2",
                         is.na(BTN_genotype) ~ "Healthy"))


Pl_Increment_OGP <-
myt_ind_ogp %>%
  filter(!is.na(BTN)) %>%
  ggplot(aes(x = OGP, y = Fi_Increment, color = BTN)) +
  geom_point() +
  scale_color_manual(values = c("blue", "red", "gray80")) +
  # scale_size_manual(values = c(2, 4, 1)) +
  theme_bw() +
  geom_hline(yintercept = quantile(myt_ind_ogp$Fi_Increment, probs = 0.5)) +
  geom_hline(yintercept = quantile(myt_ind_ogp$Fi_Increment, probs = 0.9), linetype = 2) +
  geom_hline(yintercept = quantile(myt_ind_ogp$Fi_Increment, probs = 0.1), linetype = 2) 



myt_ind_ogp %>% 
  ggplot(aes(x = OGP)) +
  geom_histogram()

myt_ind_ogp %>% 
  ggplot(aes(x = OGP, OGP_L3)) +
  geom_point() +
  geom_smooth()


myt_ind_ogp %>% 
  ggplot(aes(x = OGP_site, OGP)) +
  geom_point() +
  geom_smooth()

myt_ind_ogp %>% 
  ggplot(aes(x = OGP_site, OGP_L3)) +
  geom_point() +
  geom_smooth()




Pl_Fi_Increment <-
  myt_ind_ogp %>%
  filter(!is.na(BTN)) %>% 
  ggplot(aes(x = BTN, y = Fi_Increment)) +
  geom_boxplot() +
  geom_hline(yintercept = quantile(myt_ind_ogp$Fi_Increment, probs = 0.5)) +
  geom_hline(yintercept = quantile(myt_ind_ogp$Fi_Increment, probs = 0.9), linetype = 2) +
  geom_hline(yintercept = quantile(myt_ind_ogp$Fi_Increment, probs = 0.1), linetype = 2) 


Pl_OGP <-
myt_ind_ogp %>%
  filter(!is.na(BTN)) %>% 
  ggplot(aes(x = BTN, y = OGP)) +
  geom_boxplot() +
  geom_hline(yintercept = quantile(myt_ind_ogp$OGP, probs = 0.5)) +
  geom_hline(yintercept = quantile(myt_ind_ogp$OGP, probs = 0.9), linetype = 2) +
  geom_hline(yintercept = quantile(myt_ind_ogp$OGP, probs = 0.1), linetype = 2) 



(Pl_Fi_Increment + Pl_OGP)/Pl_Increment_OGP

# Видно, что без учета прочих ковариат (!!!), прирост у BTN2 в большинстве случаев выше общей медианы. Иными словами, мидии с BTN2 чаще демонстрируют положительную "аномалию" роста.  


# Вычисляем OGP для сайта
# 
merge(myt_rings, myt_ind %>% select(Site_code, ID2), by = "ID2")  ->
  myt_rings

healthy_mussels <-
  myt_ind_ogp %>% 
  filter(BTN == "Healthy") %>% 
  pull(ID2)

# Отобрали здоровых и тех у кого достсточно колец
myt_rings %>%
  filter(ID2 %in% healthy_mussels) %>% 
  filter(Year >= 2 & Year <=10) %>% 
  group_by(Site_code, Year) %>%
  summarise(Rings_Number = n(), L = mean(L)) %>%  
  filter(Rings_Number >= 3) ->
  mean_site_rings


results_site <- mean_site_rings %>%
  group_by(Site_code) %>%
  nest() %>%
  mutate(fit = map(data, fit_von_bertalanffy)) %>%
  unnest(fit, keep_empty = TRUE) %>%
  select(-data) 


results_site <-
  results_site %>% 
  mutate(OGP_site_2 = log10(k * Linf), OGP_L3_site_2 = log10(k * Linf^3))


myt_rings %>%
  filter(ID2 %in% healthy_mussels) %>% 
  filter(Year >= 2 & Year <=10) %>% 
  group_by(Site_code, Year) %>%
  summarise(Rings_Number = n(), L = mean(L)) %>%  
  filter(Rings_Number >= 3) %>% 
  filter(Site_code == "PORT")
  


myt_ind_ogp %>% 
  filter(BTN == "Healthy") %>% 
  filter_out(Site_code == "KHOL" & Sample %in% c("A", "B")) %>% # Удаляем данные проб из сублиторали
  group_by(Site_code) %>% 
  summarise(OGP_site = mean(OGP_site), Mean_OGP = mean(OGP), Mean_OGP_L3 = mean(OGP_L3) ) %>% 
  merge(., results_site) %>% 
  ggplot(aes(OGP_site, OGP_site_2)) +
  geom_point() +
  geom_abline() +
  geom_text(aes(label = Site_code))




myt_ind_ogp %>% 
  filter(BTN == "Healthy") %>% 
  group_by(Site_code) %>% 
  summarise(OGP_site = mean(OGP_site), Mean_OGP = mean(OGP), Mean_OGP_L3 = mean(OGP_L3) ) %>% 
  merge(., results_site) %>% 
  ggplot(aes(OGP_site, OGP_L3_site_2)) +
  geom_point() +
  geom_abline() +
  geom_text(aes(label = Site_code))


  
myt_ind_ogp %>% 
  filter(BTN == "Healthy") %>% 
  group_by(Site_code) %>% 
  summarise(OGP_site = mean(OGP_site), Mean_OGP = mean(OGP)) %>% 
  ggplot(aes(OGP_site, Mean_OGP)) +
  geom_point() +
  geom_abline()




# Сохраняем в файл "ЕДИНСТВЕННЫЙ ФАЙЛ 20.07.2026_OGP_recalculated.xlsx"

library(writexl)

results_site %>% 
  select(Site_code,OGP_site_2, OGP_L3_site_2) %>% 
  merge(myt_ind_ogp, .) ->
  myt_ind_ogp


write_xlsx(x = myt_ind_ogp, path = "Data/ЕДИНСТВЕННЫЙ ФАЙЛ 20.07.2026_OGP_recalculated.xlsx")




# Строим модель зависимости прироста текущего года от типа болезни, с ковариатами OGP и Last_Ring

# Отбор здоровых мидий, близких по размерам с больными


myt_ind_ogp %>% 
  filter(BTN != "Healthy") %>% 
  pull(L) %>% 
  range() ->
  BTN_L_range


myt_ind_ogp_size_filtered <- 
  myt_ind_ogp%>% 
  filter(L <= BTN_L_range[2]) 




myt_ind_ogp_size_filtered$BTN <- factor(myt_ind_ogp_size_filtered$BTN)
myt_ind_ogp_size_filtered$BTN <- relevel(myt_ind_ogp_size_filtered$BTN, ref = "Healthy")



Mod_Increment <- gam(Fi_Increment ~ s(OGP, by = BTN) + s(Last_ring, by = BTN) + BTN + s(Site_code, bs = "re"), data = myt_ind_ogp_size_filtered)
  

appraise(Mod_Increment)

# Все более или менее чисто


summary(Mod_Increment)

draw(Mod_Increment, parametric = T)

