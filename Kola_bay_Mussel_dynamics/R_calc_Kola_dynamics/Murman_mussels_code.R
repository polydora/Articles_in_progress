# количество пакетов избыточно
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
library(parallel)
library(mgcViz)

## тут сначала идёт анализ долговременной динамики (модели 1-3) и рисунки к ним ####
# демография ниже будет

# биомассы уже добавлены в файл, но если нужно показать, как они реконструировались из первички (но я бы без этого обошлась..), это можно сразу здесь сделать. подробнее см строки ниже в анализе демографии 338-340 и 387-417

# Читаем данные по историческим и современным точкам на Мурманском побережье
# ! файл неопрятный, могут быть ненужные столбцы, их надо удалить. его надо будет оформить в соответствии с Table S3, могу сама потом это сделать
myt <- read_excel("data/data_history_itog.xlsx")
str(myt)

# делаем бокс-кокс трансформацию для всех данных по биомассе
bc_out <- boxcoxnc(myt$B_kg)
myt$B_kg_bc <- bc_out$tf.data

# делаем переменные факоторами
myt$survey <- factor(myt$survey)
myt$period <- factor(myt$period)
myt$site <- factor(myt$site)
myt$region <- factor(myt$region)
myt$monitor <- factor(myt$monitor)

###
# Модель 1 и рис. 2
myt_survey <- 
  myt %>% 
  filter(survey %in% c("Romanova", "Antipova81", "Antipova71", "Milyutin and Sokolov"))

str(myt_survey)

# myt_survey$log_B <- log(myt_survey$B_kg+1)

# меняю базовый уровень на Романову, чтобы было удобнее смотреть саммари модели, и для графика переименовываю факторы
myt_survey$survey <- factor(myt_survey$survey, levels = c("Romanova", "Antipova71", "Antipova81", "Milyutin and Sokolov"))
levels(myt_survey$survey) <- c("VNIRO 1960-1961", "VNIRO 1971", "VNIRO 1981", "VNIRO 2002-2005")
levels(myt_survey$survey)

# Model_1
Model_long_gam <- gam(B_kg_bc ~ s(longitude_GE, by = survey) + survey, data = myt_survey)

summary(Model_long_gam)

# plot(Model_long_gam)

# draw(Model_long_gam, parametric = T)

# диагностика. тут и далее избыточное количество функций, может сократить?
# qplot(x = fitted(Model_long_gam), y = residuals(Model_long_gam), type = "pearson") + geom_smooth()
# 
# gam.check(Model_long_gam)
# 
# appraise(Model_long_gam)
# 
# simulateResiduals(Model_long_gam, plot = T)

# визуализация
MyData_gam <- 
  myt_survey %>% 
  dplyr::select(B_kg_bc, survey, longitude_GE) %>%  
  group_by(survey)

MyData_gam$B_predicted_gam <-  predict(Model_long_gam, newdata = MyData_gam)

# датасет с данными НЕвниро
myt_no_survey <- 
  myt %>% 
  filter(survey %in% "NA") %>% 
  dplyr::select(B_kg_bc, period, longitude_GE) %>%  
  group_by(period) %>% 
  as.data.frame()

levels(myt_no_survey$period) <- c("1930-1939", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "2000-2009", "2010-2020")

cols <- c("1930-1939" = "yellow", "1950-1959" = "grey", "1960-1969" = "red", "1970-1979" = "green3", "1980-1989" = "chocolate3", "2000-2009" = "deepskyblue", "2010-2020" = "darkblue")

# делаю обратную бокс-кокс трансформацию первичных данных (ну её можно и не делать было, достаточно просто логарифмировать первичку) и предсказаний, и потом их логарифмирую (+1, чтобы графики опрятнее были)
box_cox_back <- function(x, lyam = 0.22)(x*lyam + 1)^(1/lyam)

MyData_gam$back_B_predicted_gam <- box_cox_back(MyData_gam$B_predicted_gam)
MyData_gam$log_back_B_predicted_gam <- log(MyData_gam$back_B_predicted_gam+1)

MyData_gam$back_B_kg_bc <- box_cox_back(MyData_gam$B_kg_bc)
MyData_gam$log_back_B_kg_bc <- log(MyData_gam$back_B_kg_bc+1)

myt_no_survey$back_B_kg_bc <- box_cox_back(myt_no_survey$B_kg_bc)
myt_no_survey$log_back_B_kg_bc <- log(myt_no_survey$back_B_kg_bc+1)

# тут что-то похожее на итоговый рис. 2, но не идеально))
Figure2 <-
  ggplot(data = MyData_gam, aes(x = longitude_GE, y = log_back_B_predicted_gam)) + 
  geom_line(aes(color = survey), size = 1, linetype = 2) +
  geom_point(aes(y = log_back_B_kg_bc, color = survey), shape = 17, size = 2.5) +
  scale_color_manual(values = c("red", "green3", "chocolate3", "deepskyblue")) +
  new_scale_fill() +
  geom_point(data = myt_no_survey, shape = 21, size = 1.6, aes(x = longitude_GE, y = log_back_B_kg_bc, fill = period)) +
  scale_fill_manual(values = cols) +
  labs(y = "Biomass, kg", x = "Longitude, °E") +
  scale_x_continuous(n.breaks = 10, limits = c(31, 40)) +
  guides(color = guide_legend(position = "bottom"),
         fill = guide_legend(position = "bottom")) +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        legend.title = element_blank())

###
# Модель 2 и рис. 3
myt_region <- 
  myt %>% 
  filter(region %in% c("Kola Bay and vicinity", "Dalnye Zelenetsy", "Seven Islands archipelago"))


# обрезаю данные начала 30х годов
myt_region2 <- 
  myt_region %>% 
  filter(year > 1934)

str(myt_region)

# меняю базовый уровень на Кольский залив и окрестности, чтобы было удобнее смотреть саммари модели
myt_region2$region <- factor(myt_region2$region, levels = c("Kola Bay and vicinity", "Dalnye Zelenetsy", "Seven Islands archipelago"))

# Model_2
Model_U_gam <- gam(B_kg_bc ~ s(year, by = region) + region + s(site, bs = "re"),  method = "REML", data = myt_region2)

summary(Model_U_gam)

# plot(Model_U_gam)

# диагностика
# qplot(x = fitted(Model_U_gam), y = residuals(Model_U_gam), type = "pearson") + geom_smooth()
# 
# gam.check(Model_U_gam)
# 
# simulateResiduals(Model_U_gam, plot = T)

# визуализация
Mydata2 <- myt_region2 %>% 
  group_by(region) %>%
  do(data.frame(year = seq(min(.$year), max(.$year)))) %>% 
  unique()

predicted <- predict(Model_U_gam, newdata = Mydata2, se.fit = TRUE, exclude = "s(site)", newdata.guaranteed=TRUE)

Mydata2$B_predicted <- predicted$fit

MyData_B <- 
  myt_region %>% 
  dplyr::select(B_kg_bc, region, year) %>%
  group_by(region)

# обратная трансформация и логарифмирование
Mydata2$back_B_predicted <- box_cox_back(Mydata2$B_predicted)
Mydata2$log_back_B_predicted <- log(Mydata2$back_B_predicted+1)

MyData_B$back_B_kg_bc <- box_cox_back(MyData_B$B_kg_bc)
MyData_B$log_back_B_kg_bc <- log(MyData_B$back_B_kg_bc+1)

# графики - тоже что-то похожее, но не идеально)
# тут ещё надо сделать, чтоб точки не слипались
Figure3A <-
  ggplot(data = Mydata2, aes(x = year, y = log_back_B_predicted)) + 
  geom_line(aes(color = region), size = 1.5) +
  geom_point(data = MyData_B, aes(x = year, y = log_back_B_kg_bc, color = region), size= 2) +
  scale_color_manual(values = c("orange", "darkgreen", "darkviolet")) +
  labs(y = "Biomass, kg (log) \n", x = "Years of study") +
  scale_x_continuous(n.breaks = 10, limits = c(1925, 2020), expand = c(0, 2)) +
  theme(legend.position = c(0.6, 0.9),
        legend.background = element_rect(fill = "white", colour = "grey90"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        axis.line = element_line(color = "grey90"),
        panel.background = element_rect(fill = "white"),
        legend.title = element_blank())

# данные по температуре на Кольском меридиане для Figure 3B
temp <- read_excel("data/Temperature_KM.xlsx")
str(temp)

temp$anomT <- temp$temp-mean(temp$temp)
temp$norm_anomT <- (temp$anomT - mean(temp$anomT))/sd(temp$anomT)
temp$period <- ifelse(temp$norm_anomT < 0, "cold", "warm")

Figure3B <-
  ggplot(data = temp, aes(x = year, y = norm_anomT)) + 
  geom_col(aes(fill = period)) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(y = "Normalized anomaly of \n sea water temperature", x = "Years of study") +
  scale_y_continuous(breaks = c(-2.5, -1.5, -0.5, 0.5, 1.5, 2.5), expand = c(0.01, 0.2)) +
  scale_x_continuous(n.breaks = 10, expand = c(0, 2)) +
  theme(legend.position = "",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        axis.line = element_line(color = "grey90"),
        panel.background = element_rect(fill = "white"))

Figure3 <- ggarrange(Figure3A, Figure3B, nrow = 2, ncol=1, align = "hv")

###
# Модель 3 и рис. 4
# отбираю мониторинговые поселения
myt_monitor <- 
  myt %>% 
  filter(monitor %in% "yes")

myt_monitor$Period <- ifelse(myt_monitor$year < 1980, "before 1980", "after 1980")
myt_monitor$Period <- factor(myt_monitor$Period)

str(myt_monitor)

# создаю нужный порядок поселений - с запада на восток
myt_monitor$site <- factor(myt_monitor$site, levels = c("Ura", "Tyuva", "Zelenetskaya Zapadnaya", "Dolgaya", "Klimkovka", "Yarnyshnaya_lit", "Dal'niy Plyazh", "Kharlov Island_S", "Kharlov Island_N", "Veshnyak Island_S", "Bol'shoy Lickiy Island_N", "Bol'shoy Lickiy Island_S"))

# переименовываю поселения, как на рисунке в статье
levels(myt_monitor$site) <- c("Ur", "Tv", "ZZ", "Km", "Dl", "YaL", "DP", "KhS", "KhN", "Ve", "BLS", "BLN")

levels(myt_monitor$site)

# добавляю данные по Температуре на КМ (средняя Т за предшествующие 5 лет)
temp <- read_excel("data/Temperature_KM.xlsx", na = "NA")
# я лентяйка, поэтому в этом файле усредненные за предшествующие 5 лет температуры (понадобятся ниже) идут отдельным столбцом, а не получается из первички в коде

mon_temp <- merge(myt_monitor, temp, by = "year")

# меняю базовый уровень
mon_temp$Period <- factor(mon_temp$Period, levels = c("before 1980", "after 1980"))

# рисую первичные данные (рис. 4А), цвет - декады, как на рис. 2, форма - периоды
levels(mon_temp$period) <- c("1930-1939", "1950-1959", "1960-1969", "1970-1979", "1980-1989", "2000-2009", "2010-2020")

cols <- c("1930-1939" = "yellow", "1950-1959" = "grey", "1960-1969" = "red", "1970-1979" = "green3", "1980-1989" = "chocolate3", "2000-2009" = "deepskyblue", "2010-2020" = "darkblue")

Figure4A <-
  ggplot(data = mon_temp, aes(x = temp_lag5, y = log(B_kg+1), group = site, fill = period, shape = Period, size= Period)) +
  geom_point() +
  scale_fill_manual(values = cols) +
  scale_shape_manual(values = c(24, 21)) +
  scale_size_manual(values = c(2.5, 3)) +
  facet_wrap(~site, scale = "free_y", axes	= "all") +
  labs(y = "Biomass, kg (log)", x = "Water temperature, °C") +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.line = element_line(color = "grey90"),
        panel.background = element_rect(fill = "white"),
        strip.background = element_rect(color="grey90", fill="grey90"),
        strip.text = element_text(hjust = 1))

# Model_3
Model_BTPS_gam <- gam(B_kg_bc ~ s(temp_lag5, by=Period, bs = "cr", k = 5) + Period + s(site, bs = "re"), method = "REML", data = mon_temp)

# диагностика
qplot(x = fitted(Model_BTPS_gam), y = residuals(Model_BTPS_gam), type = "pearson") + geom_smooth()

simulateResiduals(Model_BTPS_gam, plot = T)

appraise(Model_BTPS_gam)
summary(Model_BTPS_gam)
plot(Model_BTPS_gam)

# визуализация
# данные для визуализации модели (рис. 4Б)
Mydata3 <- mon_temp %>% 
  group_by(Period) %>%
  do(data.frame(temp_lag5 = seq(min(.$temp_lag5), max(.$temp_lag5), length.out = 100))) %>%
  unique()

predicted <- predict(Model_BTPS_gam, newdata = Mydata3, type = "response", se.fit = TRUE, exclude = "s(site)", newdata.guaranteed=TRUE)

Mydata3$B_predicted <- predicted$fit

Mydata3$SE <- predicted$se.fit

# делаю обратную бокс-кокс трансформацию первичных данных и предсказаний, и потом их логарифмирую
Mydata3$back_B_predicted <- box_cox_back(Mydata3$B_predicted)
Mydata3$log_back_B_predicted <- log(Mydata3$back_B_predicted+1)

mon_temp$back_B_kg_bc <- box_cox_back(mon_temp$B_kg_bc)
mon_temp$log_back_B_kg_bc <- log(mon_temp$back_B_kg_bc+1)

Mydata3$CI_upper <- Mydata3$B_predicted+1.96*Mydata3$SE
Mydata3$CI_lower <- Mydata3$B_predicted-1.96*Mydata3$SE

Mydata3$back_CI_upper <- box_cox_back(Mydata3$CI_upper)
Mydata3$log_back_CI_upper <- log(Mydata3$back_CI_upper+1)

Mydata3$back_CI_lower <- box_cox_back(Mydata3$CI_lower)
Mydata3$log_back_CI_lower <- log(Mydata3$back_CI_lower+1)

# тут бы линию для geom_smooth сделать черной пунктирной, и ещё надо по периодам разделить значки, как на рис. 4А выше
Figure4B <-
  ggplot(data = Mydata3, aes(x = temp_lag5, y = log_back_B_predicted)) +
  geom_line(aes(color = Period), size = 1.5) +
  geom_point(data = mon_temp, aes(x = temp_lag5, y = log_back_B_kg_bc, color = Period), size = 2, position = "jitter") +
  geom_ribbon(aes(ymin = log_back_CI_lower, ymax = log_back_CI_upper, fill = Period), alpha = 0.2) +
  scale_color_manual(values = c("turquoise2", "red")) +
  scale_fill_manual(values = c("turquoise2", "red")) +
  scale_y_continuous(breaks = c(0, 1, 2, 3), expand = c(0, 0.3)) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  labs(y = "Biomass, kg (log)", x = "Water temperature, °C") +
  xlim(3.4, 5.1) +
  theme(legend.position = "",
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90"),
        axis.line = element_line(color = "grey90"),
        panel.background = element_rect(fill = "white")) +
  geom_smooth(data = mon_temp, aes(x = temp_lag5, y = log_back_B_kg_bc), se = F)

Figure4 <- ggarrange(Figure4A, Figure4B, nrow = 2, ncol=1)

#### демография ####
# сначала из первичных данных получаем численности мидий разных возрастов и строим кривые выживания, чтобы получить z и N0
Ya <- read_excel("data/Ya_lit_initial_data_ITOG.xlsx")

# формула для предсказания веса мидии по размеру раковины =0.000416*L^2.45
Ya$W <- 0.000416 * Ya$Length^2.45
# формально, можно на это забить..все биомассы уже фигурировали выше. но! если нужно показать, как для отдельных лет реконструировали биомассу, это нужно сделать в самом начале кода, наверное.. 

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

# считаем среднюю численность вообще - но она вроде не пригодится нам нигде в итоге
Ya_N_sum <- 
  Ya_N %>% 
  group_by(Year, Sample) %>% 
  summarise(sum_N = sum(N, na.rm = TRUE)) 

Ya_N_mean <- 
  Ya_N_sum %>% 
  group_by(Year) %>% 
  summarise(mean_N = mean(sum_N, na.rm = TRUE))
# write.table("clipboard", sep = "\t")

Ya_N_mean <- data.frame(Year = c(2000, Ya_N_mean$Year), mean_N = c(1990, Ya_N_mean$mean_N))

Ya_gen %>% 
  filter(mean_N > 0) %>%
  filter(Generation >= 1999) %>% 
  mutate(Age = as.numeric(Age)) -> df

# Вычисляем биомассу каждой генерации в тот или иной год - собственно вот и расчет биомасс. но отдельно для каждой генерации нам не надо, а вот среднюю для года сбора можно отсюда получить

Ya %>% 
  group_by(Year, Sample, Part_studied, Frame_area, Age) %>% 
  summarise(sum_W = sum(W)) %>%
  mutate(sum_W = (sum_W/Part_studied/Frame_area)/1000) %>% 
  ungroup() %>% 
  dplyr::select(-Part_studied, -Frame_area) %>%  
  group_by(Year, Sample, Age) %>% 
  summarise(sum_W = sum(sum_W)) %>% 
  dcast(Year + Sample ~ Age, data = .) -> Ya_age_W

Ya_age_W[is.na(Ya_age_W)] <- 0

Ya_W <- melt(Ya_age_W, id.vars = c("Year", "Sample"), variable.name = "Age", value.name = "Sum_W") 

# добавляем столбик с генерацией
Ya_W <- 
  Ya_W %>% 
  mutate(Generation = Year - as.numeric(Age))

Ya_gen_W <- 
  Ya_W %>% 
  group_by(Year, Generation, Age) %>% 
  summarise(mean_W = mean(Sum_W, na.rm = TRUE))

Ya_gen_W %>%
  group_by(Year) %>%
  summarise(B = sum(mean_W)) -> Ya_B_mean

# если показывать, как мы предсказывали биомассу, то это надо делать для всех годов КРОМЕ 2002, 2009, 2010 и 2013 (там взвешивали напрямую)


########################################################
df_sample_size <- 
  df %>%  
  group_by(Generation) %>% 
  summarise(N = n())

# берем генерации с 1999 года!
df %>% 
  merge(., df_sample_size) %>% 
  group_by(Generation) %>% 
  filter(Age > 1) %>% #
  filter(N > 3) %>%
  filter(Generation %in% c(1999:2014)) %>% 
  group_modify( ~ tidy(lm(log(mean_N) ~ Age, data = .))) -> df_lm_filtered 

# вытаскиваю таблицу с численностями генераций для многомерных анализов
gen_N <- dcast(data = df, formula = Generation ~ Age, value.var = "mean_N")
gen_N[is.na(gen_N)] <- 0
# нолики я потом в экселе предсказывала, но правильнее это тут сделать, и потом ещё добавить N0 и z

# вытаскиваем интерсепт
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
  summarise(min_Year = mean(Generation), max_Year = max(Year)) ->
  generation_limits


###### Данные по климатическим параметрам
clim <- read_excel("data/climate_Murman.xlsx", sheet = "data_transposed", na = "NA")
# тут ряд данных в итоге не используем, можно файл покоцать

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
# осень сентябрь-октябрь
# зима ноябрь-апрель
# весна май-июнь


# Присоединяю данные по среднесезонной температуре ВОДЫ к описанию условий существования генераций
# от включения N и B в анализ в итоге отказались, надо их убирать!

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


generation_limits$N_lifespan <- NA
generation_limits$B_lifespan <- NA

generation_limits$N_Gen_lifespan <- NA
generation_limits$B_Gen_lifespan <- NA

generation_limits$B_begin <- NA
generation_limits$N_begin <- NA

for(i in 1:nrow(generation_limits)){
  
  # Параметры для года формирования генерации 
  
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
  
  
  generation_limits$B_begin[i] <- 
    Ya_B_mean %>% 
    filter(Year %in% years) %>% 
    pull(B) %>% 
    mean
  
  generation_limits$N_begin[i] <- 
    Ya_N_mean %>% 
    filter(Year %in% years) %>% 
    pull(mean_N) %>% 
    mean
  
  
  ###############################  
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
  

  generation_limits$B_Gen_lifespan[i] <-
    Ya_gen_W %>%
    filter(Generation %in% generation_limits$Generation[i]) %>%
    pull(mean_W) %>%
    mean()
  
  
  generation_limits$N_Gen_lifespan[i] <-
    Ya_gen %>%
    filter(Generation %in% generation_limits$Generation[i]) %>%
    pull(mean_N) %>%
    mean()
  
}


df_lm_filtered %>%
  filter(term == "(Intercept)") %>%
  dplyr::  select(Generation, estimate) %>%
  mutate(Anundance_0 = exp(estimate)) %>% 
  dplyr::select(-Anundance_0) %>% 
  rename(N0 = estimate) ->
  recruitment


# Вытаскиваем данные по угловым коэфициентам
df_lm_filtered %>%
  filter(term == "Age") %>% 
  dplyr::  select(Generation, estimate) %>% 
  rename(M = estimate)-> 
  mortality

mort_No <- merge(recruitment, mortality)

# тут надо нарисовать Fig. S1 и Fig. S2 Для S2 A нужно ещё Пирсона считать, я это в экселе делала, поэтому этого куска кода вообще нет((

# BioEnv и RDA
# тк я лентяйка, у меня исходные матрицы в отдельном файле( наверное, правильнее их тут выше сконструировать. тонкий момент - в демографической матрице пропущенные значения реконструированы из кривых выживания!

dem_multivar <- read_excel("data/dem_multivar.xlsx", sheet = "Age_0_predicted",na = "NA")
dem_multivar <- as.data.frame(dem_multivar)
row.names(dem_multivar) <- dem_multivar$Generation  

dem_multivar %>% 
  filter(complete.cases(.)) -> dff

dff

No_demogr2 <- 
  dff %>% 
  dplyr::select(2:12) %>% 
  as.data.frame()

# bioenv
bioenv_rda <- bioenv(comm = No_demogr2, env = dff[,-c(1:12)], method = "spearman", index = "euclidean", metric = "euclidean", parallel = 2)
# получается так Spring_Tw_begin, Spring_Ta_begin, Spring_Wind_begin, Summer_Ta_begin,  Summer_Tw_lifespan
# with correlation  0.2592541
summary(bioenv_rda)

# rda
rda(No_demogr2 ~ Spring_Wind_begin + Summer_Tw_lifespan + Spring_Tw_begin + Summer_Ta_begin + Spring_Ta_begin, data = dff) -> ord3rda

anova(ord3rda, permutations = 9999)
anova(ord3rda, by = "axis", permutations = 9999)
anova(ord3rda, by = "margin", permutations = 9999)

# vif можно и не показывать, я думаю
vif.cca(ord3rda)

plot(ord3rda, scaling = "symmetric")
summary(ord3rda)

# рис. 5 (с rda) в ggplot не делала(
rda_scores <- fortify(ord3rda, scaling = "symmetric", display = c("sp","wa","bp"))

d <- plot(ord3rda, scaling = "symmetric")
mul <- attr(d$biplot,"arrow.mul")
# 2.159392 - это множитель для стрелок, понадобится при построении графика в ggplot

# ещё надо Fig. S3 сделать