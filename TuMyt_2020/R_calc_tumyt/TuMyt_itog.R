library(ggplot2)
library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(readxl)
library(ggpubr)
library(lubridate)
#####################################################################################################################
# считаем солёность
tide <- read.csv("tides.csv", header = F)
tide <- tide[-c(1:144),-1]

names(tide) <- c("H", "Time", "Day_part", "Date")

tide$Date[1:144] <- "25/07/2009"
tide$Date[145:nrow(tide)] <- "26/07/2009"


tide$Date2 <- paste(tide$Time, tide$Day_part, tide$Date )

tide$Date3 <- parse_date_time(tide$Date2, '%I:%M %p %d/%m%Y', tz = "Europe/Moscow")

min_time <- as.POSIXct(strptime("17:00 25.07.2009", format = "%H:%M %d.%m.%Y", tz = "Europe/Moscow"))

max_time <- as.POSIXct(strptime("13:00 26.07.2009", format = "%H:%M %d.%m.%Y", tz = "Europe/Moscow"))

tide <- tide[tide$Date3 < max_time & tide$Date3 > min_time, ]

ggplot(tide, aes(x = Date3, y = H)) + geom_line()

sal_tuv <- read_excel("Data/TuMyt_2009_2010.xlsx", sheet = "sal-temp")

sal_tuv$Dat_text <- paste(sal_tuv$Time, sal_tuv$Date)

sal_tuv$Date_true <- as.POSIXct(strptime(sal_tuv$Dat_text, '%H_%M %d_%m_%Y', tz = "Europe/Moscow"))

sal_tuv$H <- abs(sal_tuv$Height)

ggplot(tide, aes(x = Date3, y = H)) + geom_line() + geom_point(data = sal_tuv, aes(x = Date_true, y = abs(Height), size = Salinity)) + facet_grid(Transect ~ Depth) + geom_hline(yintercept = c(2, 1.5, 1, 0.5, -0.5, -1.5, -3.5))

ggplot(sal_tuv, aes(x = abs(Height), y = Salinity)) + geom_point()

Mod_tide <- lm(Salinity ~ H*Distance, data = sal_tuv)

summary(Mod_tide)

tuv <- read_excel("Data/TuMyt_2009_2010.xlsx", sheet = "для многомерки" )

tuv$Mean_Salinity_predicted <- NA
tuv$Min_Salinity_predicted <- NA

library(dplyr)

for(i in 1:nrow(tuv)){
  d <- tide %>% filter(H >= tuv$High[i])
  d$Distance <- tuv$Distance[i]
  Sal_pred <- predict(Mod_tide, newdata = d)
  tuv$Mean_Salinity_predicted[i] <- mean(Sal_pred)
  tuv$Min_Salinity_predicted[i] <- min(Sal_pred)
}

tuv$Min_Salinity_predicted

ggplot(tuv, aes(x = Distance, y = Min_Salinity_predicted)) + geom_point()
ggplot(tuv, aes(x = Distance, y = Min_Salinity_predicted)) + geom_text(aes(label = High), position = position_jitter(width = 0))

tide_salinity$Predict_sal <- predict(Mod_tide, newdata = tide_salinity)

ggplot(tide_salinity, aes(x = Date3, y = H, size = Predict_sal)) + geom_point() + geom_hline(yintercept = 1.5)

tuv_print <- tuv %>% select(Sample_ID, Min_Salinity_predicted, Mean_Salinity_predicted)

write.table(tuv_print, "Data/Salinity_predicted.csv", sep = ";", row.names = F)



#####################################################################################################################
# Делаем калькулятор по Тюве и сравниваем его предсказания с WSBL и BH
tuv_calc <- read_excel("Tu_myt_calculator.xlsx")
str(tuv_calc)

tuv_calc$sp <- ifelse(tuv_calc$str <= 0.5, 0, 1)

PT <- tuv_calc %>% group_by(pop) %>% summarise(PT=mean(ind==1), Ptros=mean(sp==1))

PT$type <- c("new", "old", "new", "old", "old", "new", "old", "old", "old")

PT$type <- c("new", "old", "new", "old", "old", "new", "old", "old", "old")


calculator <- merge(tuv_calc, PT)
str(calculator)

Model1 <- glm(sp~PT, family = "binomial", data = calculator)

plot(Model1)

summary(Model1)

MyData <- data.frame(PT=seq(0, 1, length.out = 100))

Model1_Predict <- predict(Model1, newdata = MyData, type = "response", se.fit=TRUE)

MyData$fit <- Model1_Predict$fit

MyData$se <- Model1_Predict$se.fit

MyData$Ptros_calc_BH <- with(MyData, exp(-3.9 + 5.0*PT)/(1 + exp(-3.9 + 5.0*PT)))

MyData$Ptros_calc_WSBL <- with(MyData, exp(-2.4 + 5.4*PT)/(1 + exp(-2.4 + 5.4*PT)))

df <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)

ggplot(MyData, aes(x = PT, y = fit)) + geom_line(color="red", size = 2) + theme_bw() + geom_ribbon(aes(ymin = fit-1.96*se, ymax = fit+1.96*se), fill = "gray", alpha=0.3) + ylim(0, 1) + geom_point(data=PT, aes(y=Ptros), size = 2, color = "red") + geom_line(aes(y=Ptros_calc_BH), size = 1.5) + geom_line(aes(y=Ptros_calc_WSBL), color = "blue", size = 1.5) + labs(y="predicted Ptros")  
# не забыть указать, что были выборки для ВН (черная линия) - может в тексте

#####################################################################################################################
# считаем обилие мидий на губу
tuv <- read.table("TuMyt_0910_itog.csv", sep = ";", header = T)
str(tuv)

tuv_reduc <- tuv %>% filter(!is.na(PT))

tuv_abund <- tuv_reduc %>% select(Transect, Trans, Depth2, PT, N, W, Age4, Age5, Age6, Age7 ) %>% mutate(Age_4_7 = Age4 + Age5 + Age6 + Age7) %>% select(-c(Age4, Age5, Age6, Age7))  %>% as.data.frame()

tuv_totals <- tuv_abund  %>% mutate(Gorizont = case_when(Depth2 < 0 ~ "Subtidal", Depth2 >=0 ~ "Intertidal")) %>% group_by(Trans, Gorizont) %>% summarise(PT_m = mean(PT), N_m = mean(N), W_m =  mean(W), Age_4_7_m = mean(Age_4_7))

Area <- read.csv("Area.csv")

Area$Area <-  Area$Area/1.635*10000

tuv_totals_area <- merge(tuv_totals, Area)
# тут приведены значения на квадратный метр! экземпляров - для численности, грамм - для биомассы
# численность
tuv_totals_area$Stock_N <- tuv_totals_area$N_m * tuv_totals_area$Area
# общая
Total_stock_N <- sum(tuv_totals_area$Stock_N)
# на литорали
tuv_totals_area_lit <- tuv_totals_area %>%  filter(Gorizont %in% "Intertidal")
Total_stock_N_lit <- sum(tuv_totals_area_lit$Stock_N)
# в сублиторали
tuv_totals_area_slit <- tuv_totals_area %>%  filter(Gorizont %in% "Subtidal")
Total_stock_N_slit <- sum(tuv_totals_area_slit$Stock_N)

# биомасса
tuv_totals_area$Stock_B <- tuv_totals_area$W_m * tuv_totals_area$Area
# общая
Total_stock_W <- sum(tuv_totals_area$Stock_B)/1000
# на литорали
tuv_totals_area_lit <- tuv_totals_area %>%  filter(Gorizont %in% "Intertidal")
Total_stock_W_lit <- sum(tuv_totals_area_lit$Stock_B)/1000
# в сублиторали
tuv_totals_area_slit <- tuv_totals_area %>%  filter(Gorizont %in% "Subtidal")
Total_stock_W_slit <- sum(tuv_totals_area_slit$Stock_B)/1000

# репродуктивный потенциал МЕ и МТ
tuv_totals_area$Ptros <- with(tuv_totals_area, exp(-3.9 + 5*PT_m)/(1+exp(-3.9 + 5*PT_m)))

# МТ
# общая
tuv_totals_area$N_tros <- tuv_totals_area$Ptros * tuv_totals_area$Age_4_7_m
tuv_totals_area$N_edul <- tuv_totals_area$Age_4_7_m - tuv_totals_area$N_tros
Total_stock_tros <- sum(tuv_totals_area$N_tros*tuv_totals_area$Area)
Total_stock_edul <- sum(tuv_totals_area$N_edul*tuv_totals_area$Area)
# литораль
tuv_totals_area_lit <- tuv_totals_area %>%  filter(Gorizont %in% "Intertidal")
tuv_totals_area_lit$N_tros <- tuv_totals_area_lit$Ptros * tuv_totals_area_lit$Age_4_7_m
tuv_totals_area_lit$N_edul <- tuv_totals_area_lit$Age_4_7_m - tuv_totals_area_lit$N_tros
Total_stock_tros_lit <- sum(tuv_totals_area_lit$N_tros*tuv_totals_area_lit$Area)
Total_stock_edul_lit <- sum(tuv_totals_area_lit$N_edul*tuv_totals_area_lit$Area)
# сублитораль
tuv_totals_area_slit <- tuv_totals_area %>%  filter(Gorizont %in% "Subtidal")
tuv_totals_area_slit$N_tros <- tuv_totals_area_slit$Ptros * tuv_totals_area_slit$Age_4_7_m
tuv_totals_area_slit$N_edul <- tuv_totals_area_slit$Age_4_7_m - tuv_totals_area_slit$N_tros
Total_stock_tros_slit <- sum(tuv_totals_area_slit$N_tros*tuv_totals_area_slit$Area)
Total_stock_edul_slit <- sum(tuv_totals_area_slit$N_edul*tuv_totals_area_slit$Area)

# сделать биомассу ещё для МЕ и МТ!!!
# 
# 
# 

#####################################################################################################################
# ССА по данным 09-10 года: новый OGP, новый Slope и Cover_algae в баллах

# читаем данные
tuv <- read.table("TuMyt_0910_itog.csv", header = T, sep = ";")


# отбор демографических признаков
tuv_demogr2 <- tuv %>% select(Age2_3, Age4_6, Age7_9, Age10_12, Ptros, N, W, OGP, max_L) %>% as.data.frame()
row.names(tuv_demogr2) <- tuv$Sample_ID
colSums(is.na(tuv_demogr2))

# отбор факторов среды
# не пугайтесь Habitat, это нововведение, напишу ниже 
tuv_predictors <-  tuv %>% select(Transect, Habitat, Depth, Salinity, Distance, Slope, Kelps, Width, Exposition, Cover_algae)


# Полная модель
tuv_cca3 <- cca(tuv_demogr2 ~ Distance + Salinity + Depth + Slope + Cover_algae + Kelps + Width + Exposition, data = tuv_predictors)
vif.cca(tuv_cca3) 

# Подбор оптимальной модели
tuv_cca3_0 <- cca(tuv_demogr2 ~ 1, data = tuv_predictors) 

tuv_cca3_reduced <- ordistep(tuv_cca3_0, scope = formula(tuv_cca3), direction = "forward", permutations = 9999)
vif.cca(tuv_cca3_reduced)
# тут всё в порядке

anova(tuv_cca3_reduced)
anova(tuv_cca3_reduced, by = "axis")
anova(tuv_cca3_reduced, by = "margin")
plot(tuv_cca3_reduced, scaling = "symmetric")
scores(tuv_cca3_reduced)
# и тут всё хорошо
table_cca <- summary(tuv_cca3_reduced)

# создаем датафрейм для графика
demogr_scores <- fortify(tuv_cca3_reduced, scaling = "symmetric")
demogr_scores_site <- demogr_scores[demogr_scores$Score == "sites", ] 
demogr_scores_site$Transect <- tuv_predictors$Transect 
demogr_scores_site$Depth <- tuv_predictors$Depth
demogr_scores_site$Habitat <- tuv_predictors$Habitat
demogr_scores_sp <- demogr_scores[demogr_scores$Score == "species", ] 
demogr_scores_cons <- demogr_scores[demogr_scores$Score == "biplot", ] 

# появилась следующая идея: трансекты дать маркерами разной формы (R - звезда Давида, BN - квадрат, BS - круг, SS - звездочка, SN - крестик, MidN - ромбик, MoS - треугольник вверх, MoN - треугольник вниз), 
# а разными цветами покрасить 4 типа Habitat: они определены как мидиевая банка (разрезы BS, BN, SS, SN - глубина от +0.5 до -3.5) - темно-синий цвет, отмели (разрезы BS, BN, SS, SN - глубина от +0.5 до +2) - песочный цвет, кельпы (разрезы MidN, MoS, MoN - глубина от -0.5 до -3.5) - салатовый цвет, и скалы (разрезы MidN, MoS, MoN - глубина от +0.5 до +2) - серый цвет. 
# Напоминаю, что в данных глубина у меня перекодирована "наоборот", чтобы стрелочка Depth логично смотрела вниз. И на графике размер точек соответствует глубине

ggplot(demogr_scores_site, aes(x = CCA1, y = CCA2)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + geom_point(aes(fill = Habitat, size = Depth, shape = Transect, color = Habitat, stroke = 2.5)) + scale_fill_manual(values = c("darkblue", "darkolivegreen2", "azure4", "darkgoldenrod3")) + scale_shape_manual(values = c(22, 21, 23, 25, 24, 11, 4, 8)) + scale_color_manual(values = c("darkblue", "darkolivegreen2", "azure4", "darkgoldenrod3")) + theme_bw() +  scale_size(range = c(0.5, 5.5)) + theme(legend.position ="") + geom_segment(data = demogr_scores_cons, aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "brown", arrow = arrow(type = "closed", angle = 4), size = 1) + geom_text_repel(data = demogr_scores_sp, aes(label = Label), color = "darkviolet", size = 4.5) + geom_text_repel(data = demogr_scores_cons, aes(label = Label), color = "brown", size = 4.5, box.padding = 1.2)

# пока нарисовано не очень аккуратно, но идея вроде понятна. Цветовая гамма тоже на выбор, пока сделала спокойные, ассоциированные с местностью, тона


# рисуем связь осей с предикторами

# ось 1
# Distance
D1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_predictors$Distance)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Salinity
S1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_predictors$Salinity)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Cover_algae
C1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_predictors$Cover_algae)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Depth
De1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_predictors$Depth)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Kelps
K1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_predictors$Kelps)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 

# собираю их вместе, они расположены в порядке убывания значений проекций на ось 1
Predictors_CCA1 <- ggarrange(D1, S1, K1, C1, De1, nrow = 1)
# скачивать картинку будем в нормальном формате 

# ось 2
# Distance
D2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_predictors$Distance)) + geom_hline(yintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Salinity
S2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_predictors$Salinity)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Cover_algae
C2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_predictors$Cover_algae)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Depth
De2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_predictors$Depth)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# Kelps
K2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_predictors$Kelps)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "brown") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 

# собираю их вместе, они расположены в порядке убывания значений проекций на ось 2
Predictors_CCA2 <- ggarrange(De2, K2, D2, S2, C2, ncol = 1)
# скачивать картинку будем в нормальном формате


# рисуем связь осей с демографическими признаками
# ось1
# Ptros
Ptros1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$Ptros)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# OGP
OGP1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$OGP)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# max_L
max_L1<- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$max_L)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# W
W1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$W)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# N
N1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$N)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age2_3
Age2_3_1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$Age2_3)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age4_6
Age4_6_1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$Age4_6)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age7_9
Age7_9_1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$Age7_9)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age10_12
Age10_12_1 <- ggplot(demogr_scores_site, aes(x = CCA1, y = tuv_demogr2$Age10_12)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

# собираю их вместе, они расположены в порядке убывания scores по оси 1
Demography_CCA1 <- ggarrange(Age2_3_1, N1, W1, Age4_6_1, Age10_12_1, Age7_9_1, Ptros1, max_L1, OGP1, nrow = 1)
# скачивать картинку будем в нормальном формате


# ось2
# Ptros
Ptros2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$Ptros)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 
# OGP
OGP2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$OGP)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# max_L
max_L2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$max_L)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# W
W2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$W)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# N
N2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$N)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age2_3
Age2_3_2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$Age2_3)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age4_6
Age4_6_2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$Age4_6)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age7_9
Age7_9_2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$Age7_9)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())
# Age10_12
Age10_12_2 <- ggplot(demogr_scores_site, aes(x = CCA2, y = tuv_demogr2$Age10_12)) + geom_vline(xintercept = 0)+ geom_smooth(se= F, size = 2, color = "darkviolet") + theme_bw() + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

# собираю их вместе, они расположены в порядке убывания scores по оси 1
Demography_CCA2 <- ggarrange(Ptros2, Age4_6_2, OGP2, max_L2, Age7_9_2, Age2_3_2, N2, Age10_12_2, W2, ncol = 1)
# скачивать картинку будем в нормальном формате



# Появилась идея сделать перманову по Habitat!
# обрезаю исходный файл с данными
perm_hab <- tuv %>% select(Habitat, Age2_3, Age4_6, Age7_9, Age10_12, Ptros, N, W, OGP, max_L) %>% as.data.frame()

perm_hab_log <- log(perm_hab[2:10] + 1)

#  Надо log +1!!!!!!!!!!!!

# стандартизирую переменные
# perm_hab$Age2_3 <- (perm_hab$Age2_3 - (mean(perm_hab$Age2_3))) / var(perm_hab$Age2_3)
# perm_hab$Age4_6 <- (perm_hab$Age4_6 - (mean(perm_hab$Age4_6))) / var(perm_hab$Age4_6)
# perm_hab$Age7_9 <- (perm_hab$Age7_9 - (mean(perm_hab$Age7_9))) / var(perm_hab$Age7_9)
# perm_hab$Age10_12 <- (perm_hab$Age10_12 - (mean(perm_hab$Age10_12))) / var(perm_hab$Age10_12)
# perm_hab$Ptros <- (perm_hab$Ptros - (mean(perm_hab$Ptros))) / var(perm_hab$Ptros)
# perm_hab$N <- (perm_hab$N - (mean(perm_hab$N))) / var(perm_hab$N)
# perm_hab$W <- (perm_hab$W - (mean(perm_hab$W))) / var(perm_hab$W)
# perm_hab$OGP <- (perm_hab$OGP - (mean(perm_hab$OGP))) / var(perm_hab$OGP)
# perm_hab$max_L <- (perm_hab$max_L - (mean(perm_hab$max_L))) / var(perm_hab$max_L)

# решили взять расстояние брея-кёртиса
permanova_habitat <- adonis(perm_hab_log ~ perm_hab$Habitat, method = "bray")
# брея-кёртиса не получается взять, потому что есть отрицательные значения
# мы видим, что местообитания по своей демографической структуре достоверно отличаются

# проверка равенства внутригрупповых дисперсий
dist_habitat <- vegdist(perm_hab_log, method = "bray")
PCO_habitat <- betadisper(dist_habitat, perm_hab$Habitat)
plot(PCO_habitat)

# достоверность различий отклонений от центроидов в разных группах проверяется с помощью процедуры
anova(PCO_habitat)
boxplot(PCO_habitat)
# достоверных различий разброса внутригрупповых расстояний не выявлено

# post-hoc tests
pair <- combn(unique(as.character(perm_hab$Habitat)), 2)
ncomb <- dim(pair)[2]
x <- perm_hab_log
y <- perm_hab$Habitat
for (i in 1:ncomb) {
  filter <- y %in% pair[, i]
  posthoc <- adonis(x[filter, ] ~ y[filter], method = "bray")$aov.tab$Pr[1]
  cat(pair[, i], ": p = ", posthoc, "\n", sep = " ")
}

# отмели категорически отличаются от всего остального!
0.05/6

# SIMPER
simper_habitat <- simper(perm_hab_log, perm_hab$Habitat, permutations = 999)
summary(simper_habitat)
# и тут я не помню, как трактовать результаты. где звёздочки - это значит, что признаки одинаковы? или что различаются? если различаются, то тут какое-то глобальное противоречие с пост-хоками. или я неправильно понимаю пост-хоки




#####################################################################################################################

# ДИНАМИКА
# Петру Петровичу очень нравится идея делать СА по всем-всем данным
tuv_all <- read.table("Data/TuMyt_all_itog.csv", header = T, sep = ";")
str(tuv_all)

tuv_all$Period <- factor(tuv_all$Period)
tuv_all$Transect  <- factor(tuv_all$Transect)
tuv_all$Monitoring  <- factor(tuv_all$Monitoring)


# отделяю нужные данные
tuv_all <- tuv_all %>% select(Sample_ID, Period, Distance, Transect, Habitat, Depth, Monitoring, Ptros, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, Year) %>% as.data.frame()



ggplot(tuv_all, aes(x = Period, y = Ptros)) +  geom_boxplot(aes(fill = Habitat), varwidth = T)  + geom_hline(yintercept = mean(tuv_all$Ptros, na.rm = T))  + geom_path(data = tuv_all %>% group_by(Period) %>% summarise(Y = mean(Ptros, na.rm = T)), aes(x = Period, y = Y, group = 1), color = "red", size = 1)



ggplot(tuv_all, aes(x = Period, y = N)) +  geom_boxplot(aes(fill = Habitat), varwidth = T)  + geom_hline(yintercept = mean(tuv_all$N, na.rm = T))  + geom_path(data = tuv_all %>% group_by(Period) %>% summarise(Y = mean(N, na.rm = T)), aes(x = Period, y = Y, group = 1), color = "red", size = 1)


ggplot(tuv_all, aes(x = Period, y = OGP)) +  geom_boxplot(aes(fill = Habitat), varwidth = T)  + geom_hline(yintercept = mean(tuv_all$OGP, na.rm = T))  + geom_path(data = tuv_all %>% group_by(Period) %>% summarise(Y = mean(OGP, na.rm = T)), aes(x = Period, y = Y, group = 1), color = "red", size = 1)



# Пермутационная оценка переходов от одного периода к другому


transition <- function(Param = "Ptros", perm = 999) {
  require(dplyr)
  require(reshape2)
  d <- tuv_all %>% group_by(Transect, Depth, Period) %>% 
    summarize(n = mean(get(Param), na.rm = T)) %>% 
    dcast(Transect + Depth ~ Period) %>% 
    select(-c(Transect, Depth))
  
  D_N_1_2 <- sum(d[,2] - d[,1], na.rm = T) / sum(!is.na(d[,2] - d[,1]))
  deltas_1_2 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(d[sample(1:nrow(d)),2] - d[sample(1:nrow(d)),1], na.rm = T) / sum(!is.na(d[sample(1:nrow(d)),2] - d[sample(1:nrow(d)),1]))
    deltas_1_2[i] <- D_N_perm
    i<-i+1
  }
  deltas_1_2[perm] <- D_N_1_2 
  
  D_N_2_3 <- sum(d[,3] - d[,2], na.rm = T) / sum(!is.na(d[,3] - d[,2]))
  deltas_2_3 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(d[sample(1:nrow(d)),3] - d[sample(1:nrow(d)),2], na.rm = T) / sum(!is.na(d[sample(1:nrow(d)),3] - d[sample(1:nrow(d)),2]))
    deltas_2_3[i] <- D_N_perm
    i<-i+1
  }
  deltas_2_3[perm] <- D_N_2_3 
  
  
  
  D_N_3_4 <- sum(d[,4] - d[,3], na.rm = T) / sum(!is.na(d[,4] - d[,3]))
  deltas_3_4 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(d[sample(1:nrow(d)),4] - d[sample(1:nrow(d)),3], na.rm = T) / sum(!is.na(d[sample(1:nrow(d)),4] - d[sample(1:nrow(d)),3]))
    deltas_3_4[i] <- D_N_perm
    i<-i+1
  }
  deltas_3_4[perm] <- D_N_3_4 
  
  DN_perm_1_2 <- data.frame(Compare = "1 vs 2", Delta = deltas_1_2)
  DN_perm_2_3 <- data.frame(Compare = "2 vs 3", Delta = deltas_2_3)
  DN_perm_3_4 <- data.frame(Compare = "3 vs 4", Delta = deltas_3_4)
  
  DN_perm <- rbind(DN_perm_1_2, DN_perm_2_3, DN_perm_3_4)
  DN <- data.frame(Compare = c("1 vs 2","2 vs 3", "3 vs 4"), Delta = c(D_N_1_2, D_N_2_3, D_N_3_4))
  
  
  ggplot(DN_perm, aes(x = Compare, y = Delta)) + geom_boxplot() + geom_hline(yintercept = 0) + geom_point(data = DN, shape = 21,  fill = "yellow", size = 3) + ggtitle(Param)
}


Pl1 <- transition(Param = "Ptros", perm = 9999)

Pl2 <- transition(Param = "N", perm = 9999)

Pl3 <- transition(Param = "W", perm = 9999)

Pl4 <- transition(Param = "OGP", perm = 9999)

Pl5 <- transition(Param = "max_L", perm = 9999)

Pl6 <- transition(Param = "Age2_3", perm = 9999)

Pl7 <- transition(Param = "Age4_6", perm = 9999)

Pl8 <- transition(Param = "Age7_9", perm = 9999)

Pl9 <- transition(Param = "Age10_12", perm = 9999)

library(cowplot)

plot_grid(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6, Pl7, Pl8, Pl9, ncol = 3)


# Пробую построить смешанную модель динамики показателей, где случайным фактором выступает трансекта


library(mgcv)

Mod_ptros <- gamm(log(N) ~ s(Year, k = 6), random = list(Transect = ~1),  data = tuv_all)

summary(Mod_ptros$gam)

plot(Mod_ptros$gam, residuals = T, rug = T, )

gam.check(Mod_ptros$gam)

colSums(is.na(tuv_all))

# по первому периоду есть точки без OGP и без Ptros, убираю их
tuv_all <- tuv_all[!is.na(tuv_all$OGP), ]

row.names(tuv_all) <- tuv_all$Sample_ID

# убираю ненужные для СА данные
tuv_all_ca2 <- (tuv_all[ , -c(1:8)])

# СА
tuv_ca <- cca(tuv_all_ca2, scale = TRUE)
summary(tuv_ca)
screeplot(tuv_ca,  bstick = TRUE)

plot(tuv_ca)
# не нравится мне, что он перевернулся..

# делаю датафрейм для графика
tuv_ca_f <- fortify(tuv_ca, scaling = "symmetric")
tuv_ca_f_site <- tuv_ca_f[tuv_ca_f$Score == "sites", ]
tuv_ca_f_site$Transect <- tuv_all$Transect
tuv_ca_f_site$Depth <- tuv_all$Depth
tuv_ca_f_site$Period <- tuv_all$Period
tuv_ca_f_site$Monitoring <- tuv_all$Monitoring
tuv_ca_f_sp <- tuv_ca_f[tuv_ca_f$Score == "species", ]

# график. я всё-таки возьму грех на душу и переверну ось 2. на графике форма маркеров такая же, и размер также пропорционален глубине, как и на графике с ССА по 09-10 гг. Цвета отражают разные периоды: красный - 1, черный - 2 (те точки, которые пересекаются с другими периодами), темно-серый - банка 2010 года, светло-серый - 2 (те точки, которые не с чем сравнивать), синий - 3, бирюзовый - 4. 

ggplot(tuv_ca_f_site, aes(x = CA1, y = -CA2)) +  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, size = Depth, shape = Transect, color = Monitoring, stroke = 2.5)) + scale_fill_manual(values = c("red", "black", "azure4","lightgrey", "blue2",  "aquamarine3")) + scale_color_manual(values = c("red", "black", "azure4","lightgrey", "blue2",  "aquamarine3")) + scale_shape_manual(values = c(22, 21, 23, 25, 24, 11, 4, 8)) + scale_size(range = c(0.5, 5.5)) + theme(legend.position ="") + geom_text_repel(data = tuv_ca_f_sp, aes(label = Label), color = "darkviolet", size = 4) 

# рисуем фасетки для 6 точек, чтобы отследить, как одни и те же мониторинговые точки меняют свою ординацию во времени. СА2 тоже переворачиваю
BS_05 <- tuv_ca_f_site %>%  filter(Label %in% c('BS_0.5_04', 'BS_0.5_09', 'BS_0.5_10', 'BS_0.5_12', "BS_0.5_18"))
BS_05g <- ggplot(BS_05, aes(x = CA1, y = -CA2)) +  ylim(-2, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, color = Monitoring, stroke = 2.5), size = 1, shape = 21) + scale_fill_manual(values = c("red", "black", "azure4", "blue2",  "aquamarine3")) + scale_color_manual(values = c("red", "black", "azure4", "blue2",  "aquamarine3"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 


BS_15 <- tuv_ca_f_site %>%  filter(Label %in% c('BS_-1.5_04', "BS_-1.5_09", 'BS_-1.5_18'))
BS_15g <- ggplot(BS_15, aes(x = CA1, y = -CA2)) +  ylim(-2, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, color = Monitoring, stroke = 2.5), size = 2, shape = 21) + scale_fill_manual(values = c("red", "black", "aquamarine3")) + scale_color_manual(values = c("red", "black", "aquamarine3"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

MidN_05 <- tuv_ca_f_site %>%  filter(Label %in% c('MidN_0.5_04', "MidN_0.5_09", 'MidN_0.5_12', 'MidN_0.5_18'))
MidN_05g <- ggplot(MidN_05, aes(x = CA1, y = -CA2)) +  ylim(-2, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, color = Monitoring, stroke = 2.5), size = 1, shape = 23) + scale_fill_manual(values = c("red", "black", "blue2",  "aquamarine3")) + scale_color_manual(values = c("red","black", "blue2",  "aquamarine3"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 

MidN_15 <- tuv_ca_f_site %>%  filter(Label %in% c('MidN_-1.5_12', 'MidN_-1.5_18', 'MidN_-1.5_09'))
MidN_15g <- ggplot(MidN_15, aes(x = CA1, y = CA2)) +  ylim(-2, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, color = Monitoring, stroke = 2.5), size = 2, shape = 23) + scale_fill_manual(values = c("black", "blue2",  "aquamarine3")) + scale_color_manual(values = c("black", "blue2",  "aquamarine3"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 

MoS_05 <- tuv_ca_f_site %>%  filter(Label %in% c('MoS_0.5_04', "MoS_0.5_09", 'MoS_0.5_12', 'MoS_0.5_18'))
MoS_05g <- ggplot(MoS_05, aes(x = CA1, y = CA2)) +  ylim(-2, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, color = Monitoring, stroke = 2.5), size = 1, shape = 24) + scale_fill_manual(values = c("red", "black", "blue2",  "aquamarine3")) + scale_color_manual(values = c("red","black", "blue2",  "aquamarine3"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

MoS_15 <- tuv_ca_f_site %>%  filter(Label %in% c('MoS_-1.5_12', 'MoS_-1.5_18', 'MoS_-1.5_09'))
MoS_15g <- ggplot(MoS_15, aes(x = CA1, y = CA2)) +  ylim(-2, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, color = Monitoring, stroke = 2.5), size = 2, shape = 24) + scale_fill_manual(values = c("black", "blue2",  "aquamarine3")) + scale_color_manual(values = c("black", "blue2",  "aquamarine3"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank()) 

# собираем вместе 6 фасеток. потом можно прилепить их к основному графикуб выкачав в правильном размере
ggarrange(BS_05g, BS_15g, MidN_05g, MidN_15g, MoS_05g, MoS_15g, ncol=1)


# теперь я реализую вашу идею с боксплотами от остатков - ВМ сам пересчитывает..
# 
# мне не нравится брать те точки из 09-10 года, которые никогда ни с чем не пересекаются. поэтому я их уберу
# 
# tuv_all_constant4 <- tuv_all %>%  filter(Sample_ID %in% c("BN_1.5_04", 'BS_2_05', 'BS_1.5_04', 'BS_1_04', 'BS_0.5_04', 'BS_-0.5_04', 'BS_-1.5_04', 'MidN_2_04', 'MidN_1.5_04', 'MidN_1_04', 'MidN_0.5_04', 'MoN_2_04', 'MoN_1.5_04', 'MoN_1_04', 'MoN_0.5_04', 'MoS_1.5_04', 'MoS_1_04', 'MoS_0.5_04', 'SS_0.5_05', 'R_0.5_05', 'BN_1.5_09', 'BS_2_09', 'BS_1.5_09', 'BS_1_09', 'BS_0.5_09', "BS_-0.5_09", "BS_-1.5_09", "MidN_2_09", "MidN_1.5_09", "MidN_1_09", "MidN_0.5_09", 'MoN_2_10', 'MoN_1.5_10', 'MoN_1_10', 'MoN_0.5_10', 'MoS_1.5_09', 'MoS_1_09', 'MoS_0.5_09', 'SS_0.5_10', 'R _0.5_09', 'MoS_0.5_12', 'MoS_0.5_18', 'MoS_-1.5_12', 'MoS_-1.5_18', 'MoS_-1.5_09', 'MidN_0.5_12', 'MidN_0.5_18', 'MidN_-1.5_12', 'MidN_-1.5_18', 'MidN_-1.5_09', 'BS_0.5_12', "BS_0.5_18", 'BS_-1.5_18'))
# 
# tuv_predictors2 <-  tuv_all_constant4 %>% select(Transect, Depth, Period, Distance)
# 
# tuv_all_constant_ca_res <- cca(tuv_all_constant4[,-c(1:7)] ~ Distance + Depth,  data = tuv_all_constant4)
# 
# plot(tuv_all_constant_ca_res, display = c( "cn","species"), scaling = "symmetric")
# plot(tuv_all_constant_ca_res, display = c( "cn","sites"), scaling = "symmetric")
# 
# anova(tuv_all_constant_ca_res)
# anova(tuv_all_constant_ca_res, by="axis")
# anova(tuv_all_constant_ca_res, by="margin")
# # глубина тут не значима..но пусть будет 
# 
# scores(tuv_all_constant_ca_res, choices = 1:9)
# 
# # посмотрим остатки от модели
# CA1_2_scores <- as.data.frame(scores(tuv_all_constant_ca_res, choices = 3:4)$sites)
# # делаю расстояние и глубину факторами
# tuv_all_constant4$f_Distance <- factor(tuv_all_constant4$Distance)
# tuv_all_constant4$f_Depth <- factor(tuv_all_constant4$Depth)
# 
# CA1_2_scores$Distance <- tuv_all_constant4$Distance
# CA1_2_scores$Transect <- tuv_all_constant4$Transect
# CA1_2_scores$Depth <- tuv_all_constant4$Depth
# CA1_2_scores$f_Distance <- tuv_all_constant4$f_Distance
# CA1_2_scores$f_Depth <- tuv_all_constant4$f_Depth
# CA1_2_scores$Period <- tuv_all_constant4$Period
# 
# str(CA1_2_scores)
# 
# # рисую боксплоты для оси 1. если я правильно понимаю, раз вторая ось не значима, значит, мы её и не смотрим
# distCA1f <- ggplot(CA1_2_scores, aes(x = f_Distance, y = CA1)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + theme(legend.position ="")
# 
# depthCA1f <- ggplot(CA1_2_scores, aes(x = f_Depth, y = CA1)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + theme(legend.position ="")
# 
# transectCA1f <- ggplot(CA1_2_scores, aes(x = Transect, y = CA1)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + theme(legend.position ="")
# 
# ggarrange(distCA1f, depthCA1f, transectCA1f, ncol = 1)
# #  и чего? годится это или нет?
# 
# # может неправильно делать расстояние и глубину факторами? 
# # рисую точки и геом_смуз
# distCA1 <- ggplot(CA1_2_scores, aes(x = Distance, y = CA1)) + geom_hline(yintercept = 0) + geom_point() + theme_bw() + geom_smooth() + theme(legend.position ="")
# 
# depthCA1 <- ggplot(CA1_2_scores, aes(x = Depth, y = CA1)) + geom_hline(yintercept = 0) + geom_point() + theme_bw() + geom_smooth() + theme(legend.position ="")
# 
# ggarrange(distCA1, depthCA1, ncol = 1)
# 
# # допустим, что всё ок. рисую боксплоты по периодам
# box_period <- ggplot(CA1_2_scores, aes(x = Period, y = CA1, color = Period)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + scale_color_manual(values = c("red", "black", "blue2",  "aquamarine3")) + theme(legend.position ="")
# 
# scores(tuv_all_constant_ca_res, choices = 1:9)

# если боксплот в положительной области, значит, там было много Age10_12 и Age7_9
# если боксплот в отрицательной области, значит, там было много Age2_3 и N


# перманова по периодам 1 и 2
# отбираю выборки, которые есть и там и там
# 
tuv_all_constant5 <- tuv_all %>%  filter(Sample_ID %in% c("BN_1.5_04", 'BS_2_05', 'BS_1.5_04', 'BS_1_04', 'BS_0.5_04', 'BS_-0.5_04', 'BS_-1.5_04', 'MidN_2_04', 'MidN_1.5_04', 'MidN_1_04', 'MidN_0.5_04', 'MoN_2_04', 'MoN_1.5_04', 'MoN_1_04', 'MoN_0.5_04', 'MoS_1.5_04', 'MoS_1_04', 'MoS_0.5_04', 'SS_0.5_05', 'R_0.5_05', 'BN_1.5_09', 'BS_2_09', 'BS_1.5_09', 'BS_1_09', 'BS_0.5_09', "BS_-0.5_09", "BS_-1.5_09", "MidN_2_09", "MidN_1.5_09", "MidN_1_09", "MidN_0.5_09", 'MoN_2_10', 'MoN_1.5_10', 'MoN_1_10', 'MoN_0.5_10', 'MoS_1.5_09', 'MoS_1_09', 'MoS_0.5_09', 'SS_0.5_10', 'R _0.5_09'))

perm_period <- tuv_all_constant5 %>% select(Period, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, Habitat) %>% as.data.frame()

perm_period_log <- log(perm_period[2:9] + 1)

# стандартизирую переменные
# perm_period$Age2_3 <- (perm_period$Age2_3 - (mean(perm_period$Age2_3))) / var(perm_period$Age2_3)
# perm_period$Age4_6 <- (perm_period$Age4_6 - (mean(perm_period$Age4_6))) / var(perm_period$Age4_6)
# perm_period$Age7_9 <- (perm_period$Age7_9 - (mean(perm_period$Age7_9))) / var(perm_period$Age7_9)
# perm_period$Age10_12 <- (perm_period$Age10_12 - (mean(perm_period$Age10_12))) / var(perm_period$Age10_12)
# perm_period$N <- (perm_period$N - (mean(perm_period$N))) / var(perm_period$N)
# perm_period$W <- (perm_period$W - (mean(perm_period$W))) / var(perm_period$W)
# perm_period$OGP <- (perm_period$OGP - (mean(perm_period$OGP))) / var(perm_period$OGP)
# perm_period$max_L <- (perm_period$max_L - (mean(perm_period$max_L))) / var(perm_period$max_L)

permanova_period <- adonis(perm_period_log ~ perm_period$Period * perm_period$Habitat, permutation = 99999, method = "bray")

# а дальше я не знаю, как правильно всмотреть это взаимодействие. наверное, его смотреть не нужно, потому что оно не значимо? и нужно просто смотреть период и хэбитат по отдельности?
 
# проверка равенства внутригрупповых дисперсий
dist_period <- vegdist(perm_period_log, method = "bray")
PCO_period <- betadisper(dist_period, perm_period$Period)
plot(PCO_period)

PCO_period_hab <- betadisper(dist_period, perm_period$Habitat)
plot(PCO_period_hab)

# достоверность различий отклонений от центроидов в разных группах проверяется с помощью процедуры
anova(PCO_period)
boxplot(PCO_period)

anova(PCO_period_hab)
boxplot(PCO_period_hab)
# достоверных различий разброса внутригрупповых расстояний не выявлено

# post-hoc tests habitat как тут правильно сделать пост-хок? взаимодействие ведь есть
pair <- combn(unique(as.character(perm_period$Habitat)), 2)
ncomb <- dim(pair)[2]
x <- perm_period_log
y <- perm_period$Habitat
for (i in 1:ncomb) {
  filter <- y %in% pair[, i]
  posthoc <- adonis(x[filter, ] ~ y[filter], method = "bray")$aov.tab$Pr[1]
  cat(pair[, i], ": p = ", posthoc, "\n", sep = " ")
}

# отмели отличаются от всего остального. а банка и скалы попали в одну секту после поправки бонферрони
0.05/3

# симпер
simper_period <- simper(perm_period_log, group = perm_period$Period, permutations = 9999)
summary(simper_period)

# сравниваем демогр признаки между 1 и 2 периодом

# критерий Вилкоксона

period1 <- tuv_all_constant5 %>%  filter(Period %in% "1")
period2 <- tuv_all_constant5 %>%  filter(Period %in% "2")


wilcox.test(period1$Age2_3, period2$Age2_3, paired = T)
wilcox.test(period1$N, period2$N, paired = T)
wilcox.test(period1$Age4_6, period2$Age4_6, paired = T)
wilcox.test(period1$Age7_9, period2$Age7_9, paired = T)
wilcox.test(period1$Age10_12, period2$Age10_12, paired = T)
wilcox.test(period1$W, period2$W, paired = T)
wilcox.test(period1$OGP, period2$OGP, paired = T)
wilcox.test(period1$max_L, period2$max_L, paired = T)

wilcox.test(log(period1$Age2_3 + 1), log(period2$Age2_3 + 1), paired = T)
wilcox.test(log(period1$N + 1), log(period2$N +1), paired = T)
wilcox.test(log(period1$Age4_6 + 1), log(period2$Age4_6 + 1), paired = T)
wilcox.test(log(period1$Age7_9 + 1), log(period2$Age7_9 + 1), paired = T)
wilcox.test(log(period1$Age10_12 + 1), log(period2$Age10_12 + 1), paired = T)
wilcox.test(log(period1$W +1), log(period2$W + 1), paired = T)
wilcox.test(log(period1$OGP + 1), log(period2$OGP + 1), paired = T)
wilcox.test(log(period1$max_L + 1), log(period2$max_L + 1), paired = T)

0.05/8




# берём 5 мониторинговых точек по 2-4 периоду
tuv_all_constant6 <- tuv_all %>%  filter(Sample_ID %in% c('BS_0.5_09',  "MidN_0.5_09", 'MoS_0.5_09', "MidN_-1.5_09", 'MoS_-1.5_09', 'BS_0.5_12',  "MidN_0.5_12", 'MoS_0.5_12', "MidN_-1.5_12", 'MoS_-1.5_12', 'BS_0.5_18',  "MidN_0.5_18", 'MoS_0.5_18', "MidN_-1.5_18", 'MoS_-1.5_18'))

perm_period2 <- tuv_all_constant6 %>% select(Period, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, Ptros, Habitat) %>% as.data.frame()

a <- metaMDS(perm_period2[2:10])
plot(a, type = "t")

perm_period2_log <- log(perm_period2[2:10] + 1)

permanova_period2 <- adonis(log(perm_period2[2:10] + 1) ~ perm_period2$Period * perm_period2$Habitat, permutation = 99999, method = "bray")

# проверка равенства внутригрупповых дисперсий
dist_period2 <- vegdist(perm_period2_log, method = "bray")
PCO_period2 <- betadisper(dist_period2, perm_period2$Period)
plot(PCO_period2)

PCO_period_hab2 <- betadisper(dist_period2, perm_period2$Habitat)
plot(PCO_period_hab2)

# от периода к периоду ничего не менялось, но разницу между местообитаниями он видит?

# берём 3 мониторинговых точки по всем периодам
tuv_all_constant7 <- tuv_all %>%  filter(Sample_ID %in% c('BS_0.5_04',  "MidN_0.5_04", 'MoS_0.5_04', 'BS_0.5_09',  "MidN_0.5_09", 'MoS_0.5_09', 'BS_0.5_12',  "MidN_0.5_12", 'MoS_0.5_12',  'BS_0.5_18',  "MidN_0.5_18", 'MoS_0.5_18' ))

Pl1 <- ggplot(tuv_all_constant7, aes(x = as.factor(Period), y = log(N) )) + geom_boxplot() 

Pl2 <- ggplot(tuv_all, aes(x = as.factor(Period), y = log(N) ) ) + geom_boxplot() 

library(cowplot)
plot_grid(Pl1, Pl2)





perm_period2 <- tuv_all_constant6 %>% select(Period, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, Ptros, Habitat) %>% as.data.frame()

a <- metaMDS(perm_period2[2:10])
plot(a, type = "t")

perm_period2_log <- log(perm_period2[2:10] + 1)

permanova_period2 <- adonis(log(perm_period2[2:10] + 1) ~ perm_period2$Period * perm_period2$Habitat, permutation = 99999, method = "bray")

# проверка равенства внутригрупповых дисперсий
dist_period2 <- vegdist(perm_period2_log, method = "bray")
PCO_period2 <- betadisper(dist_period2, perm_period2$Period)
plot(PCO_period2)

PCO_period_hab2 <- betadisper(dist_period2, perm_period2$Habitat)
plot(PCO_period_hab2)




#####################################################################################################################

# считаем фукус-грунт
tuv_sub <- read.table("TuMyt_Ptros.csv", header = T, sep = ";")
str(tuv_sub)

# берём вместе сублитораль и литораль в Тюве
WS <- tuv_sub %>% filter(sea == "WS")
perms <- rep(NA, 1000)

for(i in 1:1000){
  ind <- sample(1:nrow(WS), 5)
  perms[i] <- mean(WS$algae[ind] - WS$bottom[ind])
}

hist(perms)

BS <- tuv_sub %>% filter(sea == "BS")

obs_BS <- mean(BS$algae - BS$bottom)

perms <- data.frame(perm = perms)

perms <- perms %>% mutate(Q = case_when(perm<= quantile(perms$perm)[2] ~ 1,
                                          perm> quantile(perms$perm)[2] & perm <= quantile(perms$perm)[3] ~ 2, 
                                          perm> quantile(perms$perm)[3] & perm <= quantile(perms$perm)[4] ~ 3,
                                          perm> quantile(perms$perm)[4] ~ 4))


l_sl <- ggplot(perms, aes(x=perm))  + geom_histogram(aes(fill = Q), color="lightgrey") + geom_vline(xintercept = quantile(perms$perm), color = "blue", size = 1) + geom_point(aes(x=obs_BS, y = 0), fill = "red", size = 6, shape = 21) + theme_bw()  + xlim(0, 0.5)


# берём только литораль в Тюве
WS2 <- tuv_sub %>% filter(sea == "WS")
perms2 <- rep(NA, 1000)

for(i in 1:1000){
  ind <- sample(1:nrow(WS2), 3)
  perms2[i] <- mean(WS2$algae[ind] - WS2$bottom[ind])
}

hist(perms2)

BS2 <- tuv_sub %>% filter(sea == "BS") %>% filter(hor == "lit")

obs_BS2 <- mean(BS2$algae - BS2$bottom)

perms2 <- data.frame(perm2 = perms2)

perms2 <- perms2 %>% mutate(Q = case_when(perm2<= quantile(perms2$perm2)[2] ~ 1,
                                perm2> quantile(perms2$perm2)[2] & perm2 <= quantile(perms2$perm2)[3] ~ 2, 
                                perm2> quantile(perms2$perm2)[3] & perm2 <= quantile(perms2$perm2)[4] ~ 3,
                                perm2> quantile(perms2$perm2)[4] ~ 4))


l <- ggplot(perms2, aes(x=perm2))+ xlim(0, 0.5)  + geom_histogram(aes(fill = Q), color="lightgrey") + geom_vline(xintercept = quantile(perms2$perm2), color = "blue", size = 1) + geom_point(aes(x=obs_BS2, y = 0), fill = "red", size = 6, shape = 21) + theme_bw() 

ggarrange(l_sl, l, nrow = 2)

# Добавить сюда х-у диаграмму первым графиком! 
df <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)

xy <- ggplot(WS, aes(x = algae, y = bottom)) + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 1)) + geom_point(aes(size = 3), shape = 21, fill = "azure4", color = "black") + theme_bw() + xlim(0, 1) + ylim(0,1)  + geom_point(data=BS, aes(size = 3, group = hor, shape = hor), color= "red") + theme(legend.position ="")

ggarrange(xy, l_sl, l, nrow = 1)

# добавить Тюву!!! лит и слит разными маркерами!
