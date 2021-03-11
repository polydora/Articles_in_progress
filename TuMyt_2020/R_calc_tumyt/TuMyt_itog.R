library(ggplot2)
library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(readxl)
library(ggpubr)

#####################################################################################################################
# ССА по данным 09-10 года: новый OGP, новый Slope и Cover_algae в баллах

# читаем данные
tuv_demogr2 <- read.table("Data/TuMyt_0910_itog.csv", header = T, sep = ";")
row.names(tuv_demogr2) <- tuv$Sample_ID
colSums(is.na(tuv_demogr2))

# отбор демографических признаков
tuv_demogr2 <- tuv %>% select(Age2_3, Age4_6, Age7_9, Age10_12, Ptros, N, W, OGP, max_L) %>% as.data.frame()

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

# стандартизирую переменные
perm_hab$Age2_3 <- (perm_hab$Age2_3 - (mean(perm_hab$Age2_3))) / var(perm_hab$Age2_3)
perm_hab$Age4_6 <- (perm_hab$Age4_6 - (mean(perm_hab$Age4_6))) / var(perm_hab$Age4_6)
perm_hab$Age7_9 <- (perm_hab$Age7_9 - (mean(perm_hab$Age7_9))) / var(perm_hab$Age7_9)
perm_hab$Age10_12 <- (perm_hab$Age10_12 - (mean(perm_hab$Age10_12))) / var(perm_hab$Age10_12)
perm_hab$Ptros <- (perm_hab$Ptros - (mean(perm_hab$Ptros))) / var(perm_hab$Ptros)
perm_hab$N <- (perm_hab$N - (mean(perm_hab$N))) / var(perm_hab$N)
perm_hab$W <- (perm_hab$W - (mean(perm_hab$W))) / var(perm_hab$W)
perm_hab$OGP <- (perm_hab$OGP - (mean(perm_hab$OGP))) / var(perm_hab$OGP)
perm_hab$max_L <- (perm_hab$max_L - (mean(perm_hab$max_L))) / var(perm_hab$max_L)

permanova_habitat <- adonis(perm_hab[2:9] ~ perm_hab$Habitat, method = "euclidean")
# брея-кёртиса не получается взять, потому что есть отрицательные значения
# мы видим, что местообитания по своей демографической структуре достоверно отличаются

# проверка равенства внутригрупповых дисперсий
dist_habitat <- vegdist(perm_hab[2:9], method = "euclidian")
PCO_habitat <- betadisper(dist_habitat, perm_hab$Habitat)
plot(PCO_habitat)

# достоверность различий отклонений от центроидов в разных группах проверяется с помощью процедуры
anova(PCO_habitat)
boxplot(PCO_habitat)
# достоверных различий разброса внутригрупповых расстояний не выявлено

# post-hoc tests
pair <- combn(unique(as.character(perm_hab$Habitat)), 2)
ncomb <- dim(pair)[2]
x <- perm_hab[, -1]
y <- perm_hab$Habitat
for (i in 1:ncomb) {
  filter <- y %in% pair[, i]
  posthoc <- adonis(x[filter, ] ~ y[filter], method = "euclidean")$aov.tab$Pr[1]
  cat(pair[, i], ": p = ", posthoc, "\n", sep = " ")
}

# а вот теперь не знаю, правильно ли я трактую результаты..
# между собой не отличаются отмели и скалы, а также отмели и кельпы, а всё остальное отличается
# если ввести поправку Бонферрони, то всё отсанется так же
0.05/6

# SIMPER
simper_habitat <- simper(perm_hab[, 2:9], perm_hab$Habitat, permutations = 999)
summary(simper_habitat)
# и тут я не помню, как трактовать результаты. где звёздочки - это значит, что признаки одинаковы? или что различаются? если различаются, то тут какое-то глобальное противоречие с пост-хоками. или я неправильно понимаю пост-хоки

# ну и вообще не знаю, нужна тут эта перманова или нет)) и не знаю, правильно ли я её сделала


#####################################################################################################################

# ДИНАМИКА
# Петру Петровичу очень нравится идея делать СА по всем-всем данным
tuv_all <- read.table("Data/TuMyt_all_itog.csv", header = T, sep = ";")
str(tuv_all)

tuv_all$Period <- factor(tuv_all$Period)
tuv_all$Transect  <- factor(tuv_all$Transect)
tuv_all$Monitoring  <- factor(tuv_all$Monitoring)

# отделяю нужные данные
tuv_all <- tuv_all %>% select(Sample_ID, Period, Distance, Transect, Depth, Monitoring, Ptros, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L) %>% as.data.frame()

colSums(is.na(tuv_all))

# по первому периоду есть точки без OGP и без Ptros, убираю их
tuv_all <- tuv_all[!is.na(tuv_all$OGP), ]

row.names(tuv_all) <- tuv_all$Sample_ID

# убираю ненужные для СА данные
tuv_all_ca2 <- (tuv_all[ , -c(1:7)])

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


# теперь я реализую вашу идею с боксплотами от остатков
# мне не нравится брать те точки из 09-10 года, которые никогда ни с чем не пересекаются. поэтому я их уберу

tuv_all_constant4 <- tuv_all %>%  filter(Sample_ID %in% c("BN_1.5_04", 'BS_2_05', 'BS_1.5_04', 'BS_1_04', 'BS_0.5_04', 'BS_-0.5_04', 'BS_-1.5_04', 'MidN_2_04', 'MidN_1.5_04', 'MidN_1_04', 'MidN_0.5_04', 'MoN_2_04', 'MoN_1.5_04', 'MoN_1_04', 'MoN_0.5_04', 'MoS_1.5_04', 'MoS_1_04', 'MoS_0.5_04', 'SS_0.5_05', 'R_0.5_05', 'BN_1.5_09', 'BS_2_09', 'BS_1.5_09', 'BS_1_09', 'BS_0.5_09', "BS_-0.5_09", "BS_-1.5_09", "MidN_2_09", "MidN_1.5_09", "MidN_1_09", "MidN_0.5_09", 'MoN_2_10', 'MoN_1.5_10', 'MoN_1_10', 'MoN_0.5_10', 'MoS_1.5_09', 'MoS_1_09', 'MoS_0.5_09', 'SS_0.5_10', 'R_0.5_09', 'MoS_0.5_12', 'MoS_0.5_18', 'MoS_-1.5_12', 'MoS_-1.5_18', 'MoS_-1.5_09', 'MidN_0.5_12', 'MidN_0.5_18', 'MidN_-1.5_12', 'MidN_-1.5_18', 'MidN_-1.5_09', 'BS_0.5_12', "BS_0.5_18", 'BS_-1.5_18'))



####################################################################################3


tuv_predictors2 <-  tuv_all %>% select(Transect, Depth, Period, Distance)

dem_dat <- tuv_all %>% select(Age2_3, Age4_6, Age7_9,  Age10_12, N, W,OGP, max_L )

tuv_all_constant_ca_res <- cca(dem_dat ~ Distance + Depth,  data = tuv_predictors2)

plot(tuv_all_constant_ca_res, display = c( "cn","species"), scaling = "symmetric")
plot(tuv_all_constant_ca_res, display = c( "cn","sites"), scaling = "symmetric")

anova(tuv_all_constant_ca_res, by = "axis")


# посмотрим остатки от модели
CA1_2_scores <- as.data.frame(scores(tuv_all_constant_ca_res, choices = 1:8)$sites)
# делаю расстояние и глубину факторами
tuv_all_constant4$f_Distance <- factor(tuv_all_constant4$Distance)
tuv_all_constant4$f_Depth <- factor(tuv_all_constant4$Depth)

CA1_2_scores$Distance <- tuv_all$Distance
CA1_2_scores$Transect <- tuv_all$Transect
CA1_2_scores$Depth <- tuv_all$Depth
CA1_2_scores$f_Distance <- factor(tuv_all$Distance)
CA1_2_scores$f_Depth <- factor(tuv_all$Depth)
CA1_2_scores$Period <- tuv_all$Period

str(CA1_2_scores)

# рисую боксплоты для оси 1. если я правильно понимаю, раз вторая ось не значима, значит, мы её и не смотрим
distCA1f <- ggplot(CA1_2_scores, aes(x = f_Distance, y = CA1)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + theme(legend.position ="")

depthCA1f <- ggplot(CA1_2_scores, aes(x = f_Depth, y = CA1)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + theme(legend.position ="")

transectCA1f <- ggplot(CA1_2_scores, aes(x = Transect, y = CA1)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + theme(legend.position ="")

ggarrange(distCA1f, depthCA1f, transectCA1f, ncol = 1)
#  и чего? годится это или нет?

# может неправильно делать расстояние и глубину факторами? 
# рисую точки и геом_смуз
distCA1 <- ggplot(CA1_2_scores, aes(x = Distance, y = CA1)) + geom_hline(yintercept = 0) + geom_point() + theme_bw() + geom_smooth() + theme(legend.position ="")

depthCA1 <- ggplot(CA1_2_scores, aes(x = Depth, y = CA1)) + geom_hline(yintercept = 0) + geom_point() + theme_bw() + geom_smooth() + theme(legend.position ="")

ggarrange(distCA1, depthCA1, ncol = 1)

# допустим, что всё ок. рисую боксплоты по периодам
box_period <- ggplot(CA1_2_scores, aes(x = Period, y = CA1, color = Period)) + geom_hline(yintercept = 0) + geom_boxplot(size = 1.3) + theme_bw() + scale_color_manual(values = c("red", "black", "blue2",  "aquamarine3")) + theme(legend.position ="")

scores(tuv_all_constant_ca_res, choices = 1:9)

# если боксплот в положительной области, значит, там было много Age10_12 и Age7_9
# если боксплот в отрицательной области, значит, там было много Age2_3 и N

# перманову сделать не успела, надо срочно выбегать с кафедры (форс-мажор)



