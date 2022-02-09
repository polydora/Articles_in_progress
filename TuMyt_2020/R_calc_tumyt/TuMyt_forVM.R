library(ggplot2)
library(vegan)
library(dplyr)
library(ggvegan)
library(readxl)
library(ggpubr)
library(car)
library(glmmTMB)

### АНАЛИЗ ДАННЫХ 2009-2010 

## I. CCA

citation("lme4")

# читаем данные
tuv <- read.table("Data/TuMyt_0910_itog.csv", header = T, sep = ";")

# отделяем нужные данные
tuv_demogr3 <- tuv %>% select(Age2_3, Age4_6, Age7_9, Age10_12, Ptros, N, W, OGP, max_L, size_5) %>% as.data.frame()
row.names(tuv_demogr3) <- tuv$Sample_ID
# Depth здесь не высота над 0 глубин, а именно глубина. То есть сублиторальные выборки со знаком "+", а литоральные - со знаком "-"

tuv_predictors <-  tuv %>% select(Transect, Habitat, Depth, Distance, Slope, Kelps, Width, Exposition, Cover_algae)

str(tuv_predictors)

# подбираем полную модель
tuv_cca4 <- cca(tuv_demogr3 ~ Distance + Depth + Slope + Cover_algae + Kelps + Width + Exposition, data = tuv_predictors)

vif.cca(tuv_cca4) 

summary(tuv_cca4)

plot(tuv_cca4)

# Подбор оптимальной модели
tuv_cca4_0 <- cca(tuv_demogr3 ~ 1, data = tuv_predictors) 

tuv_cca4_reduced <- ordistep(tuv_cca4_0, scope = formula(tuv_cca4), direction = "forward", permutations = 9999)

vif.cca(tuv_cca4_reduced)

summary(tuv_cca4_reduced)

# оценка значимости модели, осей, предикторов
anova(tuv_cca4_reduced, permutations = 9999)
anova(tuv_cca4_reduced, by = "axis", permutations = 9999)
anova(tuv_cca4_reduced, by = "margin", permutations = 9999)

# визуализация с разными типами scaling
plot(tuv_cca4_reduced, scaling = "site")
plot(tuv_cca4_reduced, scaling = "species")
plot(tuv_cca4_reduced, scaling = "symmetric")

# поиск множителя для стрелочек
ordiArrowMul(scores(tuv_cca4_reduced, scaling = "species"), display = c("species"))

cca.res <- summary(tuv_cca4_reduced)
cca.sites <- cca.res$sites
cca.species <- cca.res$species

mul <- ggvegan:::arrowMul(cca.res$biplot, rbind(cca.sites, cca.species))

mul2 <- ordiArrowMul(cca.res, display = "species")

# создаем датафрейм для графика
demogr_scores <- fortify(tuv_cca4_reduced, scaling = "symmetric", display = c("sp","wa","bp"))

demogr_scores_site <- demogr_scores[demogr_scores$Score == "sites", ]  
demogr_scores_site$Habitat <- tuv_predictors$Habitat
demogr_scores_sp <- demogr_scores[demogr_scores$Score == "species", ] 
demogr_scores_cons <- demogr_scores[demogr_scores$Score == "biplot", ] 

# вариант графика c формой точек и цветом по Habitat
cca_itog <- ggplot(demogr_scores_site, aes(x = CCA1, y = CCA2)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_segment(data = demogr_scores_cons, aes(x = 0, y = 0, xend = CCA1 * mul2, yend = CCA2 * mul2), color = "black", arrow = arrow(type = "closed", angle = 8), size = 1) + 
  geom_point(aes(fill = Habitat, shape = Habitat), color = "black", size = 5) + 
  scale_fill_manual(values = c("blue", "chartreuse3", "azure4", "orange")) + 
  scale_shape_manual(values = c(21, 23, 24, 22)) + 
  theme_bw() +  
  theme(legend.position ="") + 
  geom_text_repel(data = demogr_scores_sp, aes(label = Label), color = "darkviolet", size = 4.5) + 
  geom_text_repel(data = demogr_scores_cons, aes(x = CCA1 * mul2, y = CCA2 * mul2, label = Label), color = "black", size = 5)


## II. PERMANOVA

# отбор и трансформация данных
perm_hab <- tuv %>% select(Habitat, Age2_3, Age4_6, Age7_9, Age10_12, Ptros, N, W, OGP, max_L, size_5) %>% as.data.frame()

perm_hab_log <- log(perm_hab[2:11] + 1)

# анализ
permanova_habitat <- adonis(perm_hab_log ~ perm_hab$Habitat, method = "bray", permutations = 9999)

# проверка равенства внутригрупповых дисперсий
dist_habitat <- vegdist(perm_hab_log, method = "bray")
PCO_habitat <- betadisper(dist_habitat, perm_hab$Habitat)
plot(PCO_habitat)

anova(PCO_habitat)
boxplot(PCO_habitat)

# ну, на этом, как кажется всё. можно ещё делать постхоки и симпер, но нужно ли? сделав одну перманову, мы просто формально ответили на вопрос, отличаются ли местообитания между собой.


## III. Регрессионный анализ зависимости Ptros от environmental parameters
# читаем данные
tuv_Ptros <- read.table("TuMyt0910_Ptros.csv", header = T, sep = ";")
str(tuv_Ptros)

# транформация и стандартизация данных
tuv_Ptros$Sample_ID <- as.factor(tuv_Ptros$Sample_ID)
tuv_Ptros$Transect <- as.factor(tuv_Ptros$Transect)
tuv_Ptros$Habitat <- as.factor(tuv_Ptros$Habitat)
tuv_Ptros$Exposition <- as.factor(tuv_Ptros$Exposition)
tuv_Ptros$Substrate <- as.factor(tuv_Ptros$Substrate)
tuv_Ptros$Kelps <- as.factor(tuv_Ptros$Kelps)
tuv_Ptros$Depth3 <- scale(tuv_Ptros$Depth)[,1]
tuv_Ptros$Cover_algae3 <- scale(tuv_Ptros$Cover_algae) [,1]
tuv_Ptros$Distance3 <- scale(tuv_Ptros$Distance) [,1]
tuv_Ptros$Slope3 <- scale(tuv_Ptros$Slope) [,1]
tuv_Ptros$Width3 <- scale(tuv_Ptros$Width) [,1]

str(tuv_Ptros)

# вот так бы выглядела полная модель
# вот полный набор предикторов: Ptros ~ Depth3 + Exposition  + Cover_algae3 + Kelps + Distance3 + Slope3 + Width3

# по vif убираем мультиколинеарные предикторы
vif(lm(Ptros ~ Depth3 + Exposition  + Cover_algae3 + Kelps + Distance3 + Slope3 + Width3, data=tuv_Ptros))

# в итоге из предикторов остались Depth3 + Exposition  + Cover_algae3 + Distance3 (убирать начали с кельпов)

# считаем такую модель
Mod_Ptros2 <- glmmTMB(Ptros ~ Depth3 + Exposition  + Cover_algae3 + Distance3 + (1|Transect), family=beta_family(link = "logit"), control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")), data=tuv_Ptros)

# проверим, можно ли её упростить
drop1(Mod_Ptros2)
# Ничего упрощать не нужно
 
summary(Mod_Ptros2)
Anova(Mod_Ptros2)

# проверка условий применимости
# остатки
plot(x=fitted(Mod_Ptros2), y=residuals(Mod_Ptros2))
qplot(x=fitted(Mod_Ptros2), y=residuals(Mod_Ptros2)) + geom_smooth()
# ну, бугорок есть..но вроде не критичен

# overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(Mod_Ptros2)


#### АНАЛИЗ ВРЕМЕННОЙ ДИНАМИКИ

### анализ динамики демографической структуры

## PERMANOVA I vs II

# читаем данные
tuv_all <- read.table("Data/TuMyt_all_itog.csv", header = T, sep = ";")
str(tuv_all)

tuv_all$Period <- factor(tuv_all$Period)

# отделяю нужные для всех анализов данные 
tuv_all <- tuv_all %>% dplyr::select(Sample_ID, Period, Distance, Transect, Habitat, Depth, Monitoring, Ptros, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, size_5) %>% as.data.frame()

colSums(is.na(tuv_all))

# по первому периоду есть точки без OGP и размеров (это одни и те же поселения), убираю их
tuv_all <- tuv_all[!is.na(tuv_all$OGP), ]

tuv_all <- tuv_all[!is.na(tuv_all$size_5), ]

row.names(tuv_all) <- tuv_all$Sample_ID

# отбираю выборки, которые изучались как в 1, так и во 2 периоде
tuv_all_constant5 <- tuv_all %>%  filter(Sample_ID %in% c("BN_1.5_04", 'BS_2_05', 'BS_1.5_04', 'BS_1_04', 'BS_0.5_04', 'BS_-0.5_04', 'BS_-1.5_04', 'MidN_2_04', 'MidN_1.5_04', 'MidN_1_04', 'MidN_0.5_04', 'MoN_2_04', 'MoN_1.5_04', 'MoN_1_04', 'MoN_0.5_04', 'MoS_1.5_04', 'MoS_1_04', 'MoS_0.5_04', 'SS_0.5_05', 'R_0.5_05', 'BN_1.5_09', 'BS_2_09', 'BS_1.5_09', 'BS_1_09', 'BS_0.5_09', "BS_-0.5_09", "BS_-1.5_09", "MidN_2_09", "MidN_1.5_09", "MidN_1_09", "MidN_0.5_09", 'MoN_2_10', 'MoN_1.5_10', 'MoN_1_10', 'MoN_0.5_10', 'MoS_1.5_09', 'MoS_1_09', 'MoS_0.5_09', 'SS_0.5_10', 'R _0.5_09'))

# отделяю данные для пермановы
perm_period <- tuv_all_constant5 %>% select(Period, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, size_5, Habitat) %>% as.data.frame()

# трансформация данных
perm_period_log <- log(perm_period[2:10] + 1)

# анализ
permanova_period <- adonis(perm_period_log ~ perm_period$Period * perm_period$Habitat, permutation = 9999, method = "bray")
# взаимодействия нет, значит, изменения во всех типах местообитаний произошло однонаправленно

# проверка условия применимости
dist_period <- vegdist(perm_period_log, method = "bray")

PCO_period <- betadisper(dist_period, perm_period$Period)
plot(PCO_period)
anova(PCO_period)
boxplot(PCO_period)

PCO_period_hab <- betadisper(dist_period, perm_period$Habitat)
plot(PCO_period_hab)
anova(PCO_period_hab)
boxplot(PCO_period_hab)

## PERMANOVA II vs III vs IV

# отделяю 5 мониторинговых поселений, которые изучались в каждый из периодов
tuv_all_constant6 <- tuv_all %>%  filter(Sample_ID %in% c('BS_0.5_09',  "MidN_0.5_09", 'MoS_0.5_09', "MidN_-1.5_09", 'MoS_-1.5_09', 'BS_0.5_12',  "MidN_0.5_12", 'MoS_0.5_12', "MidN_-1.5_12", 'MoS_-1.5_12', 'BS_0.5_18',  "MidN_0.5_18", 'MoS_0.5_18', "MidN_-1.5_18", 'MoS_-1.5_18'))

perm_period2 <- tuv_all_constant6 %>% select(Period, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, size_5, Habitat) %>% as.data.frame()

perm_period2_log <- log(perm_period2[2:10] + 1)

permanova_period2 <- adonis(perm_period2_log ~ perm_period2$Period * perm_period2$Habitat, permutation = 9999, method = "bray")
# во времени, ничего не изменилось


# проверка условия применимости
dist_period2 <- vegdist(perm_period2_log, method = "bray")

PCO_period2 <- betadisper(dist_period2, perm_period2$Period)
plot(PCO_period2)
anova(PCO_period2)

PCO_period_hab2 <- betadisper(dist_period2, perm_period2$Habitat)
plot(PCO_period_hab2)
anova(PCO_period_hab2)

## SIMPER I vs II
simper_period <- simper(perm_period_log, group = perm_period$Period, permutations = 9999)
summary(simper_period)
# значимо изменилась возрастная структура. Возросла численность молоди, а 4-9 лет снизилась


tuv_all2 <- tuv_all %>% mutate(Log_N_old =  log(Age4_6 + Age7_9 + Age10_12 +1), Log_N_juv  = log(Age2_3 + 1), P_juv = Age2_3/(Age2_3 + Age4_6 + Age7_9 + Age10_12))

ggplot(tuv_all2, aes(x = Log_N_old, y = Log_N_juv)) + geom_point(aes(shape = Habitat, fill = Period), color = "black", size = 4) + facet_wrap(~Habitat) + 
  scale_fill_manual(values = c("black", "red", "lightgrey", "white")) + 
  scale_shape_manual(values = c(21, 23, 24, 22)) + 
  theme_bw() +
  geom_abline() +
  labs(x = "Old mussels abundance (Log)", y = "Joung mussel abundance (Log)")


ggplot(tuv_all2, aes(x = size_5, y = Log_N_old)) + geom_point(aes(shape = Habitat, fill = Period), color = "black", size = 4) 

## CA

# отделяю данные
tuv_all_ca2 <- (tuv_all[ , -c(1:8)])

tuv_ca <- cca(tuv_all_ca2, scale = TRUE)
summary(tuv_ca)

# делаю датафрейм для графика
tuv_ca_f <- scores(tuv_ca, scaling = "symmetric")
tuv_ca_f_site <- as.data.frame(tuv_ca_f$sites)

tuv_all$Monitoring <- factor(tuv_all$Monitoring)

tuv_ca_f_site$Monitoring <- tuv_all$Monitoring

tuv_ca_f_site$Habitat <- tuv_all$Habitat

tuv_ca_f_sp <- as.data.frame(tuv_ca_f$species)

library(ggrepel)

ca_itog <- 
  ggplot(tuv_ca_f_site, aes(x = CA1, y = CA2)) +  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring, shape = Habitat), color = "black", size = 5) + scale_fill_manual(values = c("brown2", "black", "azure4","lightgrey", "yellow","cyan2")) + scale_color_manual(values = c("brown2", "black", "azure4","lightgrey", "black", "black")) + scale_shape_manual(values = c(21, 23, 24, 22)) + geom_text_repel(data = tuv_ca_f_sp, aes(label = row.names(tuv_ca_f_sp)), color = "darkviolet", size = 4.5) +  theme(legend.position ="") 



# тут СА2 перевернута, чтобы график был комплиментарен графику ССА (на итоговом рисунке они будут расположены рядом)

# если захочется, можно ещё рисовать фасетки с мониторинговыми точками
BS_05 <- tuv_ca_f_site %>%  filter(Label %in% c('BS_0.5_04', 'BS_0.5_09', 'BS_0.5_10', 'BS_0.5_12', "BS_0.5_18"))
BS_05g <- ggplot(BS_05, aes(x = CA1, y = -CA2)) +  ylim(-2.25, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring), color = "black", size = 3, shape = 21) + scale_fill_manual(values = c("brown2", "black", "azure4", "yellow","cyan2")) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

BS_15 <- tuv_ca_f_site %>%  filter(Label %in% c('BS_-1.5_04', "BS_-1.5_09", 'BS_-1.5_18'))
BS_15g <- ggplot(BS_15, aes(x = CA1, y = -CA2)) +  ylim(-2.25, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring), color = "black", size = 3, shape = 21) + scale_fill_manual(values = c("brown2", "black", "cyan2")) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

MidN_05 <- tuv_ca_f_site %>%  filter(Label %in% c('MidN_0.5_04', "MidN_0.5_09", 'MidN_0.5_12', 'MidN_0.5_18'))
MidN_05g <- ggplot(MidN_05, aes(x = CA1, y = -CA2)) +  ylim(-2.25, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring), color = "black", size = 3, shape = 24) + scale_fill_manual(values = c("brown2", "black", "yellow","cyan2"))  + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

MidN_15 <- tuv_ca_f_site %>%  filter(Label %in% c('MidN_-1.5_12', 'MidN_-1.5_18', 'MidN_-1.5_09'))
MidN_15g <- ggplot(MidN_15, aes(x = CA1, y = CA2)) +  ylim(-2.25, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring), color = "black", size = 3, shape = 23) + scale_fill_manual(values = c("black", "yellow","cyan2")) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

MoS_05 <- tuv_ca_f_site %>%  filter(Label %in% c('MoS_0.5_04', "MoS_0.5_09", 'MoS_0.5_12', 'MoS_0.5_18'))
MoS_05g <- ggplot(MoS_05, aes(x = CA1, y = CA2)) +  ylim(-2.25, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring), color = "black", size = 3, shape = 24) + scale_fill_manual(values = c("brown2", "black", "yellow","cyan2")) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

MoS_15 <- tuv_ca_f_site %>%  filter(Label %in% c('MoS_-1.5_12', 'MoS_-1.5_18', 'MoS_-1.5_09'))
MoS_15g <- ggplot(MoS_15, aes(x = CA1, y = CA2)) +  ylim(-2.25, 2) + xlim(-1.5, 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0) + theme_bw() + geom_point(aes(fill = Monitoring), color = "black", size = 3, shape = 23) + scale_fill_manual(values = c("black", "yellow","cyan2")) + theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), legend.position = "", panel.grid = element_blank())

# собираем вместе 6 фасеток. потом можно прилепить их к основному графику, скачав в правильном размере
ggarrange(BS_05g, BS_15g, MidN_05g, MidN_15g, MoS_05g, MoS_15g, nrow =1)

### анализ динамики таксономической структуры

# я пока ничего умнее теста Вилкоксона не придумала..

# читаем данные
tuv_dyn <- read_excel("TuMyt_Ptros_dyn.xlsx", sheet = "Sheet1")
str(tuv_dyn)

tuv_xy <-  tuv_dyn %>%  filter(Dataset %in% "xy")

tuv_xy$p1 <- as.numeric(tuv_xy$p1)
tuv_xy$p2 <- as.numeric(tuv_xy$p2)
tuv_xy$p3 <- as.numeric(tuv_xy$p3)
tuv_xy$p4 <- as.numeric(tuv_xy$p4)

wilcox.test(tuv_xy$p1, tuv_xy$p2, paired = T)
wilcox.test(tuv_xy$p2, tuv_xy$p3, paired = T)
wilcox.test(tuv_xy$p3, tuv_xy$p4, paired = T)
# значимые изменения только между 1 и 2 периодом, маргинально значимые - между 3 и 4







