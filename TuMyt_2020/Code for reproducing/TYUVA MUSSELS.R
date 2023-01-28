library(ggplot2)
library(cowplot)
library(readxl)
library(ggrepel)
library(vegan)
library(dplyr)
library(ggvegan)
library(ggpubr)
library(car)
library(glmmTMB)
library(devtools)
library(pairwiseAdonis)
library(openxlsx)
library(mgcv)
library(lubridate)

######################################################################################################################

## Salinity prediction
# The code bellow produce the assessment of salinity for each sampling sites

# Data on tide cycle
tide <- read_excel("data_Tyuva mussels.xlsx", sheet = "Tide")

# Data on salinity measured during observations in July 2009.
sal_tuv <-  read_excel("data_Tyuva mussels.xlsx", sheet = "salinity")

# Model describing the dependeny between salinity and tide height (H) and Distnce (Dist) from upper part of Tyuva inlet (the river mouth) 
Mod_tide <- lm(Salinity ~ H*Dist, data = sal_tuv)
summary(Mod_tide) # information is provided in Stable 4

# Data on sampling sites
tuv <- read_excel("data_Tyuva mussels.xlsx", sheet = "Tyuva_09-10" )

# Calculation of mean and min salinity predicted by Model "Mod_tide" for each sampling sites
tuv$Mean_Salinity_predicted <- NA
tuv$Min_Salinity_predicted <- NA
str(tuv)

for(i in 1:nrow(tuv)){
  d <- tide %>% filter(H >= tuv$Depth[i])
  d$Dist <- tuv$Dist[i]
  Sal_pred <- predict(Mod_tide, newdata = d)
  tuv$Mean_Salinity_predicted[i] <- mean(Sal_pred)
  tuv$Min_Salinity_predicted[i] <- min(Sal_pred)
}

# Data for Figure 1D construction
tide_transect_R <- tide
tide_transect_R$Transect <- "R"
tide_transect_R$Dist <- 0

tide_transect_B <- tide
tide_transect_B$Transect <- "B"
tide_transect_B$Dist <- 250

tide_transect_Mid <- tide
tide_transect_Mid$Transect <- "Mid"
tide_transect_Mid$Dist <- 1750

tide_transect_Mon <- tide
tide_transect_Mon$Transect <- "Mon"
tide_transect_Mon$Dist <- 2550

tide_transect <- rbind(tide_transect_R, tide_transect_B, tide_transect_Mid, tide_transect_Mon)

tide_transect$Salinity_predicted <- predict(Mod_tide, newdata = tide_transect)

tide_transect$Transect <- factor(tide_transect$Transect, levels = c("R", "B", "Mid", "Mon"))
sal_tuv$Transect <- factor(sal_tuv$Transect, levels = c("R", "B", "Mid", "Mon"))

# ЗАЧЕМ ЭТО????
# levels_salinity_0.5 <- tide_transect %>% filter(H>=0.5) %>% group_by(Transect) %>% summarise(Level = 0.5, Salinity_predicted = mean(Salinity_predicted))
# 
# levels_salinity_1 <- tide_transect %>% filter(H>=1) %>% group_by(Transect) %>% summarise(Level = 1, Salinity_predicted = mean(Salinity_predicted))
# 
# levels_salinity_1.5 <- tide_transect %>% filter(H>=1.5) %>% group_by(Transect) %>% summarise(Level = 1.5, Salinity_predicted = mean(Salinity_predicted))
# 
# levels_salinity_2 <- tide_transect %>% filter(H>=2) %>% group_by(Transect) %>% summarise(Level = 2, Salinity_predicted = mean(Salinity_predicted))

# И ЗАЧЕМ ВОТ ЭТО????????????? 
# levels_salinity_subtidal <- tide_transect %>% filter(H>=0) %>% group_by(Transect) %>% summarise(Level = "subtidal", Salinity_predicted = mean(Salinity_predicted))

mean_salinity <- tide_transect %>% group_by(Transect) %>% summarise(Salinity_predicted = mean(Salinity_predicted))

tide_transect$Transect <- factor(tide_transect$Transect, levels = c("Mon", "Mid", "B", "R"), labels = c("MoS", "MidN", "BN", "R"))

mean_salinity$Transect <- factor(mean_salinity$Transect, levels = c("Mon", "Mid", "B", "R"), labels = c("MoS", "MidN", "BN", "R"))

sal_tuv$Transect <- factor(sal_tuv$Transect, levels = c("Mon", "Mid", "B", "R"), labels = c("MoS", "MidN", "BN", "R"))

# средняя солёность есть, а минимальную не вижу!

Figure1D <- 
  ggplot(tide_transect, aes(x = Time, y = Salinity_predicted)) + 
  geom_line(size = 1, color = "cornflowerblue") + 
  facet_wrap(~Transect, nrow = 4)+
  theme_bw() + 
  geom_point(data = sal_tuv, aes(x= Time, y = Salinity, shape = factor(Depth)),
             size = 2.5, color = "cornflowerblue", position = position_jitter()) +
  scale_shape_manual(values = c(22, 24, 21)) +
  geom_hline(data = mean_salinity, aes(yintercept = Salinity_predicted), linetype = 2) +
  theme() +
  labs(x = "Date and Time", y = "Salinity ppt") +
  coord_flip() +
  guides(shape = "none")

######################################################################################################################

## Prediction of taxonomic structure by morphotypes
# The code below produces a prediction of the taxonomic structure of mussel settlements (Ptros) using the frequency of T-morphotypes (PT)

# Data on individual mussel shell morphotype (1- T-morphotype, 0 - E-morphotype), age and q-values in samples from Tyuva Inlet (dataset TV), brackish (dataset BL) and saline (dataset BH) habitats in the Barents Sea
calculator <- read_excel("data_Tyuva mussels.xlsx", sheet = "calculator")
str(calculator)

calculator$period <- factor(calculator$period)
calculator$dataset <- factor(calculator$dataset, levels = c("BL", "TV", "BH"))
calculator$site <- factor(calculator$site)

calculator$sp <- ifelse(calculator$q <= 0.5, 0, 1) # dependent variable

# Dataframe including information about the frequency of MT in the settlement (estimated genetically, Ptros) and the frequency of T-morphotype in the settlement (PT)
freq <- 
  calculator %>% 
  group_by(dataset, period, site, pop) %>% 
  summarise(PT = mean(morph == 1), Ptros = mean(sp == 1)) 

calculator2 <- merge(calculator, freq)
str(calculator2)

# Model describing relationships between Ptros and PT within the three Barents Sea datasets
Model_Ptros_by_PT <- glm(sp ~ PT*dataset, family = "binomial", data = calculator2)

summary(Model_Ptros_by_PT) # information is provided in Stable 3
Anova(Model_Ptros_by_PT)

# residual plots
qplot(x = fitted(Model_Ptros_by_PT), y = residuals(Model_Ptros_by_PT)) + geom_smooth()

# Data for Figure 2 construction
MyData <- calculator2 %>% group_by(dataset) %>% do(data.frame(PT = seq(0, 1, length.out = 100), Ptros = seq(0, 1, length.out = 100)))

Model_Ptros_by_PT_Predict <- predict(Model_Ptros_by_PT, newdata = MyData, type = "response", se.fit = TRUE)

MyData$fit <- Model_Ptros_by_PT_Predict$fit

MyData$se <- Model_Ptros_by_PT_Predict$se.fit

# main plot (only Tyuva)
MyData_Tyuva <- MyData %>% filter(dataset == "TV")
freq_Tyuva <- freq %>% filter(dataset == "TV")

main <- ggplot(MyData_Tyuva, aes(x = PT, y = fit)) +
  theme_test() +
  geom_point(data = freq_Tyuva, aes(y = Ptros, x = PT, fill = period),
             color = "black", shape = 21, size = 5, stroke = 1.5) +
  scale_fill_manual(values = c("white", "grey", "black")) +
  geom_line(color = "darkgrey", size = 2) +
  geom_ribbon(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se), stat = "identity", fill = "gray", alpha = 0.2) +
  scale_y_continuous(n.breaks = 6, limits = c(0, 1)) +
  scale_x_continuous(n.breaks = 6, limits = c(0, 1)) +
  geom_text_repel(data = freq_Tyuva, aes(y = Ptros, label = site, size = 3.5, color = site),
                  position = position_jitter()) +    #как нормально подписать? чтобы не накладывались на точки и линию
  scale_color_manual(values = c("black", "red", "black", "blue", "darkviolet", "black", "darkgreen", "orange")) +
  guides(color = F, size = F, fill = guide_legend(title = NULL)) +
  theme(legend.position = c(0.92, 0.1),
        legend.justification = c(0.92, 0.1),
        legend.key.size = unit(0.6, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.background = element_rect(fill = "white", colour = "grey"), 
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15)) +
  labs(y = "Ptros") 

# inset plot (3 datasets)
inset <- ggplot(MyData, aes(x = PT, y = fit, group = dataset)) +
  theme_test() +
  geom_line(aes(color = dataset), size = 1) +
  scale_color_manual(values = c("blue", "darkgrey", "red")) +
  geom_ribbon(aes(ymin = fit - 1.96*se, ymax = fit + 1.96*se), stat = "identity", fill = "gray", alpha = 0.2) +
  scale_y_continuous(n.breaks = 3, limits = c(0, 1)) +
  scale_x_continuous(n.breaks = 3, limits = c(0, 1)) +
  theme(legend.position = "", axis.title = element_blank())

# Figure 2
Figure2 <- ggdraw() +
  draw_plot(main) +
  draw_plot(inset, x = 0.1, y = 0.6, width = 0.4, height = 0.38)


######################################################################################################################

### Analysis of population and taxonomic structuring of Tyuva mussels in 2009-2010

# Data on environmental, demographic and taxonomic characteristics of the Tyuva mussel settlements in 2009-2010
tyuva_09_10 <- read_excel("data_Tyuva mussels.xlsx", sheet = "Tyuva_09-10")
str(tyuva_09_10)
# Depth is coded not from "chart datum" (as Fig. 1F and Fig. 3) but in ascending order (the lowest negative value is the uppermost horizon in the littoral, the highest positive value is the deepest horizon in the sublittoral)

tyuva_09_10$Transect <- factor(tyuva_09_10$Transect)
tyuva_09_10$Habitat <- factor(tyuva_09_10$Habitat)
tyuva_09_10$Exp <- factor(tyuva_09_10$Exp)
tyuva_09_10$Bottom <- factor(tyuva_09_10$Bottom)


## Canonical correspondence analysis (CCA)
# The code below produces an analysis of associations between all environmental, demographic and taxonomic parameters of mussel settlements using CCA

# matrix of dependent variables
tyuva_demogr <- tyuva_09_10 %>% select(N2_3, N4_6, N7_9, N10, N, B, Ptros, GI, Lmax, L5) %>% as.data.frame()
row.names(tyuva_demogr) <- tyuva_09_10$Sample_ID

# fitting of full model 
tyuva_cca_full <- cca(tyuva_demogr ~ Dist + Depth + Slope + Bottom + Cov + Kelp + Width + Exp, data = tyuva_09_10)

vif.cca(tyuva_cca_full) # multicollinearity presents

# fitting of optimal model using forward selection protocol
tyuva_predictors <-  tyuva_09_10 %>% select(Dist, Depth, Slope, Bottom, Cov, Kelp, Width, Exp)

tyuva_cca_opt0 <- cca(tyuva_demogr ~ 1, data = tyuva_predictors) 

tyuva_cca_opt <- ordistep(tyuva_cca_opt0, scope = formula(tyuva_cca_full), direction = "forward", permutations = 9999)

summary(tyuva_cca_opt)

# estimation of statistical significance of the optimal model, individual canonical axes and constraints
anova(tyuva_cca_opt, permutations = 9999)
anova(tyuva_cca_opt, by = "axis", permutations = 9999)
anova(tyuva_cca_opt, by = "margin", permutations = 9999)
# information of these analyses is provided in Stable 5

# Data for Figure 4 construction
cca_scores <- fortify(tyuva_cca_opt, scaling = "symmetric", display = c("sp","wa","bp"))
cca_scores_site <- cca_scores[cca_scores$Score == "sites", ] 
cca_scores_site$Habitat <- tyuva_09_10$Habitat
cca_scores_sp <- cca_scores[cca_scores$Score == "species", ] 
cca_scores_cons <- cca_scores[cca_scores$Score == "biplot", ] 

# !!!!!!!!!!!!!!!поиск множителя для стрелочек - придумайте, пжл, как этот пункт назвать и зачем мы это делали..



cca.res <- summary(tyuva_cca_opt)
cca.sites <- cca.res$sites
cca.species <- cca.res$species

mul <- ordiArrowMul(cca.res, display = "sites", fill = 1)

plot(tyuva_cca_opt, scaling = "symmetric", display = c("species", "cn"))

# Figure 4
Figure4 <- 
  ggplot(cca_scores_site, aes(x = CCA1, y = CCA2)) + 
  theme_test() +
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0) + 
  geom_point(aes(fill = Habitat, shape = Habitat, size = Habitat)) + 
  scale_fill_manual(values = c("cornflowerblue", "darkolivegreen3", "azure4", "darkgoldenrod1")) + 
  scale_shape_manual(values = c(21, 23, 24, 22)) + 
  scale_size_manual(values = c(5.7, 5, 4.7, 5)) +
  geom_segment(data = cca_scores_cons, aes(x = 0, y = 0, xend = CCA1 * mul, yend = CCA2 * mul),
               color = "black", arrow = arrow(type = "closed", angle = 6), size = 1) +
  geom_text_repel(data = cca_scores_sp, aes(label = Label), color = "darkviolet", size = 4.5) + 
  geom_text_repel(data = cca_scores_cons, aes(x = CCA1 * mul, y = CCA2 * mul, label = Label),
                  color = "black", size = 4) +
  guides(fill = guide_legend(title = NULL), shape = guide_legend(title = NULL), size = guide_legend(title = NULL)) +
  theme(legend.position = c(0.95, 0.9),
        legend.justification = c(0.95, 0.9),
        legend.key.size = unit(0.6, "cm"),
        legend.key.width = unit(1.5,"cm"),
        legend.background = element_rect(fill = "white", colour = "grey"), 
        axis.text = element_text(size = 13),
        axis.title = element_text(size = 15)) +
  labs (x = "CCA1  (32.4%)", y = "CCA2  (8.7%)")


## PERMANOVA
# The code below produces compare groups of settlements from different habitats (rocky littoral, sandbanks, kelp forests and the Bed) by demographic parameters and Ptros using PERMANOVA

perm_hab <- tyuva_09_10 %>% select(Habitat, N2_3, N4_6, N7_9, N10, N, B, Ptros, GI, Lmax, L5) %>% as.data.frame()
perm_hab_log <- log(perm_hab[2:11] + 1)

permanova_habitat <- adonis(perm_hab_log ~ perm_hab$Habitat, method = "bray", permutations = 9999)
permanova_habitat # information is provided in Stable 6

# check of equality of within group variance
dist_habitat <- vegdist(perm_hab_log, method = "bray")
PCO_habitat <- betadisper(dist_habitat, perm_hab$Habitat)
plot(PCO_habitat)
anova(PCO_habitat)
boxplot(PCO_habitat)

# Post hoc pairwise test
post_hoc <- pairwise.adonis(perm_hab_log, perm_hab$Habitat, sim.function = "vegdist", sim.method = "bray", p.adjust.m = "bonferroni", reduce = NULL, perm = 9999)
post_hoc # information is provided in Stable 6


## Generalized linear mixed model (GLMM)
# The code below produces an analysis of associations between Ptros and environmental parameters using GLMM

tyuva_09_10$Kelp <- as.factor(tyuva_09_10$Kelp)

# standardization of values of quantitative environmental parameters 
tyuva_09_10$Depth2 <- scale(tyuva_09_10$Depth)[,1]
tyuva_09_10$Cov2 <- scale(tyuva_09_10$Cov) [,1]
tyuva_09_10$Dist2 <- scale(tyuva_09_10$Dist) [,1]
tyuva_09_10$Slope2 <- scale(tyuva_09_10$Slope) [,1]
tyuva_09_10$Width2 <- scale(tyuva_09_10$Width) [,1]

str(tyuva_09_10)

# check of set of all predictors for collinearity by calculating the variance inflation factors
vif(lm(Ptros ~ Depth2 + Exp + Cov2 + Kelp + Dist2 + Slope2 + Width2 + Bottom, data=tyuva_09_10))

# removing Bottom
vif(lm(Ptros ~ Depth2 + Exp + Cov2 + Kelp + Dist2 + Slope2 + Width2, data=tyuva_09_10))

# removing Slope2
vif(lm(Ptros ~ Depth2 + Exp + Cov2 + Kelp + Dist2 + Width2, data=tyuva_09_10))

# removing Width2
vif(lm(Ptros ~ Depth2 + Exp + Cov2 + Kelp + Dist2, data=tyuva_09_10))

# removing Kelp, not Depth, because kelps replicate depth to some extent
vif(lm(Ptros ~ Depth2 + Exp + Cov2 + Dist2, data=tyuva_09_10))

# model fitting
Model_Ptros <- glmmTMB(Ptros ~ Depth2 + Exp + Cov2 + Dist2 + (1|Transect), family=beta_family(link = "logit"), control=glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")), data=tyuva_09_10)

summary(Model_Ptros) # information is provided in Stable 7

# residual plots
qplot(x=fitted(Model_Ptros), y=residuals(Model_Ptros)) + geom_smooth()
# обсуждалось, что это (бугорок) - ничего страшного..

# assessment of the presence of overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(Model_Ptros)


######################################################################################################################

### Temporal dynamics of the Tyuva mussels in 2004-2018

# Data on ....в многомерных анализах?

## CA

## GAM CA1

## GAM Ptros

## PERMANOVA

## SIMPER

## Wilcoxon signed-rank tests
# Data on ... подумать, делать ли для этой бурды отдельный лист..

### Analysis of historical data on Murman mussels

## GAM

## Mann-Whitney test

## Wilcoxon Test


