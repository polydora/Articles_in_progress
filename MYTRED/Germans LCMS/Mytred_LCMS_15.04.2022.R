library(readxl)
library(ggplot2)
library(dplyr)


data_lcms <- read_excel('Mytred_LCMS_Anton_UPD.xlsx', sheet = 'Dataframe')
#View(data_lcms)
str(data_lcms)

molar_masses <- read_excel('Mytred_LCMS_Anton_UPD.xlsx', sheet = 'Molar_mass')

str(molar_masses)
#View(molar_masses)

molar_mass_vec <- as.vector(molar_masses$Molar_mass)



lcms2 <- data_lcms
lcms2$Salinity <- as.factor(lcms2$Salinity)
lcms2$Morphotype <- as.factor(lcms2$Morphotype)



lcms2[,c(6:40)] <-  mapply(FUN = `/`, lcms2[,c(6:40)], molar_mass_vec)
str(lcms2)
#View(lcms2)

lcms2_hep <- lcms2 %>% filter(lcms2$Tissue == 'Hep')
#View(lcms2_hep)
str(lcms2_hep)


lcms2_gill <- lcms2 %>% filter(lcms2$Tissue == 'Gill')
#View(lcms2_gill)
str(lcms2_gill)



# ������ ���-�����

lcms2_hep_mean <- lcms2_hep %>% group_by(Salinity, Morphotype) %>% select(- c(Sample, Tissue, Data_File_Name)) %>%  summarise_all(.funs = "mean") 

library(reshape2)

lcms2_hep_mean_long <- melt(lcms2_hep_mean, id.vars = c("Salinity", "Morphotype"))


Pl_10_ed <-
lcms2_hep_mean_long %>% filter(Salinity == 10, Morphotype=="ed") %>% filter(!(variable %in% c("Malate", "Succinate"))) %>%   
  ggplot(., aes(x="", y = value, fill = variable)) + geom_col(width = 1) + coord_polar("y", start=0) + theme(axis.text.x=element_blank(), axis.line = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())

##############################

pie_mean_hep <- lcms2_hep %>% group_by(Morphotype, Salinity) %>% summarise_at(vars(Cystine:Taurine), mean)
str(pie_mean_hep)
pie_mean_hep_df <- as.data.frame(pie_mean_hep)
str(pie_mean_hep_df)
#View(pie_mean_hep_df)

write_xlsx(pie_mean_hep_df,"D:/R/Germans LCMS/Pie_Chart.xlsx")

t_pie_mean_hep_df <- as.data.frame(t(pie_mean_hep_df))

t_pie_mean_hep_df_names <- rownames(t_pie_mean_hep_df)


t_pie_mean_hep_df$Substance <- t_pie_mean_hep_df_names
str(t_pie_mean_hep_df)
#View(t_pie_mean_hep_df)

library(writexl)

write_xlsx(t_pie_mean_hep_df,"D:/R/Germans LCMS/Pie_Chart.xlsx")

### Теперь жабры

pie_mean_gill <- lcms2_gill %>% group_by(Morphotype, Salinity) %>% summarise_at(vars(Cystine:Taurine), mean)
str(pie_mean_gill)
pie_mean_gill_df <- as.data.frame(pie_mean_gill)
str(pie_mean_gill)
#View(pie_mean_gill)



t_pie_mean_gill_df <- as.data.frame(t(pie_mean_gill_df))

t_pie_mean_gill_df_names <- rownames(t_pie_mean_gill_df)


t_pie_mean_gill_df$Substance <- t_pie_mean_gill_df_names
str(t_pie_mean_gill_df)

write_xlsx(pie_mean_gill_df,"D:/R/Germans LCMS/Pie_Pie.xlsx")
#View(t_pie_mean_gill_df)

library(writexl)

write_xlsx(t_pie_mean_hep_df,"D:/R/Germans LCMS/Pie_Chart.xlsx")

#hep_tr_10 <- as.data.frame(t(pie_mean_hep[1,-c(1,2)]))

#str(hep_tr_10)
#View(hep_tr_10)


#tr_10_names <- rownames(hep_tr_10)
#str(as.vector(tr_10_names))

#hep_tr_10$Substance <- tr_10_names

###


################## RDA #####################
library(vegan)

names(lcms2_gill)[6:ncol(lcms2_gill)] <- paste("Gill_", names(lcms2_gill)[6:ncol(lcms2_gill)], sep ="" )
names(lcms2_hep)[6:ncol(lcms2_hep)] <- paste("Hep_", names(lcms2_hep)[6:ncol(lcms2_hep)], sep ="" )

lcms3 <- merge(lcms2_gill, lcms2_hep, by = "Sample")

lcms3 <- lcms3 %>% select(-c(Salinity.y, Morphotype.y, Tissue.y, Data_File_Name.y))

lcms3$Salinity.x <- as.numeric(lcms3$Salinity.x)
  
  



rda_lcms <-  rda(log(lcms3[, -c(1:5)]) ~ Salinity.x + Morphotype.x, data = lcms3)


summary(rda_lcms)


plot(rda_lcms, type = "t", display = c("species", "cn"), scaling = "species")


plot(rda_lcms, type = "p", display = c("sites", "cn"), scaling = "sites")

anova(rda_lcms, permutations = 9999)

anova(rda_lcms, by = "axis", permutations = 9999)

anova(rda_lcms, by = "margin", permutations = 9999)

anova(rda_lcms, by = "terms", permutations = 9999)


rda_lcms_res <- fortify(rda_lcms, scaling = "sites")

rda_lcms_res %>% filter(Score == "sites") %>% 
  ggplot(., aes(RDA1, RDA2)) + geom_point(aes(color = lcms3$Morphotype.x, size = lcms3$Salinity.x ))


rda_lcms_res_sp <- fortify(rda_lcms, scaling = "species")



mul2 <- ordiArrowMul(rda_lcms, display = "species")


rda_lcms_res %>% filter(Score == "species") %>% 
  ggplot(., aes(RDA1, RDA2)) + geom_text(aes(label = Label)) +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_point(data = rda_lcms_res %>% filter(Score == "centroids"), color = "blue", size = 3) +
  geom_segment(data = rda_lcms_res %>% filter(Score == "biplot") %>% filter(Label == "Salinity.x"), aes(xend = RDA1, yend = RDA2, x = 0, y = 0), color = "blue", size = 2)


### Inertia component ####

inert_comp <- inertcomp(rda_lcms, display = c("species"), proportional = FALSE )

qplot(inert_comp[ ,1], inert_comp[,2]) + geom_abline(color = "blue") + geom_text(aes(label = row.names(inert_comp))) + labs(x = "����� ������������ ����", y = "����� �������������� ����")


###############################################




hep_mds <- lcms2_hep[,6:40]

#View(hep_mds)

ord_hep <- metaMDS(hep_mds, distance = 'bray', k = 2)
ordiplot(ord_hep, display = 'sites')

hep_points <- as.data.frame(ord_hep$points)

ggplot(hep_points, aes(x = MDS1, y = MDS2, color = lcms2_hep$Salinity, shape = lcms2_hep$Morphotype)) + geom_point(size = 4)

ggplot(hep_points, aes(x = MDS1, y = MDS2, color = lcms2_hep$Morphotype)) + geom_point(size = 4)

ord_hep$stress

stressplot(ord_hep)

#А если прологарифмировать данные?
log_hep_mds <- hep_mds * 100000
str(log_hep_mds)

log_hep_mds2 <- log(log_hep_mds) + 1

#View(log_hep_mds2)
ord_hep_log <- metaMDS(log_hep_mds, distance = 'euclidean', k = 2)
ordiplot(ord_hep_log, display = 'sites')

hep_points_log <- as.data.frame(ord_hep_log$points)

ggplot(hep_points_log, aes(x = MDS1, y = MDS2, color = lcms2_hep$Salinity, shape = lcms2_hep$Morphotype)) + geom_point(size = 4)

ggplot(hep_points_log, aes(x = MDS1, y = MDS2, color = lcms2_hep$Morphotype)) + geom_point(size = 4)

ord_hep_log$stress

stressplot(ord_hep_log) #Хуйня получается



### Жабры теперь

gill_mds <- lcms2_gill[,6:40]

#View(gill_mds)

ord_gill <- metaMDS(gill_mds, distance = 'bray', k = 2)
ordiplot(ord_gill, display = 'sites')

gill_points <- as.data.frame(ord_gill$points)

ggplot(gill_points, aes(x = MDS1, y = MDS2, color = lcms2_gill$Salinity, shape = lcms2_gill$Morphotype)) + geom_point(size = 4)

ggplot(gill_points, aes(x = MDS1, y = MDS2, color = lcms2_gill$Morphotype)) + geom_point(size = 4)

ord_gill$stress

stressplot(ord_gill)

###







### 

shishi <- data_lcms

shishi$Salinity <- as.factor(shishi$Salinity)
shishi$Morphotype <- as.factor(shishi$Morphotype)
shishi$Tissue <- as.factor(shishi$Tissue)
 str(shishi)
  
 
shi_hep <- shishi %>% filter(Tissue == "Hep")
str(shi_hep)

shi_gill <- shishi %>% filter(Tissue == "Gill")
str(shi_gill)

#Cystine   
ggplot(data = shishi, aes(x = Salinity, y = Cystine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Cystine') + theme(plot.title = element_text(face = 'bold', size = 24))

сystine_mod <- lm(data = shishi, Cystine ~ Salinity * Morphotype * Tissue)
summary(cystine_mod)
anova(cystine_mod)

drop1(cystine_mod)

cystine_mod1 <- update(cystine_mod, . ~ . -Salinity:Morphotype:Tissue)
drop1(cystine_mod1)
cystine_mod2 <- update(cystine_mod1, . ~ . -Salinity:Tissue)
drop1(cystine_mod2)

summary(cystine_mod2)
anova(cystine_mod2)


cystine_hep <- lm(data = shi_hep, Cystine ~ Salinity * Morphotype)
summary(cystine_hep)


cystine_gill <- lm(data = shi_gill, Cystine ~ Salinity * Morphotype)
summary(cystine_gill)

anova(cystine_hep)
anova(cystine_gill)



#Asparagine

ggplot(data = shishi, aes(x = Salinity, y = Asparagine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Asparagine') + theme(plot.title = element_text(face = 'bold', size = 24))

Asparagine_mod <- lm(data = shishi, Asparagine ~ Salinity * Morphotype * Tissue)
anova(Asparagine_mod)


Asparagine_hep <- lm(data = shi_hep, Asparagine ~ Salinity * Morphotype)
summary(Asparagine_hep)

Asparagine_gill <- lm(data = shi_gill, Asparagine ~ Salinity * Morphotype)
summary(Asparagine_gill)

anova(Asparagine_gill)
anova(Asparagine_hep)

#Serine

ggplot(data = shishi, aes(x = Salinity, y = Serine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Serine') + theme(plot.title = element_text(face = 'bold', size = 24))

Serine_mod <- lm(data = shishi, Serine ~ Salinity * Morphotype * Tissue)
anova(Serine_mod)

Serine_hep <- lm(data = shi_hep, Serine ~ Salinity * Morphotype)
summary(Serine_hep)

Serine_gill <- lm(data = shi_gill, Serine ~ Salinity * Morphotype)
summary(Serine_gill)

anova(Serine_gill)
anova(Serine_hep)

#Alanine

ggplot(data = shishi, aes(x = Salinity, y = Alanine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Alanine') + theme(plot.title = element_text(face = 'bold', size = 24))

Alanine_mod <- lm(data = shishi, Alanine ~ Salinity * Morphotype * Tissue)
anova(Alanine_mod)


Alanine_hep <- lm(data = shi_hep, Alanine ~ Salinity * Morphotype)
summary(Alanine_hep)

Alanine_gill <- lm(data = shi_gill, Alanine ~ Salinity * Morphotype)
summary(Alanine_gill)

anova(Alanine_gill)
anova(Alanine_hep)

#OH_Proline

ggplot(data = shishi, aes(x = Salinity, y = OH_Proline), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('OH_Proline') + theme(plot.title = element_text(face = 'bold', size = 24))

OH_Proline_mod <- lm(data = shishi, OH_Proline ~ Salinity * Morphotype * Tissue)
anova(OH_Proline_mod)


OH_Proline_hep <- lm(data = shi_hep, OH_Proline ~ Salinity * Morphotype)
summary(OH_Proline_hep)

OH_Proline_gill <- lm(data = shi_gill, OH_Proline ~ Salinity * Morphotype)
summary(OH_Proline_gill)

anova(OH_Proline_gill)
anova(OH_Proline_hep)



#Glycine

ggplot(data = shishi, aes(x = Salinity, y = Glycine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Glycine') + theme(plot.title = element_text(face = 'bold', size = 24))

Glycine_mod <- lm(data = shishi, Glycine ~ Salinity * Morphotype * Tissue)
anova(Glycine_mod)


Glycine_hep <- lm(data = shi_hep, Glycine ~ Salinity * Morphotype)

Glycine_gill <- lm(data = shi_gill, Glycine ~ Salinity * Morphotype)


anova(Glycine_gill)
anova(Glycine_hep)

#Glutamine

ggplot(data = shishi, aes(x = Salinity, y = Glutamine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Glutamine') + theme(plot.title = element_text(face = 'bold', size = 24))

Glutamine_mod <- lm(data = shishi, Glutamine ~ Salinity * Morphotype * Tissue)
anova(Glutamine_mod)


Glutamine_hep <- lm(data = shi_hep, Glutamine ~ Salinity * Morphotype)

Glutamine_gill <- lm(data = shi_gill, Glutamine ~ Salinity * Morphotype)


anova(Glutamine_gill)
anova(Glutamine_hep)

#Threonine

ggplot(data = shishi, aes(x = Salinity, y = Threonine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Threonine') + theme(plot.title = element_text(face = 'bold', size = 24))

Threonine_mod <- lm(data = shishi, Threonine ~ Salinity * Morphotype * Tissue)
anova(Threonine_mod)


Threonine_hep <- lm(data = shi_hep, Threonine ~ Salinity * Morphotype)

Threonine_gill <- lm(data = shi_gill, Threonine ~ Salinity * Morphotype)


anova(Threonine_gill)
anova(Threonine_hep)

#Methionine_Sulfoxide

ggplot(data = shishi, aes(x = Salinity, y = Methionine_Sulfoxide), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Methionine_Sulfoxide') + theme(plot.title = element_text(face = 'bold', size = 24))

Methionine_Sulfoxide_mod <- lm(data = shishi, Methionine_Sulfoxide ~ Salinity * Morphotype * Tissue)
anova(Methionine_Sulfoxide_mod)


Methionine_Sulfoxide_hep <- lm(data = shi_hep, Methionine_Sulfoxide ~ Salinity * Morphotype)

Methionine_Sulfoxide_gill <- lm(data = shi_gill, Methionine_Sulfoxide ~ Salinity * Morphotype)


anova(Methionine_Sulfoxide_gill)
anova(Methionine_Sulfoxide_hep)

#Glutamate

ggplot(data = shishi, aes(x = Salinity, y = Glutamate), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Glutamate') + theme(plot.title = element_text(face = 'bold', size = 24))

Glutamate_mod <- lm(data = shishi, Glutamate ~ Salinity * Morphotype * Tissue)
anova(Glutamate_mod)

Glutamate_hep <- lm(data = shi_hep, Glutamate ~ Salinity * Morphotype)

Glutamate_gill <- lm(data = shi_gill, Glutamate ~ Salinity * Morphotype)


anova(Glutamate_gill)
anova(Glutamate_hep)

#Proline

ggplot(data = shishi, aes(x = Salinity, y = Proline), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Proline') + theme(plot.title = element_text(face = 'bold', size = 24))

Proline_mod <- lm(data = shishi, Proline ~ Salinity * Morphotype * Tissue)
anova(Proline_mod)

Proline_hep <- lm(data = shi_hep, Proline ~ Salinity * Morphotype)

Proline_gill <- lm(data = shi_gill, Proline ~ Salinity * Morphotype)


anova(Proline_gill)
anova(Proline_hep)

#Lysine

ggplot(data = shishi, aes(x = Salinity, y = Lysine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Lysine') + theme(plot.title = element_text(face = 'bold', size = 24))

Lysine_mod <- lm(data = shishi, Lysine ~ Salinity * Morphotype * Tissue)
anova(Lysine_mod)

Lysine_hep <- lm(data = shi_hep, Lysine ~ Salinity * Morphotype)

Lysine_gill <- lm(data = shi_gill, Lysine ~ Salinity * Morphotype)


anova(Lysine_gill)
anova(Lysine_hep)

#Histidine

ggplot(data = shishi, aes(x = Salinity, y = Histidine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Histidine') + theme(plot.title = element_text(face = 'bold', size = 24))

Histidine_mod <- lm(data = shishi, Histidine ~ Salinity * Morphotype * Tissue)
anova(Histidine_mod)


Histidine_hep <- lm(data = shi_hep, Histidine ~ Salinity * Morphotype)

Histidine_gill <- lm(data = shi_gill, Histidine ~ Salinity * Morphotype)


anova(Histidine_gill)
anova(Histidine_hep)

#AMP

ggplot(data = shishi, aes(x = Salinity, y = AMP), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('AMP') + theme(plot.title = element_text(face = 'bold', size = 24))

AMP_mod <- lm(data = shishi, AMP ~ Salinity * Morphotype * Tissue)
anova(AMP_mod)

AMP_hep <- lm(data = shi_hep, AMP ~ Salinity * Morphotype)

AMP_gill <- lm(data = shi_gill, AMP ~ Salinity * Morphotype)


anova(AMP_gill)
anova(AMP_hep)

#Arginine

ggplot(data = shishi, aes(x = Salinity, y = Arginine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Arginine') + theme(plot.title = element_text(face = 'bold', size = 24))

Arginine_mod <- lm(data = shishi, Arginine ~ Salinity * Morphotype * Tissue)
anova(Arginine_mod)

Arginine_hep <- lm(data = shi_hep, Arginine ~ Salinity * Morphotype)

Arginine_gill <- lm(data = shi_gill, Arginine ~ Salinity * Morphotype)


anova(Arginine_gill)
anova(Arginine_hep)

#Valine

ggplot(data = shishi, aes(x = Salinity, y = Valine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Valine') + theme(plot.title = element_text(face = 'bold', size = 24))

Valine_mod <- lm(data = shishi, Valine ~ Salinity * Morphotype * Tissue)
anova(Valine_mod)

Valine_hep <- lm(data = shi_hep, Valine ~ Salinity * Morphotype)

Valine_gill <- lm(data = shi_gill, Valine ~ Salinity * Morphotype)


anova(Valine_gill)
anova(Valine_hep)

#Methionine

ggplot(data = shishi, aes(x = Salinity, y = Methionine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Methionine') + theme(plot.title = element_text(face = 'bold', size = 24))

Methionine_mod <- lm(data = shishi, Methionine ~ Salinity * Morphotype * Tissue)
anova(Methionine_mod)

Methionine_hep <- lm(data = shi_hep, Methionine ~ Salinity * Morphotype)

Methionine_gill <- lm(data = shi_gill, Methionine ~ Salinity * Morphotype)


anova(Methionine_gill)
anova(Methionine_hep)

#Tyrosine

ggplot(data = shishi, aes(x = Salinity, y = Tyrosine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Tyrosine') + theme(plot.title = element_text(face = 'bold', size = 24))

Tyrosine_mod <- lm(data = shishi, Tyrosine ~ Salinity * Morphotype * Tissue)
anova(Tyrosine_mod)


Tyrosine_hep <- lm(data = shi_hep, Tyrosine ~ Salinity * Morphotype)

Tyrosine_gill <- lm(data = shi_gill, Tyrosine ~ Salinity * Morphotype)


anova(Tyrosine_gill)
anova(Tyrosine_hep)

#Isoleucine

ggplot(data = shishi, aes(x = Salinity, y = Isoleucine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Isoleucine') + theme(plot.title = element_text(face = 'bold', size = 24))

Isoleucine_mod <- lm(data = shishi, Isoleucine ~ Salinity * Morphotype * Tissue)
anova(Isoleucine_mod)

Isoleucine_hep <- lm(data = shi_hep, Isoleucine ~ Salinity * Morphotype)

Isoleucine_gill <- lm(data = shi_gill, Isoleucine ~ Salinity * Morphotype)


anova(Isoleucine_gill)
anova(Isoleucine_hep)

#Leucine

ggplot(data = shishi, aes(x = Salinity, y = Leucine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Leucine') + theme(plot.title = element_text(face = 'bold', size = 24))

Leucine_mod <- lm(data = shishi, Leucine ~ Salinity * Morphotype * Tissue)
anova(Leucine_mod)


Leucine_hep <- lm(data = shi_hep, Leucine ~ Salinity * Morphotype)

Leucine_gill <- lm(data = shi_gill, Leucine ~ Salinity * Morphotype)


anova(Leucine_gill)
anova(Leucine_hep)

#Phenylalanine

ggplot(data = shishi, aes(x = Salinity, y = Phenylalanine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Phenylalanine') + theme(plot.title = element_text(face = 'bold', size = 24))

Phenylalanine_mod <- lm(data = shishi, Phenylalanine ~ Salinity * Morphotype * Tissue)
anova(Phenylalanine_mod)

Phenylalanine_hep <- lm(data = shi_hep, Phenylalanine ~ Salinity * Morphotype)

Phenylalanine_gill <- lm(data = shi_gill, Phenylalanine ~ Salinity * Morphotype)


anova(Phenylalanine_gill)
anova(Phenylalanine_hep)


#Tryptophan

ggplot(data = shishi, aes(x = Salinity, y = Tryptophan), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Tryptophan') + theme(plot.title = element_text(face = 'bold', size = 24))

Tryptophan_mod <- lm(data = shishi, Tryptophan ~ Salinity * Morphotype * Tissue)
anova(Tryptophan_mod)

Tryptophan_hep <- lm(data = shi_hep, Tryptophan ~ Salinity * Morphotype)

Tryptophan_gill <- lm(data = shi_gill, Tryptophan ~ Salinity * Morphotype)


anova(Tryptophan_gill)
anova(Tryptophan_hep)

#2_Oxoglutarate
#str(shishi)

#ggplot(data = shishi, aes(x = Salinity, y = 2_Oxoglutarate), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('2_Oxoglutarate') + theme(plot.title = element_text(face = 'bold', size = 24))

#dva_Oxoglutarate_mod <- lm(data = shishi, 2_Oxoglutarate ~ Salinity * Morphotype * Tissue)
#anova(dva_Oxoglutarate_mod)


#Malate

ggplot(data = shishi, aes(x = Salinity, y = Malate), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Malate') + theme(plot.title = element_text(face = 'bold', size = 24))

Malate_mod <- lm(data = shishi, Malate ~ Salinity * Morphotype * Tissue)
anova(Malate_mod)

Malate_hep <- lm(data = shi_hep, Malate ~ Salinity * Morphotype)

Malate_gill <- lm(data = shi_gill, Malate ~ Salinity * Morphotype)


anova(Malate_gill)
anova(Malate_hep)

#Lactate

ggplot(data = shishi, aes(x = Salinity, y = Lactate), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Lactate') + theme(plot.title = element_text(face = 'bold', size = 24))

Lactate_mod <- lm(data = shishi, Lactate ~ Salinity * Morphotype * Tissue)
anova(Lactate_mod)

Lactate_hep <- lm(data = shi_hep, Lactate ~ Salinity * Morphotype)

Lactate_gill <- lm(data = shi_gill, Lactate ~ Salinity * Morphotype)


anova(Lactate_gill)
anova(Lactate_hep)

#Succinate

ggplot(data = shishi, aes(x = Salinity, y = Succinate), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Succinate') + theme(plot.title = element_text(face = 'bold', size = 24))

Succinate_mod <- lm(data = shishi, Succinate ~ Salinity * Morphotype * Tissue)
anova(Succinate_mod)

Succinate_hep <- lm(data = shi_hep, Succinate ~ Salinity * Morphotype)

Succinate_gill <- lm(data = shi_gill, Succinate ~ Salinity * Morphotype)


anova(Succinate_gill)
anova(Succinate_hep)

#GABA

ggplot(data = shishi, aes(x = Salinity, y = GABA), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('GABA') + theme(plot.title = element_text(face = 'bold', size = 24))

GABA_mod <- lm(data = shishi, GABA ~ Salinity * Morphotype * Tissue)
anova(GABA_mod)

GABA_hep <- lm(data = shi_hep, GABA ~ Salinity * Morphotype)

GABA_gill <- lm(data = shi_gill, GABA ~ Salinity * Morphotype)


anova(GABA_gill)
anova(GABA_hep)

#Carnitine

ggplot(data = shishi, aes(x = Salinity, y = Carnitine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Carnitine') + theme(plot.title = element_text(face = 'bold', size = 24))

Carnitine_mod <- lm(data = shishi, Carnitine ~ Salinity * Morphotype * Tissue)
anova(Carnitine_mod)

Carnitine_hep <- lm(data = shi_hep, Carnitine ~ Salinity * Morphotype)

Carnitine_gill <- lm(data = shi_gill, Carnitine ~ Salinity * Morphotype)


anova(Carnitine_gill)
anova(Carnitine_hep)

#Citrulline

ggplot(data = shishi, aes(x = Salinity, y = Citrulline), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Citrulline') + theme(plot.title = element_text(face = 'bold', size = 24))

Citrulline_mod <- lm(data = shishi, Citrulline ~ Salinity * Morphotype * Tissue)
anova(Citrulline_mod)

Citrulline_hep <- lm(data = shi_hep, Citrulline ~ Salinity * Morphotype)

Citrulline_gill <- lm(data = shi_gill, Citrulline ~ Salinity * Morphotype)


anova(Citrulline_gill)
anova(Citrulline_hep)

#Ornithine

ggplot(data = shishi, aes(x = Salinity, y = Ornithine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Ornithine') + theme(plot.title = element_text(face = 'bold', size = 24))

Ornithine_mod <- lm(data = shishi, Ornithine ~ Salinity * Morphotype * Tissue)
anova(Ornithine_mod)

Ornithine_hep <- lm(data = shi_hep, Ornithine ~ Salinity * Morphotype)

Ornithine_gill <- lm(data = shi_gill, Ornithine ~ Salinity * Morphotype)


anova(Ornithine_gill)
anova(Ornithine_hep)

#CMP

ggplot(data = shishi, aes(x = Salinity, y = CMP), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('CMP') + theme(plot.title = element_text(face = 'bold', size = 24))

CMP_mod <- lm(data = shishi, CMP ~ Salinity * Morphotype * Tissue)
anova(CMP_mod)

CMP_hep <- lm(data = shi_hep, CMP ~ Salinity * Morphotype)

CMP_gill <- lm(data = shi_gill, CMP ~ Salinity * Morphotype)


anova(CMP_gill)
anova(CMP_hep)

#GMP

ggplot(data = shishi, aes(x = Salinity, y = GMP), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('GMP') + theme(plot.title = element_text(face = 'bold', size = 24))

GMP_mod <- lm(data = shishi, GMP ~ Salinity * Morphotype * Tissue)
anova(GMP_mod)

GMP_hep <- lm(data = shi_hep, GMP ~ Salinity * Morphotype)

GMP_gill <- lm(data = shi_gill, GMP ~ Salinity * Morphotype)


anova(GMP_gill)
anova(GMP_hep)

#Argininosuccinic_acid

ggplot(data = shishi, aes(x = Salinity, y = Argininosuccinic_acid), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Argininosuccinic_acid') + theme(plot.title = element_text(face = 'bold', size = 24))

Argininosuccinic_acid_mod <- lm(data = shishi, Argininosuccinic_acid ~ Salinity * Morphotype * Tissue)
anova(Argininosuccinic_acid_mod)

Argininosuccinic_acid_hep <- lm(data = shi_hep, Argininosuccinic_acid ~ Salinity * Morphotype)

Argininosuccinic_acid_gill <- lm(data = shi_gill, Argininosuccinic_acid ~ Salinity * Morphotype)


anova(Argininosuccinic_acid_gill)
anova(Argininosuccinic_acid_hep)

#Taurine

ggplot(data = shishi, aes(x = Salinity, y = Taurine), group = Tissue) + geom_boxplot(aes(fill = Morphotype)) + facet_wrap(facets = shishi$Tissue, nrow = 2) + scale_fill_manual(values = c('dodgerblue2', 'red'))  + ggtitle('Taurine') + theme(plot.title = element_text(face = 'bold', size = 24))

Taurine_mod <- lm(data = shishi, Taurine ~ Salinity * Morphotype * Tissue)
anova(Taurine_mod)

Taurine_hep <- lm(data = shi_hep, Taurine ~ Salinity * Morphotype)

Taurine_gill <- lm(data = shi_gill, Taurine ~ Salinity * Morphotype)


anova(Taurine_gill)
anova(Taurine_hep)

#Metaboanalyst


#PCA
library(vegan)

hep_pca <- rda(hep_mds, scale = TRUE)
summary(hep_pca)

eigenvals(hep_pca)

screeplot(hep_pca, type = 'lines', bstick = TRUE)
bstick(hep_pca)

scores(hep_pca, display = 'species', choices = c(1,2,3,4), scaling = 0)
scores(hep_pca, display = 'sites', choices = c(1,2,3,4), scaling = 0)

biplot(hep_pca, display = 'sites', scaling = 1)
biplot(hep_pca, display = 'species', scaling = 2)

df_load_hep <- as.data.frame(scores(hep_pca, display = 'species', choices = c(1,2,3,4), scaling = 2))
df_load_hep$hjust[df_load_hep$PC1 >= 0] <- -0.1
df_load_hep$hjust[df_load_hep$PC1 < 0] <- 1
df_load_hep$vjust[df_load_hep$PC2 >= 0] <- -0.1
df_load_hep$vjust[df_load_hep$PC2 < 0] <- 1

library(grid)

ar <- arrow(length = unit(0.25, 'cm'))

p_hep_load <- ggplot(df_load_hep) + geom_text(aes(x = PC1, y = PC2, label = row.names(df_load_hep)), size = 3, vjust = df_load_hep$vjust, hjust = df_load_hep$hjust) + geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), colour = 'grey40', arrow = ar) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9))

df_scores_hep <- as.data.frame(scores(hep_pca, display = 'sites', choices = c(1,2,3,4), scaling = 1))

df_scores_hep1 <- as.data.frame(scores(hep_pca, display = 'sites', choices = c(1,2,3,4), scaling = 1))

p_scores_hep <- ggplot(df_scores_hep1, aes(x = PC1, y = PC2, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

p_scores_hep_PC1_PC3 <- ggplot(df_scores_hep1, aes(x = PC1, y = PC3, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

p_scores_hep_PC2_PC3 <- ggplot(df_scores_hep1, aes(x = PC2, y = PC3, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

p_scores_hep_PC4_PC3 <- ggplot(df_scores_hep1, aes(x = PC4, y = PC3, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

p_scores_hep_PC1_PC4 <- ggplot(df_scores_hep1, aes(x = PC1, y = PC4, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

nrow(lcms2_hep)
#View(df_scores_hep1)

df_scores_hep1$Salinity <- lcms2_hep$Salinity
df_scores_hep1$Morphotype <- lcms2_hep$Morphotype

library(cowplot)

plot_grid(p_hep_load, p_scores_hep, 
          rel_widths = c(0.36, 0.64))

row.names(df_load_hep)

#Теперь проверим дисперсионным анализом, отличается ли по двум главным компонентам у нас животные

df_hep_an <- data.frame(Salinity = lcms2_hep$Salinity, Morphotype = lcms2_hep$Morphotype, scores(hep_pca, display = 'sites', choices = c(1,2,3,4), scaling = 1))
#View(df_hep_an)

hep_pca_mod <- lm(data = df_hep_an, PC1 ~ Salinity * Morphotype)
hep_pca_mod2 <- lm(data = df_hep_an, PC2 ~ Salinity * Morphotype)
anova(hep_pca_mod)
anova(hep_pca_mod2)

hep_pca_mod3 <- lm(data = df_hep_an, PC3 ~ Salinity * Morphotype)
anova(hep_pca_mod3)

hep_pca_mod4 <- lm(data = df_hep_an, PC4 ~ Salinity * Morphotype)
anova(hep_pca_mod4)

ggplot(df_hep_an, aes(x = Salinity, y = PC1)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))   

ggplot(df_hep_an, aes(x = Salinity, y = PC2)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))  

ggplot(df_hep_an, aes(x = Salinity, y = PC3)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))  

ggplot(df_hep_an, aes(x = Salinity, y = PC4)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))



###MDS_THE_BIG_ONE

data_big_one_pca <- read_excel('D:/R/Germans LCMS/Mytred_LCMS_Anton_UPD.xlsx', sheet = 'PCA_THE_BIG_ONE')
#View(data_big_one_pca)
str(data_big_one_pca)
data_big_one_pca$Salinity <- as.factor(data_big_one_pca$Salinity)
mds_big_one <- data_big_one_pca[,4:73]

#View(mds_big_one)

ord_big_one <- metaMDS(mds_big_one, distance = 'bray', k = 2)
ordiplot(ord_big_one, display = 'sites')

hep_points_big_one <- as.data.frame(ord_big_one$points)

ggplot(hep_points_big_one, aes(x = MDS1, y = MDS2, color = data_big_one_pca$Salinity, shape = data_big_one_pca$Morphotype)) + geom_point(size = 4)

ggplot(hep_points_big_one, aes(x = MDS1, y = MDS2, color = data_big_one_pca$Morphotype)) + geom_point(size = 4)

ord_big_one$stress

stressplot(ord_big_one)


###PCA THE BIG ONE

big_pca <- rda(mds_big_one, scale = TRUE)
summary(big_pca)
#View(mds_big_one)
eigenvals(big_pca)

screeplot(big_pca, type = 'lines', bstick = TRUE)
bstick(big_pca)

scores(big_pca, display = 'species', choices = c(1,2,3,4,5,6,7), scaling = 0)
scores(big_pca, display = 'sites', choices = c(1,2,3,4,5,6,7), scaling = 0)

biplot(big_pca, display = 'sites', scaling = 1)
biplot(big_pca, display = 'species', scaling = 2)

df_load_big <- as.data.frame(scores(big_pca, display = 'species', choices = c(1,2,3,4,5,6,7), scaling = 2))
df_load_big$hjust[df_load_big$PC1 >= 0] <- -0.1
df_load_big$hjust[df_load_big$PC1 < 0] <- 1
df_load_big$vjust[df_load_big$PC2 >= 0] <- -0.1
df_load_big$vjust[df_load_big$PC2 < 0] <- 1

library(grid)

ar <- arrow(length = unit(0.25, 'cm'))

p_hep_load <- ggplot(df_load_big) + geom_text(aes(x = PC1, y = PC2, label = row.names(df_load_big)), size = 3, vjust = df_load_big$vjust, hjust = df_load_big$hjust) + geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), colour = 'grey40', arrow = ar) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9))

df_scores_big <- as.data.frame(scores(big_pca, display = 'sites', choices = c(1,2,3,4,5,6,7), scaling = 1))

#df_scores_hep1 <- as.data.frame(scores(hep_pca, display = 'sites', choices = c(1,2,3,4), scaling = 1))
df_scores_big$Salinity <- data_big_one_pca$Salinity
df_scores_big$Morphotype <- data_big_one_pca$Morphotype



p_scores_big <- ggplot(df_scores_big, aes(x = PC1, y = PC2, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

#p_scores_hep_PC1_PC3 <- ggplot(df_scores_hep1, aes(x = PC1, y = PC3, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

#p_scores_hep_PC2_PC3 <- ggplot(df_scores_hep1, aes(x = PC2, y = PC3, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

#p_scores_hep_PC4_PC3 <- ggplot(df_scores_hep1, aes(x = PC4, y = PC3, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

#p_scores_hep_PC1_PC4 <- ggplot(df_scores_hep1, aes(x = PC1, y = PC4, colour = Salinity, shape = Morphotype)) + coord_equal(xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9)) + geom_point(size = 3)

df_big_an <- data.frame(Salinity = data_big_one_pca$Salinity, Morphotype = data_big_one_pca$Morphotype, scores(big_pca, display = 'sites', choices = c(1,2,3,4,5,6,7), scaling = 1))
#View(df_hep_an)

big_pca_mod <- lm(data = df_big_an, PC1 ~ Salinity * Morphotype)
big_pca_mod2 <- lm(data = df_big_an, PC2 ~ Salinity * Morphotype)
anova(big_pca_mod)
anova(big_pca_mod2)

big_pca_mod3 <- lm(data = df_big_an, PC3 ~ Salinity * Morphotype)
anova(big_pca_mod3)

big_pca_mod4 <- lm(data = df_big_an, PC4 ~ Salinity * Morphotype)
anova(big_pca_mod4)

ggplot(df_big_an, aes(x = Salinity, y = PC1)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))   

ggplot(df_big_an, aes(x = Salinity, y = PC2)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))  

ggplot(df_big_an, aes(x = Salinity, y = PC3)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))  

ggplot(df_big_an, aes(x = Salinity, y = PC4)) + geom_boxplot(aes(fill = Morphotype)) + scale_fill_manual(values = c('dodgerblue2', 'red'))




























