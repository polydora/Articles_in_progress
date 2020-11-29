library(geomorph)
library(ggplot2)
library(vegan)
library(dplyr)
library(readxl)
library(reshape2)




# digitize2d("images/JPG_m1/MorphTrEd_1_Tros_29_LM.jpg",13,tpsfile="MorphTrEd_1_Tros_29_LM")



reads <- function(path = "./Data/Lands/"){

  files1 <- list.files(path)
  
  i = 1
  data <- read.table(paste(path, files1[i], sep = ""))
  
  measure1 <- data.frame(ID = i, file = files1[i], data)
  
  for(i in 2:length(files1)) {
    data <- read.table(paste(path, files1[i], sep = ""))
    print(i)
    X <- data.frame(ID = i, file = files1[i], data) 
    measure1 <- rbind(measure1, X)
  } 
  measure1
}



All <- reads()

ID <- All %>% select(ID, file) %>% unique(.)


# write.table(ID, "clipboard", row.names = F, sep = "\t")

# Генетические маркеры
ids <- read_xlsx("Data/Species_from_different_areas.xlsx", na = "NA") 

str(ids)

ids$Tr <- as.numeric(ids$Tr)
ids$Ed <- as.numeric(ids$Ed)
ids$Ga <- as.numeric(ids$Ga)

str(ids)



#################################################3
# Геометрическая морфометрия
#################################################


# Удаляю те файлы, которые не имеют генетических оценок
All <- All[All$file %in% ids$file, ]



# Проверка правильности заполеннеия матрицы лендмарков
All[is.na(All$V1)|is.na(All$V2),]
as.data.frame(table(All$file))




# Создание матрицы с лэндмарками

# landmarks_included <- c(1:9) #Чистый абрис раковины

landmarks_included <- c(1:14) #абрис раковины и внутренние отпечатки

# ++++++++++++++++++++++++++


myt_matr <- array(rep(NA, length(unique(All$ID))*length(landmarks_included)*2), dim = c(length(landmarks_included), 2, length(unique(All$ID))))


for(i in 1:length(unique(All$file))){
  id <- unique(All$file)[i]
  d <- All[All$file == id , ]
  d <- d[landmarks_included, ] #Отбор лэндмарков, описывающих внешний контур раковины
  myt_matr[ , , i] <- as.matrix(d[  , c(3,4)])
  
}


# Проверка на соответствие матриц

ids[ids$file == "ga_42_result.txt", ]


myt_matr[ , , 51]





# Прокрустово преобразование

myt_gpa <- gpagen(myt_matr)

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "flipX")


# Точки абриса
myt_links <- data.frame(LM1 = c(1:9,  10:14), LM2 = c(2:9, 1, 11:14, 14))

# myt_links <- data.frame(LM1 = c(1:9), LM2 = c(2:9, 1))


myt_links <- as.matrix(myt_links)





# Усреденная мидия
plotAllSpecimens(myt_gpa$coords,mean = T, links=myt_links)


ref <- mshape(myt_gpa$coords) 

plotRefToTarget(ref, ref, method = "TPS", links = myt_links)


# # Мидия с выпуклым брюшным краем
plotRefToTarget(ref, myt_gpa$coords[, , 9],
                method = "TPS", mag = 1,
                links = myt_links)

# # Мидия с вогнутым брюшным краем
plotRefToTarget(ref, myt_gpa$coords[, , 55],
                method = "TPS", mag = 1,
                links = myt_links)



# # Мидия с высоким спинным краем
plotRefToTarget(ref, myt_gpa$coords[, , 22],
                method = "TPS", mag = 1,
                links = myt_links)


##################################################################################
# # Аномальная мидия
plotRefToTarget(ref, myt_gpa$coords[, , 51],
                method = "TPS", mag = 1,
                links = myt_links)



# Удаляем аномальную мидию

myt_gpa <- gpagen(myt_matr[ , ,-51])

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "flipX")




# Получение картинки для заданной точки в морфоспейсе

# Ординация мидий в осях PCA

pca_myt_gpa <- gm.prcomp(myt_gpa$coords)

Plot_myt_gpa <- plot(pca_myt_gpa)

# Локатор 
picknplot.shape(Plot_myt_gpa)

##########################################################################33333



# Рисую картинки для крайних точек в компонентном анализе

PC_score_myt_gpa <- pca_myt_gpa$x

PC1 <- PC_score_myt_gpa[ , 1]
preds_PC1 <- shape.predictor(myt_gpa$coords, x= PC1, Intercept = FALSE, 
                         pred1 = min(PC1), pred2 = mean(PC1), pred3 = max(PC1)) 
plotRefToTarget(ref, preds_PC1$pred1, links = myt_links)
plotRefToTarget(ref, preds_PC1$pred2, links = myt_links)
plotRefToTarget(ref, preds_PC1$pred3, links = myt_links)


PC2 <- PC_score_myt_gpa[ , 2]
preds_PC2 <- shape.predictor(myt_gpa$coords, x= PC2, Intercept = FALSE, 
                             pred1 = min(PC2), pred2 = mean(PC2), pred3 = max(PC2)) 
plotRefToTarget(ref, preds_PC2$pred1, links = myt_links)
plotRefToTarget(ref, preds_PC2$pred2, links = myt_links)
plotRefToTarget(ref, preds_PC2$pred3, links = myt_links)





#### Рисуем ординацию мидий в морфоспейсе 

PC_score_myt_gpa_df<-as.data.frame(PC_score_myt_gpa)

# 
PC_score_myt_gpa_df <- data.frame(PC_score_myt_gpa_df[, 1:2], Sp = ids$Sp[-51], Area = ids$Area[-51], Tr = ids$Tr[-51], Ed = ids$Ed[-51], Ga = ids$Ga[-51])

# PC_score_myt_gpa_df <- data.frame(PC_score_myt_gpa_df[, 1:2], Sp = ids$Sp, Area = ids$Area)


PC_score_myt_gpa_df$Area2 <- ifelse(PC_score_myt_gpa_df$Area == "Gaseid", "Gaseid", "Ref")



PC_score_myt_gpa_df <- PC_score_myt_gpa_df[complete.cases(PC_score_myt_gpa_df), ]


PC_score_myt_gpa_df$Sp3 <- NA 
  
PC_score_myt_gpa_df$Sp3 [PC_score_myt_gpa_df$Sp == "Ga"] <- "Ga"

PC_score_myt_gpa_df$Sp3 [PC_score_myt_gpa_df$Sp == "Ed"] <- "Ed"

PC_score_myt_gpa_df$Sp3 [PC_score_myt_gpa_df$Sp == "Tr"] <- "Tr"

PC_score_myt_gpa_df$Sp3 [is.na(PC_score_myt_gpa_df$Sp3)]<- "H"




PC_score_myt_gpa_df$Sp <- factor(PC_score_myt_gpa_df$Sp, levels = c("Tr", "Ed", "EdTr", "EdGaTr", "EdGa",  "GaTr", "Ga"))

PC_score_myt_gpa_df$Sp3 <- factor(PC_score_myt_gpa_df$Sp3, levels = c("Tr", "Ed", "H", "Ga"))

PC_score_myt_gpa_df_Gaseid <- PC_score_myt_gpa_df %>% filter(Area == "Gaseid")

PC_score_myt_gpa_df_Ref <- PC_score_myt_gpa_df %>% filter(Area2 == "Ref")


# Квартили по каждой из компоенент

PC1_Q25 <- quantile(PC_score_myt_gpa_df$Comp1, probs = 0.25)
PC1_Q50 <- quantile(PC_score_myt_gpa_df$Comp1, probs = 0.5)
PC1_Q75 <- quantile(PC_score_myt_gpa_df$Comp1, probs = 0.75)

PC2_Q25 <- quantile(PC_score_myt_gpa_df$Comp2, probs = 0.25)
PC2_Q50 <- quantile(PC_score_myt_gpa_df$Comp2, probs = 0.5)
PC2_Q75 <- quantile(PC_score_myt_gpa_df$Comp2, probs = 0.75)




ggplot() + aes(y = Comp2, x = Comp1)+
  geom_point(data = PC_score_myt_gpa_df_Ref, aes(fill = Sp3, shape = Sp3),  size = 4)  +   scale_shape_manual(values = c(21:25, 11)) +
  scale_fill_manual(values = c("red", "blue", "yellow", "gray")) +
  geom_point(data = PC_score_myt_gpa_df_Gaseid, shape = 24, size = 2, fill = "gray") +     theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = c(PC2_Q25, PC2_Q50, PC2_Q75), linetype = 2) +
  geom_vline(xintercept = c(PC1_Q25, PC1_Q50, PC1_Q75), linetype = 2) +
  theme(legend.position = "bottom") + 
  labs(shape = "Референсные генотипы")+
  guides(fill = "none")


# Значения компонент у референсных генотипов

ggplot(PC_score_myt_gpa_df_Ref, aes(x = Sp3, y = Comp1, fill = Sp3)) + geom_boxplot()  + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 0)) + theme_bw() + scale_fill_manual(values = c("red", "blue", "yellow", "gray")) + guides(fill = "none")

ggplot(PC_score_myt_gpa_df_Ref, aes(x = Sp3, y = Comp2, fill = Sp3)) + geom_boxplot()  + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 0)) + theme_bw() + scale_fill_manual(values = c("red", "blue", "yellow", "gray")) + guides(fill = "none")


# 
# 
# # Распределение по квадрантам
# 
# PC_score_myt_gpa_df_Gaseid$Quadr <-  
#   case_when(PC_score_myt_gpa_df_Gaseid$Comp1 >=0 & PC_score_myt_gpa_df_Gaseid$Comp2 >= 0 ~ "I",
#             PC_score_myt_gpa_df_Gaseid$Comp1 < 0 & PC_score_myt_gpa_df_Gaseid$Comp2 > 0 ~ "II",
#             PC_score_myt_gpa_df_Gaseid$Comp1 < 0 & PC_score_myt_gpa_df_Gaseid$Comp2 < 0 ~ "III",
#             PC_score_myt_gpa_df_Gaseid$Comp1 > 0 & PC_score_myt_gpa_df_Gaseid$Comp2 < 0 ~ "IV")

# 
# PC_score_myt_gpa_df_Gaseid %>%  group_by(Quadr, Sp) %>% summarise(freq = n()) %>% ggplot(., aes(x = Sp, y = freq)) + geom_col() + facet_wrap(~Quadr)

# 
# PC_score_myt_gpa_df_Gaseid_long <- melt(PC_score_myt_gpa_df_Gaseid, id.vars = c("Comp1","Comp2","Sp","Area",  "Area2", "Sp3", "Quadr"), variable.name = "Species", value.name = "q_score")
# 
# 
# ggplot(PC_score_myt_gpa_df_Gaseid_long, aes(x = Species, y = q_score)) + geom_boxplot() + facet_wrap(~ Quadr) + geom_hline(yintercept = 0.5)
# 



# Распределение квартилям первой главной компоненты

PC1_Q25 <- quantile(PC_score_myt_gpa_df_Gaseid$Comp1, probs = 0.25)
PC1_Q50 <- quantile(PC_score_myt_gpa_df_Gaseid$Comp1, probs = 0.5)
PC1_Q75 <- quantile(PC_score_myt_gpa_df_Gaseid$Comp1, probs = 0.75)


# PC_score_myt_gpa_df_Gaseid$Quant <-  
#   case_when(PC_score_myt_gpa_df_Gaseid$Comp1 <= PC1_Q25 ~ "A",
#             PC_score_myt_gpa_df_Gaseid$Comp1 <= PC1_Q50 & PC_score_myt_gpa_df_Gaseid$Comp1 > PC1_Q25 ~ "B",
#             PC_score_myt_gpa_df_Gaseid$Comp1 <= PC1_Q75 & PC_score_myt_gpa_df_Gaseid$Comp1 > PC1_Q50 ~ "C",
#             PC_score_myt_gpa_df_Gaseid$Comp1 > PC1_Q75  ~ "D")
# 
# 
# PC_score_myt_gpa_df_Gaseid_long2 <- melt(PC_score_myt_gpa_df_Gaseid, id.vars = c("Comp1","Comp2","Sp","Area",  "Area2", "Sp3", "Quadr", "Quant"), variable.name = "Species", value.name = "q_score")
# 
# 
# ggplot(PC_score_myt_gpa_df_Gaseid_long2, aes(x = Species, y = q_score)) + geom_boxplot() + facet_wrap(~ Quant, nrow = 1) + geom_hline(yintercept = 0.5)
# 


# ++++++++++++++++++++++

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", se = F,  method.args = list(family = "binomial"), ...)
}

ggplot(PC_score_myt_gpa_df_Gaseid_long, aes(x = Comp1, y = q_score, color = Species, fill = Species)) +
  geom_point(shape = 21, size = 4, color = "black") +  
  binomial_smooth(formula = y ~ splines::ns(x, 3)) +
  scale_fill_manual(values = c("red", "blue", "yellow")) +
  scale_color_manual(values = c("red", "blue", "yellow")) + 
  theme(panel.background = element_rect(fill = "gray70"), panel.grid = element_blank()) +   geom_vline(xintercept = c(PC1_Q25, PC1_Q50, PC1_Q75), linetype = 2) + 
  labs(x = "Shape PC1", y = "Structure score")
  


  
  
ggplot(PC_score_myt_gpa_df_Gaseid_long, aes(x = Comp2, y = q_score, color = Species, fill = Species)) +
  geom_point(shape = 21, size = 4, color = "black") +  
  binomial_smooth(formula = y ~ splines::ns(x, 3)) +
  scale_fill_manual(values = c("red", "blue", "yellow")) +
  scale_color_manual(values = c("red", "blue", "yellow")) + 
  theme(panel.background = element_rect(fill = "gray70"), panel.grid = element_blank()) +   geom_vline(xintercept = c(PC2_Q25, PC2_Q50, PC2_Q75), linetype = 2) 





# #Строим линейную модель, опсывающую форму раковины от пола и генотипа
# 
# gdf <- geomorph.data.frame(myt_gpa, Ga = ids_morph$Ga, Ed = ids_morph$Ed, Ga = ids_morph$Ga, Sp = ids_morph$Sp, Sp3 = PC_score_myt_gpa_df$Sp3, Sex = ids_morph$Sex)
# 
# 
# fit.genotype <- procD.lm(coords ~  Sp, data = gdf, print.progress = T, SS.type = "II") 
# 
# 
# summary(fit.genotype)
# 
# # anova(fit.genotype)
# 
# plot(fit.genotype)
# 







# Выводим форму среднюю  характерную для разных видов

Ga <- as.vector(ids_morph$Ga) 

X <- model.matrix(~ Sp - 1, data = ids_morph )


preds_Ed <- shape.predictor(myt_gpa$coords, x = X[,1], Intercept = FALSE, 
                             pred1 = 1)
preds_Ga <- shape.predictor(myt_gpa$coords, x = X[,5], Intercept = FALSE, 
                            pred1 = 1)

preds_EdGa <- shape.predictor(myt_gpa$coords, x = X[,2], Intercept = FALSE, 
                            pred1 = 1)

preds_EdGaTr <- shape.predictor(myt_gpa$coords, x = X[,3], Intercept = FALSE, 
                              pred1 = 1)

preds_EdTr <- shape.predictor(myt_gpa$coords, x = X[,4], Intercept = FALSE, 
                                pred1 = 1)
preds_GaTr <- shape.predictor(myt_gpa$coords, x = X[,6], Intercept = FALSE, 
                              pred1 = 1)



plotRefToTarget(ref, ref, links = myt_links)

plotRefToTarget(ref, preds_Ed$pred1, links = myt_links)
plotRefToTarget(ref, preds_Ga$pred1, links = myt_links)
plotRefToTarget(ref, preds_EdGa$pred1, links = myt_links)
plotRefToTarget(ref, preds_EdGaTr$pred1, links = myt_links)
plotRefToTarget(ref, preds_EdTr$pred1, links = myt_links)
plotRefToTarget(ref, preds_GaTr$pred1, links = myt_links)


plotRefToTarget(preds_Ga$pred1, preds_EdGa$pred1, links = myt_links)

