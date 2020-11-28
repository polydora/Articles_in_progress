library(geomorph)
library(ggplot2)
library(vegan)
library(dplyr)
library(readxl)



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

# Генетические маркеры и классические морфологические маркеры по  McDonald
ids <- read_xlsx("Data/Species_from_different_areas.xlsx", na = "NA") 

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

landmarks_included <- c(1:9) #Чистый абрис раковины

# landmarks_included <- c(1:14) #абрис раковины и внутренние отпечатки

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
PC_score_myt_gpa_df <- data.frame(PC_score_myt_gpa_df[, 1:4], Sp = ids$Sp[-51], Area = ids$Area[-51])

# PC_score_myt_gpa_df <- data.frame(PC_score_myt_gpa_df[, 1:4], Sp = ids$Sp, Area = ids$Area)


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


PC_score_myt_gpa_df$Area <- factor(PC_score_myt_gpa_df$Area, levels = c("Gaseid", "Bergen", "Ar", "Candao", "Porto", "Vigo"))


ggplot(PC_score_myt_gpa_df, aes(y = Comp2, x = Comp1)) + 
  geom_point(aes(shape = Sp3, fill = Sp3, alpha = Area2), size = 4)  + 
  scale_shape_manual(values = c(21:25, 11)) +
  scale_fill_manual(values = c("red", "blue", "gray", "yellow")) +
  scale_alpha_manual(values = c(0.3, 1)) +
  geom_text(data = PC_score_myt_gpa_df_Gaseid, aes(label = Sp))+
  theme_bw() +
  theme(panel.grid = element_blank()) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# +
#   xlim(-0.5, 0.5)

# +
#   labs(x = paste("PC 2 (",PC2_prop, "%)", sep =""), y = paste("PC 1 (",PC1_prop, "%)", sep =""))  + 
#   theme(legend.position = "bottom")



ggplot(PC_score_myt_gpa_df, aes(x = Sp, y = Comp1)) + geom_boxplot() + facet_grid(~Area) + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90))

ggplot(PC_score_myt_gpa_df, aes(x = Sp, y = Comp2)) + geom_boxplot() + facet_grid(~Area) + geom_hline(yintercept = 0) + theme(axis.text.x = element_text(angle = 90))



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

