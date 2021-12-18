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
gds <- read_xlsx("Data/gds_Granitny.xlsx", na = "NA") 
nrow(gds)

gds<- gds %>% mutate(Species = ifelse(Structure > 0.5, "Tr", "Ed"))

gds <- gds %>% filter(!is.na(ID))

#################################################3
# Геометрическая морфометрия
#################################################


# Удаляю те файлы, которые не имеют генетических оценок
# All <- All[All$file %in% ids$file, ]



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




myt_matr[ , , 50]





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


# # Мидия с structure = 0.999
plotRefToTarget(ref, myt_gpa$coords[, , 1],
                method = "TPS", mag = 1,
                links = myt_links)

# # Мидия с structure = 0.001
plotRefToTarget(ref, myt_gpa$coords[, , 44],
                method = "TPS", mag = 1,
                links = myt_links)


# Получение картинки для заданной точки в морфоспейсе

# Ординация мидий в осях PCA

pca_myt_gpa <- gm.prcomp(myt_gpa$coords)

Plot_myt_gpa <- plot(pca_myt_gpa)

# Локатор 
# picknplot.shape(Plot_myt_gpa)

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

ggplot(PC_score_myt_gpa_df, aes(Comp1, Comp2)) + 
  geom_point(aes(color = (gds$Species)), size = 3) + 
  theme_bw() + 
  scale_color_manual(values = c("blue", "red")) +
  labs(color = "Генотипы")











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

