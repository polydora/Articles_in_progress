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


# Генетические маркеры и классические морфологические маркеры по  McDonald
ids <- read_xlsx("Data/gen_markers.xlsx", na = "NA") 

ids$Tr <- as.numeric(ids$Tr)

str(ids)


ids <- ids[complete.cases(ids[,1:6]), ]



# Датафрейм с генетическими данными и измерениями классическиз морфометрических признаков
ids_morph <- merge(ids, data.frame(file = unique(All$file)))






#################################################3
# Классическая морфометрия
#################################################

ids_morph_McDonald <- ids_morph %>% filter(complete.cases(ids_morph))


ids_morph_McDonald_traits <- ids_morph_McDonald %>% select(7:15)%>% select( -a, -L)

ids_morph_McDonald_traits <- log10(ids_morph_McDonald_traits) / log10(ids_morph_McDonald$L)



mod_rda <- rda(ids_morph_McDonald_traits ~  Ga, data = ids_morph_McDonald)


anova(mod_rda, permutations = 99999)

anova(mod_rda, by = "margin", permutations = 99999)

anova(mod_rda, by = "axis", permutations = 99999)



plot(mod_rda,  display = c("sp", "cn"))

qplot(ids_morph_McDonald$Ga, ids_morph_McDonald$L) + geom_smooth(method = "lm")

qplot(ids_morph_McDonald$Ga, ids_morph_McDonald$aam) + geom_smooth(method = "lm")

qplot(ids_morph_McDonald$Ga, ids_morph_McDonald$lpr) + geom_smooth(method = "lm")

qplot(ids_morph_McDonald$Ga, ids_morph_McDonald$hp) + geom_smooth(method = "lm")


#################################################3
# Геометрическая морфометрия
#################################################


# Удаляю те файлы, которые не имеют генетических оценок
All <- All[All$file %in% ids_morph$file, ]



# Проверка правильности заполеннеия матрицы лендмарков
All[is.na(All$V1)|is.na(All$V2),]
as.data.frame(table(All$file))




# Создание матрицы с лэндмарками
# landmarks_included <- c(1, 3, 4:14) #Чистый абрис раковины

landmarks_included <- c(1, 3, 4:14, 16:20) #абрис раковины и внутренние отпечатки


myt_matr <- array(rep(NA, length(unique(All$ID))*length(landmarks_included)*2), dim = c(length(landmarks_included), 2, length(unique(All$ID))))


for(i in 1:length(unique(All$file))){
  id <- unique(All$file)[i]
  d <- All[All$file == id , ]
  d <- d[landmarks_included, ] #Отбор лэндмарков, описывающих внешний контур раковины
  myt_matr[ , , i] <- as.matrix(d[  , c(3,4)])
  
}


# Проверка на соответствие матриц
myt_matr[ , , 25]

ids_morph[ids_morph$file == "g46_landmarks.txt", ]




# Прокрустово преобразование

myt_gpa <- gpagen(myt_matr)

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "flipX")


# Точки абриса
myt_links <- data.frame(LM1 = c(1:13,  14:17), LM2 = c(2:13, 1, 15:18))

# myt_links <- data.frame(LM1 = c(1:13), LM2 = c(2:13, 1))


myt_links <- as.matrix(myt_links)





# Усреденная мидия
plotAllSpecimens(myt_gpa$coords,mean = T, links=myt_links)


ref <- mshape(myt_gpa$coords) 

plotRefToTarget(ref, ref, method = "TPS", links = myt_links)


# # Выгнутая мидия
plotRefToTarget(ref, myt_gpa$coords[, , 16],
                method = "TPS", mag = 1,
                links = myt_links)

# # Вогнутая мидия
plotRefToTarget(ref, myt_gpa$coords[, , 25],
                method = "TPS", mag = 1,
                links = myt_links)


# Получение картинки для заданной точки в морфоспейсе

Plot_myt_gpa <- plot(gm.prcomp(myt_gpa$coords))
# picknplot.shape(Plot_myt_gpa)





# Рисую картинки для крайних точек в компонентном анализе

PC_score_myt_gpa <- gm.prcomp(myt_gpa$coords)$x

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


PC_score_myt_gpa<-as.data.frame(PC_score_myt_gpa)

ggplot(PC_score_myt_gpa, aes(x = Comp1, y = Comp2)) + geom_point(aes(color = ids_morph$Sex, size = ids_morph$Ga))



#Строим линейную модель, опсывающую форму раковины от пола и генотипа

gdf <- geomorph.data.frame(myt_gpa, Tr = ids_morph$Tr, Ed = ids_morph$Ed, Ga = ids_morph$Ga, Sex = ids_morph$Sex)


fit.genotype <- procD.lm(coords ~  Sex * Ga, data = gdf, print.progress = T, SS.type = "II") 


summary(fit.genotype)

# anova(fit.genotype)

plot(fit.genotype)



# Выводим среднюю форму характерную для разных полов

Sex <- as.numeric(ids_morph$Sex == "f") 
preds_Sex <- shape.predictor(myt_gpa$coords, x= Sex, Intercept = FALSE, 
                            pred1 = 1,  pred2 = 0) 
plotRefToTarget(ref, preds_Sex$pred1, links = myt_links)
plotRefToTarget(ref, preds_Sex$pred2, links = myt_links)





# Выводим форму среднюю  характерную для разных видов

Ga <- as.vector(ids_morph$Ga) 
preds_Ga <- shape.predictor(myt_gpa$coords, x= Ga, Intercept = FALSE, 
                             pred1 = mean(Ga[Ga<=0.2]), pred2 = mean(Ga[Ga>0.2 & Ga<0.8]), pred3 = mean(Ga[Ga>=0.8])) 
plotRefToTarget(ref, preds_Ga$pred1, links = myt_links)
plotRefToTarget(ref, preds_Ga$pred2, links = myt_links)
plotRefToTarget(ref, preds_Ga$pred3, links = myt_links)








######################################3


PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr <0.5 & PCA_scores$Ed < 0.5] <- "Ga"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed < 0.5] <- "Tr"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr < 0.5 & PCA_scores$Ed >= 0.5] <- "Ed"  
PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed < 0.5] <- "Ga_Tr"  
PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr < 0.5 & PCA_scores$Ed >= 0.5] <- "Ga_Ed"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed >= 0.5] <- "Ed_Tr"  
PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed >= 0.5] <- "Ga_Ed_Tr"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr < 0.5 & PCA_scores$Ed < 0.5] <- "Bl"  



ggplot(PCA_scores, aes(x = Comp1, y = Comp2))  + geom_point(aes(fill = Sp, shape = Sex), size = 4) + scale_shape_manual(values = c(21, 23))
  
  
  geom_text(aes(label = 1:nrow(PCA_scores)))



plotRefToTarget(myt_gpa$coords[, , 1], myt_gpa$coords[, , 1],
                method = "TPS", mag = 1,
                links = myt_links)

plotRefToTarget(myt_gpa$coords[, , 30], myt_gpa$coords[, , 30],
                method = "TPS", mag = 1,
                links = myt_links)


ggplot(PCA_scores, aes(x = Sp, y = Comp2)) + geom_boxplot(notch = T)


ord <- data.frame(PC1 = PCA_scores$Comp1, PC2 = PCA_scores$Comp2)
env <- data.frame(Tr = PCA_scores$Tr, Ed = PCA_scores$Ed, Ga =  PCA_scores$Ga) 
efit <- envfit(ord, env)
plot(ord)
plot(efit)



summary(PCA)

ggplot(PCA_scores, aes(x = Ga, y = Comp1)) + geom_point() + geom_smooth()
ggplot(PCA_scores, aes(x = Ga, y = Comp2)) + geom_point() + geom_smooth()
ggplot(PCA_scores, aes(x = Ga, y = Comp3)) + geom_point() + geom_smooth()
ggplot(PCA_scores, aes(x = Ga, y = Comp4)) + geom_point() + geom_smooth()
ggplot(PCA_scores, aes(x = Ga, y = Comp5)) + geom_point() + geom_smooth()

