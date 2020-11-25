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

ids_morph$file %in% unique(All$file)

length(unique(All$file))

#################################################3
# Классическая морфометрия
#################################################

ids_morph_McDonald <- ids_morph %>% filter(complete.cases(ids_morph))


ids_morph_McDonald_traits <- ids_morph_McDonald %>% select(9:17)%>% select( -a, -L)

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

ids_morph[ids_morph$file == "ga_42_result.txt", ]


myt_matr[ , , 25]





# Прокрустово преобразование

myt_gpa <- gpagen(myt_matr)

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "rotateC")

myt_gpa <- rotate.coords(myt_gpa, type = "flipX")


# Точки абриса
# myt_links <- data.frame(LM1 = c(1:9,  10:14), LM2 = c(2:9, 1, 11:14, 14))

myt_links <- data.frame(LM1 = c(1:9), LM2 = c(2:9, 1))


myt_links <- as.matrix(myt_links)





# Усреденная мидия
plotAllSpecimens(myt_gpa$coords,mean = T, links=myt_links)


ref <- mshape(myt_gpa$coords) 

plotRefToTarget(ref, ref, method = "TPS", links = myt_links)


# # Мидия с выпуклым брюшным краем
plotRefToTarget(ref, myt_gpa$coords[, , 22],
                method = "TPS", mag = 1,
                links = myt_links)

# # Мидия с вогнутым брюшным краем
plotRefToTarget(ref, myt_gpa$coords[, , 7],
                method = "TPS", mag = 1,
                links = myt_links)

##################################################################################
# # Аномальная мидия
plotRefToTarget(ref, myt_gpa$coords[, , 25],
                method = "TPS", mag = 1,
                links = myt_links)



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

PC_score_myt_gpa_df <- data.frame(PC_score_myt_gpa_df[, 1:4], Sp = ids_morph$Sp)

PC_score_myt_gpa_df$Sp3 <- NA 
  
PC_score_myt_gpa_df$Sp3 [PC_score_myt_gpa_df$Sp == "Ga"] <- "Ga"

PC_score_myt_gpa_df$Sp3 [PC_score_myt_gpa_df$Sp == "Ed"] <- "Ed"

PC_score_myt_gpa_df$Sp3 [is.na(PC_score_myt_gpa_df$Sp3)]<- "H"



PC_score_myt_gpa_df$Sp <- factor(PC_score_myt_gpa_df$Sp, levels = c("Ed", "EdTr", "EdGaTr", "EdGa",  "GaTr", "Ga"))

PC_score_myt_gpa_df$Sp3 <- factor(PC_score_myt_gpa_df$Sp3, levels = c("Ed", "H", "Ga"))


PC_score_myt_gpa_df$Tr <- ids_morph$Tr 
PC_score_myt_gpa_df$Ed <- ids_morph$Ed 
PC_score_myt_gpa_df$Ga <- ids_morph$Ga 



Pl_pca <- ggplot(PC_score_myt_gpa_df, aes(y = Comp2, x = Comp1)) + 
  geom_point(aes(shape = Sp3, fill = Sp3), size = 4)  + 
  scale_shape_manual(values = c(21:25, 11)) +
  scale_fill_manual(values = c("blue", "black", "yellow")) 

# +
#   xlim(-0.5, 0.5)

# +
#   labs(x = paste("PC 2 (",PC2_prop, "%)", sep =""), y = paste("PC 1 (",PC1_prop, "%)", sep =""))  + 
#   theme(legend.position = "bottom")



ggplot(PC_score_myt_gpa_df, aes(x = Sp, y = Comp1)) + geom_boxplot()

ggplot(PC_score_myt_gpa_df, aes(x = Sp, y = Comp2)) + geom_boxplot()



#Строим линейную модель, опсывающую форму раковины от пола и генотипа

gdf <- geomorph.data.frame(myt_gpa, Ga = ids_morph$Ga, Ed = ids_morph$Ed, Ga = ids_morph$Ga, Sp = ids_morph$Sp, Sp3 = PC_score_myt_gpa_df$Sp3, Sex = ids_morph$Sex)


fit.genotype <- procD.lm(coords ~  Sp, data = gdf, print.progress = T, SS.type = "II") 


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

