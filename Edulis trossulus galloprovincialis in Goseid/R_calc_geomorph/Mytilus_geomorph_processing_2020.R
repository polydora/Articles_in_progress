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


ids <- read_xlsx("Data/gen_markers.xlsx") 
ids$Tr <- as.numeric(ids$Tr)
ids$Ed <- as.numeric(ids$Ed)
ids$Ga <- as.numeric(ids$Ga)




ids <- ids[complete.cases(ids), ]

ids <- merge(ids, data.frame(file = unique(All$file)))



# Удаляю те файлы, которые не имеют генетических оценок
All <- All[All$file %in% ids$file, ]



# Проверка правильности заполеннеия матрицы лендмарков
All[is.na(All$V1)|is.na(All$V2),]
as.data.frame(table(All$file))




# Создание матрицы с лэндмарками
myt_matr <- array(rep(NA, length(unique(All$ID))*20*2), dim = c(20, 2, length(unique(All$ID))))


for(i in 1:length(unique(All$file))){
  id <- unique(All$file)[i]
  d <- All[All$file == id , ]
  myt_matr[ , , i] <- as.matrix(d[ , c(3,4)])
  
}



# Прокрустово преобразование

myt_gpa <- gpagen(myt_matr)

# Точки абриса
myt_links <- data.frame(LM1 = c(1, 3, 4:13, 2, 16, 17, 18, 19), LM2 = c(3:13, 1, 15, 17, 18, 19, 20))

myt_links <- as.matrix(myt_links)

# Усреденная мидия
plotAllSpecimens(myt_gpa$coords,mean = T, links=myt_links)

ref <- mshape(myt_gpa$coords) 

plotRefToTarget(ref, ref, method = "TPS", links = myt_links)


# Выгнутая мидия
plotRefToTarget(ref, myt_gpa$coords[, , 16], 
                method = "vector", mag = 1, 
                links = myt_links)

# Вогнутая мидия
plotRefToTarget(ref, myt_gpa$coords[, , 29], 
                method = "vector", mag = 1, 
                links = myt_links)





plotRefToTarget(myt_gpa$coords[, , 16], myt_gpa$coords[, , 29],
                method = "vector", mag = 1,
                links = myt_links)


plotRefToTarget(myt_gpa$coords[, , 29], myt_gpa$coords[, , 16],
                method = "vector", mag = 1,
                links = myt_links)




PCA <- gm.prcomp(myt_gpa$coords)

plot(PCA)

PCA_scores <- as.data.frame(PCA$x)

PCA_scores$file <- unique(All$file)



PCA_scores <- merge(PCA_scores, ids, by = "file")

PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr <0.5 & PCA_scores$Ed < 0.5] <- "Ga"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed < 0.5] <- "Tr"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr < 0.5 & PCA_scores$Ed >= 0.5] <- "Ed"  
PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed < 0.5] <- "Ga_Tr"  
PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr < 0.5 & PCA_scores$Ed >= 0.5] <- "Ga_Ed"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed >= 0.5] <- "Ed_Tr"  
PCA_scores$Sp[PCA_scores$Ga >= 0.5 & PCA_scores$Tr >= 0.5 & PCA_scores$Ed >= 0.5] <- "Ga_Ed_Tr"  
PCA_scores$Sp[PCA_scores$Ga < 0.5 & PCA_scores$Tr < 0.5 & PCA_scores$Ed < 0.5] <- "Bl"  



ggplot(PCA_scores, aes(x = Comp1, y = Comp2)) + geom_point(aes(fill = Sp), shape = 21, size = 4, position = "jitter")


ggplot(PCA_scores, aes(x = Sp, y = Comp1)) + geom_boxplot(notch = F)


ord <- data.frame(PC1 = PCA_scores$Comp1, PC2 = PCA_scores$Comp2)
env <- data.frame(Tr = PCA_scores$Tr, Ed = PCA_scores$Ed, Ga =  PCA_scores$Ga) 
efit <- envfit(ord, env)
plot(ord)
plot(efit)
