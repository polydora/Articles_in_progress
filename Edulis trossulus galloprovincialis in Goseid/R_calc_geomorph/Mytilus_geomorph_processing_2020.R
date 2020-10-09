library(geomorph)
library(ggplot2)
library(vegan)
library(dplyr)
library(readxl)



# digitize2d("images/JPG_m1/MorphTrEd_1_Tros_29_LM.jpg",13,tpsfile="MorphTrEd_1_Tros_29_LM")



reads <- function(path = "Data/"){

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


ids <- read_xls("Data/geomorph_gen_ID.xls", sheet = "geomorph_gen_ID")


# Проверка правильности заполеннеия матрицы лендмарков
All[is.na(All$V1)|is.na(All$V2),]
table(All$file)




# Создание матрицы с лэндмарками
myt_matr <- array(rep(NA, length(unique(All$ID))*20*2), dim = c(20, 2, length(unique(All$ID))))


for(i in 1:length(unique(All$ID))){
  id <- unique(All$ID)[i]
  d <- All[All$ID ==id , ]
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



plotAllSpecimens(myt_gpa$coords)



PCA <- gm.prcomp(myt_gpa$coords)

plot(PCA)
str(PCA)

PCA_scores <- as.data.frame(PCA$x)

PCA_scores$ID <- 1:nrow(PCA_scores)

PCA_scores <- merge(PCA_scores, ids)


gen_markers <- read_xlsx("Data/gen_markers.xlsx")


PCA_scores <- merge(PCA_scores, gen_markers, by = "Gen_ID")


ggplot(PCA_scores, aes(x = Comp1, y = Comp2)) + geom_point(aes(size = galo))


ggplot(PCA_scores, aes(x = Comp1, y = Comp2)) + geom_point(aes(size = Sex))




# ref <- mshape(Y.gpa$coords)

myt_ref <- mshape(myt_gpa$coords)


plotRefToTarget(myt_ref, myt_ref, method = "TPS")

m <- matrix(data = c(1, 1, 2, 2,
                     3, 3, 3, 3,
                     3, 3, 3, 3),
            nrow = 3, ncol = 4, byrow = TRUE)

l <- layout(m, heights = c(3, 2))


op <- par( mar = c(4, 4, 1, 1))
plotRefToTarget(myt_ref, myt_gpa$coords[, , 7], method = "vector", mag = 1)
plotRefToTarget(myt_ref, myt_gpa$coords[, , 1], method = "points", mag = 1)


plotRefToTarget(myt_ref, myt_gpa$coords[, , 2], method = "TPS", mag = 2)

par(op)



labels <- rep(NA,length(unique(All$ID)) )
  
for(i in 1:length(unique(All$ID))) {
  id <- unique(All$ID)[i]
  d <- All[All$ID ==id , ]
  labels[i] <- as.character((d[1,3]))
}

Spec <- labels
Site <- labels
#????? ??????????????? ? for ????????? 1,3 -> site -> ????????? ?? 1,2 -> Spec

# 
# 
# colvec <- c(" Morphtred_1" = "yellow"," Morphtred_5" = "red")

# colvec <- c("E" = "yellow2","T" = "red")


colvec_site <- Site 

colvec_site [Site == "Morphtred_1"] <- "red"
colvec_site [Site == "Morphtred_2"] <- "yellow"
colvec_site [Site == "Morphtred_4"] <- "black"
colvec_site [Site == "Morphtred_5"] <- "blue"


colvec_spec <- Spec 
colvec_spec [Spec == "Tr"] <- "red"

colvec_spec [Spec == "Ed"] <- "yellow"



summary(myt_gpa)

# plotTangentSpace(myt_gpa$coords, axis1 = 1, axis2 = 2, groups = colvec_site, warpgrids = T )

plotTangentSpace(myt_gpa)


gdf <- geomorph.data.frame(myt_gpa, species = Spec, site = Site)


myt_lm <- procD.lm(coords ~ species * site, data = gdf, iter = 999, RRPP = TRUE) # randomize residuals

summary(myt_lm)

plot(myt_lm, type = "PC", pch = 19, col = colvec_site)



# Усредненная мдия
myt_ref <- mshape(myt_gpa$coords[ , , ])


plotRefToTarget(myt_ref, myt_ref, method = "TPS")



# # Усредненная Мидия из Morphtred_1
# 
# myt_ref_Morphtred_1 <- mshape(myt_gpa$coords[ , , which(gdf$site == "Morphtred_1") ])
# 
# 
# plotRefToTarget(myt_ref_Morphtred_1, myt_ref, method = "TPS")
# 
# 
# # Усредненная Мидия из Morphtred_2
# 
# myt_ref_Morphtred_2 <- mshape(myt_gpa$coords[ , , which(gdf$site == "Morphtred_2") ])
# 
# 
# plotRefToTarget(myt_ref_Morphtred_2, myt_ref, method = "TPS")
# 
# 
# 
# # Усредненная Мидия из Morphtred_4
# 
# myt_ref_Morphtred_4 <- mshape(myt_gpa$coords[ , , which(gdf$site == "Morphtred_4") ])
# 
# 
# plotRefToTarget(myt_ref_Morphtred_4, myt_ref, method = "TPS")
# 
# 
# # Усредненная Мидия из Morphtred_5
# 
# myt_ref_Morphtred_5 <- mshape(myt_gpa$coords[ , , which(gdf$site == "Morphtred_5") ])
# 
# 
# plotRefToTarget(myt_ref_Morphtred_5, myt_ref, method = "TPS")
# 
# 
# 
# # Искривления раковины средней для каждого сайта
# 
# plotRefToTarget(myt_ref_Morphtred_4, myt_ref_Morphtred_4, method = "TPS")
# 
# 
# plotRefToTarget(myt_ref_Morphtred_4, myt_ref_Morphtred_5, method = "TPS")
# 
# plotRefToTarget(myt_ref_Morphtred_4, myt_ref_Morphtred_1, method = "TPS")
# 
# 
# plotRefToTarget(myt_ref_Morphtred_4, myt_ref_Morphtred_2, method = "TPS")
# 
# 
# 
# 
# 
# myt_pairs <- advanced.procD.lm(f1= myt_gpa$coords ~ Site + Spec, f2=  ~  Spec, 
#                                    groups = ~ Site, iter=999)
# summary(myt_pairs)
# 
# plot(myt_lm)
# 
# 
#  morphol.disparity(coords ~ site + species, groups= ~ site, data = gdf, iter=499)
# 
#                   