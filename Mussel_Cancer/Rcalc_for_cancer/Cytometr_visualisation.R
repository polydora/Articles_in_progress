# Скрипт для анализа даных с проточного цитометра
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.10")


# BiocManager::install("flowCore")
# BiocManager::install("ggcyto")
# BiocManager::install("flowMap")

library(flowCore)
library(ggcyto)

# um22 <-read.FCS("Data/FCS/UM22.fcs", transformation=FALSE) 
# um36 <-read.FCS("Data/FCS/UM36.fcs", transformation=FALSE) 
# um2 <-read.FCS("Data/FCS/UM2.fcs", transformation=FALSE) 
# um41 <-read.FCS("Data/FCS/UM41.fcs", transformation=FALSE) 
# um3 <-read.FCS("Data/FCS/UM3.fcs", transformation=FALSE) 
# um4 <-read.FCS("Data/FCS/UM4.fcs", transformation=FALSE) 
# 
# 
# autoplot(um22,"FL1-H","FL2-H")
# autoplot(um22,"FL1-H")
# 
# 
# 
# # fs <-read.flowSet(path = "Data/FCS/")
# # 
# # pl <- autoplot(x, "FSC-A", "SSC-A")
# # fortify(fs)
# 
# 


df_extractor <- function(x = um22, Gate_FSC_A = 260E4, Gate_SSC_A = 440E4){
  require(dplyr)
  df <- fortify(x)
  names(df) <- gsub("-", "_", names(df))
  df <- df %>% filter(FSC_A < Gate_FSC_A & SSC_A < Gate_SSC_A & FSC_A > 0 & SSC_A > 0)
  df <- df %>% select(-.rownames, -name, -Time)
  df$id <- 1
  as.data.frame(df)
}


df_extractor_2 <- function(x = um22, Gate_FSC_A = 260E4, Gate_SSC_A = 440E4){
  require(dplyr)
  df <- fortify(x)
  names(df) <- gsub("-", "_", names(df))
  df <- df %>% filter(FSC_A < Gate_FSC_A & SSC_A < Gate_SSC_A & FSC_A > 0 & SSC_A > 0)
  df <- df %>% select()
  df$id <- 1
  as.data.frame(df)
}




files <- list.files("Data/FCS", pattern = "^UM")
dfs <- list(NULL)

for(i in 1:length(files)){
  case <- read.FCS(filename = paste("Data/FCS/",files[i], sep = ""), transformation=FALSE)
  dfs[[i]] <- df_extractor(x = case)
  print(i)
  
}



str(dfs)



# Визуализация данных проточной цитометрии для избранных файлов



plot_cytometry <- function(file = "UM22.fcs"){
  require(ggplot2)
  require(dplyr)
  df <- dfs[[which(files == file)]]
  Pl_SSC_FSC <- ggplot(df, aes(x = FSC_A, y = SSC_A))+
    geom_point(color = "grey20", size = 0.01)+
    geom_density2d(color = "yellow") +
    geom_point(aes(x=median(FSC_A), y= median(SSC_A)), color="blue", size=4)+
    ggtitle(file)
  Pl_DAPI <- ggplot(df, aes(x = FL7_A)) + 
    geom_density() +
    scale_x_continuous(trans='log10')+
    ggtitle(file)
  
  Plots <- list(Pl_SSC_FSC, Pl_DAPI)

}

library(patchwork)

(plot_cytometry("UM4.fcs")[[1]] + plot_cytometry("UM4.fcs")[[2]]) /
(plot_cytometry("UM41.fcs")[[1]] + plot_cytometry("UM41.fcs")[[2]]) / 
(plot_cytometry("UM36.fcs")[[1]] + plot_cytometry("UM36.fcs")[[2]]) /
((plot_cytometry("UM48.fcs")[[1]] + plot_cytometry("UM48.fcs")[[2]]))



ggplot(dfs, aes(x = FSC_A, y = SSC_A)) +
  geom_point(color = "grey20", size = 0.1)+
  geom_density2d(color = "yellow")+
  facet_wrap(~ Name)+
  ylim(0, 440E4) +
  xlim(0, 260E4)

###########################


# BiocManager::install("flowMap")

library(flowMap)
library(dplyr)



files[1]

names(dfs[[1]])


medians <- data.frame(Med_FSC = rep(NA, length(files)), Med_SSC = NA)

for(i in 1:length(files)){
medians$File[i] <- files[i]  
medians$Med_FSC[i] <- median(dfs[[i]]$FSC_A)
medians$Med_SSC[i] <- median(dfs[[i]]$SSC_A)
medians$Med_DAPI[i] <- median(dfs[[i]]$FL7_A)

print(i)
}




resMulti <- makeDistmat(samples=dfs, sampleSize=100,ndraws=10)

save(resMulti, file = "floMap.RData")

load("floMap.RData")


library(vegan)

Dist_Matr <- resMulti$distmat


diag(Dist_Matr) <- 0


Dist_Matr <- abs(Dist_Matr)

ord <- metaMDS(com = Dist_Matr, autotransform = F)

ord$points

plot(ord, type = "t")



dots <- data.frame(id = 1:length(files), File = files, ord$points)

dots$Mussel <- gsub(".fcs", "", dots$File)
dots$Mussel <- gsub("UM", "", dots$Mussel)


dots2 <- merge(medians, dots, by = "File")

ggplot(dots2, aes(x = Med_DAPI,  y =  N_dots, size = N_dots))  +  geom_point() + theme_bw()

ggplot(dots2, aes(x = MDS1,  y =  Med_DAPI))  +  geom_point() + theme_bw()


ggplot(dots2, aes(x = N_dots))  +  geom_histogram() + theme_bw()


M <- lm(N_dots ~ Med_DAPI+Med_FSC, data = dots2)

summary(M)

as.data.frame(ord$points, dots)

write.table(dots, "clipboard", sep = "\t")


dots <- read.table("clipboard", sep = "\t", header = T)

ggplot(dots, aes(MDS1, MDS2, size = N_dots)) + geom_point()
