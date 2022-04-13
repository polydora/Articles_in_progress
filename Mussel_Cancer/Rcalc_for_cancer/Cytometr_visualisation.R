# Скрипт для анализа даных с проточного цитометра
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.10")


# BiocManager::install("flowCore")
# BiocManager::install("ggcyto")
# BiocManager::install("flowMap")

library(flowCore)
library(ggcyto)



df_extractor <- function(x = um22, Gate_FSC_A = 260E4, Gate_SSC_A = 440E4){
  require(dplyr)
  df <- fortify(x)
  names(df) <- gsub("-", "_", names(df))
  
  names(df)[names(df) == "DAPI_A"] <- "FL7_A" 
  
  df_quant <- df %>% summarise(Min_FSC_A = quantile(FSC_A, probs = 0.01), Min_SSC_A = quantile(SSC_A, probs = 0.01))
  df <- df %>% filter(FSC_A < Gate_FSC_A & SSC_A < Gate_SSC_A & FSC_A > df_quant$Min_FSC_A & SSC_A > df_quant$Min_SSC_A)
  df <- df %>% select(FSC_A, SSC_A, FL7_A)
  df$id <- 1
  as.data.frame(df)
}




# files <- list.files("Data/FCS", pattern = "^UM")

files <- list.files("Data/FCS")


dfs <- list(NULL)

for(i in 1:length(files)){
  case <- read.FCS(filename = paste("Data/FCS/",files[i], sep = ""), transformation=FALSE)
  dfs[[i]] <- df_extractor(x = case)
  print(i)
  
}


str(dfs)




cytom_event <- rep(NA,length(files))

for(i in 1:length(files)){
  cytom_event[i] <- nrow(dfs[[i]])
  setTxtProgressBar(pb,i)
}


hist(cytom_event)



#################################

# Распределение мидий в соответствии с результатами проточной цитометрии


n_quant <-50 

probs <- c(seq(0, 0.8, 1/(n_quant)),  1)

(length(probs) - 1)^2


cytom_quantiles <- matrix(rep(NA, (length(probs) - 1)^2*length(files)), nrow = (length(probs) - 1)^2)


pb = txtProgressBar(min = 0, max = length(files), initial = 0) 

for(i in 1:length(files)){
  df_trial <- dfs[[i]]
  df_quant <- data.frame(
    FSC_A = cut(df_trial$FSC_A , breaks = quantile(df_trial$FSC_A, probs = probs), labels= 1:(length(probs)-1), include.lowest=TRUE), 
    SSC_A = cut(df_trial$SSC_A , breaks = quantile(df_trial$SSC_A, probs = probs), labels= 1:(length(probs)-1), include.lowest=TRUE)
    )
  cytom_quantiles[,i] <-  as.data.frame(table(df_quant))[,3]/nrow(df_trial)
  setTxtProgressBar(pb,i)
}

cytom_quantiles <- as.data.frame(t(cytom_quantiles))


# Унификация наименований файлов

row.names(cytom_quantiles) <- gsub(".fcs", "", files)
# row.names(cytom_quantiles) <- gsub("Specimen_001_", "", row.names(cytom_quantiles))
# row.names(cytom_quantiles) <- gsub("_0.*", "", row.names(cytom_quantiles))
# row.names(cytom_quantiles) <- gsub("-", "_", row.names(cytom_quantiles))





library(vegan)



ord <- cca(cytom_quantiles)

summary(ord)

ord_scores <- as.data.frame(scores(ord)$sites)




plot(ord, display = "sites")


cancer_mussel <- c("NUK1-9",  "Nuk1_28", "NUK1-56", "NUK1-60",  "UM36", "UM41", "UM48", "Specimen_001_MChK-29_031", "hemo_japan_161","hemo_japan_54", "hemo_japan_181", "hemo_japan_181(2)", "hemo_japan_181(3)" )

healthy_mussel <- c("hemo_japan_17", "hemo_japan_38")

ord_scores_cancer <- ord_scores %>% filter(row.names(.) %in% cancer_mussel)

ord_scores_healthy <- ord_scores %>% filter(row.names(.) %in% healthy_mussel)



library(ggrepel)

ggplot(ord_scores, aes(CA1, CA2)) + geom_point() + geom_point(data = ord_scores_cancer, color = "blue", size = 4) + geom_text_repel(data=ord_scores_cancer, aes(label = row.names(ord_scores_cancer))) + 
  geom_point(data = ord_scores_healthy, color = "red", size = 4) + 
  geom_text_repel(data=ord_scores_healthy, aes(label = row.names(ord_scores_healthy))) +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0)


ggplot(ord_scores, aes(CA1)) + geom_density()

ggplot(ord_scores, aes(CA2)) + geom_density()


#############################

# Визуализация "образа" скеттер-диграммы, характерного для разных значений PC1, PC2 и их комбинаций

ord_PC_loadings <- scores(ord)$species

ord_PC_loadings2 <- cbind(expand.grid(FSC_A = 1:(length(probs) - 1), SSC_A = 1:(length(probs) - 1)), ord_PC_loadings)

ord_PC_loadings2 <-
  ord_PC_loadings2 %>% mutate(Quadrant = case_when(
  PC1>0 & PC2>0 ~ "I",
  PC1<0 & PC2>0 ~ "II",
  PC1<0 & PC2<0 ~ "III",
  PC1>0 & PC2<0 ~ "IV"))



ord_PC_scores2 <-ord_scores %>% 
   mutate(Quadrant = case_when(
    PC1>0 & PC2>0 ~ "I",
    PC1<0 & PC2>0 ~ "II",
    PC1<0 & PC2<0 ~ "III",
    PC1>0 & PC2<0 ~ "IV"))



ggplot(ord_PC_loadings2, aes(FSC_A, SSC_A, fill = Quadrant )) + geom_tile() + scale_fill_manual(values = c("red", "white","gray","yellow"))


Quadr_I <- row.names(ord_PC_scores2)[ord_PC_scores2$Quadrant == "I"] 
Quadr_II <- row.names(ord_PC_scores2)[ord_PC_scores2$Quadrant == "II"] 
Quadr_III <- row.names(ord_PC_scores2)[ord_PC_scores2$Quadrant == "III"] 
Quadr_IV <- row.names(ord_PC_scores2)[ord_PC_scores2$Quadrant == "IV"] 




############################################
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

# selected_files <- c(6, 61, 14, 51, 33, 58, 23, 41, 36, 48)
# selected_files <- c(11, 22, 26, 60, 64, 9)
# selected_files <- c(13, 8, 28, 30)

selected_files <- Quadr_IV




for(i in selected_files){
  pl <-plot_cytometry(paste(i, ".fcs", sep = ""))[[1]] + plot_cytometry(paste(i, ".fcs", sep = ""))[[2]] 
  ggsave(plot = pl, file = paste(i,".jpg", sep = ""))

}


healthy_mussel <- c("hemo_japan_17", "hemo_japan_38")

(plot_cytometry("Nuk1_28.fcs")[[1]] + plot_cytometry("Nuk1_28.fcs")[[2]]) / 
  (plot_cytometry("NUK1-56.fcs")[[1]] + plot_cytometry("NUK1-56.fcs")[[2]]) / 
  (plot_cytometry("hemo_japan_181(2).fcs")[[1]] + plot_cytometry("hemo_japan_181(2).fcs")[[2]]) /
  (plot_cytometry("hemo_japan_181(3).fcs")[[1]] + plot_cytometry("hemo_japan_181(3).fcs")[[2]])

cancer_mussel <- c("NUK1-9",  "Nuk1_28", "NUK1-56", "NUK1-60",  "UM36", "UM41", "UM48", "Specimen_001_MChK-29_031", "hemo_japan_161","hemo_japan_54", "hemo_japan_181", "hemo_japan_181(2)", "hemo_japan_181(3)" )


(plot_cytometry("NUK1-9.fcs")[[1]] + plot_cytometry("NUK1-9.fcs")[[2]])

(plot_cytometry("UM1.fcs")[[1]] + plot_cytometry("UM1.fcs")[[2]])


(plot_cytometry("UM41.fcs")[[1]] + plot_cytometry("UM41.fcs")[[2]]) / 
  (plot_cytometry("UM36.fcs")[[1]] + plot_cytometry("UM36.fcs")[[2]]) /
  ((plot_cytometry("UM48.fcs")[[1]] + plot_cytometry("UM48.fcs")[[2]]))







###########################


# BiocManager::install("flowMap")

library(flowMap)
library(dplyr)





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
