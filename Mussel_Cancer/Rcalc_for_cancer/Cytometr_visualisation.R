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




#################################

n_quant <-10 

probs <- c(seq(0,0.8,1/(n_quant)),  1)

length(probs)

cytom_quantiles <- matrix(rep(NA, n_quant^2*length(files)), nrow = n_quant^2)


for(i in 1:length(files)){
  df_trial <- dfs[[i]]
  df_quant <- data.frame(
    FSC_A = cut(df_trial$FSC_A , breaks = quantile(df_trial$FSC_A, probs = probs), labels= 1:(length(probs)-1), include.lowest=TRUE), 
    SSC_A = cut(df_trial$SSC_A , breaks = quantile(df_trial$SSC_A, probs = probs), labels= 1:(length(probs)-1), include.lowest=TRUE)
    )
  cytom_quantiles[,i] <-  as.data.frame(table(df_quant))[,3]
  print(i)
  
}

cytom_quantiles <- as.data.frame(t(cytom_quantiles))

row.names(cytom_quantiles) <- gsub(".fcs", "", files)

library(vegan)

ord <- cca(cytom_quantiles)

summary(ord)





plot(ord, display = "sites")

env_f <- envfit(ord ~ N_dots, data = dots, na.rm = T)

plot(env_f)


biplot(ord, type = "t")

scores(ord)

ord_scores <- as.data.frame(scores(ord, choices = 1:5)$sites)

plot(hclust(dist(ord_scores), method = "ward.D2"))





ord_scores$File <- files
  
dots <- read.table("clipboard", sep = "\t", header = T)

ord_dots <- merge(ord_scores, dots)

ggplot(ord_dots, aes(PC1, PC2, size = N_dots)) + geom_point()



ord_mds <- metaMDS(cytom_quantiles)

plot(ord_mds, display = "sites", type = "t")

env_f2 <- envfit(ord_mds ~ N_dots, data = dots, na.rm = T)

plot(env_f)


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
selected_files <- c(13, 8, 28, 30)

library(dplyr)
scores(ord)$sites %>% as.data.frame(.) %>%  filter(CA1 < quantile(CA1, probs = 0.25))

for(i in selected_files){
  pl <-plot_cytometry(paste("UM", i, ".fcs", sep = ""))[[1]] + plot_cytometry(paste("UM", i, ".fcs", sep = ""))[[2]] 
  ggsave(plot = pl, file = paste("UM", i,".jpg", sep = ""))
  print(i)
  
}




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
