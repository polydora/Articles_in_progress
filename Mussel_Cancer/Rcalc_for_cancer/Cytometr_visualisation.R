# Скрипт для анализа даных с проточного цитометра

library(flowCore)
library(ggcyto)

um22 <-read.FCS("Data/FCS/UM22.fcs", transformation=FALSE) 
um36 <-read.FCS("Data/FCS/UM36.fcs", transformation=FALSE) 
um2 <-read.FCS("Data/FCS/UM2.fcs", transformation=FALSE) 
um41 <-read.FCS("Data/FCS/UM41.fcs", transformation=FALSE) 
um3 <-read.FCS("Data/FCS/UM3.fcs", transformation=FALSE) 
um4 <-read.FCS("Data/FCS/UM4.fcs", transformation=FALSE) 


autoplot(um22,"FL1-H","FL2-H")
autoplot(um22,"FL1-H")



# fs <-read.flowSet(path = "Data/FCS/")
# 
# pl <- autoplot(x, "FSC-A", "SSC-A")
# fortify(fs)


df_extractor <- function(x = um22){
  df <- fortify(x)
  data.frame(FSC_A = df$"FSC-A", SSC_A = df$"SSC-A", Name = sub(".fcs", "", unique(df$.rownames))) 
}





dfs <- rbind(df_extractor(um36), 
             df_extractor(um22),
             df_extractor(um2),
             df_extractor(um3),
             df_extractor(um41), 
             df_extractor(um4)
)


sam_3 <- df_extractor(um3)
sam_41 <- df_extractor(um41)
sam_36 <- df_extractor(um36)
sam_22 <- df_extractor(um22)
sam_2 <- df_extractor(um2)
sam_4 <- df_extractor(um4)



dfs$Name <- factor(dfs$Name, levels = c("UM2", "UM3", "UM4", "UM22", "UM36", "UM41"))


ggplot(dfs, aes(x = FSC_A, y = SSC_A)) +
  geom_point(color = "grey20", size = 0.1)+
  geom_density2d(color = "yellow")+
  facet_wrap(~ Name)+
  ylim(0, 440E4) +
  xlim(0, 260E4)

###########################


# BiocManager::install("flowMap")

library(flowMap)
# sam1 <-read.table(system.file("extdata/sample.txt",package="flowMap"), header=T) 
# 
# sam2 <-read.table(system.file("extdata/sample.txt",package="flowMap"), header=T)


library(dplyr)

sam_3 <- sam_3 %>% select(-Name) %>% mutate(id = 1) 
sam_3 <- sam_3 %>% filter((FSC_A <260E4 ) & (SSC_A < 440E4))

sam_41 <- sam_41 %>% select(-Name) %>% mutate(id = 1)
sam_41 <- sam_41 %>% filter((FSC_A <260E4 ) & (SSC_A < 440E4))

sam_4 <- sam_4 %>% select(-Name) %>% mutate(id = 1)
sam_4 <- sam_4 %>% filter((FSC_A <260E4 ) & (SSC_A < 440E4))


sam_2 <- sam_2 %>% select(-Name) %>% mutate(id = 1)
sam_2 <- sam_2 %>% filter((FSC_A <260E4 ) & (SSC_A < 440E4))


sam_22 <- sam_22 %>% select(-Name) %>% mutate(id = 1)
sam_22 <- sam_22 %>% filter((FSC_A <260E4 ) & (SSC_A < 440E4))


sam_36 <- sam_36 %>% select(-Name) %>% mutate(id = 1)
sam_36 <- sam_36 %>% filter((FSC_A <260E4 ) & (SSC_A < 440E4))



resMulti <- makeDistmat(samples=list(sam_2, sam_3, sam_4, sam_22, sam_36,sam_41),sampleSize=100,ndraws=10)

as.dist(resMulti$distmat)

library(vegan)

diag(resMulti$distmat) <- 0


resMulti$distmat <- abs(resMulti$distmat)

ord <- metaMDS(com = resMulti$distmat)

plot(ord, type = "t")





cl <- hclust(d = as.dist(resMulti$distmat), method = "ward.D")

plot(cl)

resMulti=makeDistmat(samples=list(sam1,sam2),sampleSize=100,ndraws=100)




require(gplots)
par(mar=c(0,0,0,0))

heatmapCols<-colorRampPalette(c("red","yellow","white","blue"))(50)

heatmap.2(resMulti$distmat,trace="none",col=heatmapCols,symm=FALSE,dendrogram="none",Rowv=FALSE,Colv=FALSE)
