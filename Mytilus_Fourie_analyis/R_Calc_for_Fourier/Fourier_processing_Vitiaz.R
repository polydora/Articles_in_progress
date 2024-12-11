library(readxl)
library(dplyr)
library(Momocs)
library(ggplot2)


# dat <- read.table("аутлайны_Витязь_2024.csv", sep = ";", header = TRUE, dec = "." )
# 
# str(dat)
# 
# for(i in unique(dat$ID) ){
#   dat %>%
#     filter(ID == i) %>%
#     select(x, y) -> df
#   
#   write.table(df, file = paste("Coordinates/",i,".txt",sep = ""), row.names = FALSE, col.names = FALSE, sep = "\t")
# }



folder <- paste("Coordinates/", list.files("Coordinates"), sep = "")

coords <- import_txt(txt.paths = folder)

myt <- Out(coords)

names(myt)

df_names_myt <- data.frame(ID = as.numeric(names(myt)) )

coo_check(myt)

str(myt)

panel(myt, names = T)

df_gen <- read_excel("морфология таблица.xlsx")

df_gen <- merge(df_names_myt, df_gen,  sort = F)



df_gen %<>% 
  filter(as.character(ID) %in% names(myt$coo))


table(df_gen$Morphotype, df_gen$Genotype)

table(df_gen$Morphotype, df_gen$Visual_Type)


myt$fac <- df_gen


myt %T>%                    
  stack() %>%                  
  efourier(6, norm=TRUE) %>% 
  filter(!Genotype %in% c("-") ) %>% 
  MDS(method = "euclidean") %>% 
  plot_MDS(f = ~Genotype, chull = T, labelgroups = T)




myt %T>%                    
  stack() %>%                  
  efourier(6, norm=TRUE) %>% 
  filter(!Genotype %in% c("-") ) %>% 
  # filter(Genotype != "tr/ga") %>% 
  PCA(scale. = F) ->
  pca_results


library(RColorBrewer)



pca_results%T>%    # Principal Component Analysis
  plot_PCA(~Genotype, axes = c(1, 2), points = T, morphospace = T, chull= T, chullfilled =F, labelpoints = F, labelgroups = T, palette = col_autumn) 


str(pca_results)




pca_results$x %>% 
  as.data.frame() ->
  pca_df

pca_df$ID <- as.numeric(row.names(pca_df))

pca_df_fac <-
merge(pca_df, pca_results$fac)

pca_df_fac$Genotype <- factor(pca_df_fac$Genotype, levels = c("tr", "tr/ga", "ga"))

ggplot(pca_df_fac, aes(x = Genotype, y = PC1)) +
  geom_boxplot()

ggplot(pca_df_fac, aes(x = L, y = PC1, color = Genotype)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(pca_df_fac, aes(x = Morphotype, y = PC1)) +
  geom_boxplot()


ggplot(pca_df_fac, aes(x = Visual_Type, y = PC1)) +
  geom_boxplot()




