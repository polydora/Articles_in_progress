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



coo_check(myt)

str(myt)

panel(myt, names = T)

df_gen <- read_excel("морфология таблица.xlsx")


str(myt)

names(myt$coo)



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

palette(nlevels(f))

# %>% 
# LDA(~Gabitus) %>%                # Linear Discriminant Analysis
# plot_CV()              

str(pca_results)

qplot(pca_results$fac$L, pca_results$x[,2]) + geom_smooth()

pca_results$fac

pca_df <- data.frame(pca_results$fac, PC1 = pca_results$x[,1], PC2 = pca_results$x[,2])

ggplot(pca_df, aes(x =Genotype, y = PC1)) +
  geom_boxplot()

ggplot(pca_df, aes(x =Morphotype, y = PC1)) +
  geom_boxplot()

ggplot(pca_df, aes(x =Visual_Type, y = PC1)) +
  geom_boxplot()




pca_df <- 
  pca_df %>% 
  mutate(GA_pure = case_when(
    Genotype == "ga" ~ 1,
    Genotype != "ga" ~ 0)) %>% 
    mutate(TR_pure = case_when(
      Genotype == "tr" ~ 1,
      Genotype != "tr" ~ 0)) %>% 
  mutate(Hybr = case_when(
    Genotype == "tr/ga" ~ 1,
    Genotype != "tr/ga" ~ 0))
  
  

model_GA <- glm(GA_pure ~ Morphotype + Visual_Type +   L  +  PC1 + PC2, data = pca_df )

car::vif(model_GA)

car::Anova(model_GA)


model_TR <- glm(TR_pure ~ Morphotype + Visual_Type +   L  +  PC1 + PC2, data = pca_df )

car::vif(model_TR)

car::Anova(model_TR)

summary(model_TR)


model_Hybr <- glm(Hybr ~ Morphotype + Visual_Type +   L  +  PC1 + PC2, data = pca_df )

car::vif(model_Hybr)

car::Anova(model_Hybr)

summary(model_Hybr)

