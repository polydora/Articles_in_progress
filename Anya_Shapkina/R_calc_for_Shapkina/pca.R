library(readxl)
library(dplyr)

df <- read_excel("Data/Sd_morph.xlsx", na = "NA")

means <- df %>% select(-c( sample, coordinates_L, coordinates_D, location, sex)) %>%  summarise_all(.funs = function(x) mean(x, na.rm = T))


df %>% select(-c( sample, coordinates_L, coordinates_D, location, sex))

df2 <- df %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))    

library(vegan)



df_pca <- rda(df2[,-c(1:7)] )

plot(df_pca)

plot(df_pca, display = "species")



summary(df_pca)


df_pca_scores <- as.data.frame(scores(df_pca)$sites)

library(ggplot2)
ggplot(df_pca_scores, aes(x = df2$location, y = PC1)) + geom_boxplot()

ggplot(df_pca_scores, aes(x = df2$location, y = PC2)) + geom_boxplot()




