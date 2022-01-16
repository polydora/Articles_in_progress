library(lubridate)
library(dplyr)
library(ggplot2)


path = "D:/Data_LMBE/Obskaya Bay additional data/Species characteristics/"



files <- list.files(path)

df_species <- NULL

for(name in files){
  df <- read.table(paste(path, "/",name, sep = ""), sep = "\t", header = T)
  df <- df %>% select(-gbifID)
  df <- unique(df)
  if(sum (is.na(df$species)) > 0) df$species <- df$genus
  df_species <- rbind(df_species, df)
  print(name)
}

dd <- df_species %>%  filter(species == "Gammarus tigrinus") 

hist(dd$decimalLatitude)

native_species <- c("Limnodrilus hoffmeisteri", 
                    "Mysis relicta",
                    "Halicryptus spinulosus",
                    "Saduria entomon",
                    "Saduria sabini",
                    "Saduria sibirica",
                    "Gammaracanthus lacustris",
                    "Gammaracanthus",
                    "Monoporeia affinis",
                    "Pontoporeia femorata",
                    "Portlandia aestuariorum",
                    "Marenzelleria"
                    )



df_species$Status <- ifelse(df_species$species %in% native_species, "Native", "PNIS") 

df_species_native <- df_species %>% filter(Status == "Native")

df_species_PNIS <- df_species %>% filter(Status == "PNIS")


ggplot(df_species_native, aes(y = decimalLatitude, x = species)) + geom_boxplot() + facet_wrap(~Status) + geom_hline(yintercept = 66, linetype = 2) + theme(axis.text.x = element_text(angle = 90)) 

ggplot(df_species_PNIS, aes(y = decimalLatitude, x = species)) + geom_boxplot() + facet_wrap(~Status) + geom_hline(yintercept = 66, linetype = 2) + theme(axis.text.x = element_text(angle = 90)) 


# Биогеографическая статистика


library(moments)
library(e1071)

species_stat <- df_species %>% group_by(species, Status) %>% summarise(Median_lat = median(decimalLatitude), Upper_lat = max(decimalLatitude), Q_low = quantile(decimalLatitude, probs = 0.025), Q_up = quantile(decimalLatitude, probs = 0.975), N_S_skewness = skewness(decimalLatitude, type = 1), N_S_asymmetry = log(abs(Q_up - Median_lat)/abs(Q_low - Median_lat)), Prop_polar = mean(decimalLatitude > 66.5622))

plot(species_stat$N_S_asymmetry, species_stat$N_S_skewness)


library(vegan)


spec <-  species_stat %>% select(-c(1,2))


mod <- rda(spec[ , -1])

scores_rda <- scores(mod)

species_rda <- as.data.frame(scores_rda$sites)

species_rda$species <- species_stat$species 
species_rda$Status <- species_stat$Status 

ggplot(species_rda, aes(PC1, PC2, color = Status)) + geom_text(aes(label = species)) + scale_color_manual(values = c("blue", "red"))

