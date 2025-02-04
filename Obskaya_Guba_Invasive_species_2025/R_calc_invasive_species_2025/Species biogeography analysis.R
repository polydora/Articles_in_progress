
# install.packages("spocc")


library(lubridate)
library(dplyr)
library(ggplot2)
library(spocc)
library(readxl)


# Пакетное скачивание данных по видам

# add_PNIS <- read.csv("Data/additional_PNIS.csv")

# add_PNIS <- read_excel("Data/export_gisd_PNIS.xlsx")

# add_PNIS <- read_excel("Data/Additional_Native_and_PNIS_species.xlsx")


# add_PNIS <- 
#   add_PNIS %>% 
#   filter(From == "CMI_report")

# add_PNIS <-
#   add_PNIS %>%
#   filter(Status == "PNIS" & Group == "Plancton")

add_PNIS <- data.frame(Species = c("Cylichna occulta", "Cylichna alba"), Group = "Benthos", Status = "DNA_PNIS")



for(name in add_PNIS$Species){

  df_add_PNIS <- occ2df(occ(query = name, from = "gbif", limit = 5000, has_coords = TRUE))
  df_add_PNIS_short <- 
    df_add_PNIS %>% 
    dplyr::select(name, longitude, latitude) %>% unique
  names(df_add_PNIS_short) <- c("species", "lon", "lat")
  df_add_PNIS_short$species <- name
  write.table(file = paste("Data/", name,".csv", sep = ""), x = df_add_PNIS_short, sep = ";", row.names = F)
  print(name)
}








# df_add_PNIS_short <- df_add_PNIS %>% select(name, longitude, latitude) %>% unique
# 
# names(df_add_PNIS_short) <- c("species", "lon", "lat")






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



df_species_short <- df_species %>% select(species, decimalLongitude, decimalLatitude) 

names(df_species_short) <- c("species", "lon", "lat")



df_species_full <- rbind(df_species_short, df_add_PNIS_short)


write.csv(df_species_full, "native_and_PNIS_occurence.csv", row.names = F)



##### Planctonic animals 


path = "D:/Data_LMBE/Obskaya Bay additional data/Planctonic species characteristic 2/"



files <- list.files(path)

df_species <- NULL

for(name in files){
  df <- read.table(paste(path, "/",name, sep = ""), sep = ";", header = T)
  df <- df %>% select(species)
  df <- unique(df)
  if(sum (is.na(df$species)) > 0) df$species <- df$genus
  df_species <- rbind(df_species, df)
  print(name)
}


library(spocc)

df_add_plancton <- occ2df(occ(query = df_species$species, from = "gbif", limit = 5000, has_coords = TRUE))


df_add_plancton <- df_add_plancton %>% select(name, longitude, latitude) %>% unique

names(df_add_plancton) <- c("species", "lon", "lat")


write.csv(df_add_plancton, file = "Data/plancton_occurence_2.csv", row.names = F)




##############################

df_species <- read.csv("Data/native_and_PNIS_occurence.csv")
df_species <- unique(df_species)


exclude <- c("Acartia tonsa",
             "Oithona davisae",
             "Cercopagis pengoi")

df_species <- df_species %>% filter(!species %in% exclude)


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
                    "Marenzelleria",
                    "Ampharete vega",
                    "Mysis oculata"
                    )



df_species$Status <- ifelse(df_species$species %in% native_species, "Native", "PNIS") 

df_species_native <- df_species %>% filter(Status == "Native")

df_species_PNIS <- df_species %>% filter(Status == "PNIS")


ggplot(df_species_native, aes(y = lat, x = species)) + geom_boxplot() + facet_wrap(~Status) + geom_hline(yintercept = 66, linetype = 2) + theme(axis.text.x = element_text(angle = 90)) 

ggplot(df_species_PNIS, aes(y = lon, x = species)) + geom_boxplot() + facet_wrap(~Status) + geom_hline(yintercept = 66, linetype = 2) + theme(axis.text.x = element_text(angle = 90)) 


# Биогеографическая статистика


library(moments)
library(e1071)

species_stat <- df_species %>% group_by(species, Status) %>% summarise(N_records = n(), Median_lat = median(lat), Upper_lat = max(lat), Q_low = quantile(lat, probs = 0.025), Q_up = quantile(lat, probs = 0.975), N_S_skewness = skewness(lat, type = 1), N_S_asymmetry = log(abs(Q_up - Median_lat)/abs(Q_low - Median_lat)), Prop_polar = mean(lat > 66.5622))

plot(species_stat$N_S_asymmetry, species_stat$N_S_skewness)


library(vegan)


spec <-  species_stat %>% select(-c(1,2,3))


mod <- rda(spec[ , -1])


biplot(mod)


scores_rda <- scores(mod)

inertcomp(mod)

species_rda <- as.data.frame(scores_rda$sites)

species_rda$species <- species_stat$species 
species_rda$Status <- species_stat$Status 

ggplot(species_rda, aes(PC1, PC2, color = Status)) + geom_text(aes(label = species)) + scale_color_manual(values = c("blue", "red"))

ggplot(species_rda, aes(PC1, PC2, color = Status)) + geom_point(aes(label = species)) + scale_color_manual(values = c("blue", "red"))


species_rda %>% filter(PC1 >0 & PC2 <0 )



