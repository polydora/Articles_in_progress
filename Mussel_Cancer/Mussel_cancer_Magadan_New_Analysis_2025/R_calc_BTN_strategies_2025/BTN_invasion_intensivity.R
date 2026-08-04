# В этом скрипте анализиуется ИНТЕНСИВНОСТЬ инвазии в связи с предикторами.
library(readxl)
library(ggrepel)
library(dplyr)
library(reshape2)
library(cowplot)
library(magrittr)
library(patchwork)
library(vegan)
library(mgcv)
library(gratia)
library(broom.mixed) 



default_theme <- 
  theme_bw() + 
  theme(axis.text = element_text(size = 15), axis.title.y = element_text(size = 15), axis.title.x = element_text(size = 20) )

theme_set(default_theme)


##############################
points <- 
  read_excel("Data/Magadan_2021_2023_ecology_cleaned.xlsx", sheet = "Points  characteristic 2021-23", na = "NA")

points_local <- 
  points %>% 
  filter(lat > 59.3)




myt <- read_excel("Data/ЕДИНСТВЕННЫЙ ФАЙЛ.xlsx", na = "NA")

str(myt)

myt <- 
  myt %>% 
  filter_out(Site_code == "KHOL" & Sample %in% c("A", "B") )


myt <-
  myt %>% 
  dplyr::select(-Site) %>% 
  mutate(Site = Site_code)


myt %<>%
  mutate(Lineage = case_when(BTN_genotype == "BTN1" ~ "BTN1",
                             BTN_genotype == "BTN1/BTN2.1" ~ "BTN1",
                             BTN_genotype == "BTN1/BTN2.2" ~ "BTN1",
                             BTN_genotype == "BTN1_Am" ~ "BTN1",
                             BTN_genotype == "BTN2.1" ~ "BTN2",
                             BTN_genotype == "BTN2.1/BTN1" ~ "BTN2",
                             BTN_genotype == "BTN2.2" ~ "BTN2",
                             is.na(BTN_genotype) ~ NA,
                             BTN_genotype == "noBTN" ~ NA
                             )
  )


cover <- read_excel("Data/Magadan_2021_2023_ecology_cleaned.xlsx", sheet = "Покрытия миидий 2023")

cover %>% 
  group_by(Site) %>% 
  summarise(Mean_Cover = mean(`Number of squares`)/30) ->
  site_cover

site_cover <-
  site_cover %>% 
  mutate(Cover_Type = ifelse(Mean_Cover >= 0.10, "High", "Low"))

Cover_factor <- 
  site_cover %>% 
  group_by(Cover_Type) %>% 
  summarise(Mean_Cover = mean(Mean_Cover)) %>% 
  pull(Mean_Cover) 

High_factor <- (Cover_factor[1])
Low_factor <- (Cover_factor[2])



size <- read_excel("Data/Magadan_2021_2023_ecology_cleaned.xlsx", na = "NA", sheet = "Размерная струкутра 2023 2021")

size <- size[complete.cases(size), ]

library(reshape2)

scam <- dcast(Year + Site ~ Size_class, data = size)

area <- read_excel("Data/Magadan_2021_2023_ecology_cleaned.xlsx", na = "NA", sheet = "Площадь проб на размер")

sample_area <- 
  area %>% 
  group_by(Year, Site) %>% 
  summarise(Total_area = sum(Area))

scam <- 
  scam %>% group_by(Year, Site)


scam [ ,3:ncol(scam)] <- 
  round((scam[ ,3:ncol(scam)] / sample_area$Total_area) *10000, 0)


LW <- read_excel("Data/Magadan_2021_mussel_L_W.xlsx")
LW$W <- as.numeric(gsub(pattern = ",", replacement = ".", x = LW$W))

# unique(LW$Sample)

mod_W <- lm(W ~ I(L^3) - 1, data = LW)

LW$Predicted <- predict(mod_W)


df <- 
  merge(site_cover, scam, all.y = T) 


# Расставляем типы покрытия (Cove_Type), оцененные по воспоминаниям 

df$Cover_Type[c(3,4, 11)] <- "Low"
df$Mean_Cover[c(3,4, 11)] <- Low_factor

df$Cover_Type[c(14, 23)] <- "High"
df$Mean_Cover[c(14, 23)] <- High_factor


SCAM <- df

# SCAM[,5:ncol(SCAM)] <- SCAM[,5:ncol(SCAM)] * df$Mean_Cover


SCAM <-
  SCAM %>%
  select(-c(Mean_Cover, Cover_Type)) %>%
  arrange(Year)

size_classes <- data.frame(L = as.numeric(gsub(x = names(SCAM)[-c(1:2)], pattern = "L", replacement = "")))


size_classes$W <- predict(mod_W, newdata = size_classes)

Biomass <- as.numeric(as.matrix(SCAM[ , -c(1:2)]) %*% as.vector(size_classes$W))

df$Biomass <- Biomass 



SCAM <- df

SCAM <-
  SCAM %>%
  select(-c(Cover_Type)) %>%
  arrange(Year)




# Альтернативная оуенка покрытия мидий

cover_score <- read_excel("data/проективное покрытие в баллах.xlsx")

cover_score <-
  cover_score %>% 
  select(-c(Comments, Scale1, Scale2))


SCAM <- merge(SCAM, cover_score) 

SCAM <- 
  SCAM %>% 
  select(-Mean_Cover)

growth <- read.table("Data/Mussel_growth_Magadan_2021_2023.csv", sep = ";", header = T, dec = ",")


growth <- 
  growth %>% 
  filter_out(Site_code == "PLO")


growth$OGP_site[growth$Site_code == "KHOL" & growth$Year == 2023] <- 1.065483

growth <- 
  growth %>% 
  filter_out(Site_code == "KHOL" & Sample %in% c("A", "B"))


growth <-
  growth %>% 
  dplyr::select(-Site) %>% 
  rename(Site = Site_code) 

ogp <- 
  growth %>% 
  group_by(Site) %>% 
  summarise(OGP = mean(OGP_site))



pca_scam <- rda(decostand(SCAM[ , -c(1, 2)], method = "standardize" ))


sum_pca_scam <- summary(pca_scam)


pca_scam_size_scores <- as.data.frame(scores(pca_scam)$species)


pca_scam_scores <- as.data.frame(scores(pca_scam)$sites)

pca_scam_scores$N_Juv <- SCAM$L3    

pca_scam_scores$N_Large = SCAM$L8 + SCAM$L13 + SCAM$L18 + SCAM$L18 + SCAM$L23 + SCAM$L28 + SCAM$L33 + SCAM$L38 + SCAM$L43 

pca_scam_scores$N_Total <- SCAM$L3 + SCAM$L8 + SCAM$L13 + SCAM$L18 + SCAM$L23 + SCAM$L28 + SCAM$L33 + SCAM$L38 + SCAM$L43 + SCAM$L48 + SCAM$L53 + SCAM$L58

pca_scores_scam <- data.frame(Year = SCAM$Year, Site = SCAM$Site,  Biomass = SCAM$Biomass, Cover = SCAM$Cover_score, pca_scam_scores)


pca_scores_scam <-
  pca_scores_scam %>% 
  mutate(Site_Year = paste(Site, "_", Year, sep = ""))

pca_scores_scam$Site_Year <- gsub(pattern = "20", replacement = "", x = pca_scores_scam$Site_Year) 


## Mussel population structure



pca_scores_scam <-
  merge(pca_scores_scam, df , all = T) 


Sites_Year <- 
  pca_scores_scam %>%
  arrange(PC1) %>% 
  pull(Site_Year)%>% 
  unique() 

Sites_Year_labels <- 
  pca_scores_scam %>%
  arrange(PC1) %>%
  mutate(Site_Year_PC1 = paste(Site_Year, "(", round(PC1,1), ")", sep = "")) %>%
  pull(Site_Year_PC1)%>% 
  unique() 




size <-
  size %>%
  mutate(Site_Year = paste(Site,"_", Year, sep = ""))

size$Site_Year <- gsub(pattern = "20", replacement = "", x = size$Site_Year) 


size$Site_Year <- factor(size$Site_Year, levels = Sites_Year, labels = Sites_Year_labels)



Pl_size_stricture <-
  size %>% 
  ggplot(., aes(x = L)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Site_Year, scales = "free_y", dir = "v", ncol = 4) +
  theme_bw() +
  # theme(strip.text = element_blank()) +
  labs(x = "Size classes", y = "Count")


growth %>% 
  select(Site, Year, L) %>% 
  filter(complete.cases(.)) ->
  analyzed_myt_size

size %>% 
  select(Year, Site, Site_Year) %>% 
  unique(.) %>% 
  merge(., analyzed_myt_size) ->
  analyzed_myt_size



Pl_size_stricture +
  geom_rug(data = analyzed_myt_size, aes(y = 0, x = L), color = "blue")


##########


myt %>%
  filter_out(is.na(Lineage)) ->
  df_infected_mussels
  
  
merge(df_infected_mussels, points) ->
  df_infected_mussels
  
merge(df_infected_mussels, pca_scores_scam) ->
  df_infected_mussels


merge(df_infected_mussels, ogp) ->
  df_infected_mussels


df_infected_mussels %>% 
  mutate(Fi_Prop_Aneuploid = 2*asin(sqrt(Rate_of_aneuploid_cells/100))*180/pi) %>% 
  filter_out(is.na(Fi_Prop_Aneuploid)) ->
  df_infected_mussels

df_infected_mussels$Lineage <- factor(df_infected_mussels$Lineage)
  

mod_intens <- gam(Fi_Prop_Aneuploid ~ 
                    s((Dist_Port), by = Lineage,  bs = "cs", k = 5) + 
                    s((fetch), by = Lineage,  bs = "cs", k = 5) +  
                    s((PC1), by = Lineage,  bs = "cs", k = 5) + 
                    s((PC2), by = Lineage,  bs = "cs", k = 5) + 
                    s((OGP), by = Lineage, bs = "cs", k = 5) +  
                    Lineage +  
                    s(Year, Site, bs = "re"),  
                  family = "gaussian", 
                  method = "REML", 
                  data = df_infected_mussels ) 



mod_intens <- gam(Fi_Prop_Aneuploid ~ 
                    s((Dist_Port), by = Lineage,  bs = "cs", k = 5) + 
                    s((fetch), by = Lineage,  bs = "cs", k = 5) +  
                    s((PC1), by = Lineage,  bs = "cs", k = 5) + 
                    s((PC2), by = Lineage,  bs = "cs", k = 5) + 
                    s((OGP), by = Lineage, bs = "cs", k = 5) +  
                    Lineage,  
                  family = "gaussian", 
                  method = "REML", 
                  data = df_infected_mussels ) 


summary(mod_intens)

# Никаких значимых связей интенсивности заражения и изученных предикторов не наййдено 


