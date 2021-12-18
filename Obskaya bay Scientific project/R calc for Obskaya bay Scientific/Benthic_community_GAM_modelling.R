library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(mgcv)


# Data reading ############
# All Station position
stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)

stat_sal <- stat_full %>% select(Station, Long, Lat, Bottom_Salinity_Aug_20) 

names(stat_sal)[4] <- "Sal"



# Data on benthic communities ######
bent <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Benthos")


bent_B <- bent %>% filter(Type == "Biomass") %>% select(-Type)

bent_N <- bent %>% filter(Type == "Abundance") %>% select(-Type)




Spec_abund <- bent_N %>% select(-Station) %>% colSums(.) %>% t() %>% as.vector() 

spec_total_abund <- data.frame(Sp = bent_N %>% select(-Station) %>% colnames(), Spec_abund)

N_porog <- 500 

spec_selected <- spec_total_abund %>% filter(Spec_abund > N_porog)

bent_short <- bent_N %>% select(Station, spec_selected$Sp)

names(bent_short)

bent_short <-
  bent_short %>% mutate(Oligochaeta = Oligochaeta_gen._sp. + Oligochaeta_gen._spp.) %>% 
  select(-c(Oligochaeta_gen._sp., Oligochaeta_gen._spp., Oligochaeta_gen._sp._cocons,Senecella_siberica) )


bent_long <- melt(bent_short, id.vars = "Station", variable.name = "Species", value.name = "N")


bent_data <- merge(stat_sal, bent_long)

length(unique(bent_data$Species))

#### GAM modelling ################


Mod <- gam(log(N+1) ~  s(Sal, by = Species, bs = c("tp"), k = 5),   data = bent_data, method = "REML", family = "gaussian")


draw(Mod)


summary(Mod)




library(gratia)

Mod2 <- gam(log(N+1) ~ s(Sal, Species, bs = c("tp", "re"), k = c(5, 13) ),  data = bent_data, method = "REML", family = "gaussian")

summary(Mod2)
