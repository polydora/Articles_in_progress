#### Код для вычисления корреляций между временным рядом обилия вида и временным рядом внешнего фактора, плотноно независимого фактора.####

year_exclude <- c(1961, 1962, 1963, 1964)

phen_acartia <- read.csv("data/Phenological_tables/Acartia_phenology.csv", header = TRUE)

phen_calanus <- read.csv("data/Phenological_tables/Calanus_phenology.csv", header = TRUE)

phen_centropages <- read.csv("data/Phenological_tables/Centropages_phenology.csv", header = TRUE)

phen_microsetella <- read.csv("data/Phenological_tables/Microsetella_phenology.csv", header = TRUE)


phen_microsetella[phen_microsetella$Year %in% year_exclude,  2:5 ] <- NA 


phen_oithona <- read.csv("data/Phenological_tables/Oithona_phenology.csv", header = TRUE)

phen_pseudocalanus <- read.csv("data/Phenological_tables/Pseudocalanus_phenology.csv", header = TRUE)

phen_temora <- read.csv("data/Phenological_tables/Temora_phenology.csv", header = TRUE)

# phen_triconia <- read.csv("data/Phenological_tables/Triconia_phenology.csv", header = TRUE)



phen_all <- rbind(phen_acartia, phen_calanus, phen_centropages, phen_microsetella,phen_oithona, phen_pseudocalanus,  phen_temora)


# phen_all <- phen_all[!(phen_all$Year %in% year_exclude), ]


median_peak <- phen_all %>% group_by(Species) %>% summarise(Median_peak = median(Days_perc_50, na.rm = TRUE))


phen_all$Species <- factor(phen_all$Species, levels = median_peak$Species[order(median_peak$Median_peak)], ordered = TRUE)

# Новые имена для переменных

names(phen_all) <- c("Year", "Begin", "Middle", "End", "Peak", "Species")


Abundance <- read.csv("data/abundance.csv", header = TRUE) # # Данные по обилию видов, усреденнные по всему столбу воды за май-октябрь !!!!

env <- read.csv("data/env_gap_filled_short.csv", header = TRUE)  #Данные по средовым показателям



Abundance_melt <- melt(Abundance, id.vars = "Year", variable.name = "Species", value.name = "N")
Abundance_melt$Species <- gsub("_N", "", Abundance_melt[, 2]) 



Abundance_melt$Species <- factor(Abundance_melt$Species, levels = c("Pseudocalanus", "Calanus", "Microsetella", "Oithona",  "Centropages", "Acartia", "Temora"))


# Abundance_melt$Log_N <- log(Abundance_melt$N)



# Abundance_melt <- Abundance_melt[Abundance_melt$Year %in% 1963:2018, ]



Abundance_phen <- merge(Abundance_melt, phen_all, by = c("Year", "Species"))

Abundance_phen$Log_N <- log(Abundance_phen$N + 1)



Deltas <- Abundance_phen %>% group_by(Species) %>% do(data.frame(R = .$Log_N[2:length(.$Log_N)] - .$Log_N[1:(length(.$Log_N)-1)], Delta_Begin = .$Begin[2:length(.$Begin)] - .$Begin[1:(length(.$Begin)-1)], Delta_End = .$End[2:length(.$End)] - .$End[1:(length(.$End)-1)] ))


ggplot(Deltas, aes(x = Delta_Begin, y = R)) + geom_point() + facet_wrap(~Species, ncol = 2) + geom_smooth(method = "lm", se = FALSE)




Deltas_2 <- Deltas %>% group_by(Species) %>% do(data.frame(Delta_R_2 = .$R[2:length(.$R)] - .$R[1:(length(.$R)-1)], Delta_Begin_2 = .$Delta_Begin[2:length(.$Delta_Begin)] - .$Delta_Begin[1:(length(.$Delta_Begin)-1)], Delta_End_2 = .$Delta_End[2:length(.$Delta_End)] - .$Delta_End[1:(length(.$Delta_End)-1)] ))


Pl_R <- ggplot(Deltas_2, aes(x = Delta_Begin_2, y = Delta_R_2))                                               
Pl_R + geom_point() + facet_wrap(~Species, ncol = 2) + geom_smooth(method = "lm", se = FALSE)





# Коэффициент качественной корреляции по Royama 1981####

congrs <- Deltas %>% group_by(Species) %>% do(data.frame(
  Congr = ifelse( 
    ((.$R[1:(length(.$R)-1)] >= 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] >=0) | 
       (.$R[1:(length(.$R)-1)] < 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] <0)) &
      ( ((.$R[2:(length(.$R))] >=  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] >=  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]) |
           (.$R[2:(length(.$R))] <  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] <  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]))), 1, 0)))


congrs %>% group_by(Species) %>% summarise( (sum(Congr == 1) - sum(Congr == 0)) / (sum(Congr == 1) + sum(Congr == 0)))


ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1961, 1963) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1962, 1964) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1963, 1965) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1964, 1966) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1965, 1967) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1966, 1968) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1967, 1969) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1968, 1970) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1969, 1971) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1970, 1972) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1971, 1973) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1973, 1975) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1974, 1976) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1975, 1977) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1976, 1977) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1977, 1979) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1978, 1980) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1979, 1981) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1980, 1982) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1981, 1983) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1982, 1984) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1983, 1985) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1984, 1986) 

ggplot() + geom_line(data = phen_pseudocalanus, aes(x = Year, y = Days_perc_15)) + geom_line(data = Abundance, aes(x = Year, y = log(Pseudocalanus_N + 1)*20)) + xlim(1985, 1987) 






# Пермутационная оценка корреляции между временными рядами



# В этой части анализа все делается при исключении Microsetella, у которой есть NA
###################################################################################

Abundance_phen$Log_N <- log(Abundance_phen$N + 1)

Abundance_phen2 <- Abundance_phen[Abundance_phen$Species != "Microsetella", ] 

# Детрендинг временных рядов

detrend_abund <- Abundance_phen2 %>% group_by(Species) %>% do(data.frame(Year = .$Year, Log_N = resid(lm(Log_N ~ Year, data = .)), Begin = resid(lm(Begin ~ Year, data = .)), End = resid(lm(End ~ Year, data = .))))


Deltas <- detrend_abund %>% group_by(Species) %>% do(data.frame(R = .$Log_N[2:length(.$Log_N)] - .$Log_N[1:(length(.$Log_N)-1)], Delta_Begin = .$Begin[2:length(.$Begin)] - .$Begin[1:(length(.$Begin)-1)], Delta_End = .$End[2:length(.$End)] - .$End[1:(length(.$End)-1)] ))


Deltas_2 <- Deltas %>% group_by(Species) %>% do(data.frame(Delta_R_2 = .$R[2:length(.$R)] - .$R[1:(length(.$R)-1)], Delta_Begin_2 = .$Delta_Begin[2:length(.$Delta_Begin)] - .$Delta_Begin[1:(length(.$Delta_Begin)-1)], Delta_End_2 = .$Delta_End[2:length(.$Delta_End)] - .$Delta_End[1:(length(.$Delta_End)-1)] ))


# Коэффициент качественной корреляции по Royama 1981####

congrs <- Deltas %>% group_by(Species) %>% do(data.frame(
  Congr = ifelse( 
    ((.$R[1:(length(.$R)-1)] >= 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] >=0) | 
       (.$R[1:(length(.$R)-1)] < 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] <0)) &
      ( ((.$R[2:(length(.$R))] >=  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] >=  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]) |
           (.$R[2:(length(.$R))] <  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] <  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]))), 1, 0)))


Royama_cors_qual <- congrs %>% group_by(Species) %>% summarise( Cor =  (sum(Congr == 1) - sum(Congr == 0)) / (sum(Congr == 1) + sum(Congr == 0)))


# Royama_cors_quant <- Deltas_2 %>% group_by(Species) %>% summarise( Cor = cor(Delta_R_2, Delta_Begin_2))

# 
# 
# Royama_perms <- function(x){
#   x$Log_N <- sample(x$Log_N)
#   x$Begin <- sample(x$Begin)
#   x$End <- sample(x$End)
#   x 
# }





perms_qual <- data.frame(Species = Royama_cors_qual$Species, Perms_gr_qual = 0)

# perms_quant <- data.frame(Species = Royama_cors_qual$Species, Perms_gr_quant = 0)


for(i in 1:9999) {
  
  Abundance_phen_perm <- Abundance_phen2 %>% group_by(Species) %>%  do(data.frame(Year = .$Year,  Log_N = sample(.$Log_N), Begin = sample(.$Begin), End = sample(.$End) ))
  
  detrend_abund_perm <- Abundance_phen_perm %>% group_by(Species) %>% do(data.frame(Year = .$Year, Log_N = resid(lm(Log_N ~ Year, data = .)), Begin = resid(lm(Begin ~ Year, data = .)), End = resid(lm(End ~ Year, data = .))))
  
  
  Deltas <- detrend_abund_perm %>% group_by(Species) %>% do(data.frame(R = .$Log_N[2:length(.$Log_N)] - .$Log_N[1:(length(.$Log_N)-1)], Delta_Begin = .$Begin[2:length(.$Begin)] - .$Begin[1:(length(.$Begin)-1)], Delta_End = .$End[2:length(.$End)] - .$End[1:(length(.$End)-1)] ))
  
  Deltas_2 <- Deltas %>% group_by(Species) %>% do(data.frame(Delta_R_2 = .$R[2:length(.$R)] - .$R[1:(length(.$R)-1)], Delta_Begin_2 = .$Delta_Begin[2:length(.$Delta_Begin)] - .$Delta_Begin[1:(length(.$Delta_Begin)-1)], Delta_End_2 = .$Delta_End[2:length(.$Delta_End)] - .$Delta_End[1:(length(.$Delta_End)-1)] ))
  
  
  congrs <- Deltas %>% group_by(Species) %>% do(data.frame(
    Congr = ifelse( 
      ((.$R[1:(length(.$R)-1)] >= 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] >=0) | 
         (.$R[1:(length(.$R)-1)] < 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] <0)) &
        ( ((.$R[2:(length(.$R))] >=  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] >=  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]) |
             (.$R[2:(length(.$R))] <  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] <  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]))), 1, 0)))
  
  
  Royama_cors_perm_qual <- congrs %>% group_by(Species) %>% summarise(Cor =  (sum(Congr == 1) - sum(Congr == 0)) / (sum(Congr == 1) + sum(Congr == 0)))
  
  
  # Royama_cors_perm_quant <- Deltas_2 %>% group_by(Species) %>% summarise( Cor = cor(Delta_R_2, Delta_Begin_2))
  
  gr_qual <- ifelse(abs(Royama_cors_perm_qual$Cor) >= abs(Royama_cors_qual$Cor), 1, 0  )
  perms_qual$Perms_gr_qual <- perms_qual$Perms_gr_qual + gr_qual
  
  # gr_quant <- ifelse(abs(Royama_cors_perm_quant$Cor) >= abs(Royama_cors_quant$Cor), 1, 0  )
  # perms_quant$Perms_gr_quant <- perms_quant$Perms_gr_quant + gr_quant
  
  
  print(i)
}



perms_qual
perms_qual$Species <- Royama_cors_qual$Species
perms_qual$Perms_gr_qual <- perms_qual$Perms_gr_qual + 1 
perms_qual$p <- perms_qual$Perms_gr_qual/10000
perms_qual$p_adj <- p.adjust(perms_qual$p, method = "BH")
perms_qual$Royama_cors <- Royama_cors_qual$Cor

perms_qual_no_Microsetella <- perms_qual

# perms_quant
# perms_qual$Species <- Royama_cors_quant$Species
# perms_quant$Perms_gr_quant <- perms_quant$Perms_gr_quant + 1 
# perms_quant$p <- perms_quant$Perms_gr_quant/10000
# perms_quant$p_adj <- p.adjust(perms_quant$p, method = "BH")
# perms_quant$Royama_cors <- Royama_cors_quant$Cor


# perms$p_adj <- p.adjust(perms$p, method = "BH")

# save("perms_quant", file = "data/Royama_cors_quant.RData")

save("perms_qual_no_Microsetella", file = "data/Royama_cors_qual.RData")


load("data/Royama_cors_qual.RData")

perms_qual_no_Microsetella




# В этой части анализа все делается для Microsetella
###################################################################################

Abundance_phen3 <- Abundance_phen[Abundance_phen$Species == "Microsetella", ] 

Abundance_phen3 <- Abundance_phen3[!(Abundance_phen3$Year %in% year_exclude), ]
# Детрендинг временных рядов

detrend_abund <- Abundance_phen3 %>% group_by(Species) %>% do(data.frame(Year = .$Year, Log_N = resid(lm(Log_N ~ Year, data = .)), Begin = resid(lm(Begin ~ Year, data = .)), End = resid(lm(End ~ Year, data = .))))


Deltas <- detrend_abund %>% group_by(Species) %>% do(data.frame(R = .$Log_N[2:length(.$Log_N)] - .$Log_N[1:(length(.$Log_N)-1)], Delta_Begin = .$Begin[2:length(.$Begin)] - .$Begin[1:(length(.$Begin)-1)], Delta_End = .$End[2:length(.$End)] - .$End[1:(length(.$End)-1)] ))


Deltas_2 <- Deltas %>% group_by(Species) %>% do(data.frame(Delta_R_2 = .$R[2:length(.$R)] - .$R[1:(length(.$R)-1)], Delta_Begin_2 = .$Delta_Begin[2:length(.$Delta_Begin)] - .$Delta_Begin[1:(length(.$Delta_Begin)-1)], Delta_End_2 = .$Delta_End[2:length(.$Delta_End)] - .$Delta_End[1:(length(.$Delta_End)-1)] ))


# Коэффициент качественной корреляции по Royama 1981####

congrs <- Deltas %>% group_by(Species) %>% do(data.frame(
  Congr = ifelse( 
    ((.$R[1:(length(.$R)-1)] >= 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] >=0) | 
       (.$R[1:(length(.$R)-1)] < 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] <0)) &
      ( ((.$R[2:(length(.$R))] >=  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] >=  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]) |
           (.$R[2:(length(.$R))] <  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] <  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]))), 1, 0)))


Royama_cors_qual <- congrs %>% group_by(Species) %>% summarise( Cor =  (sum(Congr == 1) - sum(Congr == 0)) / (sum(Congr == 1) + sum(Congr == 0)))


# Royama_cors_quant <- Deltas_2 %>% group_by(Species) %>% summarise( Cor = cor(Delta_R_2, Delta_Begin_2))

# 
# 
# Royama_perms <- function(x){
#   x$Log_N <- sample(x$Log_N)
#   x$Begin <- sample(x$Begin)
#   x$End <- sample(x$End)
#   x 
# }





perms_qual <- data.frame(Species = Royama_cors_qual$Species, Perms_gr_qual = 0)

# perms_quant <- data.frame(Species = Royama_cors_qual$Species, Perms_gr_quant = 0)


for(i in 1:9999) {
  
  Abundance_phen_perm <- Abundance_phen3 %>% group_by(Species) %>%  do(data.frame(Year = .$Year,  Log_N = sample(.$Log_N), Begin = sample(.$Begin), End = sample(.$End) ))
  
  detrend_abund_perm <- Abundance_phen_perm %>% group_by(Species) %>% do(data.frame(Year = .$Year, Log_N = resid(lm(Log_N ~ Year, data = .)), Begin = resid(lm(Begin ~ Year, data = .)), End = resid(lm(End ~ Year, data = .))))
  
  
  Deltas <- detrend_abund_perm %>% group_by(Species) %>% do(data.frame(R = .$Log_N[2:length(.$Log_N)] - .$Log_N[1:(length(.$Log_N)-1)], Delta_Begin = .$Begin[2:length(.$Begin)] - .$Begin[1:(length(.$Begin)-1)], Delta_End = .$End[2:length(.$End)] - .$End[1:(length(.$End)-1)] ))
  
  Deltas_2 <- Deltas %>% group_by(Species) %>% do(data.frame(Delta_R_2 = .$R[2:length(.$R)] - .$R[1:(length(.$R)-1)], Delta_Begin_2 = .$Delta_Begin[2:length(.$Delta_Begin)] - .$Delta_Begin[1:(length(.$Delta_Begin)-1)], Delta_End_2 = .$Delta_End[2:length(.$Delta_End)] - .$Delta_End[1:(length(.$Delta_End)-1)] ))
  
  
  congrs <- Deltas %>% group_by(Species) %>% do(data.frame(
    Congr = ifelse( 
      ((.$R[1:(length(.$R)-1)] >= 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] >=0) | 
         (.$R[1:(length(.$R)-1)] < 0 & .$Delta_Begin[1:(length(.$Delta_Begin)-1)] <0)) &
        ( ((.$R[2:(length(.$R))] >=  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] >=  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]) |
             (.$R[2:(length(.$R))] <  .$R[1:(length(.$R)-1)] & .$Delta_Begin[2:(length(.$Delta_Begin))] <  .$Delta_Begin[1:(length(.$Delta_Begin)-1)]))), 1, 0)))
  
  
  Royama_cors_perm_qual <- congrs %>% group_by(Species) %>% summarise(Cor =  (sum(Congr == 1) - sum(Congr == 0)) / (sum(Congr == 1) + sum(Congr == 0)))
  
  
  # Royama_cors_perm_quant <- Deltas_2 %>% group_by(Species) %>% summarise( Cor = cor(Delta_R_2, Delta_Begin_2))
  
  gr_qual <- ifelse(abs(Royama_cors_perm_qual$Cor) >= abs(Royama_cors_qual$Cor), 1, 0  )
  perms_qual$Perms_gr_qual <- perms_qual$Perms_gr_qual + gr_qual
  
  # gr_quant <- ifelse(abs(Royama_cors_perm_quant$Cor) >= abs(Royama_cors_quant$Cor), 1, 0  )
  # perms_quant$Perms_gr_quant <- perms_quant$Perms_gr_quant + gr_quant
  
  
  print(i)
}



perms_qual
perms_qual$Species <- Royama_cors_qual$Species
perms_qual$Perms_gr_qual <- perms_qual$Perms_gr_qual + 1 
perms_qual$p <- perms_qual$Perms_gr_qual/10000
perms_qual$p_adj <- p.adjust(perms_qual$p, method = "BH")
perms_qual$Royama_cors <- Royama_cors_qual$Cor
perms_qual_Microsetella <- perms_qual 


# perms_quant
# perms_qual$Species <- Royama_cors_quant$Species
# perms_quant$Perms_gr_quant <- perms_quant$Perms_gr_quant + 1 
# perms_quant$p <- perms_quant$Perms_gr_quant/10000
# perms_quant$p_adj <- p.adjust(perms_quant$p, method = "BH")
# perms_quant$Royama_cors <- Royama_cors_quant$Cor



perms_qual <- rbind(perms_qual_no_Microsetella,perms_qual_Microsetella) 


save("perms_qual", file = "data/Royama_cors_qual.RData")

