# В этом скрипте проведен код анализа фенологических матриц для всех видов копепод

library(ggplot2)

library(dplyr)

library(reshape2)

library(vegan)




# Часть 1. первичные фенологические данные

phen_acartia <- read.csv("data/Phenological_tables/Acartia_phenology.csv", header = TRUE)

phen_calanus <- read.csv("data/Phenological_tables/Calanus_phenology.csv", header = TRUE)

phen_centropages <- read.csv("data/Phenological_tables/Centropages_phenology.csv", header = TRUE)

phen_microsetella <- read.csv("data/Phenological_tables/Microsetella_phenology.csv", header = TRUE)

phen_oithona <- read.csv("data/Phenological_tables/Oithona_phenology.csv", header = TRUE)

phen_pseudocalanus <- read.csv("data/Phenological_tables/Pseudocalanus_phenology.csv", header = TRUE)

phen_temora <- read.csv("data/Phenological_tables/Temora_phenology.csv", header = TRUE)

phen_triconia <- read.csv("data/Phenological_tables/Triconia_phenology.csv", header = TRUE)





phen_all <- rbind(phen_acartia, phen_calanus, phen_centropages, phen_microsetella,phen_oithona, phen_pseudocalanus,  phen_temora, phen_triconia )



median_peak <- phen_all %>% group_by(Species) %>% summarise(Median_peak = median(Days_perc_50))


phen_all$Species <- factor(phen_all$Species, levels = median_peak$Species[order(median_peak$Median_peak)], ordered = TRUE)


# phen_all$Duration <- phen_all$Days_perc_85 -  phen_all$Days_perc_15



# Новые имена для переменных

names(phen_all) <- c("Year", "Begin", "Meadle", "End", "Peak", "Species")





names(phen_all)

ggplot(phen_all, aes(x = Species, y = Peak)) + geom_boxplot()


ggplot(phen_all, aes(x = Species, y = Begin)) + geom_boxplot()


ggplot(phen_all, aes(x = Species, y = Meadle)) + geom_boxplot()


ggplot(phen_all, aes(x = Species, y = End)) + geom_boxplot()


ggplot(phen_all, aes(x =  Begin, y = End)) + geom_point() + facet_wrap(~Species) + geom_smooth(method = "lm")



## Срденяя численность видов в летние месяцы 

# Подготовка данных ####
plankt_wide <- read.csv("data/Plankton_raw_tidy.csv", header = TRUE)
vars <- read.csv("data/Plankton_raw_tidy_variables.csv", header = T)


d <- (melt(plankt_wide, id.vars = c("Day", 	"Month", 	"Year", 	"Data"), variable.name = "Var_ID", value.name = "Abundance"))


plankt <- merge(d, vars)

## Среденвзвешанные значения, объединение слоев

plankt_1 <- plankt[plankt$Level == "0-10", ]
nrow(plankt_1)

plankt_2 <- plankt[plankt$Level == "10-25", ]
nrow(plankt_2)

plankt_3 <- plankt[plankt$Level == "25-bottom", ]
nrow(plankt_3)


names(plankt_1)

plankt_mean <- plankt_1

plankt_mean$Abund_mean <- (plankt_1$Abundance*10 + plankt_2$Abundance*15)/25


Abund_summer <- plankt_mean[plankt_mean$Stage == "Total" & plankt$Month %in% 6:9, ]

Abund_summer <- Abund_summer[!is.na(Abund_summer$Species), ]

Abund_summer$Species[is.na(Abund_summer$Species)]

names(Abund_summer)


Abundance <- Abund_summer %>% group_by(Year, Species) %>% summarise(Abund_mean = mean(Abund_mean, na.rm = TRUE))



Abundance$Species <- factor(Abundance$Species,labels = c("Acartia","Calanus","Centropages", "Microsetella", "Oithona", "Pseudocalanus","Temora","Triconia" ))

ggplot(Abundance, aes(x = Year, y = Abund_mean)) + geom_line() + facet_wrap(~Species, scales = "free_y") + geom_smooth(method = "loess")



phen_all_abund <- merge(phen_all, Abundance, by = c("Year", "Species"))

ggplot(phen_all_abund, aes(x = Year, y = log(Abund_mean+1) )) + geom_line() + facet_wrap(~Species, scales = "free_y") + geom_smooth(method = "lm")




ggplot(phen_all_abund, aes(x = Begin, y = log(Abund_mean) )) + geom_point() + facet_wrap(~Species, scales = "free") + geom_smooth(method = "lm")


ggplot(phen_all_abund, aes(x = Meadle, y = log(Abund_mean) )) + geom_point() + facet_wrap(~Species, scales = "free") + geom_smooth(method = "lm")



## Часть 2. Многомерный анализ


phen_melt <- melt(phen_all, id.vars = c("Year", "Species"))
phenol <- dcast(phen_melt[phen_melt$Species != "Triconia",], formula = Year ~ Species  + variable, value.var = "value" ) 


# phen_melt2 <- phen_melt %>% group_by(Year, Species) %>% summarise(Index = mean(value)) 
# phenol <- dcast(phen_melt2, formula = Year ~ Species, value.var = "Index" ) 

Abundance$Abund_mean <- log(Abundance$Abund_mean + 1) #Viva la logarithm

abund_melt <- melt(Abundance, id.vars = c("Year", "Species"))
abund_melt$variable <- "N"

abund <- dcast(abund_melt, formula = Year ~ variable + Species , value.var = "value")

abund <- abund[ , !(names(abund) %in% c("Year", "N_Triconia"))] 





env <- read.csv("data/env_gap_filled_short.csv", header = TRUE) 

env <- env[,-1]


env_abund <- cbind(env, abund)


names(env)
# 
# env_abund <- env_abund[,-1]

names(env_abund)

row.names(phenol) <- phenol$Year  

# phenol <- phenol[,-1]

# 
# phenol <- phenol[,-3] #Убираем Triconia


phen_cca <- cca(phenol[, -1] ~ ., data =  env_abund)

summary(phen_cca)

vif.cca(phen_cca)[order(vif.cca(phen_cca))]

# phen_cca <- update(phen_cca, . ~ . - Ice_clear_M)
# 
# vif.cca(phen_cca)[order(vif.cca(phen_cca))]

#Оставляем предикторы с VIF<10

anova(phen_cca)
anova(phen_cca, by="axis")

plot(phen_cca)


ordistep(phen_cca)



phen_cca_reduced <- cca(  phenol[, -1] ~ t_3 + Su_start + Su_start_1 + Su_duration_1 + t.Spring_1 + N_Acartia + N_Calanus + N_Microsetella + N_Oithona, data = env_abund)

plot(phen_cca_reduced, scaling = "sites")

summary(phen_cca_reduced)

anova(phen_cca_reduced)

anova(phen_cca_reduced, by = "axis", permutations = 9999)

anova(phen_cca_reduced, by = "margin", permutations = 9999)


plot(phen_cca_reduced, scaling = "sites")








fortify(phen_cca_reduced, scaling = "sites")



gg_phen_cca_reduced <- fortify(phen_cca_reduced, scaling = "sites")

gg_phen_cca_reduced$Score

autoplot(phen_cca_reduced, layers = c("species", "biplot"))



# Фенологическая матрица в сокращенном виде используется фенологический индекс 

phen_all2 <- phen_all

phen_all2$Phen_ind <- apply(phen_all2[ , 2:5], MARGIN = 1, FUN = mean)


phen_melt2 <- melt(phen_all2[ , -c(2:5)], id.vars = c("Year", "Species"))
phenol2 <- dcast(phen_melt2[phen_melt2$Species != "Triconia",], formula = Year ~ Species  + variable, value.var = "value" ) 


phenol2 <- phenol2[!(phenol2$Year %in% year_exclude), ]

# phen_melt2 <- phen_melt %>% group_by(Year, Species) %>% summarise(Index = mean(value)) 
# phenol <- dcast(phen_melt2, formula = Year ~ Species, value.var = "Index" ) 

Abundance$Abund_mean <- log(Abundance$Abund_mean + 1) #Viva la logarithm

abund_melt <- melt(Abundance, id.vars = c("Year", "Species"))
abund_melt$variable <- "N"

abund <- dcast(abund_melt, formula = Year ~ variable + Species , value.var = "value")

abund <- abund[ , !(names(abund) %in% c("Year", "N_Triconia"))] 


phen_cca2 <- cca(phenol2[, -1] ~ ., data =  env_abund)


phen_cca2_red <- ordistep(phen_cca2)

plot(phen_cca2_red, scaling = "sites")

anova(phen_cca2_red)

anova(phen_cca2_red, by = "axis")

anova(phen_cca2_red, by = "margin")

anova(phen_cca2_red, by = "terms")




ggplot(Abundance, aes(x = Year, y = log(Abund_mean))) + geom_point() + facet_wrap(~Species, scales = "free") + geom_smooth(method = "lm") + xlim(1963, 2018)



#### Код для вычисления корреляций между временным рядом обилия вида и временным рядом внешнего фактора, плотноно независимого фактора.####

phen_acartia <- read.csv("data/Phenological_tables/Acartia_phenology.csv", header = TRUE)

phen_calanus <- read.csv("data/Phenological_tables/Calanus_phenology.csv", header = TRUE)

phen_centropages <- read.csv("data/Phenological_tables/Centropages_phenology.csv", header = TRUE)

phen_microsetella <- read.csv("data/Phenological_tables/Microsetella_phenology.csv", header = TRUE)

phen_oithona <- read.csv("data/Phenological_tables/Oithona_phenology.csv", header = TRUE)

phen_pseudocalanus <- read.csv("data/Phenological_tables/Pseudocalanus_phenology.csv", header = TRUE)

phen_temora <- read.csv("data/Phenological_tables/Temora_phenology.csv", header = TRUE)

# phen_triconia <- read.csv("data/Phenological_tables/Triconia_phenology.csv", header = TRUE)


year_exclude <- c(1961, 1962)

phen_all <- rbind(phen_acartia, phen_calanus, phen_centropages, phen_microsetella,phen_oithona, phen_pseudocalanus,  phen_temora)


phen_all <- phen_all[!(phen_all$Year %in% year_exclude), ]


median_peak <- phen_all %>% group_by(Species) %>% summarise(Median_peak = median(Days_perc_50))


phen_all$Species <- factor(phen_all$Species, levels = median_peak$Species[order(median_peak$Median_peak)], ordered = TRUE)

# Новые имена для переменных

names(phen_all) <- c("Year", "Begin", "Middle", "End", "Peak", "Species")


Abundance <- read.csv("data/abundance.csv", header = TRUE) # Данные по обилию видов, усреденнные по всему столбу воды за март-сентябрь.

env <- read.csv("data/env_gap_filled_short.csv", header = TRUE)  #Данные по средовым показателям



Abundance_melt <- melt(Abundance, id.vars = "Year", variable.name = "Species", value.name = "N")
Abundance_melt$Species <- gsub("_N", "", Abundance_melt[, 2]) 



Abundance_melt$Species <- factor(Abundance_melt$Species, levels = c("Pseudocalanus", "Calanus", "Microsetella", "Oithona",  "Centropages", "Acartia", "Temora"))


# Abundance_melt$Log_N <- log(Abundance_melt$N)



Abundance_melt <- Abundance_melt[Abundance_melt$Year %in% 1963:2018, ]



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



Abundance_phen$Log_N <- log(Abundance_phen$N + 1)

# Детрендинг временных рядов

detrend_abund <- Abundance_phen %>% group_by(Species) %>% do(data.frame(Year = .$Year, Log_N = resid(lm(Log_N ~ Year, data = .)), Begin = resid(lm(Begin ~ Year, data = .)), End = resid(lm(End ~ Year, data = .))))


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


Royama_cors_quant <- Deltas_2 %>% group_by(Species) %>% summarise( Cor = cor(Delta_R_2, Delta_Begin_2))



Royama_perms <- function(x){
  x$Log_N <- sample(x$Log_N)
  x$Begin <- sample(x$Begin)
  x$End <- sample(x$End)
  x 
}





perms_qual <- data.frame(Species = Royama_cors$Species, Perms_gr_qual = 0)

perms_quant <- data.frame(Species = Royama_cors$Species, Perms_gr_quant = 0)


for(i in 1:9999) {

Abundance_phen_perm <- Royama_perms(Abundance_phen)

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


Royama_cors_perm_quant <- Deltas_2 %>% group_by(Species) %>% summarise( Cor = cor(Delta_R_2, Delta_Begin_2))

gr_qual <- ifelse(abs(Royama_cors_perm_qual$Cor) >= abs(Royama_cors_qual$Cor), 1, 0  )
perms_qual$Perms_gr_qual <- perms_qual$Perms_gr_qual + gr_qual

gr_quant <- ifelse(abs(Royama_cors_perm_quant$Cor) >= abs(Royama_cors_quant$Cor), 1, 0  )
perms_quant$Perms_gr_quant <- perms_quant$Perms_gr_quant + gr_quant


print(i)
}



perms_qual
perms_qual$Species <- Royama_cors$Species
perms_qual$Perms_gr_qual <- perms_qual$Perms_gr_qual + 1 
perms_qual$p <- perms_qual$Perms_gr_qual/10000
perms_qual$p_adj <- p.adjust(perms_qual$p, method = "BH")
perms_qual$Royama_cors <- Royama_cors_qual$Cor



perms_quant
perms_qual$Species <- Royama_cors$Species
perms_quant$Perms_gr_quant <- perms_quant$Perms_gr_quant + 1 
perms_quant$p <- perms_quant$Perms_gr_quant/10000
perms_quant$p_adj <- p.adjust(perms_quant$p, method = "BH")
perms_quant$Royama_cors <- Royama_cors_quant$Cor


perms$p_adj <- p.adjust(perms$p, method = "BH")

save("perms_quant", file = "data/Royama_cors_quant.RData")

save("perms_qual", file = "data/Royama_cors_qual.RData")


load("data/Royama_cors_qual.RData")

perms_qual



###################
anova(phen_cca, phen_cca_reduced, permutations = 9999)
summary(phen_cca)


qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Microsetella"], y = Abundance_phen$Peak[Abundance_phen$Species == "Oithona"] ) + geom_smooth(method = "lm")

qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Microsetella"], y = Abundance_phen$Peak[Abundance_phen$Species == "Oithona"] ) + geom_smooth(method = "lm")

qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Microsetella"], y = Abundance_phen$Begin[Abundance_phen$Species == "Oithona"] ) + geom_smooth(method = "lm")


qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Microsetella"], y = Abundance_phen$Begin[Abundance_phen$Species == "Oithona"] ) + geom_smooth(method = "lm")


qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Acartia"], y = Abundance_phen$Peak[Abundance_phen$Species == "Temora"] ) + geom_smooth(method = "lm")


qplot(x = env2$SpSD, y = Abundance_phen$End[Abundance_phen$Species == "Centropages"] ) + geom_smooth(method = "lm")



qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Acartia"], y = Abundance_phen$Begin[Abundance_phen$Species == "Microsetella"] ) + geom_smooth(method = "lm")



qplot(x = Abundance_phen$Log_N[Abundance_phen$Species == "Acartia"], y = Abundance_phen$Peak[Abundance_phen$Species == "Oithona"] ) + geom_smooth(method = "lm")
