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
