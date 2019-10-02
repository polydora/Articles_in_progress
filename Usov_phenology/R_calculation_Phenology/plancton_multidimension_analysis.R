library(vegan)
library(reshape2)
library(doBy)

env <- read.csv("data/env_gap_filled.csv", header = TRUE) 

names(env)[1] <- "Year"



phen_calanus <- log_param_Calanus[,c("Days_perc_15",	"Days_perc_50",	"Days_perc_85",	"Peak_Days_from_year_start")]

names(phen_calanus) <- c("Cal_15", "Cal_50", "Cal_85", "Cal_peak")

phen_calanus$Cal_15[is.na(phen_calanus$Cal_15)] <- phen_calanus$Cal_peak[is.na(phen_calanus$Cal_15)]

phen_calanus$Cal_50[is.na(phen_calanus$Cal_50)] <- phen_calanus$Cal_peak[is.na(phen_calanus$Cal_50)]

phen_calanus$Cal_85[is.na(phen_calanus$Cal_85)] <- phen_calanus$Cal_peak[is.na(phen_calanus$Cal_85)]

phen_calanus$Sp <- "Calanus"





phen_centropages <- log_param_Centropages[,c("Days_perc_15",	"Days_perc_50",	"Days_perc_85",	"Peak_Days_from_year_start")]

names(phen_centropages) <- c("Cent_15", "Cent_50", "Cent_85", "Cent_peak")

phen_centropages$Sp <- "Centropages"





phen_temora <- log_param_Temora[,c("Days_perc_15",	"Days_perc_50",	"Days_perc_85","Peak_Days_from_year_start")]

names(phen_temora) <- c("Tem_15", "Tem_50", "Tem_85", "Tem_peak")


phen_temora$Sp <- "Temora"




phen_oithona <- log_param_Oithona[,c("Days_perc_15",	"Days_perc_50",	"Days_perc_85",		"Peak_Days_from_year_start")]

names(phen_oithona) <- c("Oit_15", "Oit_50", "Oit_85", "Oit_peak")

phen_oithona$Sp <- "Oithona"



# 
# phenol <- cbind(phen_calanus, phen_centropages, phen_temora, phen_oithona) 
# 
# phenol$Year <- 1961:2018

# names(phen_calanus) <- names(phen_centropages)



# phenol <- rbind(phen_calanus, phen_centropages, phen_temora, phen_oithona)


phenol <- cbind(phen_calanus, phen_centropages, phen_temora, phen_oithona)

nrow(phenol)

phenol <- phenol[, -c(5, 10, 15, 20)]




## Срденяя численность видов в летние месяцы 
Abund_summer <- plankt_mean[plankt_mean$Stage == "Total" & plankt$Month %in% 6:9, ]

Abund_summer <- Abund_summer[!is.na(Abund_summer$Species), ]

Abund_summer$Species[is.na(Abund_summer$Species)]

names(Abund_summer)

Abundance <- summaryBy(Abund_mean ~ Species + Year, data = Abund_summer, FUN = function(x) mean(x, na.rm = T), keep.names = T)

ggplot(Abundance, aes(x = Year, y = Abund_mean)) + geom_line() + facet_wrap(~Species, scales = "free_y") + geom_smooth(method = "lm")




#Удаляем 1972 и 2014 гг Так как там нет возможности честно избавиться от невозможности подобрать модель для кумуляты


nrow(phenol)
length(1961:2018)

phenol$Year <- 1961:2018

phenol2 <- phenol[!(phenol$Year %in% c(1972, 2014)), ]

colSums(is.na(phenol))

env2 <- env[!(env[,1] %in% c(1972, 2014)), ]

colSums(is.na(env))

Abundance2 <- Abundance[!(Abundance$Year %in% c(1972, 2014)), ]

Abundance3 <- dcast(Abundance2, Year ~ Species)

env_abund <- cbind(env2, Abundance3)

names(env_abund)[38:41] <- c("N_Calanus", "N_Centropages", "N_Oithona", "N_Temora")



## CCA


phen_cca <- cca(phenol2[,-c(ncol(phenol2))] ~ ., data = env2)


vif.cca(phen_cca)[order(vif.cca(phen_cca))]

phen_cca <- update(phen_cca, . ~ . - Ice_clear_M)

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . - t_Summer)

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . - Ice_thick_M )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . - t_Spring  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . - Su_start  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . - t_6   )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . - Su_duration_1   )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . -  NAO_1  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . -  Su_duration  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . -  t_May  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]


phen_cca <- update(phen_cca, . ~ . -   t_June  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . -   Su_start_1  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . -   t_July  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . -   NAO  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . -  t_Sep  )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



phen_cca <- update(phen_cca, . ~ . - Ice_clear_G   )

vif.cca(phen_cca)[order(vif.cca(phen_cca))]



### Предикторы отобранные волевым решением

phen_cca_selected <- cca(phenol2[,-c(ncol(phenol2))] ~ t_peak + t_3 + Su_start + t_Spring + t_Summer +  Ice_clear_K  + Su_start_1  + Su_end_1 + NAO + 	NAO_1 +	AOI_DJFM	+ AOI_1 + N_Calanus +  N_Centropages + N_Oithona + N_Temora,   data = env_abund)

vif.cca(phen_cca_selected)

ordistep(phen_cca_selected)

phen_cca_selected_final <- cca(phenol2[, -c(ncol(phenol2))] ~ t_3 + t_Spring + AOI_1 + N_Calanus +  N_Oithona +  N_Centropages + N_Temora, data = env_abund )


plot(phen_cca_selected_final)

anova(phen_cca_selected_final)

anova(phen_cca_selected_final, by = "axis")


anova(phen_cca_selected_final, by = "margin")


plot(phen_cca_selected_final, display = "sp")



plot(phen_cca_selected_final, scaling = "sites")



summary(phen_cca)

ordistep(phen_cca)


phen_cca_final <- cca(formula = phenol2[, -ncol(phenol2)] ~ t_3 + t_7 + Su_end_1 + t.Spring , data = env2)

vif.cca(phen_cca_final)

anova(phen_cca_final, permutations = 9999)

anova(phen_cca_final, by = "axis", permutations = 9999)

anova(phen_cca_final, by = "margin", permutations = 9999)

anova(phen_cca_final, by = "terms", permutations = 9999)


plot(phen_cca_final)


scores(phen_cca_final)


Scores <- data.frame(Year = phenol2$Year, CCA1 = scores(phen_cca_final)$sites[, 1] )


Abund_CCA <- merge(Abundance2, Scores, by = "Year")

env_CCA <- merge(env2, Scores, by = "Year")



ggplot(Abund_CCA, aes(x = CCA1, y =  Abund_mean)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Species, scales = "free_y")


ggplot(env_CCA, aes(x = CCA1, y =  t.Spring)) + geom_point() + geom_smooth(method = "lm") 
ggplot(env_CCA, aes(x = CCA1, y =  t_7)) + geom_point() + geom_smooth(method = "lm") 

ggplot(env_CCA, aes(y = CCA1, x =  Year)) + geom_point() + geom_smooth() 


env2$п.їYear

ggplot(env2, aes(x = п.їYear, y = t_Spring)) + geom_point() + geom_smooth()

ggplot(env2, aes(x = п.їYear, y = Su_end_1)) + geom_point() + geom_smooth(method = "lm")



ggplot(env2, aes(x = Year, y = t_3)) + geom_point() + geom_smooth(method = "lm")




# CCA Для матрицы предикторов
env_cca <- cca(env[, -1] ~ Year, data = env)

plot(env_cca, display = "sp")
anova(env_cca)
scores(env_cca)$species




## MDS

phen_mds <- metaMDS(phenol[, -ncol(phenol)], distance = "euclidean" )

phen_mds_env <- envfit(formula = phen_mds ~ t_3 + t_7 + Su_end_1 + t.Spring, data = env[, -1])

plot(phen_mds_env)

ordisurf(formula = phen_mds ~ Su_end_1 , data = env[, -1])



## Пробую описать тренды в фенологической матрице для каждого вида в отдельнсти

# Тренд-матрица

years <- 1961:2018


# Для Calanus
trend_dist <- dist(years[!(years %in% c(1972))])

dist_calanus <- dist(phen_calanus[!(years %in% c(1972)), -5])

mantel_calanus <- mantel(dist_calanus, trend_dist, permutations = 9999)



# Для Centropages
trend_dist <- dist(years)

dist_centropages <- dist(phen_centropages[,-5])

mantel_centropages <- mantel(dist_centropages, trend_dist, permutations = 9999)



# Для Temora
trend_dist <- dist(years)

dist_temora <- dist(phen_temora [ ,-5 ])

mantel_temora <- mantel(dist_temora, trend_dist, permutations = 9999)



# Для Oithona
trend_dist <- dist(years[!(years %in% c(2014))])

dist_oithona <- dist(phen_oithona[!(years %in% c(2014)), -5])

mantel_oithona <- mantel(dist_oithona, trend_dist, permutations = 9999)



#### Второй вариант анализа фенологии. В качестве ковариаты выступает вид.

phen_calanus2 <- phen_calanus
phen_centropages2 <- phen_centropages
phen_temora2 <- phen_temora
phen_oithona2 <- phen_oithona

names(phen_calanus2) <- names(phen_centropages2) <- names(phen_temora2) <- names(phen_oithona2) <- c("Begin_phase", "Meadle_phase", "End_phase", "Peak", "Sp" )



phen_sp <- rbind(phen_calanus2,phen_centropages2,phen_temora2, phen_oithona2)

phen_sp$Duration <- phen_sp$End_phase - phen_sp$Begin_phase

phen_sp$Phen_Index <- apply(phen_sp[, 1:4], MARGIN = 1, FUN = mean) 



ggplot(phen_sp, aes(x = Sp, y = Duration)) + geom_boxplot()

ggplot(phen_sp, aes(x = Sp, y = Peak)) + geom_boxplot()

ggplot(phen_sp, aes(x = Sp, y = Begin_phase)) + geom_boxplot()

ggplot(phen_sp, aes(x = Sp, y = Meadle_phase)) + geom_boxplot()

ggplot(phen_sp, aes(x = Sp, y = End_phase)) + geom_boxplot()

ggplot(phen_sp, aes(x = Sp, y = Phen_Index)) + geom_boxplot()






ggplot(phen_sp, aes(x = Year, y = Peak)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Sp, ncol = 2)

ggplot(phen_sp, aes(x = Year, y = Begin_phase)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Sp, ncol = 2)

ggplot(phen_sp, aes(x = Year, y = End_phase)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Sp, ncol = 2)

ggplot(phen_sp, aes(x = Year, y = Meadle_phase)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Sp, ncol = 2)


ggplot(phen_sp, aes(x = Year, y = Phen_Index)) + geom_point() + geom_smooth(method = "gam") + facet_wrap(~Sp, ncol = 2)





phen_sp$Year <- rep(1961:2018, 4)

phen_sp2 <- phen_sp[phen_sp$Year != 2014, ] 


cca_phen_sp <- cca(phen_sp2[,-c(5, 7)] ~ Sp + Year, data = phen_sp2)

plot(cca_phen_sp, scaling = "sites", display = "species")

scores(cca_phen_sp)


anova(cca_phen_sp)

anova(cca_phen_sp, by = "axis")

anova(cca_phen_sp, by = "margin")
