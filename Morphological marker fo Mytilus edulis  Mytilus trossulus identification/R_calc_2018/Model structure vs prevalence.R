library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(car)
library(doBy)
library(pROC)
library(betareg)
library(lmtest)
library(broom)
library(mgcv)



myt <- read.table("data_salinity3.csv", header = T, sep = ";")

myt <- myt[myt$dataset != "overseas", ]

myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)

# Оставляем только мидий, у которых есть оценка морфотипа
myt2 <- myt[!is.na(myt$ind), ]





# Вводим обозначения для морфотипов
myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)



# Бинарное обозначение видов

myt2$Sp2 <- ifelse(myt2$Sp == "M.trossulus", 1, 0)


#Correct identification
myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )



# Частота M.trossulus в популяции, вычисленная как срденее значение structure

freq_MT <- summaryBy( str ~ pop, data = myt2)
names(freq_MT) <- c("pop", "freq_MT")

myt2 <- merge(myt2, freq_MT)

# # Подразделяем дмнные на три сабсета
# 
# myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "fresh"] <- "Barents_fresh" 
# myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "normal"] <- "Barents_normal" 
# myt2$Subset[myt2$sea == "white" & myt2$sal_place == "normal"] <- "White" 
# myt2$Subset[myt2$sea == "white" & myt2$sal_place == "fresh"] <- "White" 



#Оставляем только данные, на основе, которых строится модель
# myt3 <- myt2[myt2$dataset == "testing", ]
# myt2 <- myt2[myt2$dataset == "training", ]


# myt2 %>% group_by(Subset) %>% summarise(n_pop = length(unique(pop)))


# myt3 %>% group_by(Subset) %>% summarise(n_pop = length(unique(pop)))


names(myt2)

ggplot(myt2, aes(x = freq_MT, y = str)) + geom_density2d() + facet_wrap(~facet) 


mod_gam <- gam(ind ~ s(str, freq_MT, by = facet) + facet, data = myt2, family = "binomial")


summary(mod_gam)

mod_logist <- glm(ind ~ str*freq_MT*facet, data = myt2, family = "binomial")

summary(mod_logist)


new_data <-  myt2 %>% group_by(facet) %>% do(expand.grid(str = seq(min(.$str), max(.$str), length.out = 100), freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100))) 

  
  
  # expand.grid(str = seq(0, 1, length.out = 10), freq_MT = seq(0, 1, length.out = 10), facet = c("BF", "BN", "W" ))

new_data$Predict_gam <- predict(mod_gam, newdata = new_data,type = "response" )

new_data$Predict_logist <- predict(mod_logist, newdata = new_data,type = "response" )



ggplot(new_data, aes(x = freq_MT, y = str)) + geom_raster(aes(fill = Predict_gam)) + facet_wrap(~facet) + scale_fill_gradient(low = "yellow", high = "red") + geom_density2d(data = myt2, n = 20) + geom_point(data = myt2, aes(fill = ind), position = position_jitter(width = 0.01), shape = 21) + theme_bw()


ggplot(new_data, aes(x = freq_MT, y = str)) + geom_raster(aes(fill = Predict_logist)) + facet_wrap(~facet) + scale_fill_gradient(low = "yellow", high = "red") + geom_density2d(data = myt2) + geom_point(data = myt2, aes(fill = ind), position = position_jitter(width = 0.01), shape = 21) + theme_bw()







###################################

# Соотношение доли Т-морфотипа и доли MT в каждой из популяций
prop_T_MT <-  myt2 %>% group_by(Subset, pop) %>% do(data.frame(Prop_MT = mean(.$Sp2), N_MT = sum(.$Sp2 == 1), N_ME = sum(.$Sp2 == 0),  Prop_T = mean(.$ind), N_T = sum(.$ind ==  1), N_E = sum(.$ind == 0))) 


prop_T_MT_testing <-  myt3 %>% group_by(Subset, pop) %>% do(data.frame(Prop_MT = mean(.$Sp2), N_MT = sum(.$Sp2 == 1), N_ME = sum(.$Sp2 == 0),  Prop_T = mean(.$ind), N_T = sum(.$ind ==  1), N_E = sum(.$ind == 0))) 

names(myt2)

str(myt2)

pop_struct <- myt2 %>% group_by(Subset, pop) %>% summarise(a = sum(Sp2 == 1 & morph == "T_m"), b =  sum(Sp2 == 1 & morph == "E_m"), c = sum(Sp2 == 0 & morph == "T_m"), d = sum(Sp2 == 0 & morph == "E_m") )


pop_struct$Ptros <- with(pop_struct, (a+b)/(a+b+c+d))

pop_struct$P_T_tros <- with(pop_struct,  a/(a+b))
pop_struct$P_E_edu <- with(pop_struct,  d/(c+d))

pop_struct$P_tros_T <- with(pop_struct,   a/(a+c))
pop_struct$P_edu_E <- with(pop_struct,   d/(b+d))



pop_struct_long <- melt(pop_struct, id.vars = c("pop", "Subset", "Ptros", "a", "b", "c", "d"), variable.name = "Characteristic")



pop_struct_long$logit_val <- with(pop_struct_long, log(value/(1-value)))



ggplot(pop_struct_long, aes(x = Ptros, y = logit_val, color = Characteristic)) + geom_point() + facet_grid(Characteristic~Subset) + geom_smooth(method = "lm") 

ggplot(pop_struct, aes(x = P_T_tros, y = P_E_edu)) + geom_point() + facet_wrap(~Subset) + geom_smooth(method = "lm") 


Mod_tros <- lm(P_T_tros ~ Ptros*P_tros_T*Subset, data = pop_struct)
anova(Mod_tros)
summary(Mod_tros)

Mod_edu <- lm(P_E_edu ~ Ptros*P_edu_E*Subset, data = pop_struct)
anova(Mod_edu)
summary(Mod_edu)


