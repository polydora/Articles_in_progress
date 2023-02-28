library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)


sys0 <- read_excel("Data/Sd_morph2.xlsx", na = "NA")

hapl <- read_excel("Data/haplogroups.xlsx", na = "NA")

sys <- merge(sys0, hapl)




tr_excluded <- c("3_pereopod_ischium_ant_spines", "3_pereopod_caprus_ant_spines", "3_pereopod_caprus_post_spines", "4_pereopod_caprus_ant_spines", "4_pereopod_caprus_post_spines", "5_pereopod_caprus_ant_spines", "karina")

sys <- sys %>% select(!tr_excluded)

sys %>% select(-c(sample, coordinates_Lat, coordinates_Long, location,    sex, haplotype )) %>% summarise_all(.funs = function(x) sd(x, na.rm = T)/mean(x, na.rm = T) ) %>% t() %>% hist()



ggplot(sys, aes(x = length)) + geom_histogram(bins = 10) + facet_wrap(~sex, nrow = 2)

###НАчиная с какого размера (длины карапакса) можем определить пол? ###

ggplot(sys, aes(x = length, y = height)) + geom_point() 



sys %>% filter(sex == "m") %>% pull(length) %>% min()
###6,5мм###

nrow(sys)
### пока наша выборка - 73 особи####


sys2_predictors <- sys%>% filter(length > 6.5) %>% filter(complete.cases(.)) %>%  select(c(sample, height, length, sample, coordinates_Lat, coordinates_Long, location, sex, haplotype)) 

sys2 <- sys %>% filter(length > 6.5) %>% filter(complete.cases(.))  %>% select(-c(sample, height, length, coordinates_Lat, coordinates_Long, location, sex, haplotype))
  


nrow(sys2)
#### теперь - 54 -- это взрослые особи###

sys2 %>% summarise_all(.funs = function(x) sd(x, na.rm = T)/mean(x, na.rm = T) ) %>% t() %>% hist()

# Ничего формально не выкинуть, надо использовать все признаки


mod_cca <- rda(sys2 ~ length  + height, data = sys2_predictors)

plot(mod_cca, display = c("species", "cn"))


summary(mod_cca)

anova(mod_cca, permutations = 9999)

###изменчивость размера особей объясняется каноническими осями, неканонические с размерами телв не связаны####

cca_unconstr <- scores(mod_cca, choices = c("PC1", "PC2"))$sites %>% as.data.frame()

ca_unconstr_pred <- cbind(sys2_predictors, cca_unconstr )


cca_unconstr_tr <- scores(mod_cca, choices = c("PC1", "PC2"))$species %>% as.data.frame()



#добавили локации##

ggplot(ca_unconstr_pred, aes(x = PC1, y = PC2, color = location) ) + geom_point() + geom_vline(xintercept = 0) 

#Все разваливается на две группы, в основном располагаясь по разные стороны от CA1=0. Значит, есть две морфогруппы, для которых различаются значения CA1 и CA2. Соотносятся ли они с регионом или гаплотипом по coi? Нет, хотя...### 



ca_unconstr_pred$location <- factor(ca_unconstr_pred$location, levels = c("North_Atlantic","South_Atlantic", "Indian" ))

###Дальше исследуем PC1 и PC2###
ggplot(ca_unconstr_pred, aes(x = location, y = PC1)) + geom_boxplot() 

# PC1 вяжется с регионами

ggplot(ca_unconstr_pred, aes(x = haplotype, y = PC1)) + geom_boxplot() 


# PC1 вяжется с гаплотипами !


# Визуализация связи признаков с PCA1 и PCA2

library(ggrepel) #Пакет, чтобы не наползали подписи на точки

ggplot(cca_unconstr_tr, aes(x = PC1, y = PC2)) + geom_point() + geom_text_repel(aes(label = rownames(cca_unconstr_tr))) + geom_vline(xintercept = 0) 

# Важно! У большинства признаков происходит увеличение их значений при уменьшении  значений PC1, то есть при движении на юг. 
  

# Можно построить еще карту 


library(tidyverse)
world <- map_data("world")

ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region), fill = "gray"
  ) +
  geom_point(data = ca_unconstr_pred, aes(coordinates_Long, coordinates_Lat, color = PC1), position = position_jitter(width = 5, height = 5)) + 
  scale_color_gradient(low = "yellow", high = "red")
  
  

# Можно показать широтный градиент и вот так

ggplot(ca_unconstr_pred, aes(x = coordinates_Lat, y = PC1)) + geom_point() + geom_smooth(method = "lm") + geom_hline(yintercept = 0) 



# qplot(x = ca_unconstr_pred$length, y = ca_unconstr_pred$PC1) + geom_smooth(method = "lm")
# 
# qplot(x = ca_unconstr_pred$length, y = ca_unconstr_pred$PC2) + geom_smooth(method = "lm")




lm_sys <- lm(PC1 ~ location + haplotype + sex, data = ca_unconstr_pred)
summary(lm_sys)

library(car)
Anova(lm_sys)

# PC1 - связана с локацией. В Сев. атлантике PC1 выше.

lm_sys <- lm(PC2 ~ location + haplotype + sex, data = ca_unconstr_pred)
summary(lm_sys)

## Связи незначимы###


# Самые важные признаки 

PC1_traits <- as.data.frame(scores(mod_cca, display = "species", choices = "PC1")) 

PC1_traits$Trait <- row.names(PC1_traits)
row.names(PC1_traits)<- NULL

PC1_traits[order(PC1_traits$PC1), ]

# То есть в Сев атлантике более выраженными являются  t_lat_spines, 5_pereopod_ischium_post_spines, d_teeth, 5_som_ser_right, v_teeth и менее всего выражнными являются  3_pereopod_merus_post_spines, 4_pereopod_merus_post_spines. Последние два признака самые сильные, так как самые высокие нагрузки по PC1 (по модулю). Можно рассматривать только их.


# Смотрим на сырые данные

# Придется лечить кривые названия переменных
sys4 <- sys

sys4$location <- factor(sys4$location, levels = c("North_Atlantic","South_Atlantic", "Indian" ))

sys4$pereopod_4_merus_post_spines <- sys$"4_pereopod_merus_post_spines"
sys4$pereopod_3_merus_post_spines <- sys$"3_pereopod_merus_post_spines"
sys4$som_4_ser_left <- sys$"4_som_ser_left"


# Смотрим только на признаки с самыми большими нагрузками
sys4 %>% 
  ggplot(., aes(x = location, y = pereopod_4_merus_post_spines)) + geom_boxplot()

sys4 %>% 
  ggplot(., aes(x = location, y = pereopod_3_merus_post_spines)) + geom_boxplot()

# Тут, кажись, есть градиент


sys4 %>% 
  ggplot(., aes(x = location, y = som_4_ser_left)) + geom_boxplot()





# Про признаки с положительными нагрузками, похоже ничего не сказать

sys4 %>% 
  ggplot(., aes(x = location, y = pereopod_5_ischium_post_spines)) + geom_boxplot()


sys4 %>% 
  ggplot(., aes(x = location, y = d_teeth)) + geom_boxplot()


sys4 %>% 
  ggplot(., aes(x = location, y = v_teeth)) + geom_boxplot()

# Построй остальные графики сама.







