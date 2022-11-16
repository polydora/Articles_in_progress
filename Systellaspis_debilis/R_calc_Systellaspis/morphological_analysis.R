library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)


sys <- read_excel("Data/Sd_morph2.xlsx", na = "NA")

hapl <- read_excel("Data/haplogroups.xlsx", na = "NA")

sys <- merge(sys, hapl)


tr_excluded <- c("3_pereopod_ischium_ant_spines", "3_pereopod_caprus_ant_spines", "3_pereopod_caprus_post_spines", "4_pereopod_caprus_ant_spines", "4_pereopod_caprus_post_spines", "5_pereopod_caprus_ant_spines", "karina", "t_lat_spines")

sys <- sys %>% select(!tr_excluded)



ggplot(sys, aes(x = length)) + geom_histogram(bins = 10) + facet_wrap(~sex, nrow = 2)

ggplot(sys, aes(x = length, y = height)) + geom_point() 



sys %>% filter(sex == "m") %>% pull(length) %>% min()


nrow(sys)


# Формируем датафрейм с предикторами
sys2_predictors <- sys%>% filter(length > 6.5) %>% filter(complete.cases(.)) %>%  select(c(sample, height, length, sample, coordinates_Lat, coordinates_Long, location, sex, haplotype)) 


# Убираем мелких особей и формируем датафрейм со значениями признаков
sys2 <- sys %>% filter(length > 6.5) %>% filter(complete.cases(.))  %>% select(-c( height, length, coordinates_Lat, coordinates_Long, location, sex, haplotype))

row.names(sys2) <- sys2$sample

sys2 <- sys2 %>% select(-sample)
  







# Проводим анализ, где предикторами выступают размеры особей. Это нужно для того, чтобы получить ординацию в ося, основанных на остатках от модели, то есть признаков без учета размеров особей.

mod_cca <- cca(sys2 ~ length  + height, data = sys2_predictors)



plot(mod_cca, display = c("species", "cn"))

# Первая каноническая ось (CC1) это размер.

summary(mod_cca)


# Извлекаем координаты в неогрнаиченных моделью осях

cca_unconstr <- scores(mod_cca, choices = c("CA1", "CA2"))$sites %>% as.data.frame()



# Комбинируем со значениями предикторов
ca_unconstr_pred <- cbind(sys2_predictors, cca_unconstr )





ggplot(ca_unconstr_pred, aes(x = CA1, y = CA2, color = location) ) + geom_point() 

#Все разваливается на две группы?

# Строим дендрограмму для того, чтобы было проще выделять группы
dendr <- 
ca_unconstr_pred %>% select(CA1, CA2) %>% dist(.) %>% 
  hclust(., method = "ward.D" )


plot(dendr)

order <- dendr$order #Это порядок особей в дендрограмме 

# Определяем границу первого кластера


clusters <- data.frame(Specimens = row.names(ca_unconstr_pred)[order])

clusters$Group <- "Group2"
clusters$Group[1:which(row.names(ca_unconstr_pred)[order] == "Sd63")] <- "Group1"


# 4_pereopod_ischium_ant_spines
# 5_pereopod_ischium_ant_spines

row.names(sys2) <- row.names(ca_unconstr_pred)



sys3 <- sys2 %>% mutate(Group = ifelse(row.names(.) %in% clusters$Specimens [clusters$Group == "Group1"]  , "Group1", "Group2"))


# Средние значения признаков по группам
sys3  %>%  group_by(Group) %>% summarise_all(.funs = mean) %>% t() %>% as.data.frame() %>% setNames(c("Group1", "Group2"))





# Расставляем номера групп в датафрейме со значениями предикторов
row.names(sys2_predictors) <-  row.names(ca_unconstr_pred)

sys2_predictors <- sys2_predictors %>%  mutate(Group = ifelse(row.names(.) %in% clusters$Specimens [clusters$Group == "Group1"]  , "Group1", "Group2")) 



ggplot(sys2_predictors, aes(x = Group, y = coordinates_Lat)) + geom_boxplot()


ggplot(sys2_predictors, aes(x = Group, y = coordinates_Long )) + geom_boxplot()


ggplot(sys2_predictors, aes(x = Group, y = length )) + geom_boxplot()


ggplot(sys2_predictors, aes(x = coordinates_Long, y = coordinates_Lat, color = Group )) + geom_point(size = 3, position = position_jitter(width = 5, height = 5))



sys2_predictors %>% group_by(Group, sex) %>% summarise(N = n())

sys2_predictors %>% group_by(Group, haplotype) %>% summarise(N = n())


sys2_predictors %>% group_by(haplotype, Group) %>% summarise(N = n())



ggplot(ca_unconstr_pred, aes(x = coordinates_Lat , y = CA1)) + geom_point() 

ggplot(ca_unconstr_pred, aes(x = coordinates_Long , y = CA1)) + geom_point() 


ggplot(ca_unconstr_pred, aes(x = coordinates_Lat , y = CA2)) + geom_point() 

ggplot(ca_unconstr_pred, aes(x = coordinates_Long , y = CA2)) + geom_point() 



ggplot(ca_unconstr_pred, aes(x = haplotype, y = CA2)) + geom_boxplot() 

ggplot(ca_unconstr_pred, aes(x = haplotype, y = CA1)) + geom_boxplot() 

ggplot(ca_unconstr_pred, aes(x = CA2)) + geom_histogram(bins = 10) + facet_wrap(~haplotype, nrow = 2) 





qplot(x = ca_unconstr_pred$length, y = ca_unconstr_pred$CA1) + geom_smooth(method = "lm")

qplot(x = ca_unconstr_pred$length, y = ca_unconstr_pred$CA2) + geom_smooth(method = "lm")





ggplot(ca_unconstr_pred, aes(x = location, y = CA1)) + geom_boxplot()

ggplot(ca_unconstr_pred, aes(x = location, y = CA2)) + geom_boxplot()



