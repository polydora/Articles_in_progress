library(readxl)
library(dplyr)
library(vegan)
library(ggplot2)


sys <- read_excel("Data/Sd_morph2.xlsx", na = "NA")

hapl <- read_excel("Data/haplogroups.xlsx", na = "NA")

sys <- merge(sys, hapl)


tr_excluded <- c("3_pereopod_ischium_ant_spines", "3_pereopod_caprus_ant_spines", "3_pereopod_caprus_post_spines", "4_pereopod_caprus_ant_spines", "4_pereopod_caprus_post_spines", "5_pereopod_caprus_ant_spines", "karina")

sys <- sys %>% select(!tr_excluded)



ggplot(sys, aes(x = length)) + geom_histogram(bins = 10) + facet_wrap(~sex, nrow = 2)

ggplot(sys, aes(x = length, y = height)) + geom_point() 



sys %>% filter(sex == "m") %>% pull(length) %>% min()


nrow(sys)

sys2_predictors <- sys%>% filter(length > 6.5) %>% filter(complete.cases(.)) %>%  select(c(sample, height, length, sample, coordinates_Lat, coordinates_Long, location, sex, haplotype)) 

sys2 <- sys %>% filter(length > 6.5) %>% filter(complete.cases(.))  %>% select(-c(sample, height, length, coordinates_Lat, coordinates_Long, location, sex, haplotype))
  






nrow(sys2)

mod_cca <- cca(sys2 ~ length  + height, data = sys2_predictors)

plot(mod_cca, display = c("species", "cn"))


summary(mod_cca)



cca_unconstr <- scores(mod_cca, choices = c("CA1", "CA2"))$sites %>% as.data.frame()

ca_unconstr_pred <- cbind(sys2_predictors, cca_unconstr )





ggplot(ca_unconstr_pred, aes(x = CA1, y = CA2, color = location) ) + geom_point() + geom_vline(xintercept = 0) 

#Все разваливается на две группы?

dendr <- 
ca_unconstr_pred %>% select(CA1, CA2) %>% dist(.) %>% 
  hclust(., method = "ward.D" )


plot(dendr)

str(dendr)

order <- dendr$order

# 4_pereopod_ischium_ant_spines
# 5_pereopod_ischium_ant_spines

names(sys3)

sys3 <- sys2 %>% mutate(Group = ifelse(ca_unconstr_pred$CA2 >= 0 & ca_unconstr_pred$CA1 > -1.3 , "Group1", "Group2"))

sys3 %>% select(16, 20, 24) %>%  group_by(Group) %>% summarise_all(.funs = mean)


sys2_predictors %>%  mutate(Group = ifelse(ca_unconstr_pred$CA2 >= 0 & ca_unconstr_pred$CA1 > -1.3, "Group1", "Group2")) %>% group_by(Group) %>% summarise(Mean_L = mean(length), mean(sex == "f"))









ggplot(ca_unconstr_pred, aes(x = location, y = CA1)) + geom_boxplot() 




ggplot(ca_unconstr_pred, aes(x = CA2)) + geom_histogram(bins = 10) 



ggplot(ca_unconstr_pred, aes(x = coordinates_Long, y = CA2, color = haplotype)) + geom_point() 




ggplot(ca_unconstr_pred, aes(x = haplotype, y = CA2)) + geom_boxplot() 

ggplot(ca_unconstr_pred, aes(x = haplotype, y = CA1)) + geom_boxplot() 

ggplot(ca_unconstr_pred, aes(x = CA2)) + geom_histogram(bins = 10) + facet_wrap(~haplotype, nrow = 2) 





qplot(x = ca_unconstr_pred$length, y = ca_unconstr_pred$CA1) + geom_smooth(method = "lm")

qplot(x = ca_unconstr_pred$length, y = ca_unconstr_pred$CA2) + geom_smooth(method = "lm")





ggplot(ca_unconstr_pred, aes(x = location, y = CA1)) + geom_boxplot()

ggplot(ca_unconstr_pred, aes(x = location, y = CA2)) + geom_boxplot()




lm_sys <- lm(CA1 ~ location + haplotype + sex, data = ca_unconstr_pred)
summary(lm_sys)

lm_sys <- lm(CA2 ~ location + haplotype + sex, data = ca_unconstr_pred)
summary(lm_sys)



lm_sys <- lm(CA1 ~  + sex, data = ca_unconstr_pred)
summary(lm_sys)

lm_sys <- lm(CA2 ~ location + sex, data = ca_unconstr_pred)
summary(lm_sys)








cca_CA1 <- scores(mod_cca, choices = c("CA1"))$species %>% as.data.frame()


data.frame(location = sys2_predictors$location, tr = sys2$'5_pereopod_ischium_ant_spines') %>% group_by(location) %>% summarise(mean = mean(tr))


data.frame(location = sys2_predictors$location, tr = sys2$'4_pereopod_ischium_ant_spines') %>% group_by(location) %>% summarise(mean = mean(tr))


data.frame(location = sys2_predictors$location, tr = sys2$'v_teeth') %>% group_by(location) %>% summarise(mean = mean(tr))





cca_CA2 <- scores(mod_cca, choices = c("CA2"))$species %>% as.data.frame()







data.frame(location = sys2_predictors$location, tr = sys2$'5_pereopod_ischium_ant_spines') %>% group_by(location) %>% summarise(mean = mean(tr))


data.frame(location = sys2_predictors$location, tr = sys2$'4_pereopod_ischium_ant_spines') %>% group_by(location) %>% summarise(mean = mean(tr))


data.frame(location = sys2_predictors$location, tr = sys2$'v_teeth') %>% group_by(location) %>% summarise(mean = mean(tr))





