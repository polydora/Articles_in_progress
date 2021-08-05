library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(readxl)

tuv <- read.table("Data/TuMyt_all.csv",header =T, sep = ",")

tuv_all <- tuv



tuv_short <- tuv %>%  select(-c(Sample_ID, PT, N_for_PT, Ptros)) 

tuv_ca <- cca(tuv_short[,-c(1:4)])

plot(tuv_ca, display = "species")

scores(tuv_ca)
summary(tuv_ca)

ca_scores <- scores(tuv_ca)$sites

cbind(tuv_short %>% select(Year, Period, Transect, High), ca_scores) %>%
  filter(Transect %in% c("BS", "MidN", "MoS")) %>% 
  ggplot(., aes(x = factor(Period), y = CA1)) + geom_boxplot() + facet_wrap(~Transect) + geom_hline(yintercept = 0)  




cbind(tuv_short %>% select(Year, Period, Transect, High), ca_scores) %>%
    ggplot(., aes(x = CA1, y = CA2)) + geom_point(aes(color = Period), size = 4) + facet_wrap(~Transect)




cbind(tuv_short %>% select(Year, Period, Transect, High), ca_scores) %>%
  ggplot(., aes(x = Transect, y = CA2)) + geom_boxplot() + geom_hline(yintercept = 0) + facet_wrap(~Period)





cbind(tuv_short %>% select(Year, Period, Transect, High), ca_scores) %>%   ggplot(., aes(x = factor(Period), y = CA1)) + geom_boxplot() + geom_hline(yintercept = 0) 



# Пермутационная оценка переходов от одного периода к другому

tuv_all <- read.table("Data/TuMyt_all_itog.csv", header = T, sep = ";")
str(tuv_all)

tuv_all$Period <- factor(tuv_all$Period)
tuv_all$Transect  <- factor(tuv_all$Transect)
tuv_all$Monitoring  <- factor(tuv_all$Monitoring)


# отделяю нужные данные
tuv_all <- tuv_all %>% select(Sample_ID, Period, Distance, Transect, Habitat, Depth, Monitoring, Ptros, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, Year) %>% as.data.frame()


transition <- function(Param = "Ptros", perm = 999) {
  require(dplyr)
  require(reshape2)
  d <- tuv_all %>% group_by(Transect, Depth, Period) %>% 
    summarize(n = mean(get(Param), na.rm = T)) %>% 
    dcast(Transect + Depth ~ Period) %>% 
    select(-c(Transect, Depth))
  
  D_N_1_2 <- sum(d[,2] - d[,1], na.rm = T) / sum(!is.na(d[,2] - d[,1]))
  deltas_1_2 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(d[sample(1:nrow(d)),2] - d[sample(1:nrow(d)),1], na.rm = T) / sum(!is.na(d[sample(1:nrow(d)),2] - d[sample(1:nrow(d)),1]))
    deltas_1_2[i] <- D_N_perm
    i<-i+1
  }
  deltas_1_2[perm] <- D_N_1_2 
  
  D_N_2_3 <- sum(d[,3] - d[,2], na.rm = T) / sum(!is.na(d[,3] - d[,2]))
  deltas_2_3 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(d[sample(1:nrow(d)),3] - d[sample(1:nrow(d)),2], na.rm = T) / sum(!is.na(d[sample(1:nrow(d)),3] - d[sample(1:nrow(d)),2]))
    deltas_2_3[i] <- D_N_perm
    i<-i+1
  }
  deltas_2_3[perm] <- D_N_2_3 
  
  
  
  D_N_3_4 <- sum(d[,4] - d[,3], na.rm = T) / sum(!is.na(d[,4] - d[,3]))
  deltas_3_4 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(d[sample(1:nrow(d)),4] - d[sample(1:nrow(d)),3], na.rm = T) / sum(!is.na(d[sample(1:nrow(d)),4] - d[sample(1:nrow(d)),3]))
    deltas_3_4[i] <- D_N_perm
    i<-i+1
  }
  deltas_3_4[perm] <- D_N_3_4 
  
  DN_perm_1_2 <- data.frame(Compare = "1 vs 2", Delta = deltas_1_2)
  DN_perm_2_3 <- data.frame(Compare = "2 vs 3", Delta = deltas_2_3)
  DN_perm_3_4 <- data.frame(Compare = "3 vs 4", Delta = deltas_3_4)
  
  DN_perm <- rbind(DN_perm_1_2, DN_perm_2_3, DN_perm_3_4)
  DN <- data.frame(Compare = c("1 vs 2","2 vs 3", "3 vs 4"), Delta = c(D_N_1_2, D_N_2_3, D_N_3_4))
  
  
  ggplot(DN_perm, aes(x = Compare, y = Delta)) + geom_boxplot() + geom_hline(yintercept = 0) + geom_point(data = DN, shape = 21,  fill = "yellow", size = 3) + ggtitle(Param)
}


Pl1 <- transition(Param = "Ptros", perm = 9999)

Pl2 <- transition(Param = "N", perm = 9999)

Pl3 <- transition(Param = "W", perm = 9999)

Pl4 <- transition(Param = "OGP", perm = 9999)

Pl5 <- transition(Param = "max_L", perm = 9999)

Pl6 <- transition(Param = "Age2_3", perm = 9999)

Pl7 <- transition(Param = "Age4_6", perm = 9999)

Pl8 <- transition(Param = "Age7_9", perm = 9999)

Pl9 <- transition(Param = "Age10_12", perm = 9999)

library(cowplot)

plot_grid(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6, Pl7, Pl8, Pl9, ncol = 3)





#функция transition2 проводит премутации только в пределах однотипных выборок, то есть тех, которые были взяты и в начальном и в конечном перодах.

transition2 <- function(Param = "Ptros", perm = 999) {
  require(ggplot2)
  require(dplyr)
  require(reshape2)
  d <- tuv_all %>% group_by(Transect, Depth, Period) %>% 
    summarize(n = mean(get(Param), na.rm = T)) %>% 
    dcast(Transect + Depth ~ Period) %>% 
    select(-c(Transect, Depth))
  
  
  dd <- d %>% filter(!is.na(d[,1]) & !is.na(d[,2]))  
  D_N_1_2 <- sum(dd[,2] / dd[,1], na.rm = T) / sum(!is.na(dd[,2] - dd[,1]))
  deltas_1_2 <- rep(NA, perm +1)
  
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(dd[sample(1:nrow(dd)),2] / dd[sample(1:nrow(dd)),1], na.rm = T) / sum(!is.na(dd[sample(1:nrow(dd)),2] - dd[sample(1:nrow(dd)),1]))
    deltas_1_2[i] <- D_N_perm
    i<-i+1
  }
  deltas_1_2[perm] <- D_N_1_2 
  
  
  dd <- d %>% filter(!is.na(d[,2]) & !is.na(d[,3]))
  D_N_2_3 <- sum(dd[,3] / dd[,2], na.rm = T) / sum(!is.na(dd[,3] - dd[,2]))
  deltas_2_3 <- rep(NA, perm +1)
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(dd[sample(1:nrow(dd)),3] / dd[sample(1:nrow(dd)),2], na.rm = T) / sum(!is.na(dd[sample(1:nrow(dd)),3] - dd[sample(1:nrow(dd)),2]))
    deltas_2_3[i] <- D_N_perm
    i<-i+1
  }
  deltas_2_3[perm] <- D_N_2_3 
  
  
  dd <- d %>% filter(!is.na(d[,3]) & !is.na(d[,4]))
  D_N_3_4 <- sum(dd[,4] / dd[,3], na.rm = T) / sum(!is.na(dd[,4] - dd[,3]))
  deltas_3_4 <- rep(NA, perm +1)
  
  i <- 1
  while (i <= perm) {
    D_N_perm <- sum(dd[sample(1:nrow(dd)),4] / dd[sample(1:nrow(dd)),3], na.rm = T) / sum(!is.na(dd[sample(1:nrow(dd)),4] - dd[sample(1:nrow(dd)),3]))
    D_N_perm
    deltas_3_4[i] <- D_N_perm
    i<-i+1
  }
  deltas_3_4[perm] <- D_N_3_4 
  
  DN_perm_1_2 <- data.frame(Compare = "1 vs 2", Delta = deltas_1_2)
  DN_perm_2_3 <- data.frame(Compare = "2 vs 3", Delta = deltas_2_3)
  DN_perm_3_4 <- data.frame(Compare = "3 vs 4", Delta = deltas_3_4)
  
  DN_perm <- rbind(DN_perm_1_2, DN_perm_2_3, DN_perm_3_4)
  DN <- data.frame(Compare = c("1 vs 2","2 vs 3", "3 vs 4"), Delta = c(D_N_1_2, D_N_2_3, D_N_3_4))
  
  
  ggplot(DN_perm, aes(x = Compare, y = Delta)) + geom_boxplot() + geom_hline(yintercept = 1) + geom_point(data = DN, shape = 21,  fill = "yellow", size = 3) + ggtitle(Param)
}


Pl1 <- transition2(Param = "Ptros", perm = 9999)

Pl2 <- transition2(Param = "N", perm = 9999)

Pl3 <- transition2(Param = "W", perm = 9999)

Pl4 <- transition2(Param = "OGP", perm = 9999)

Pl5 <- transition2(Param = "max_L", perm = 9999)

Pl6 <- transition2(Param = "Age2_3", perm = 9999)

Pl7 <- transition2(Param = "Age4_6", perm = 9999)

Pl8 <- transition2(Param = "Age7_9", perm = 9999)

Pl9 <- transition2(Param = "Age10_12", perm = 9999)

library(cowplot)

plot_grid(Pl1, Pl2, Pl3, Pl4, Pl5, Pl6, Pl7, Pl8, Pl9, ncol = 3)







