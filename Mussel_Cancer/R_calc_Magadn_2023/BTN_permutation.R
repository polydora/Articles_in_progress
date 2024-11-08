# В этом коде пытаемся посмотреть соответсвие микро и макро распрееения предсказаниям нулевой модели


library(readxl)
library(dplyr)
library(magrittr)
library(reshape2)
library(ggplot2)
library(cowplot)


# Данные по разновидности BTN
myt <- read_excel("Data/summary table_Magadan_itog.xlsx", sheet = "data")

myt %<>%
  mutate(BTN2 = BTN2.1 + BTN2.2 + BTN2.SAM)

myt_23 <- 
  myt %>% 
  filter(Year == 2023)

prob_BTN1 <- sum(myt_23$BTN1)/sum(myt_23$N)
prob_BTN2 <- sum(myt_23$BTN2)/sum(myt_23$N)


myt_23 %<>%
  select(-c(Site, Lat,   Lon,  Year,   Date, BTN2.1, BTN2.2, BTN2.SAM,  DN_FC  ))



redistribution <- function(x, probability){
  rbinom(n = nrow(x), size = x$N,  prob = probability)
}
  

# Характеризуем варьирование частот BTN1 и BTN2 в пределах сайта и между сайтами в одном анализе


df_samples <- 
  myt_23 %>% 
  group_by(Site_code) %>% 
  summarise(N_samples = length(N), SD_BTN1 = sd(BTN1/N), SD_BTN2 = sd(BTN2/N)) %>% 
  mutate(SS_BTN1 = SD_BTN1^2*(N_samples - 1), SS_BTN2 = SD_BTN2^2*(N_samples - 1))

SS_BTN1_samples <- sum(df_samples$SS_BTN1)#/ nrow(df_samples)
SS_BTN2_samples <- sum(df_samples$SS_BTN2)#/ nrow(df_samples)



myt_sites <- 
  myt_23 %>% 
  group_by(Site_code) %>% 
  summarise(N = sum(N), BTN1 = sum(BTN1), BTN2 = sum(BTN2))


SS_BTN1_sites <- ((sd(myt_sites$BTN1/myt_sites$N))^2*(nrow(myt_sites)-1))#/nrow(myt_sites)
SS_BTN2_sites <- ((sd(myt_sites$BTN2/myt_sites$N))^2*(nrow(myt_sites)-1))#/nrow(myt_sites)





df_perm <- 
  myt_23 %>% 
  select(Site_code, Sample, N, BTN1, BTN2)

N_perm <- 10000

perm_SS <- data.frame(SS_BTN1_perm_samples = rep(NA, N_perm), SS_BTN2_perm_samples = NA)
perm_SS_sites <- data.frame(SS_BTN1_perm_sites = rep(NA, N_perm), SS_BTN2_perm_sites = NA)



for(i in 1:N_perm){
  df_perm$BTN1 = redistribution(x = df_perm, probability = prob_BTN1)
  df_perm$BTN2 = redistribution(x = df_perm, probability = prob_BTN2)
  
  dd <-
    df_perm %>% 
    group_by(Site_code) %>% 
    summarise(N_samples = length(N), SD_BTN1 = sd(BTN1/N), SD_BTN2 = sd(BTN2/N)) %>% 
    mutate(SS_BTN1 = SD_BTN1^2*(N_samples - 1), SS_BTN2 = SD_BTN2^2*(N_samples - 1))
  
  perm_SS$SS_BTN1_perm_samples[i] <- sum(dd$SS_BTN1)#/ nrow(dd)
  perm_SS$SS_BTN2_perm_samples[i] <- sum(dd$SS_BTN2)#/ nrow(dd)

  
  dd2 <-
    df_perm %>% 
    group_by(Site_code) %>% 
    summarise(N = sum(N), BTN1 = sum(BTN1), BTN2 = sum(BTN2)) 
  
  perm_SS_sites$SS_BTN1_perm_sites[i] <- (sd(dd2$BTN1/dd2$N)^2*(nrow(dd2)-1))#/nrow(dd2)
  
  perm_SS_sites$SS_BTN2_perm_sites[i] <- (sd(dd2$BTN2/dd2$N)^2*(nrow(dd2)-1))#/nrow(dd2)
    
  print(i)
}  


perm_SS_samples_sites <- cbind(perm_SS, perm_SS_sites)



# install.packages("ggdensity")


library(ggdensity)



p_value_BTN1 <- mean( (perm_SS_samples_sites$SS_BTN1_perm_sites > SS_BTN1_sites) | (perm_SS_samples_sites$SS_BTN1_perm_samples > SS_BTN1_samples))


p_value_BTN2 <- mean( (perm_SS_samples_sites$SS_BTN2_perm_sites > SS_BTN2_sites) | (perm_SS_samples_sites$SS_BTN2_perm_samples > SS_BTN2_samples))



# install.packages("ggdensity")


library(ggdensity)



# Pl_2d_BTN1 <- 
# ggplot(perm_SS_samples_sites, aes(x = SS_BTN1_perm_sites, y = SS_BTN1_perm_samples)) +
#   geom_point() +
#   annotate(x = SS_BTN1_sites, y = SS_BTN1_samples, geom = "point", shape = 21,  fill = "yellow", size = 4) +
#   annotate(x = SS_BTN1_sites, xend = SS_BTN1_sites, y = 0, yend = SS_BTN1_samples, geom = "segment") +
#   annotate(x = SS_BTN1_sites, xend = 0, y = SS_BTN1_samples, yend = SS_BTN1_samples, geom = "segment") +
#   ggtitle(paste("BTN1, p = ", p_value_BTN1)) 
# +
  # stat_ellipse(type = "t", color = "yellow")

Pl_2d_BTN1 <- 
ggplot(perm_SS_samples_sites, aes(x = SS_BTN1_perm_sites, y = SS_BTN1_perm_samples)) +
  annotate(x = SS_BTN1_sites, y = SS_BTN1_samples, geom = "point", shape = 21,  fill = "yellow", size = 4) +
  annotate(x = SS_BTN1_sites, xend = SS_BTN1_sites, y = 0, yend = SS_BTN1_samples, geom = "segment") +
  annotate(x = SS_BTN1_sites, xend = 0, y = SS_BTN1_samples, yend = SS_BTN1_samples, geom = "segment") +
  ggtitle("BTN1") +
  geom_hdr()



# Pl_2d_BTN2 <- 
# ggplot(perm_SS_samples_sites, aes(x = SS_BTN2_perm_sites, y = SS_BTN2_perm_samples)) +
#   geom_point() +
#   annotate(x = SS_BTN2_sites, y = SS_BTN2_samples, geom = "point", shape = 21,  fill = "yellow", size = 4)+
#   annotate(x = SS_BTN2_sites, xend = SS_BTN2_sites, y = 0, yend = SS_BTN2_samples, geom = "segment") +
#   annotate(x = SS_BTN2_sites, xend = 0, y = SS_BTN2_samples, yend = SS_BTN2_samples, geom = "segment") 
# +
#   ggtitle(paste("BTN2, p = ", p_value_BTN2)) +  stat_ellipse(type = "t", level = 0.95, color = "yellow")

Pl_2d_BTN2 <-
ggplot(perm_SS_samples_sites, aes(x = SS_BTN2_perm_sites, y = SS_BTN2_perm_samples)) +
  annotate(x = SS_BTN2_sites, y = SS_BTN2_samples, geom = "point", shape = 21,  fill = "yellow", size = 4)+
  annotate(x = SS_BTN2_sites, xend = SS_BTN2_sites, y = 0, yend = SS_BTN2_samples, geom = "segment") +
  annotate(x = SS_BTN2_sites, xend = 0, y = SS_BTN2_samples, yend = SS_BTN2_samples, geom = "segment") +
  ggtitle("BTN2") +
  geom_hdr()



plot_grid(Pl_2d_BTN1, Pl_2d_BTN2)



###  Ковариация BTN1 vs BTN2  одновременно в пределах сайта и между сайтами


myt_cov_samples <- 
  myt_23 %>% 
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
  group_by(Site_code) %>% 
  mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)

Cov_BTN1_BTN2_sample <- sum(myt_cov_samples$Cov_BTN1_BTN2)


myt_cov_sites <- 
  myt_23 %>% 
  group_by(Site_code) %>% 
  summarize(N = sum(N), BTN1 = sum(BTN1), BTN2 = sum(BTN2)) %>% 
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
  mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)

Cov_BTN1_BTN2_site <- sum(myt_cov_sites$Cov_BTN1_BTN2)




df_perm <- 
  myt_23 %>%
  group_by(Site_code) %>% 
  summarize(N = sum(N)) %>% 
  select(Site_code, N)




df_perm <- 
  myt_23 %>% 
  select(Site_code, Sample, N)

N_perm <- 1000

perm_cov <- data.frame(Cov_perm = rep(NA, N_perm))

for(i in 1:N_perm){
  df_perm$BTN1 = redistribution(x = df_perm, probability = prob_BTN1)
  df_perm$BTN2 = redistribution(x = df_perm, probability = prob_BTN2)
  
  dd <-
    df_perm %>% 
    mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
    group_by(Site_code) %>% 
    mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)
  
  perm_cov$Cov_perm[i] <- sum(dd$Cov_BTN1_BTN2)
  
  dd2 <-
    df_perm %>% 
    group_by(Site_code) %>%
    mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
    
    mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)
  
  perm_cov$Cov_perm[i] <- sum(dd$Cov_BTN1_BTN2)
  
  
  
  print(i)
}  


p_value_Cov_BTN1_BTN2_sample <- mean(perm_cov$Cov_perm >= Cov_BTN1_BTN2_sample)


Pl_Cov_BTN1_BTN2_samples <- 
  ggplot(perm_cov, aes(x = Cov_perm)) +
  geom_histogram() +
  annotate(x = Cov_BTN1_BTN2_sample, y = 0,geom = "point", size = 3, color = "blue") +
  ggtitle(paste("Covariation BTN1 BTN2 для проб в пределах сайта, p = ",p_value_Cov_BTN1_BTN2_sample)) 

Pl_Cov_BTN1_BTN2_samples +
  geom_vline(xintercept = prob_BTN1 * prob_BTN2 * nrow(myt_23) )







##############################################################

# Характеризуем варьирование частот BTN1 и BTN2 в пределах сайта


# df_samples <- 
# myt_23 %>% 
#   group_by(Site_code) %>% 
#   summarise(N_samples = length(N), SD_BTN1 = sd(BTN1/N), SD_BTN2 = sd(BTN2/N)) %>% 
#   mutate(SS_BTN1 = SD_BTN1^2/(N_samples - 1), SS_BTN2 = SD_BTN2^2/(N_samples - 1))
# 
# SS_BTN1_samples <- sum(df_samples$SS_BTN1)/ nrow(df_samples)
# SS_BTN2_samples <- sum(df_samples$SS_BTN2)/ nrow(df_samples)
# 
#   
#   
# 
# df_perm <- 
#   myt_23 %>% 
#   select(Site_code, Sample, N)
# 
# 
# myt_sites <- 
#   myt_23 %>% 
#   group_by(Site_code) %>% 
#   summarise(N = sum(N), BTN1 = sum(BTN1), BTN2 = sum(BTN2))
# 
# 
# 
# 
# 
# SS_BTN1_sites <- ((sd(myt_sites$BTN1/myt_sites$N))^2*(nrow(myt_sites)-1))/nrow(myt_sites)
# SS_BTN2_sites <- ((sd(myt_sites$BTN2/myt_sites$N))^2*(nrow(myt_sites)-1))/nrow(myt_sites)
# 
# 
# 
# 
# df_perm_site <- 
#   myt_sites %>% 
#   select(Site_code, N)
# 
# 
# 
# 
# 
# N_perm <- 1000
# 
# perm_SS <- data.frame(SS_BTN1_perm = rep(NA, N_perm), SS_BTN2_perm = NA)
# 
# for(i in 1:N_perm){
#   df_perm$BTN1 = redistribution(x = df_perm, probability = prob_BTN1)
#   df_perm$BTN2 = redistribution(x = df_perm, probability = prob_BTN2)
#   
#   dd <-
#   df_perm %>% 
#     group_by(Site_code) %>% 
#     summarise(N_samples = length(N), SD_BTN1 = sd(BTN1/N), SD_BTN2 = sd(BTN2/N)) %>% 
#     mutate(SS_BTN1 = SD_BTN1^2/(N_samples - 1), SS_BTN2 = SD_BTN2^2/(N_samples - 1))
#   
#   perm_SS$SS_BTN1_perm[i] <- sum(dd$SS_BTN1)/ nrow(dd)
#   perm_SS$SS_BTN2_perm[i] <- sum(dd$SS_BTN2)/ nrow(dd)
#   
#   print(i)
# }  
# 
# 
# p_value_BTN1 <- mean(perm_SS$SS_BTN1_perm >= SS_BTN1_samples)
# 
# p_value_BTN2 <- mean(perm_SS$SS_BTN2_perm >= SS_BTN2_samples)
# 
# 
# 
# Pl_BTN1_samples <- 
# ggplot(perm_SS, aes(x = SS_BTN1_perm)) +
#   geom_histogram(bins = 30) +
#   annotate(x = SS_BTN1_samples, y = 0,geom = "point", size = 3, color = "blue") +
#   ggtitle(paste("BTN1 для проб в пределах сайта, p = ",p_value_BTN1)) +
#   xlim(0, max(c(perm_SS$SS_BTN1, perm_SS$SS_BTN2)))
# 
# 
# Pl_BTN2_samples <-
# ggplot(perm_SS, aes(x = SS_BTN2_perm)) +
#   geom_histogram(bins = 30) +
#   annotate(x = SS_BTN2_samples, y = 0,geom = "point", size = 3, color = "blue") +
#   ggtitle(paste("BTN2 для проб в пределах сайта, p = ",p_value_BTN2))+
#   xlim(0, max(c(perm_SS$SS_BTN1, perm_SS$SS_BTN2)))
# 
# 
# 
# plot_grid(Pl_BTN1_samples, Pl_BTN2_samples, nrow = 2)
# 

#####


  
# Характеризуем варьирование частот BTN1 и BTN2 между сайтами


# myt_sites <- 
#   myt_23 %>% 
#   group_by(Site_code) %>% 
#   summarise(N = sum(N), BTN1 = sum(BTN1), BTN2 = sum(BTN2))
# 
# 
# 
# 
# 
# SS_BTN1_sites <- ((sd(myt_sites$BTN1/myt_sites$N))^2*(nrow(myt_sites)-1))/nrow(myt_sites)
# SS_BTN2_sites <- ((sd(myt_sites$BTN2/myt_sites$N))^2*(nrow(myt_sites)-1))/nrow(myt_sites)
# 
# 
# 
# 
# df_perm_site <- 
#   myt_sites %>% 
#   select(Site_code, N)
# 
# N_perm <- 1000
# 
# perm_SS_sites <- data.frame(SS_BTN1_perm = rep(NA, N_perm), SS_BTN2_perm = NA)
# 
# for(i in 1:N_perm){
#   df_perm_site$BTN1 = redistribution(x = df_perm_site, probability = prob_BTN1)
#   df_perm_site$BTN2 = redistribution(x = df_perm_site, probability = prob_BTN2)
#   
#   
#   perm_SS_sites$SS_BTN1_perm[i] <- ((sd(df_perm_site$BTN1/myt_sites$N))^2*(nrow(df_perm_site)-1))/nrow(df_perm_site)
#   
#   perm_SS_sites$SS_BTN2_perm[i] <- ((sd(df_perm_site$BTN2/myt_sites$N))^2*(nrow(df_perm_site)-1))/nrow(df_perm_site)
#   
#   print(i)
# }  
# 
# 
# p_value_BTN1_sites <- mean(perm_SS_sites$SS_BTN1_perm >= SS_BTN1_sites)
# 
# p_value_BTN2_sites <- mean(perm_SS_sites$SS_BTN2_perm >= SS_BTN2_sites)
# 
# 
# Pl_BTN1_sites <- 
#   ggplot(perm_SS_sites, aes(x = SS_BTN1_perm)) +
#   geom_histogram(bins = 30) +
#   annotate(x = SS_BTN1_sites, y = 0,geom = "point", size = 3, color = "blue") +
#   ggtitle(paste("BTN1 между сайтами, p = ",p_value_BTN1_sites)) +
#   xlim(0, max(c(perm_SS_sites$SS_BTN1, perm_SS_sites$SS_BTN2)))
# 
# 
# Pl_BTN2_sites <- 
#   ggplot(perm_SS_sites, aes(x = SS_BTN2_perm)) +
#   geom_histogram(bins = 30) +
#   annotate(x = SS_BTN2_sites, y = 0,geom = "point", size = 3, color = "blue") +
#   ggtitle(paste("BTN2 между сайтами, p = ",p_value_BTN2_sites)) +
#   xlim(0, max(c(perm_SS_sites$SS_BTN1, perm_SS_sites$SS_BTN2)))
# 
# 
# 
# plot_grid(Pl_BTN1_sites, Pl_BTN2_sites, nrow = 2)
# 

#####

# Ковариация BTN1 и BTN2 на уровне отдельных выборок 


myt_cov_samples <- 
  myt_23 %>% 
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
  group_by(Site_code) %>% 
  mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)

Cov_BTN1_BTN2_sample <- sum(myt_cov_samples$Cov_BTN1_BTN2)




df_perm <- 
  myt_23 %>% 
  select(Site_code, Sample, N)

N_perm <- 1000

perm_cov <- data.frame(Cov_perm = rep(NA, N_perm))

for(i in 1:N_perm){
  df_perm$BTN1 = redistribution(x = df_perm, probability = prob_BTN1)
  df_perm$BTN2 = redistribution(x = df_perm, probability = prob_BTN2)
  
  dd <-
    df_perm %>% 
    mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
    group_by(Site_code) %>% 
    mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)
  
  perm_cov$Cov_perm[i] <- sum(dd$Cov_BTN1_BTN2)
  
  print(i)
}  


p_value_Cov_BTN1_BTN2_sample <- mean(perm_cov$Cov_perm >= Cov_BTN1_BTN2_sample)


Pl_Cov_BTN1_BTN2_samples <- 
  ggplot(perm_cov, aes(x = Cov_perm)) +
  geom_histogram() +
  annotate(x = Cov_BTN1_BTN2_sample, y = 0,geom = "point", size = 3, color = "blue") +
  ggtitle(paste("Covariation BTN1 BTN2 для проб в пределах сайта, p = ",p_value_Cov_BTN1_BTN2_sample)) 

Pl_Cov_BTN1_BTN2_samples +
  geom_vline(xintercept = prob_BTN1 * prob_BTN2 * nrow(myt_23) )

################################

# Ковариация BTN1 и BTN2 на уровне сайтов 


myt_cov_sites <- 
  myt_23 %>% 
  group_by(Site_code) %>% 
  summarize(N = sum(N), BTN1 = sum(BTN1), BTN2 = sum(BTN2)) %>% 
  mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
  mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)

  


Cov_BTN1_BTN2_site <- sum(myt_cov_sites$Cov_BTN1_BTN2)




df_perm <- 
  myt_23 %>%
  group_by(Site_code) %>% 
  summarize(N = sum(N)) %>% 
  select(Site_code, N)

N_perm <- 10000

perm_cov <- data.frame(Cov_perm = rep(NA, N_perm))

for(i in 1:N_perm){
  df_perm$BTN1 = redistribution(x = df_perm, probability = prob_BTN1)
  df_perm$BTN2 = redistribution(x = df_perm, probability = prob_BTN2)
  
  dd <-
    df_perm %>% 
    mutate(Prop_BTN1 = BTN1/N, Prop_BTN2 = BTN2/N) %>% 
    group_by(Site_code) %>% 
    mutate(Cov_BTN1_BTN2 = Prop_BTN1 * Prop_BTN2)
  
  perm_cov$Cov_perm[i] <- sum(dd$Cov_BTN1_BTN2)
  
  print(i)
}  


p_value_Cov_BTN1_BTN2_site <- mean(perm_cov$Cov_perm >= Cov_BTN1_BTN2_site)


Pl_Cov_BTN1_BTN2_sites <- 
  ggplot(perm_cov, aes(x = Cov_perm)) +
  geom_histogram() +
  annotate(x = Cov_BTN1_BTN2_site, y = 0,geom = "point", size = 3, color = "blue") +
  ggtitle(paste("Covariation BTN1 BTN2 между сайтами, p = ",p_value_Cov_BTN1_BTN2_site)) 

Pl_Cov_BTN1_BTN2_sites +
  geom_vline(xintercept = prob_BTN1 * prob_BTN2 * nrow(myt_cov_sites) )

