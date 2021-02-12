library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(readxl)

tuv <- read.table("Data/TuMyt_all.csv",header =T, sep = ",")

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
