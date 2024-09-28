# 
# 
# df <- read_excel("Data/west_sound 1995-2024.xls", sheet = "Таблица видов сокращ.")
# 
# zrs <- 
# merge(sp_df_short, df)
# 
# nrow(zrs)
# 
# 
# library(openxlsx)
# 
# write.xlsx(zrs, "Data/west_sound_1995_2024_cleaned.xlsx")
# 

library(readxl)

zrs <- read_excel("Data/west_sound_1995_2024_cleaned.xlsx")
zrs_stations <- read_excel("Data/west_sound_1995_2024_cleaned.xlsx", sheet = "Stations")

zrs <- 
  zrs %>% 
  filter(!valid_name %in% c("Ascidia",  "Bryozoa", "Hydrozoa", "Pantopoda"))


nrow(zrs)

zrs_phyto <- 
  zrs %>% 
  filter(phylum %in% c("Rhodophyta", "Chlorophyta", "Ochrophyta"))
nrow(zrs_phyto)


zrs_zoo <- 
  zrs %>% 
  filter(!phylum %in% c("Rhodophyta", "Chlorophyta", "Ochrophyta"))



nrow(zrs_zoo)

library(vegan)

str(zrs_zoo)
nrow(zrs_zoo)

zrs_zoo2 <- 
zrs_zoo %>% 
  dplyr::select(-c(1:4)) %>% 
  t()




  plot(specaccum(zrs_zoo2, method = "collector"), col = "blue")

  
  
H <- data.frame(H = diversity(zrs_zoo2), Total_N = rowSums(zrs_zoo2)) 

 
  



zrs_diversity <- cbind(zrs_stations, H) 

library(ggplot2)

Pl_H <- 
ggplot(zrs_diversity, aes(x = Year, y = H)) + 
  geom_point() +
  geom_smooth(method = "gam") +
  geom_hline(yintercept = mean(zrs_diversity$H), linetype = 2) +
  theme_bw() +
  labs(x = "Годы", y = "Видовое разнообразие (H)")

Pl_Total_N <- 
  ggplot(zrs_diversity, aes(x = Year, y = (Total_N) )) + 
  geom_point() +
  geom_smooth(method = "lm") +
  geom_hline(yintercept = mean(zrs_diversity$Total_N), linetype = 2) +
  theme_bw()+
  labs(x = "Годы", y = "Количество особей в пробах")


  
library(cowplot)

plot_grid(Pl_H, Pl_Total_N, nrow = 1, labels = "AUTO")
