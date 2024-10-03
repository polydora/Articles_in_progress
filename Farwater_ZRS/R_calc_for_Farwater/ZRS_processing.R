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
library(dplyr)
library(reshape2)



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


zin <- read_excel("Data/Материалы отчетов ЗИН_2005_2011.xlsx", sheet = "Abundance")
zin_stations <- read_excel("Data/Материалы отчетов ЗИН_2005_2011.xlsx", sheet = "Stations")

zin_stations <- 
  zin_stations %>%
  mutate(long = E_degr + E_min/60, lat = N_degr + N_min/60 )




zin_zrs_zoo <- 
  zin %>% 
  dplyr::select(valid_name,phylum, class, Type,  ZIN_2006_Bank6, ZIN_2007_Bank4, ZIN_2007_Bank6) %>% 
  filter (Type == "N") %>% 
  filter(! phylum %in% c("Rhodophyta", "Chlorophyta", "Ochrophyta", "Tracheophyta")) %>% 
  filter(ZIN_2006_Bank6 + ZIN_2007_Bank4 + ZIN_2007_Bank6 !=0) %>% 
  dplyr::select(-c(phylum, class, Type))
  

zrs_zoo <-   
zrs_zoo %>%  
  dplyr::select(-c(phylum, class, Species))

all_zrs_zoo <- merge(zrs_zoo, zin_zrs_zoo, all = TRUE)


nrow(all_zrs_zoo)

library(vegan)


all_zrs_zoo2 <- 
all_zrs_zoo %>% 
  dplyr::select(-c(1:4))

all_zrs_zoo2[is.na(all_zrs_zoo2)] <- 0




plot(specaccum(t(all_zrs_zoo2), method = "random"), ci.col = "gray", col = "blue", ci.type = "polygon" )

  
  
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


# Оценка обилия видов в ЗРС

zin_zrs <- 
zin %>% 
  dplyr::select(valid_name, Type, ZIN_2006_Bank6, ZIN_2007_Bank4, ZIN_2007_Bank6) %>%
  melt(id.vars = c("valid_name", "Type"))



  zin_zrs %>% 
  filter(Type == "N") %>% 
  group_by(valid_name) %>% 
  dplyr::summarise(Mean = mean(value), SE = sd(value)/length(value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "N") ->
  Mean_N

  zin_zrs %>% 
  filter(Type == "B") %>% 
  group_by(valid_name) %>% 
  dplyr::summarise(Mean = mean(value), SE = sd(value)/length(value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "B") ->
  Mean_B


library(tidyr)
  
  rbind(Mean_N, Mean_B) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = Value, values_from = c("Mean", "SE", "Order")) %>% 
    filter(Order_N <= 10 | Order_B <= 0) %>% 
    dplyr::select(valid_name, Mean_N, SE_N, Mean_B, SE_B ) %>% 
    write.table("clipboard", row.names = F, sep = "\t", dec = ",")


  
# Литоральные сборы ЗИН в районе ЗРС отбраны только станции, в которых много фукоидов. Эти описания используются для описания сообщества, связанного с фукоидами

zin_long <-  
zin %>% 
  select(valid_name,	Type,	ZIN_2005_St1,	ZIN_2005_St2,	ZIN_2005_St3,	ZIN_2005_St4,	ZIN_2006_St1,	ZIN_2006_St2,	ZIN_2006_St3,	ZIN_2006_St4,	ZIN_2007_St1,	ZIN_2007_St2,	ZIN_2007_St3,	ZIN_2007_St4,	ZIN_2009_St3,	ZIN_2009_St4,	ZIN_2010_St1,	ZIN_2010_St2,	ZIN_2010_St3,	ZIN_2010_St4,	ZIN_2011_St1,	ZIN_2011_St2,	ZIN_2011_St3,	ZIN_2011_St4)%>%
  melt(id.vars = c("valid_name", "Type"))

zin_long %>% 
  filter(Type == "N") %>% 
  group_by(valid_name) %>% 
  dplyr::summarise(Mean = mean(value), SE = sd(value)/length(value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "N") ->
  Mean_N

zin_long %>% 
  filter(Type == "B") %>% 
  group_by(valid_name) %>% 
  dplyr::summarise(Mean = mean(value), SE = sd(value)/length(value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "B") ->
  Mean_B


library(tidyr)

rbind(Mean_N, Mean_B) %>%
  as.data.frame() %>% 
  pivot_wider(names_from = Value, values_from = c("Mean", "SE", "Order")) %>% 
  filter(Order_N <= 20 | Order_B <= 20) %>% 
  dplyr::select(valid_name, Mean_N, SE_N, Mean_B, SE_B ) %>% 
  write.table("clipboard", row.names = F, sep = "\t", dec = ",")


