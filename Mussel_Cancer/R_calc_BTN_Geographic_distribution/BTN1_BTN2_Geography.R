# В этом скрипте анализируется географическое распространение BTN1 и BTN2 по данным "собственного" генотипирования

library(ggmap)
library(readxl)
library(ggrepel)
library(dplyr)
library(reshape2)




world <- map_data("world")

Pl_map <- 
  ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region),
    fill = "gray"
  ) 



btn <- read_excel("Data/MtBTN geography 211124.xlsx", sheet = "Data_for_analyzis")

btn_Magadan <- read_excel("Data/individual data_Magadan2023_inter2.xlsx", sheet = "clean_R")

btn_Magadan_no_ecology <-
btn_Magadan %>% 
  filter(Ecology == "no") %>%
  select(Site_code, Latitude, Longitude, Year, N, BTN_genotype) %>% 
  group_by(Site_code) %>% 
  summarise(Lat = mean(Latitude), Lon = mean(Longitude), Year = mean(Year), N_FC = mean(N), BTN1 = sum(BTN_genotype == "BTN1"), BTN2 = sum(BTN_genotype %in% c("BTN2.1", "BTN2.2"))) %>% 
  rename(Sample_ID  = Site_code) %>% 
  mutate(Sea = "Sea of Okhotsk") %>% 
  melt(id.vars = c("Sample_ID", "Year", "Sea", "Lat", "Lon", "N_FC"), variable.name = "BTN_Type") %>% 
  mutate(Prop = value/N_FC) %>% 
  dplyr::select(-value)




btn <- 
  btn %>% 
  mutate(N_BTN1 = BTN1, N_BTN2 = BTN2, BTN1 = BTN1/N_FC, BTN2 = BTN2/N_FC)

btn_long <-
  btn %>% 
  select(-c(BTN2_1, BTN2_2)) %>% 
  melt(id.vars = c("Sample_ID", "Year", "Date", "Sea", "Place", "Description", "Lat", "Lon", "N_FC", "DN", "N_BTN1", "N_BTN2"), value.name = "Prop", variable.name = "BTN_Type") %>%
  select( "Sample_ID",  "Year","Sea", "Lat", "Lon", "N_FC", "BTN_Type",    "Prop" ) %>% 
  mutate(N = Prop*N_FC)

btn_long$Prop * btn_long$N_FC

# btn_long <-
# rbind(btn_long, btn_Magadan_no_ecology)

Pl_map + 
  geom_point(data = btn_long %>% filter(Prop > 0), aes(x = Lon, y = Lat, size = Prop), shape = 21) +
  facet_wrap(~BTN_Type)




ggplot(btn_long %>% filter(Prop != 0), aes(x = Prop, fill = BTN_Type, color = BTN_Type)) + 
  geom_density(alpha = 0.3) 

ggplot(btn_long %>% filter(Prop != 0), aes(x = BTN_Type, y = Prop, fill = BTN_Type)) + 
  geom_violin() 

# Если в некотором местообитании рак присутствует, то Частота BTN2 чуть выше, чем частота BTN1



ggplot(btn_long  %>% filter(Prop != 0), aes(x = BTN_Type, y = Lat)) +
  geom_boxplot()

# BTN1 имеет более северное распространение




