library(ggmap)
library(mapproj)
library(readxl)
library(dplyr)



############### Data reading and Data preparation ###############################

##### Data reading
myt_full <- read_excel(path = "data/myt_full_2024.xlsx", sheet = "Samples")


ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel", "Karel"), Port = c("Kandalaksha", "Vitino", "Umba", "Chupa", "Keret", "Kovda"), Status = c("Active", "Active", "Abandoned", "Abandoned", "Abandoned", "Abandoned"  ), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178, 66.696754), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656, 32.875396))


# sites_fetch_df <- read.table("data/Distred_samples_fetch_values_2023.csv", sep = ",", header = T)
# 
# sites_fetch_df[,1:5] <- sites_fetch_df[,1:5]/1000 

# Сonventional coordinates of the top of the Kandalaksha Bay
Shore_boundary = c(67.162360, 32.332371)


river_full <- read.table("data/Rivers_2021.csv", sep = ",", header = T)

myt_full <- 
  myt_full %>%
  mutate(Ptros =  exp(-2.4 + 5.4 * Prop_T)/(1 + exp(-2.4 + 5.4 * Prop_T)) ) %>% 
  mutate(Log_Min_dist_port = log(Min_dist_port),
         Log_Min_dist_river = log(Min_dist_river),
         Log_Average_Fetch = log(Average_Fetch),
         Log_Fetch = log(Fetch))

myt_full$Position <- factor(myt_full$Position)

myt_full$Position <- relevel(myt_full$Position, ref = "Bottom")

myt_full$Port_Status <- factor(myt_full$Port_Status)

myt_full$Port_Status <- relevel(myt_full$Port_Status, ref = "Abandoned")

myt_full$River_Size <- factor(myt_full$River_Size)

myt_full$River_Size <- relevel(myt_full$River_Size, ref = "Small")

myt_full$Site <- factor(myt_full$Site)



boundary = 0.5

myt_site <- myt_full %>% 
  group_by(Site) %>% 
  # select(Lat, Lon, N_T, N_E, Salinity, Min_dist_river, River, River_Size, Min_dist_river_Large, Min_dist_port, Port, Port_Status, Average_Fetch, Fetch, Dist_cut) %>% 
  summarise(Lat = mean(Lat), Lon = mean(Lon), N_T = sum(N_T), N_E = sum(N_E), Salinity = mean(Salinity), Min_dist_river = mean(Min_dist_river), River = unique(River), River_Size = unique(River_Size), Min_dist_river_Large = mean(Min_dist_river_Large),  Min_dist_port = mean(Min_dist_port), Port = unique(Port), Port_Status = unique(Port_Status), Average_Fetch = mean(Average_Fetch), Fetch = mean(Fetch), Dist_cut = mean(Dist_cut)) %>% 
  mutate(Prop_T = N_T/(N_T+N_E)) %>% 
  mutate(Ptros = exp(-2.4 + 5.4 * Prop_T)/(1 + exp(-2.4 + 5.4 * Prop_T)))

myt_site_substr <-
  myt_full %>%
  group_by(Site, Position) %>%
  summarise(N_E = sum(N_E), N_T = sum(N_T),
            Salinity = mean(Salinity),
            Min_dist_river = mean(Min_dist_river),
            River_Size = unique(River_Size),
            Average_Fetch = mean(Average_Fetch),
            Fetch = mean(Fetch),
            Min_dist_port = mean(Min_dist_port),
            Port_Status = unique(Port_Status),
            Lon = mean(Lon),
            Lat = mean(Lat)) %>%
  mutate(Prop_T = N_T/(N_T + N_E)) %>%
  mutate(Ptros =  exp(-2.4 + 5.4 * Prop_T)/(1 + exp(-2.4 + 5.4 * Prop_T))) %>%  
  mutate(Mt_dominated = ifelse(Ptros >= boundary, 1, 0))


myt_site_substr <- 
  myt_site_substr %>% 
  mutate(Log_Min_dist_port = log(Min_dist_port),
         Log_Min_dist_river = log(Min_dist_river),
         Log_Fetch = log(Fetch))

myt_test <- read_excel("data/myt_White_Sea_testing_data_set.xlsx")

myt_test_site <- myt_test %>% group_by(Site, Position) %>%  
  summarise(N_E = sum(N_E), N_T = sum(N_T),
            Salinity = mean(Salinity, na.rm = T),
            Min_dist_river = mean(Min_dist_river),
            River_Size = unique(River_Size),
            Average_Fetch = mean(Average_Fetch),
            Fetch = mean(Fetch),
            Min_dist_port = mean(Min_dist_port),
            Port_Status = unique(Port_Status),
            Lon = mean(Lon),
            Lat = mean(Lat)) %>%
  mutate(Prop_T = N_T/(N_T + N_E)) %>%
  mutate(Fi_T = 2*asin(sqrt(Prop_T))*180/pi) %>%
  mutate(Ptros = exp(-2.4 + 5.4 * Prop_T)/(1 + exp(-2.4 + 5.4 * Prop_T)),
         Fi_tros = 2*asin(sqrt(Ptros))*180/pi) %>% 
  mutate(Mt_dominated = ifelse(Ptros >= boundary, 1, 0))

myt_test_site <- 
  myt_test_site %>% 
  mutate(Log_Min_dist_port = log(Min_dist_port),
         Log_Min_dist_river = log(Min_dist_river),
         Log_Average_Fetch = log(Average_Fetch),
         Log_Fetch = log(Fetch))
#########################################################################



# Shape file reading
ggWhite_Sea <- read.csv("data/ggWhite_Sea_2021.csv")
ggKand_upper <- read.csv("data/ggKand_upper_2021.csv")

ggKand_upper2 <- read.csv("data/ggKand_upper.csv")


## Coordinates for maps

Full_x <- c(3, 43)
Full_y <- c(55, 72)


White_sea_x <- c(31, 45) 
White_sea_y <- c(63,68.9)

Kand_upper_x <- c(min(myt_site$Lon) - 0.1, max(myt_site$Lon) + 0.1)
Kand_upper_y <- c(min(myt_site$Lat) - 0.1, max(myt_site$Lat) + 0.1)



Plot_White_sea <- 
  ggplot(ggWhite_Sea, aes(x=Lon, y=Lat, group=group)) + 
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = White_sea_x, ylim = White_sea_y) +  
  theme(axis.text.x =element_blank(), axis.text.y= element_blank(), axis.ticks=element_blank(), axis.title.x =element_blank(),  axis.title.y= element_blank(), plot.background = element_rect(fill = "white"), panel.grid = element_blank(), panel.background = element_rect(fill = "white"))

pol<-data.frame(Lon = Kand_upper_x, Lat = Kand_upper_y, group = 1)

Plot_White_sea2 <- 
  Plot_White_sea + 
  geom_rect(data = pol, aes(xmin = Lon[1], xmax = Lon[2], ymin = Lat[1], ymax = Lat[2]), color = "black", fill = NA, size = 1) + 
  ggtitle("White Sea")


ggsave(filename = "figures/Plot_White_Sea.eps", plot = Plot_White_sea)  


Plot_Kand_upper <- 
  ggplot(ggKand_upper, aes(x=Lon, y=Lat, group=group)) + 
  geom_polygon(fill = "gray70", colour = "black") + 
  coord_map(xlim = Kand_upper_x, ylim = Kand_upper_y) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(hjust = 0.5))

Plot_Kand_upper + geom_polygon(data = pol, aes(x = Lon, y = Lat, group = 1))

Plot_Kand_upper_2 <- 
  ggplot(ggKand_upper2, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill = "gray70", colour = "black") + 
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white")) +
  theme(plot.title = element_text(hjust = 0.5))

myt_test_site_2 <- 
  myt_test_site %>% 
  group_by(Site) %>% 
  summarise(Lat = mean(Lat), Lon = mean(Lon))
  
  
Plot_Kand_upper_2_test <-   
Plot_Kand_upper_2 + 
  geom_point(data = myt_test_site_2, aes(x = Lon, y = Lat, group = 1), size = 4, shape = 21, fill="blue") +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank()) 


  ggsave(plot = Plot_Kand_upper_2_test, filename = "figures/Sites_Testing_data_set.svg")

  ggsave(plot = Plot_Kand_upper, filename = "figures/Kandalaksha_Bay.svg")

########################## Maps with Ptros ##################################


myt_site2 <- myt_site %>% arrange((Ptros))

myt_site_substr2 <- myt_site_substr %>% arrange((Ptros))


Plot_Kand_upper_PT <-
  Plot_Kand_upper +
  geom_point(data = myt_site2, aes(x = Lon, y = Lat, group = 1, fill = Ptros), shape = 21, size = 3) +
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank()) +
  labs(fill = "Ptros")


Plot_Kand_upper_PT_Substr <-
  Plot_Kand_upper +
  geom_point(data = myt_site_substr2, aes(x = Lon, y = Lat, group = 1, fill = Ptros), shape = 21, size = 3) +
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.15,0.1), legend.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), strip.text = element_blank()) +
  labs(fill = "") +
  facet_wrap(~ Position)




Kand_upper_x2 <- c(32.2, 33.06)
Kand_upper_y2 <- c(66.85, 67.16)

Plot_Kand_upper_PT_2_Substr <-
  Plot_Kand_upper +
  geom_point(data = myt_site_substr2, aes(x = Lon, y = Lat, group = 1, fill = Ptros), shape = 21, size = 5) +
  coord_map(xlim = Kand_upper_x2, ylim = Kand_upper_y2)+
  scale_fill_gradient(low = "yellow", high = "red") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(), strip.text = element_blank()) +
  labs(fill = "Ptros") +
  guides(fill = "none") +
  facet_wrap(~Position)


ggsave(filename = "figures/Plot_Ptros_no_size_insertion_Substr.svg", plot = Plot_Kand_upper_PT_2_Substr)  

  
ggsave(filename = "figures/Plot_Ptros_no_size_Substr.svg", plot = Plot_Kand_upper_PT_Substr)  

#########################################################




Plot_Kand_upper2 <- 
  Plot_Kand_upper + 
  geom_point(data = ports, aes(x = Lon, y = Lat, group = 1), size = 3)


myt_site <- myt_site %>% arrange(desc(Average_Fetch))

Plot_Kand_upper2_Fetch <- 
  Plot_Kand_upper + 
  geom_point(data = myt_site, aes(x = Lon, y = Lat, group = 1, size = Average_Fetch), shape = 21, fill = "orange", color = "black") +
  scale_fill_gradient(low = "yellow", high = "blue") +
  guides(size = "none") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())




myt_site <- myt_site %>% arrange(desc(Salinity))

Plot_Kand_upper2_Salinity <- 
  Plot_Kand_upper + 
  geom_point(data = myt_site, aes(x = Lon, y = Lat, group = 1, fill = Salinity), shape = 21, size = 4) +
  scale_fill_gradient(low = "yellow", high = "blue") +
  # guides(fill = "") +
  theme(legend.direction = "horizontal", legend.position = c(0.8,0.1), legend.background = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) +
  labs(fill = "")



ggsave(filename = "figures/Plot_Kand_upper_Fetch.svg", plot = Plot_Kand_upper2_Fetch)  


ggsave(filename = "figures/Plot_Kand_upper_Salinity.svg", plot = Plot_Kand_upper2_Salinity)  



ggsave(filename = "figures/Plot_Kand_upper.eps", plot = Plot_Kand_upper2)  


library(grid)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.5, height = 0.5, x = 0.78, y = 0.75) #plot area for the inset map
print(Plot_Kand_upper2,vp=v1) 
print(Plot_White_sea2,vp=v2)


# Карта сайтов с тестовым датасетом и сайтов с низкой и высокой соленостью

myt_site_low_salinity <- myt_site %>% filter(Salinity <= 12)
myt_site_high_salinity <- myt_site %>% filter(Salinity >= 27)


Plot_Kand_upper_extrem_salinity <-
Plot_Kand_upper +
  geom_point(data = myt_site_low_salinity, aes(x = Lon, y = Lat, group = 1), size = 4, shape = 21, fill = "cyan") +
  geom_point(data = myt_site_high_salinity, aes(x = Lon, y = Lat, group = 1), size = 4, shape = 21, fill = "blue") +
  geom_point(data = myt_test_site, aes(x = Lon, y = Lat, group = 1), shape = 24, fill = "red", size = 2)


ggsave(filename = "figures/Plot_Kand_upper_extremesalinity_and_testing.svg", plot = Plot_Kand_upper_extrem_salinity)  



