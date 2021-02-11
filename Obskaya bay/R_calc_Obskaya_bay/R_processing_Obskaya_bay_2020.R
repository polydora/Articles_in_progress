
# Packages ######################

library(ggmap)
library(mapproj)
library(maps)

library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools

library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)

library(readxl)
library(dplyr)
library(cowplot)
library(mgcv)
library(reshape2)

library(png)

library(vegan)


# install.packages("gpclib")

# install.packages("rgeos")

# install.packages("gpclib", type="source")


# library(rgdal)

# 
# Ob_x <- c(70, 79)
# Ob_y <- c(70, 75)
# 

# 
# gshhs.l.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_l.b"
# 
# gshhs.f.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_f.b"
# wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"
# wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"
# 
# 
# wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"
# wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"
# 
# Ob_f <- getRgshhsMap(fn = gshhs.f.b, xlim = Ob_x, ylim = Ob_y)
# # Rivers <- getRgshhsMap(wdb_rivers.f.b, xlim = Ob_x, ylim = Ob_y)
# # Borders <- Rgshhs(wdb_borders.f.b, xlim = Ob_x, ylim = Ob_y)
# 
# plot(Ob_f)
# 
# Ob_df <- fortify(Ob_f)
# 
# write.csv(Ob_df, file = "Data/Obskaya_bay.csv")



# Data reading ####################

Ob_df <- read.csv("Data/Obskaya_bay_map.csv") # Map poligones

# All Station position
stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)

# Data on organic matter from the file "Содержание органического вещества в ДО.docx"

org <- read.csv("Data/Organic matter_2020.csv")

org <- org[org$Station %in% stat_full$Station, ]

# Data on Granulmetric from the file "Грансостав ДО.docx"

granul <- read_excel("Data/Granulometry_2020.xlsx",  na = "NA")


granul$Small <- with(granul, F_0.002 + `F_0.01-0.002` + `F_0.05-0.01`)              

granul$Big <- rowSums(granul[ ,8:15]  )


granul$Season <- factor(granul$Season)


# Chlorophyll a 

chlor <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Chlorophyll august 2020", na = "NA")


# Biogenes

biogen <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Biogenes_August", na = "NA")

biogen_full <- biogen %>% filter(Exclude == 0) %>% select(-Exclude)

biogen_full_melt <- melt(biogen_full, id.vars = c("Station", "Layer"))

biogen_full_2 <- dcast(biogen_full_melt, Station ~ Layer + variable )



# Zooplancton

zoopl_long <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Zooplancton_long", na = "NA")

zoopl <- dcast(zoopl_long %>%select(-B), Station ~ Taxa) 

zoopl[is.na(zoopl)] <- 0

zoopl$Station_ID <- as.numeric(gsub("O","", zoopl$Station))

zoopl <- zoopl[order(zoopl$Station_ID), ] %>% select(-Station_ID)



# Ship activity and Sattelite data on suspended matter and distances to dumping

# ships <- read.table("Data/Stations_tsm_ships.shp.txt", header = T, sep = "\t")
# write.table(ships, "clipboard", sep = "\t")

ships <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Ship_activity")

ships <- ships %>% filter(Exclude != 1) %>% select(Station, Ships, Distance_Dump, Distance_to_Drag)

ships2 <- merge(stat_full %>% select(Station, Lat, Long), ships)

ships2$Station_ID <- as.numeric(gsub("O","", ships2$Station))

ships2 <- ships2[order(ships2$Station_ID), ] %>% select(-Station_ID)


# Basic map layer####################

theme_set(theme_bw())

# Coordinate limites 
# Ob_x <- c(71, 79)

Ob_x <- c(71, 75)
Ob_y <- c(70, 73.2)

# Sabetta
Sabetta_y <- 71.235220
Sabetta_x <- 72.126754

# Terminal

Terminal_y <- 71.010886
Terminal_x <- 73.793525



Pl_Obskaya_bay <- 
ggplot(Ob_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank()) +
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 21, fill = "red") + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "yellow") 




# Obskaya_bay_cover #####

  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank()) 



Obskaya_bay_cover <-function(plt){
  plt <- plt + 
    geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
    coord_map(xlim = Ob_x, ylim = Ob_y) + 
    theme_bw() +  
    theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
    theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
    theme(axis.ticks = element_blank()) +
    geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 21, fill = "red") + 
    geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "yellow") +
    geom_point(data = stat_full, aes(x = Long, y = Lat, group = 1), size = 0.5) +
    theme(strip.background = element_blank())
  plt
}



##################################################################################
## Визуализация распределения параметров среды ###################################

# Станции
Pl_all_stat <- Pl_Obskaya_bay + 
  geom_point(data = stations, aes(x = Long, y = Lat, group = 1, color = factor(Exclude) ), size =1) + 
  scale_color_manual(values = c("black", "blue")) + 
  guides(color = "none")


ggsave("figures/stations.png", Pl_all_stat, dpi = 600)

# Spatial grides ############


# Ob_x <- c(71, 75)
# Ob_y <- c(70, 73.2)

grid_data <- expand.grid(Lat = seq(from = 70, to =72.9, length.out = 100), Long =  seq(from = 71, to =75.2 , length.out = 100))


# Ships activity ###################

Pl_Obskaya_bay + geom_point(data = ships2, aes(x = Long, y = Lat, size = log(Ships+1), group = 1))


# Dumping distance ###################

Pl_Obskaya_bay + geom_point(data = ships2, aes(x = Long, y = Lat, size = Distance_to_Drag, group = 1))
Pl_Obskaya_bay + geom_point(data = ships2, aes(x = Long, y = Lat, size = Distance_Dump, group = 1))




# Depth ########################

depth <- stat_full %>% mutate(Depth = (Depth_aug_20+Depth_sep_20)/2) %>% select(Station, Long, Lat, Depth)


# fit_depth <- gam(formula = Depth  ~ lo(Long, Lat), data = depth, rgrid = grid_data, family = gaussian)
# 
# depth_pred <- mypredict.gam(object = fit_depth , newdata = grid_data, reference = "mean")


fit_depth <- gam(formula = log(Depth +1)  ~ s(Long, Lat, bs = "tp"), data = depth, family = gaussian)

depth_pred <- exp(predict(fit_depth, newdata = grid_data, type = "response")) - 1


Pl_depth <- ggplot() + 
  geom_tile(data = grid_data, aes(x = Long, y = Lat, fill = (depth_pred), group = 1))  + 
  scale_fill_gradient(low = "white", high = "blue", breaks = c(0, 5, 10, 15, 20, 25)) +
  labs(fill = "Depth (m)")

Pl_depth <- Obskaya_bay_cover(Pl_depth)

ggsave("figures/depth.png", Pl_depth, dpi = 600)



# Transparency  #######################

transp <- stat_full %>%  mutate(Transparency = rowMeans(select(., Transparency_aug_20, Transparency_sep_20), na.rm = TRUE)) %>% select(Station, Long, Lat, Transparency)



fit_transp <- gam(formula = log(Transparency +1)  ~ s(Long, Lat, bs = "tp"), data = transp, family = gaussian)

transp_pred <- exp(predict(fit_transp, newdata = grid_data, type = "response")) - 1



Pl_transp <- ggplot() + 
  geom_tile(data = grid_data, aes(x = Long, y = Lat, fill = (transp_pred), group = 1))  + 
  scale_fill_gradient(low = "black", high = "white", breaks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5)) +
  labs(fill = "Transparency (m)")

Pl_transp <- Obskaya_bay_cover(Pl_transp)



ggsave("figures/transparency.png", Pl_transp, dpi = 600)


# Slinity ###############

sal_surf_aug <- stat_full %>%  select(Station, Long, Lat, Surface_Salinity_Aug_20) %>% mutate(Season = "August", Layer = "Surface") %>% rename(salinity = Surface_Salinity_Aug_20)

sal_surf_sep <- stat_full %>%  select(Station, Long, Lat, Surface_Salinity_Sep_20) %>% mutate(Season = "September", Layer = "Surface") %>% rename(salinity = Surface_Salinity_Sep_20)


sal_bottom_aug <- stat_full %>%  select(Station, Long, Lat, Bottom_Salinity_Aug_20) %>% mutate(Season = "August", Layer = "Bottom")%>% rename(salinity = Bottom_Salinity_Aug_20)

sal_bottom_sep <- stat_full %>%  select(Station, Long, Lat, Bottom_Salinity_Sep_20) %>% mutate(Season = "September", Layer = "Bottom") %>% rename(salinity = Bottom_Salinity_Sep_20)

salinity <- rbind(sal_surf_aug, sal_surf_sep, sal_bottom_aug, sal_bottom_sep)

# max(salinity$salinity, na.rm = T)


fit_sal <- gam(formula = log(salinity)  ~ s(Long, Lat, bs = "tp", by = interaction(Layer, Season)) + Layer * Season, data = salinity, family = gaussian)
# 
# summary(fit_sal)

grid_data_season <-
  rbind(grid_data %>% mutate(Season = "August", Layer = "Surface"),
      grid_data %>% mutate(Season = "September", Layer = "Surface"),
      grid_data %>% mutate(Season = "August", Layer = "Bottom"),
      grid_data %>% mutate(Season = "September", Layer = "Bottom")
)

grid_data_season$Layer <- factor(grid_data_season$Layer, levels = c("Surface", "Bottom"))


# nrow(grid_data_season)

sal_pred <- exp(predict(fit_sal, newdata = grid_data_season, type = "response")) 

# length(sal_pred)

# # Обрезка неправдоподобных предсказаний

sal_pred[sal_pred > max(salinity$salinity, na.rm = TRUE)] <- max(salinity$salinity, na.rm = TRUE)  


Pl_sal <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (sal_pred)))  + 
  scale_fill_gradient(low = "yellow", high = "red", limits = c(0, 38)) + 
  labs(fill = "salinity (psu)")+ 
  facet_grid(Season ~ Layer) + 
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank()) +
  theme(strip.background = element_blank())

Pl_sal <- Pl_sal + geom_point(data = stat_full, aes(x = Long, y = Lat, group = 1), size = 0.5)



ggsave("figures/salinity.png", Pl_sal, dpi = 600)


# Turbidity ########################

turb_surf_aug <- stat_full %>%  select(Station, Long, Lat, Surface_Turbidity_Aug_20) %>% mutate(Season = "August", Layer = "Surface") %>% rename(Turbidity = Surface_Turbidity_Aug_20)

turb_surf_sep <- stat_full %>%  select(Station, Long, Lat, Surface_Turbidity_Sep_20) %>% mutate(Season = "September", Layer = "Surface") %>% rename(Turbidity = Surface_Turbidity_Sep_20)


turb_bottom_aug <- stat_full %>%  select(Station, Long, Lat, Bottom_Turbidity_Aug_20) %>% mutate(Season = "August", Layer = "Bottom")%>% rename(Turbidity = Bottom_Turbidity_Aug_20)

turb_bottom_sep <- stat_full %>%  select(Station, Long, Lat, Bottom_Turbidity_Sep_20) %>% mutate(Season = "September", Layer = "Bottom") %>% rename(Turbidity = Bottom_Turbidity_Sep_20)

turbidity <- rbind(turb_surf_aug, turb_surf_sep, turb_bottom_aug, turb_bottom_sep)

# max(turbinity$turbinity, na.rm = T)


fit_turb <- gam(formula = log(Turbidity)  ~ s(Long, Lat, bs = "tp", by = interaction(Layer, Season)) + Layer * Season, data = turbidity, family = gaussian)


grid_data_season <-
  rbind(grid_data %>% mutate(Season = "August", Layer = "Surface"),
        grid_data %>% mutate(Season = "September", Layer = "Surface"),
        grid_data %>% mutate(Season = "August", Layer = "Bottom"),
        grid_data %>% mutate(Season = "September", Layer = "Bottom")
  )


grid_data_season$Layer <- factor(grid_data_season$Layer, levels = c("Surface", "Bottom"))

# nrow(grid_data_season)

turb_pred <- exp(predict(fit_turb, newdata = grid_data_season, type = "response")) 

# max(turb_pred)

# # Обрезка неправдоподобных предсказаний

# turb_pred[sal_pred > max(turbidity$Turbidity, na.rm = TRUE)] <- max(turbidity$Turbidity, na.rm = TRUE)

Pl_turb <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (turb_pred)))  + 
  scale_fill_gradient(low = "white", high = "black") + 
  labs(fill = "Turbidity (NTU)")+ 
  facet_grid(Season ~ Layer) +
  theme(strip.background = element_blank())

Pl_turb <- Obskaya_bay_cover(Pl_turb)

ggsave("figures/turbidity.png", Pl_turb, dpi = 600)


  # Suspended matter ###########################

susp_surf_aug <- stat_full %>%  select(Station, Long, Lat, Surface_Susp_Aug_20) %>% mutate(Season = "August", Layer = "Surface") %>% rename(Susp = Surface_Susp_Aug_20)

susp_surf_sep <- stat_full %>%  select(Station, Long, Lat, Surface_Susp_Sep_20) %>% mutate(Season = "September", Layer = "Surface") %>% rename(Susp = Surface_Susp_Sep_20)


susp_bottom_aug <- stat_full %>%  select(Station, Long, Lat, Bottom_Susp_Aug_20) %>% mutate(Season = "August", Layer = "Bottom")%>% rename(Susp = Bottom_Susp_Aug_20)

susp_bottom_sep <- stat_full %>%  select(Station, Long, Lat, Bottom_Susp_Sep_20) %>% mutate(Season = "September", Layer = "Bottom") %>% rename(Susp = Bottom_Susp_Sep_20)


susp_interm_aug <- stat_full %>%  select(Station, Long, Lat, Interm_Susp_Aug_20) %>% mutate(Season = "August", Layer = "Intermediate")%>% rename(Susp = Interm_Susp_Aug_20)

susp_interm_sep <- stat_full %>%  select(Station, Long, Lat, Interm_Susp_Sep_20) %>% mutate(Season = "September", Layer = "Intermediate") %>% rename(Susp = Interm_Susp_Sep_20)



susp <- rbind(susp_surf_aug, susp_surf_sep, susp_bottom_aug, susp_bottom_sep, susp_interm_aug, susp_interm_sep)

# unique(susp$Layer)


grid_data_season <-
  rbind(grid_data %>% mutate(Season = "August", Layer = "Surface"),
        grid_data %>% mutate(Season = "September", Layer = "Surface"),
        grid_data %>% mutate(Season = "August", Layer = "Bottom"),
        grid_data %>% mutate(Season = "September", Layer = "Bottom"),
        grid_data %>% mutate(Season = "August", Layer = "Intermediate"),
        grid_data %>% mutate(Season = "September", Layer = "Intermediate")
  )


grid_data_season$Layer <- factor(grid_data_season$Layer, levels = c("Surface", "Intermediate", "Bottom"))


fit_susp <- gam(formula = log(Susp)  ~ s(Long, Lat, bs = "tp", by = interaction(Layer, Season)) + Layer * Season, data = susp, family = gaussian)

# summary(fit_susp)


susp_pred <- exp(predict(fit_susp, newdata = grid_data_season, type = "response")) 

# max(turb_pred)

# # Обрезка неправдоподобных предсказаний

# turb_pred[sal_pred > max(turbidity$Turbidity, na.rm = TRUE)] <- max(turbidity$Turbidity, na.rm = TRUE)

Pl_susp <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (susp_pred)))  + 
  scale_fill_gradient(low = "white", high = "black") + 
  labs(fill = "Suspendet \nMatter (mg/l)")+ 
  facet_grid(Season ~ Layer) +
  theme(strip.background = element_blank())

Pl_susp <- Obskaya_bay_cover(Pl_susp)

ggsave("figures/suspended_matter.png", Pl_susp, dpi = 600)


# Organic matter ###########################


org_aug <- org %>%  select(Station, Long, Lat, Org_Aug_20) %>% mutate(Season = "August") %>% rename(Org = Org_Aug_20)

org_sep <- org %>%  select(Station, Long, Lat, Org_Sep_20) %>% mutate(Season = "September") %>% rename(Org = Org_Sep_20)


organic <- rbind(org_aug,org_sep)

organic$Season <- factor(organic$Season)

grid_data_season <-
  rbind(grid_data %>% mutate(Season = "August"),
        grid_data %>% mutate(Season = "September")
  )


# str(organic)

fit_org <- gam(formula = log(Org + 1)  ~ s(Long, Lat, bs = "tp", by = Season) +  Season, data = organic, family = gaussian)



org_pred <- exp(predict(fit_org, newdata = grid_data_season, type = "response")) 


Pl_org <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (org_pred)))  + 
  scale_fill_gradient(low = "white", high = "red") + 
  labs(fill = "Organic \nMatter (%)")+ 
  facet_wrap(~Season, ncol = 2) +
  theme(strip.background = element_blank())

Pl_org <- Obskaya_bay_cover(Pl_org)

ggsave("figures/organic_matter.png", Pl_org, dpi = 600)


# Chlorophyll-a  ############################

chlor$Layer <- factor(chlor$Layer, levels = c("Surface", "Intermediate", "Bottom") )

# hist(chlor$Chla)

fit_chla <- gam(formula = log(Chla + 1)  ~ s(Long, Lat, bs = "tp", by = Layer) +  Layer, data = chlor, family = gaussian)


grid_data_layer <-
  rbind(grid_data %>% mutate(Layer = "Surface"),
        grid_data %>% mutate(Layer = "Intermediate"), 
        grid_data %>% mutate(Layer = "Bottom")
        )


grid_data_layer$Layer <- factor(grid_data_layer$Layer, levels = c("Surface", "Intermediate", "Bottom"))

chla_pred <- exp(predict(fit_chla, newdata = grid_data_layer, type = "response")) 


Pl_chla <- 
  ggplot(data = grid_data_layer, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (chla_pred)))  + 
  scale_fill_gradient(low = "white", high = "darkgreen") + 
  labs(fill = "Chl-a (mkd/l)")+ 
  facet_wrap(~ Layer, ncol = 3) +
  theme(strip.background = element_blank())

Pl_chla <- Obskaya_bay_cover(Pl_chla)


ggsave("figures/chl-a.png", Pl_chla, dpi = 600)


# Granulometric #######################################

granul_pca <- rda(granul %>% filter(Season == "August") %>% select(-c(Station, Long, Lat, Season), -c(Big, Small)))

plot(granul_pca, display = "species")

granul$Small <- with(granul, F_0.002 + `F_0.01-0.002` + `F_0.05-0.01`)              
                    
granul$Big <- rowSums(granul[ ,8:15]  )


granul$Season <- factor(granul$Season)




grid_data_season <-
  rbind(grid_data %>% mutate(Season = "August"))




fit_gran_small <- gam(formula = log(Small + 1)  ~ s(Long, Lat, bs = "tp"), data = granul, family = gaussian)



granul_small_pred <- exp(predict(fit_gran_small, newdata = grid_data_season, type = "response")) 

granul_small_pred[granul_small_pred >100] <- 100

Pl_granul_small <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (granul_small_pred)))  + 
  scale_fill_gradient(low = "white", high = "black") + 
  labs(fill = "Smaller 0.05 (%)")+ 
  facet_wrap(~Season, ncol = 2) +
  theme(strip.background = element_blank())

Pl_granul_small <- Obskaya_bay_cover(Pl_granul_small)




fit_gran_large <- gam(formula = log(Big + 1)  ~ s(Long, Lat, bs = "tp"), data = granul, family = gaussian)



granul_large_pred <- exp(predict(fit_gran_large, newdata = grid_data_season, type = "response")) 

granul_large_pred[granul_large_pred >100] <- 100

Pl_granul_large <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, group = 1)) + 
  geom_tile(aes(fill = (granul_large_pred)))  + 
  scale_fill_gradient(low = "white", high = "black") + 
  labs(fill = "Larger 0.05 (%)")+ 
  facet_wrap(~Season, ncol = 2) +
  theme(strip.background = element_blank())

Pl_granul_large <- Obskaya_bay_cover(Pl_granul_large)

 plot_grid(Pl_granul_large, Pl_granul_small, ncol = 2, labels = c("A", "B"))

ggsave("figures/granulometric.png", plot_grid(Pl_granul_large, Pl_granul_small, ncol = 2, labels = c("A", "B")), dpi = 600)


# Biogenes #################################





###########################################################
# Predictors ##############################################

# Подготовка таблицы предикторов ####################################
predictors <- stat_full %>% select(Station, Lat, Long,  Depth_aug_20, Transparency_aug_20, Surface_Salinity_Aug_20, Bottom_Salinity_Aug_20, Surface_Turbidity_Aug_20, Bottom_Turbidity_Aug_20, Surface_Susp_Aug_20, Interm_Susp_Aug_20, Bottom_Susp_Aug_20)

# nrow(predictors)

chlor_surface <- chlor %>% filter(Layer == "Surface") %>% select(Station, Chla) %>% rename(Chla_surface = Chla)


chlor_intermed <- chlor %>% filter(Layer == "Intermediate") %>% select(Station, Chla) %>% rename(Chla_intermed = Chla)

chlor_bottom <- chlor %>% filter(Layer == "Bottom") %>% select(Station, Chla) %>% rename(Chla_bottom = Chla)



predictors2 <- merge(predictors, chlor_surface, all.x = T)

predictors3 <- merge(predictors2, chlor_intermed, all.x = T)

predictors4 <- merge(predictors3, chlor_bottom, all.x = T)


predictors5 <- merge(predictors4, org %>% select(Station, Org_Aug_20), all.x = T)


predictors6 <- merge(predictors5, granul %>% filter(Season == "August") %>% select(Station, Small, Big) %>% rename(Fine_particles = Small, Coarce_particles = Big), all.x =T)


predictors7 <- merge(predictors6, biogen_full_2, all.x = T)

# nrow(predictors7)

# # All variables from intermediate layer will be excluded from analysis 
# 
# predictors8 <- predictors7 %>% select(-c(Interm_Susp_Aug_20, Chla_intermed, Intermediate_Color, Intermediate_O2, Intermediate_pH,  Intermediate_Amon_N, Intermediate_Nitrit_N, Intermediate_Nitrat_N, Intermediate_Total_N, Intermediate_Mineral_P, Intermediate_Toal_P, Intermediate_Si, Intermediate_Organic_N, Intermediate_Organic_P))

predictors8 <- merge(predictors7, ships2 %>% select(Station, Ships, Distance_Dump, Distance_to_Drag), all.x = TRUE)

# nrow(predictors8)



# Variable names cleaning

var_names <- names(predictors8)

var_names <- gsub("Bottom", "Bot", var_names)

var_names <- gsub("Surface", "Surf", var_names)

var_names <- gsub("Intermediate", "Int", var_names)

var_names <- gsub("_aug_20", "", var_names)

var_names <- gsub("_Aug_20", "", var_names)


names(predictors8) <- var_names

write.table(predictors8, "Data/predictors.csv", sep = ",", row.names = F)


# Reconstruction of missed values ############################################

NA_present <- predictors8 %>% summarise_all(funs(sum(is.na(.)))) %>% t() %>% as.data.frame()%>% mutate(Param = row.names(.))

library(imputeTS)
predictors9 <- na_mean(predictors8)

predictors9$Station_ID <- as.numeric(gsub("O","", predictors9$Station))

predictors10 <- predictors9[order(predictors9$Station_ID), ] %>% select(-Station_ID)

# Variable names cleaning

var_names <- names(predictors10)

var_names <- gsub("Bottom", "Bot", var_names)

var_names <- gsub("Surface", "Surf", var_names)

var_names <- gsub("Intermediate", "Int", var_names)

var_names <- gsub("_aug_20", "", var_names)

var_names <- gsub("_Aug_20", "", var_names)


names(predictors10) <- var_names

# Analysis of biotic parameters #####################################




# Zooplancton #######################################################

env_zoopl <- predictors10 %>% filter(Station %in% zoopl$Station) #%>% select(-c(Station, Lat, Long))

row.names(env_zoopl) <- env_zoopl$Station

# zoopl_CCA <- cca(zoopl[ , -1] ~ ., data = env_zoopl%>% select(-c(Station, Lat, Long)))
# 
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# 
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P , data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P - Bot_Total_N, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P - Bot_Total_N -Bot_pH, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P - Bot_Total_N -Bot_pH - Bot_O2, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P - Bot_Total_N -Bot_pH - Bot_O2 - Int_Organic_P, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P - Bot_Total_N -Bot_pH - Bot_O2 - Int_Organic_P - Surf_Color, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# zoopl_CCA <- cca(zoopl[ , -1] ~ . - Fine_particles - Surf_Toal_P - Bot_Toal_P -Int_Toal_P - Surf_Total_N -Int_Total_N - Int_Mineral_P - Bot_Total_N -Bot_pH - Bot_O2 - Int_Organic_P - Surf_Color - Bot_Salinity - Bot_Nitrit_N, data = env_zoopl%>% select(-c(Station, Lat, Long)))
# vif.cca(zoopl_CCA)[which.max(vif.cca(zoopl_CCA))]
# 
# 
# plot(zoopl_CCA)





zoopl_cca_0 <- cca(zoopl[, -1] ~ 1, data =  env_zoopl%>% select(-c(Station, Lat, Long)))



zoopl_cca_reduced_forw <- ordistep(zoopl_cca_0, scope = formula(zoopl_CCA), direction = "forward")

plot(zoopl_cca_reduced_forw)


# ordistep(zoopl_CCA, direction = "backward")
# 
# zoopl_cca_reduced <- 
#   cca(formula = zoopl[, -1] ~ Depth + Transparency + Surf_Salinity + Surf_Turbidity +
#       Surf_Susp + Interm_Susp + Chla_surface + Chla_bottom + Bot_Color + Bot_Si + Bot_Organic_P +
#       Int_pH + Int_Nitrit_N + Int_Nitrat_N + Int_Si + Surf_O2 + Surf_pH + Surf_Nitrat_N + Surf_Si,
#     data = env_zoopl)
# 
# 
# 
# anova(zoopl_cca_reduced)
# anova(zoopl_cca_reduced, by = "axis")
# anova(zoopl_cca_reduced, by = "margin")
# 
# 
# zoopl_cca_reduced_2 <- 
#   cca(formula = zoopl[, -1] ~ Transparency + Surf_Salinity + Surf_Turbidity +
#         Chla_surface + Bot_Color + Bot_Organic_P +
#         Int_Nitrit_N + Surf_O2 + Surf_pH + Surf_Si,
#       data = env_zoopl)
# 
# 
# anova(zoopl_cca_reduced_2, by = "axis")
# anova(zoopl_cca_reduced_2, by = "margin")
# 
# plot(zoopl_cca_reduced_2, display = c("sites", "cn"), choice = c(1,2))
# 
# 
# scores(zoopl_cca_reduced_2)$sites

anova(zoopl_cca_reduced_forw)
anova(zoopl_cca_reduced_forw, by = "axis")
anova(zoopl_cca_reduced_forw, by = "margin")


zoopl_CCA_scores <- as.data.frame(scores(zoopl_cca_reduced_forw, choices = 1:3)$sites) 

zoopl_CCA_scores$Station_ID <- as.numeric(row.names(zoopl_CCA_scores))

zoopl_CCA_scores2 <- cbind(zoopl_CCA_scores[order(zoopl_CCA_scores$Station_ID), ],env_zoopl%>% select(c(Station, Lat, Long)))




# Map of Zooplancton  CCA distributions by interpolation

gginterp <- function(x, y, envir, nx, ny){
  # функция интерполирует значения переменных среды
  require(akima)
  require(reshape2)
  # Интерполяция
  fld <- interp(x = x, y = y, z = envir, xo=seq(min(x), max(x), length.out = nx), yo=seq(min(y), max(y), length.out = ny), linear = FALSE, extrap = F)
  # Перевод результатов в "длинную" форму
  df_env <- melt(fld$z, na.rm = TRUE)
  colnames(df_env) <- c("x", "y", "envir")
  # Замена по порядковым номерам настоящими значениями переменных среды
  df_env$x <- fld$x[df_env$x]
  df_env$y <- fld$y[df_env$y]
  return(df_env)
}

ggmapinterp <- function(x, y, envir, nx, ny){
  # функция рисует график интерполированной переменной
  df_env <- gginterp(x, y, envir, nx, ny)
  require(ggplot2)
  p <- ggplot(data = df_env, aes(x = x, y = y)) +
    geom_tile(aes(fill = envir)) # + stat_contour(colour = "gray40")
  return(p)
}



Pl_zoopl_CCA1_interp <- ggmapinterp(x = zoopl_CCA_scores2$Long, y = zoopl_CCA_scores2$Lat, envir = zoopl_CCA_scores2$CCA1, nx = 200, ny = 200) 


Pl_zoopl_CCA1_interp <- Obskaya_bay_cover(Pl_zoopl_CCA1_interp) + 
  scale_fill_gradient(low = "red", high = "yellow") + 
  labs(fill = "CCA1")


ggsave("figures/zoopl_CCA1_splines.png", Pl_zoopl_CCA1_interp, dpi = 600)

Pl_zoopl_CCA2 <- ggmapinterp(x = zoopl_CCA_scores2$Long, y = zoopl_CCA_scores2$Lat, envir = zoopl_CCA_scores2$CCA2, nx = 200, ny = 200) 


Pl_zoopl_CCA2 <- Obskaya_bay_cover(Pl_zoopl_CCA2) + 
  scale_fill_gradient(high = "darkgreen", low = "white") + 
  labs(fill = "CCA2")



Pl_zoopl_CCA3 <- ggmapinterp(x = zoopl_CCA_scores2$Long, y = zoopl_CCA_scores2$Lat, envir = zoopl_CCA_scores2$CCA3, nx = 200, ny = 200) 


Pl_zoopl_CCA3 <- Obskaya_bay_cover(Pl_zoopl_CCA3) + 
  scale_fill_gradient(high = "blue", low = "white") + 
  labs(fill = "CCA2")



# Визуализация на картес помощью GAM

fit_zoopl_CCA1 <- gam(formula = CCA1  ~ s(Long, Lat, bs = "tp"), data = zoopl_CCA_scores2, family = gaussian)

zoopl_CCA1_pred <- predict(fit_zoopl_CCA1, newdata = grid_data, type = "response")



Pl_zoopl_CCA1 <- ggplot() + 
  geom_tile(data = grid_data, aes(x = Long, y = Lat, fill = (zoopl_CCA1_pred), group = 1))  + 
  scale_fill_gradient(low = "red", high = "yellow") +
  labs(fill = "CCA1")

Pl_zoopl_CCA1 <- Obskaya_bay_cover(Pl_zoopl_CCA1)


ggsave("figures/zoopl_CCA1_gam.png", Pl_zoopl_CCA1, dpi = 600)


######################
fit_zoopl_CCA2 <- gam(formula = CCA2  ~ s(Long, Lat, bs = "tp"), data = zoopl_CCA_scores2, family = gaussian)

zoopl_CCA2_pred <- predict(fit_zoopl_CCA2, newdata = grid_data, type = "response")



Pl_zoopl_CCA2 <- ggplot() + 
  geom_tile(data = grid_data, aes(x = Long, y = Lat, fill = (zoopl_CCA2_pred), group = 1))  + 
  scale_fill_gradient(low = "yellow", high = "darkgreen") +
  labs(fill = "CCA2")

Pl_zoopl_CCA2 <- Obskaya_bay_cover(Pl_zoopl_CCA2)


ggsave("figures/zoopl_CCA2_gam.png", Pl_zoopl_CCA2, dpi = 600)


######################

fit_zoopl_CCA3 <- gam(formula = CCA3  ~ s(Long, Lat, bs = "tp"), data = zoopl_CCA_scores2, family = gaussian)

zoopl_CCA3_pred <- predict(fit_zoopl_CCA3, newdata = grid_data, type = "response")



Pl_zoopl_CCA3 <- ggplot() + 
  geom_tile(data = grid_data, aes(x = Long, y = Lat, fill = (zoopl_CCA3_pred), group = 1))  + 
  scale_fill_gradient(low = "yellow", high = "gray50") +
  labs(fill = "CCA3")

Pl_zoopl_CCA3 <- Obskaya_bay_cover(Pl_zoopl_CCA3)


ggsave("figures/zoopl_CCA3_gam.png", Pl_zoopl_CCA3, dpi = 600)



# # BioEnv ##########################
# 
# library(parallel)
# 
# bioenv(zoopl[, -1], env_zoopl %>% select(-c(Station, Lat, Long)), trace = TRUE, parallel = 4, upto = 3)
# 
# 








