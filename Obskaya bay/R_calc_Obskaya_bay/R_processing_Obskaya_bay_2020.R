
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
stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)




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



## Визуализация распределения параметров среды

# Станции
Pl_all_stat <- Pl_Obskaya_bay + 
  geom_point(data = stations, aes(x = Long, y = Lat, group = 1, color = factor(Exclude) ), size = 0.2) + 
  scale_color_manual(values = c("black", "blue")) + 
  guides(color = "none")


# Spatial grides ############

grid_data <- expand.grid(Lat = seq(from = Ob_y[1], to =Ob_y[2], length.out = 100), Long =  seq(from = Ob_x[1], to =Ob_x[2] +0.1 , length.out = 100))



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
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank())+
  labs(fill = "Depth")
  



# Transparency  #######################

transp <- stat_full %>%  mutate(Transparency = rowMeans(select(., Transparency_aug_20, Transparency_sep_20), na.rm = TRUE)) %>% select(Station, Long, Lat, Transparency)



fit_transp <- gam(formula = log(Transparency +1)  ~ s(Long, Lat, bs = "tp"), data = transp, family = gaussian)

transp_pred <- exp(predict(fit_transp, newdata = grid_data, type = "response")) - 1



Pl_transp <- ggplot() + 
  geom_tile(data = grid_data, aes(x = Long, y = Lat, fill = (transp_pred), group = 1))  + 
  scale_fill_gradient(low = "black", high = "white", breaks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5)) +   
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank())+
  labs(fill = "Transparency (m)")



# Slinity ###############

sal_surf_aug <- stat_full %>%  select(Station, Long, Lat, Surface_Salinity_Aug_20) %>% mutate(Season = "August", Layer = "Surface") %>% rename(Salinity = Surface_Salinity_Aug_20)

sal_surf_sep <- stat_full %>%  select(Station, Long, Lat, Surface_Salinity_Sep_20) %>% mutate(Season = "September", Layer = "Surface") %>% rename(Salinity = Surface_Salinity_Sep_20)


sal_bottom_aug <- stat_full %>%  select(Station, Long, Lat, Bottom_Salinity_Aug_20) %>% mutate(Season = "August", Layer = "Bottom")%>% rename(Salinity = Bottom_Salinity_Aug_20)

sal_bottom_sep <- stat_full %>%  select(Station, Long, Lat, Bottom_Salinity_Sep_20) %>% mutate(Season = "September", Layer = "Bottom") %>% rename(Salinity = Bottom_Salinity_Sep_20)

salinity <- rbind(sal_surf_aug, sal_surf_sep, sal_bottom_aug, sal_bottom_sep)




fit_sal <- gam(formula = log(Salinity +1)  ~ s(Long, Lat, bs = "tp", by = interaction(Layer, Season)) + Layer + Season, data = salinity, family = gaussian)

summary(fit_sal)

grid_data_season <-
  rbind(grid_data %>% mutate(Season = "August", Layer = "Surface"),
      grid_data %>% mutate(Season = "September", Layer = "Surface"),
      grid_data %>% mutate(Season = "August", Layer = "Bottom"),
      grid_data %>% mutate(Season = "September", Layer = "Bottom")
)



sal_pred <- exp(predict(fit_sal, newdata = grid_data_season, type = "response")) - 1


Pl_sal <- 
  ggplot(data = grid_data_season, aes(x = Long, y = Lat, fill = (sal_pred), group = 1)) + 
  geom_tile()  + 
  scale_fill_gradient(low = "yellow", high = "red") + 
  facet_grid(Season ~ Layer) +   
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank())+
  labs(fill = "Salinity(psu)")

