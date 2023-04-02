
library(waver)

fetch <- fetch_len_multi(pts = points2, bearings = c(0, 90, 180, 270), shoreline = map2, dmax = 10000, spread = 0, method = "btree", projected = TRUE)

fetch_mean <- apply(fetch, MARGIN = 1, FUN = mean)

qplot(myt_site$Average_Fetch, fetch_mean/100)


ggplot(map) + geom_sf() + geom_point(data = myt_site[89, ], aes(x = Lon, y = Lat), size = 3, 
                                     shape = 21, fill = "darkred")



# if (!require(remotes))
#   install.packages("remotes")

# Install windfetch
# remotes::install_github("blasee/windfetch")

library(windfetch)
library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1; sf_use_s2() is TRUE

# Read a shapefile of the North Carolina coastline
nc = st_read(system.file("shape/nc.shp", package = "sf"), quiet = TRUE)

# plot(nc)


# Create an sf geometry
ncg = st_geometry(nc)

# Project the coastline using the NAD83 projection
nc_proj = st_transform(ncg, "epsg:32119")
plot(nc_proj)

# Create a dataframe of the sites to calculate wind fetch at
sites_df = data.frame(lon = c(-76, -76, -77.3),
                      lat = c(36, 35.3, 34.5))

# Include spatial reference information by transforming to an sf object
sites_points = st_as_sf(sites_df, coords = c("lon", "lat"), crs = "epsg:4326")

# Project the points onto the same projection as the coastline
sites_points_proj = st_transform(sites_points, "epsg:32119")


plot(ncg, col = "lightgrey", border = "grey")
plot(sites_points, pch = "x", add = TRUE)
text(st_coordinates(sites_points), labels = paste("Site", 1:3), pos = 3)


# Calculate wind fetch by passing in the projected sf polygons object (nc_proj)
# and the projected points object (sites_points_proj) to the windfetch function.
my_fetch_proj = windfetch(nc_proj, sites_points_proj, progress_bar = FALSE)
#> checking site locations are not on land

summary(my_fetch_proj)

plot(my_fetch_proj, axes = TRUE)
plot(nc_proj, col = "lightgrey", border = "grey", add = TRUE)


# Transform the CRS back to lats and longs
my_fetch_latlon = crs_transform(my_fetch_proj, "epsg:4326")


# Plot the fetch vectors and the coastline in lat lon space
plot(my_fetch_latlon, axes = TRUE)
plot(ncg, col = "lightgrey", border = "grey", add = TRUE)

# Transform the fetch object to an sf object
my_fetch_sf = as_sf(my_fetch_latlon)
my_fetch_sf

# Export the fetch vectors to an ESRI shape file for further investigation
st_write(nc_fetch_sf, "nc_fetch.shp", driver = "ESRI Shapefile")



example(windfetch)

######## Начало ввода данных для карт ###########################

library(ggmap)
library(mapproj)
library(maps)
library(rgeos) #этот пакет содержит какую-то хрень, которая позволяет обойти проблему пр чтении фалов средсвами maptools. 
#Att! Этот пакет должен быть загружен до maptools
library(mapdata)
library(maptools) # Rgshhs
library(PBSmapping)
library(sf)


library(readxl)
library(dplyr)
library(cowplot)
library(png)
library(reshape2)

library(mgcv)
library(vegan)


# Создаем карту по обычным лекалам


Kand_x <- c(31.75, 36.2)
Kand_y <- c(65.6, 67.25)


gshhs.l.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_l.b"

gshhs.f.b <- "d:/Data_LMBE/Maps/Gshhs/gshhs_f.b"
wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"
wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"

wdb_rivers.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_rivers_f.b"
wdb_borders.f.b <- "d:/Data_LMBE/Maps/Gshhs/wdb_borders_f.b"


# Kand_f <- getRgshhsMap(fn = gshhs.f.b, xlim = Kand_x, ylim = Kand_y)

# save(Kand_f, file = "Kand_f.RData")

load("Kand_f.RData")

plot(Kand_f)



############################3

##### Data reading
myt <- read.table("data/Distred_samples_fetch_corrected_2021.csv", header = T, sep = ",")

sal <- read.table("data/Distred_samples_salinity_2021.csv", header = T, sep = ",")

myt <- merge(myt, sal, all = T)

river_full <- read.table("data/Rivers_2021.csv", sep = ",", header = T)

river <- river_full %>% select(-Source)


ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel", "Karel"), Port = c("Kandalaksha", "Vitino", "Umba", "Chupa", "Keret", "Kovda"), Status = c("Active", "Active", "Abandoned", "Abandoned", "Abandoned", "Abandoned"  ), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178, 66.696754), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656, 32.875396))


sites_fetch_df <- read.table("data/Distred_samples_fetch_values_2021.csv", sep = ",", header = T)

nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


# The distance to mouth of the neares river 

df_river <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = river)

df_river[1,] <- NA

for(i in 1:nrow(myt)) {
  df_river[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = river)
  df_river$Site[i] <- as.character(myt$Site)[i]
}


names(df_river) <- c( "Shore_river", "River",  "Drainage_Area", "River_Size", "Lat_river", "Lon_river", "Min_dist_river", "Site" )



# The distance to mouth of the neares Large river 

river_Large <- river %>% filter(River_Size == "Large")

df_river_Large <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = river_Large)

df_river_Large[1,] <- NA

for(i in 1:nrow(myt)) {
  df_river_Large[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = river_Large)
  df_river_Large$Site[i] <- as.character(myt$Site)[i]
}

names(df_river_Large) <- c( "Shore_river_Large", "River_Large",  "Drainage_Area_Large", "River_Size_Large", "Lat_river_Large", "Lon_river_Large", "Min_dist_river_Large", "Site" )



# The distance to the neares port 

df_port <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(myt)) {
  df_port[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(myt$Site)[i]
}

names(df_port) <- c("Shore_port",  "Port","Port_Status", "Lat_port", "Lon_port", "Min_dist_port", "Site")

# Merging all data in one data set

d1 <- cbind(myt, df_river %>% select( - Site))

d2 <- cbind(d1, df_river_Large %>% select( - Site))

d3 <- cbind(d2, df_port %>% select(-Site))

d4 <- merge(d3, sites_fetch_df, by = "Site")

myt_full <- d4 %>% select(-River_Size_Large)

Shore_boundary = c(67.162360, 32.332371)

# Conversion of coordinates to radians
Shore_boundary <- Shore_boundary*pi/180

# Calculating the distance from each site to the top of the Kandalksha bay
myt_full$Dist_cut <- with(myt_full, acos(sin(Shore_boundary[1])*sin(Lat*pi/180) + cos(Shore_boundary[1])*cos(Lat*pi/180)*cos(Shore_boundary[2] - Lon*pi/180))) * 6371

myt_full <- myt_full %>% mutate(Prop_T = N_T / (N_T + N_E))

# sites_excluded <- c("chupa_fg", "umba_pioner", "umba_06", "umba_fg", "umba_sovhoz", "umba_kamni", "umba_bridge", "umba_pikut", "padan", "porya", "Vor5", "Ovech", "oenij", "Korg", "Mat", "Mal", "salnij", "Lubch", "kanal",  "Vor4", "Vor2", "Kurt", "Ryazh4", "Ryazh5", "Youzh")
# 
# myt_full <- myt_full %>% filter(! Site %in% sites_excluded) 

myt_full$Lat2 <- myt_full$Lat + rep(seq(0, 0.00000005, by = 0.00000001), nrow(myt_full)/6)

myt_full$Lon2 <- myt_full$Lon + rep(seq(0, 0.00000005, by = 0.00000001), nrow(myt_full)/6)

myt_full$Position <- factor(myt_full$Position)

myt_full$Position <- relevel(myt_full$Position, ref = "Bottom")

myt_full$Port_Status <- factor(myt_full$Port_Status)

myt_full$Port_Status <- relevel(myt_full$Port_Status, ref = "Abandoned")

myt_full$River_Size <- factor(myt_full$River_Size)

myt_full$River_Size <- relevel(myt_full$River_Size, ref = "Small")

myt_full$Site <- factor(myt_full$Site)

myt_site <- myt_full %>% 
  group_by(Shore, Site) %>% 
  select(Lat, Lon, N_T, N_E, Salinity, Min_dist_river, River, River_Size, Min_dist_river_Large, Min_dist_port, Port, Port_Status, Average_Fetch, Dist_cut) %>% 
  summarise(Lat = mean(Lat), Lon = mean(Lon), N_T = sum(N_T), N_E = sum(N_E), Salinity = mean(Salinity), Min_dist_river = mean(Min_dist_river), River = unique(River), River_Size = unique(River_Size), Min_dist_river_Large = mean(Min_dist_river_Large),  Min_dist_port = mean(Min_dist_port), Port = unique(Port), Port_Status = unique(Port_Status), Average_Fetch = mean(Average_Fetch),   Dist_cut = mean(Dist_cut)) %>% 
  mutate(Prop_T = N_T/(N_T+N_E))

########## Конец вода данных для карт ########################################

points <- myt_site %>% select(Lat, Lon)  

points_sf <- st_as_sf(points, coords = c("Lon", "Lat") )

st_crs(points_sf) <-  st_crs(Kand_f)

map_sf <- st_as_sf(Kand_f, crs = st_crs(Kand_f))


plot(map_sf)
plot(points_sf, add = T)


map_sp <- st_transform(map_sf, crs = "+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +units=m +no_defs")

points_sp <- st_transform(points_sf, crs = "+proj=tmerc +lat_0=0 +lon_0=27 +k=1 +x_0=3500000 +y_0=0 +ellps=intl +units=m +no_defs")

plot(map_sp)
plot(points_sp, add = T)


# citation("windfetch")

my_fetch_proj = windfetch(map_sp, points_sp, progress_bar = T, max_dist = 100, n_directions = 18)

fetch_long <- as.data.frame(summary(my_fetch_proj))
str(fetch_long)


fetch_prev <- myt_site %>% select(Site, Average_Fetch) 

fetch_prev$site_name <- paste("Site", 1:nrow(myt_site))


library(reshape2)
dcast(data = fetch_long, formula = site_name ~ quadrant ) %>% merge(., fetch_prev ) %>% write.table(., "clipboard", sep = "\t", row.names = F)
  


fetch_prev <- merge(fetch_prev, fetch_sum)

library(units)

qplot(fetch_prev$Mean_fetch/1000, fetch_prev$Average_Fetch) + geom_abline()

ggplot(map_sp) + geom_sf() + geom_point(data = points_sf, aes(x = Lon, y = Lat), size = 4, 
                                     shape = 23, fill = "darkred")




