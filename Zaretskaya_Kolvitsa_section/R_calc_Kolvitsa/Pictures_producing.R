library(dplyr)
library(ggplot2)


occur <- read.table("Data/occurences_depth.csv", sep = ";", header = T)

# Границы Северной Атлантики
lat_min = 0
lat_max = 80
lon_min = -80
lon_max = 20


# границы Ледовитого океана
lat_min = 60
lat_max = 90
lon_min = -180
lon_max = 180

occur_reduced <-
  occur %>%
  filter((decimalLongitude > -180 & decimalLongitude < 180) & (decimalLatitude > 60 & decimalLatitude < 90)) # Обрезаем по граница Ледовитого океана

occur_reduced %>%
  group_by(Species) %>%
  summarise(N = n(), Median_depth = median(depth), Min_depth = min(depth), Max_depth = max(depth))


unique(occur_reduced$Species)



occur_reduced %>%
  filter(Species %in% c("Mytilus edulis", "Dipolydora quadrilobata","Galathowenia oculata",    "Portlandia arctica", "Nicomache minor"  )) %>%
ggplot(aes(x = Species, y = depth)) +
  geom_boxplot()



occur_reduced %>%
  filter(Species %in% c("Mytilus edulis", "Dipolydora quadrilobata","Galathowenia oculata",    "Portlandia arctica", "Nicomache minor"  )) %>%
  group_by(Species) %>%
  summarise(N = n(), Median_depth = median(depth), Min_depth = min(depth), Max_depth = max(depth)) %>%
  write.table("clipboard", row.names = F, sep = "\t", dec = ",")

