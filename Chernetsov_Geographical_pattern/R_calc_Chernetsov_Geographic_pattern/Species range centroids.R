library(dplyr)
library(broom)
library(readxl)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(reshape2)
library(scatterpie)



# Поверка Соответствия полигонов гнездованию и зимовкам  
# В некоторых случаях их имена меняются местами


plot_range <- function(x = 1){
  files <- list.files("Data/Bird_Range_Polygons/")
  range_data <- st_read(paste("Data/Bird_Range_Polygons/", files[x], sep =""))
  range_data$Name = c("Wintering", "Nesting")
  ggplot()+
    geom_sf(data = range_data, aes(fill = Name), alpha = 0.9) +
    ggtitle(files[x])
}

files <- list.files("Data/Bird_Range_Polygons/")



plot_range(15)

#

# write.table(files, "clipboard", sep = "\t", row.names = F)






### Вычисление центроидов ареалов гнездования и зимовки видов

species <- read_excel("Data/Species_polygon_revision.xlsx")  

# Sci_Name = species$Sci_Name[1]

centroids <- function(Sci_Name){
  range_data <- st_read(paste("Data/Bird_Range_Polygons/", Sci_Name, ".kml", sep = ""))
  if(Sci_Name == "Ficedula parva") range_data$Name = c("Nesting", "Wintering", "Wintering")
  else
    if(species$Correct_area_Type[species$Sci_Name == Sci_Name] == 0) range_data$Name = c("Wintering", "Nesting") else range_data$Name = c("Nesting", "Wintering")
    
    range_data %>% 
      group_by(Name) %>% 
      st_make_valid() %>%
      # Упрощение геометрии (уменьшение детализации)
      st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
      st_centroid() %>% 
      st_coordinates() %>% 
      as.data.frame() -> range_mean
    
    df <- if(Sci_Name == "Ficedula parva") data.frame(Range_type = c(range_data$Name[1], range_data$Name[2], range_data$Name[3]), Species = Sci_Name) else data.frame(Range_type = c(range_data$Name[1], range_data$Name[2]),
                 Species = Sci_Name)
    
    cbind(df, range_mean)
    
}

species_range_sentroids <- NULL

for(Sci_Name in species$Sci_Name){
  species_range_sentroids <- 
    rbind(species_range_sentroids, centroids(Sci_Name))  
}

write.table(x = species_range_sentroids, "clipboard", sep = "\t", row.names = F, dec = "," )


# Особые центроиды


library(sf)

P_collybita_range_data <- st_read("Data/Bird_Range_Polygons/Phylloscopus collybita.kml")

P_tristis_range_data <- st_read("Data/Bird_Range_Polygons/Phylloscopus tristis.kml")

plot(P_collybita_range_data[2,1])
plot(P_tristis_range_data[1,1])

# Nesting
combined_polygon <- st_union(P_collybita_range_data[2,1], P_tristis_range_data[1,1])

combined_polygon %>% 
  group_by(Name) %>% 
  st_make_valid() %>%
  # Упрощение геометрии (уменьшение детализации)
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() -> range_mean



plot(combined_polygon)


# Wintering
combined_polygon2 <- st_union(P_collybita_range_data[1,1], P_tristis_range_data[2,1])

plot(combined_polygon2)

combined_polygon2 %>% 
  group_by(Name) %>% 
  st_make_valid() %>%
  # Упрощение геометрии (уменьшение детализации)
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() -> range_mean


############################

F_parva_range_data <- st_read("Data/Bird_Range_Polygons/Ficedula parva.kml")

F_albicilla_range_data <- st_read("Data/Bird_Range_Polygons/Ficedula albicilla.kml")


F_parva_valid <- st_make_valid(F_parva_range_data[1,1])
F_albicilla_valid <- st_make_valid(F_albicilla_range_data[1,1])

plot(F_parva_range_data[1,1])

plot(F_albicilla_range_data[1,1])


# Nesting
combined_polygon <- st_union(F_parva_valid, F_albicilla_valid)

plot(combined_polygon)

combined_polygon %>% 
  group_by(Name) %>% 
  st_make_valid() %>%
  # Упрощение геометрии (уменьшение детализации)
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() -> range_mean



# Nesting

F_parva_valid <- st_make_valid(F_parva_range_data[2,1])
F_albicilla_valid <- st_make_valid(F_albicilla_range_data[2,1])

combined_polygon <- st_union(F_parva_valid, F_albicilla_valid)

plot(combined_polygon)

combined_polygon %>% 
  group_by(Name) %>% 
  st_make_valid() %>%
  # Упрощение геометрии (уменьшение детализации)
  st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
  st_centroid() %>% 
  st_coordinates() %>% 
  as.data.frame() -> range_mean

