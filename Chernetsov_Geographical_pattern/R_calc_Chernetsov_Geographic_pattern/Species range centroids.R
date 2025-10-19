# Поверка Соответствия полигонов гнездованию и зимовкам  
# В некоторых случаях их имена меняются местами

plot_range <- function(x = 1){
  files <- list.files("Data/Bird_Range_Polygons/")
  range_data <- st_read(paste("Data/Bird_Range_Polygons/", files[x], sep =""))
  if(files[x] == "Ficedula parva.kml") range_data$Name = c("Wintering", "Nesting", "Nesting") else range_data$Name = c("Wintering", "Nesting")
  ggplot()+
    geom_sf(data = range_data, aes(fill = Name), alpha = 0.9) +
    ggtitle(files[x])
}


plot_range(16)
#

write.table(files, "clipboard", sep = "\t", row.names = F)



### Вычисление центроидов ареалов гнездования и зимовки видов

species <- read_excel("Data/Table1.xls", sheet = "Species")  

centroids <- function(Sci_Name){
  range_data <- st_read(paste("Data/Bird_Range_Polygons/", Sci_Name, ".kml", sep = ""))
  if(Sci_Name == "Ficedula parva") range_data$Name = c("Nesting", "Wintering", "Wintering")
  else
    if(species$Reversed[species$Sci_Name == Sci_Name] == 0) range_data$Name = c("Wintering", "Nesting") else range_data$Name = c("Nesting", "Wintering")
    
    range_data %>% 
      group_by(Name) %>% 
      st_make_valid() %>%
      # Упрощение геометрии (уменьшение детализации)
      st_simplify(preserveTopology = TRUE, dTolerance = 0.01) %>%
      st_centroid() %>% 
      st_coordinates() %>% 
      as.data.frame() -> range_mean
    
    df <- if(Sci_Name == "Ficedula parva") data.frame(Range_type = c(range_data$Name[1], range_data$Name[2], range_data$Name[3]), Species = Sci_Name) 
    else 
      data.frame(Range_type = c(range_data$Name[1], range_data$Name[2]),
                 Species = Sci_Name)
    
    cbind(df, range_mean)
    
}

species_range_sentroids <- NULL

for(Sci_Name in species$Sci_Name){
  species_range_sentroids <- 
    rbind(species_range_sentroids, centroids(Sci_Name))  
}

write.table(x = species_range_sentroids, "clipboard", sep = "\t", row.names = F, dec = "," )
