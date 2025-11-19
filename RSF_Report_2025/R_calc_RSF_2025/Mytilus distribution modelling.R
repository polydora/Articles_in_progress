# В этом коде строится модель распределения видов комплекса "M.edulis"

library(robis)
library(dplyr)

# species_list <- match_taxa("Dipolydora quadrilobata")

occurrences_M.edulis <- occurrence(
  scientificname = "Mytilus edulis",
  fields = c("scientificName", "decimalLongitude", "decimalLatitude")
)


occurrences_M.trossulus <- occurrence(
  scientificname = "Mytilus trossulus",
  fields = c("scientificName", "decimalLongitude", "decimalLatitude")
)


occurrences_M.galloprovincialis <- occurrence(
  scientificname = "Mytilus galloprovincialis",
  fields = c("scientificName", "decimalLongitude", "decimalLatitude")
)


df_Mytilus_occurence <- 
  rbind(occurrences_M.edulis, occurrences_M.trossulus, occurrences_M.galloprovincialis)


df_Mytilus_occurence <-
df_Mytilus_occurence %>% 
  select(-id) %>% 
  rename(Lon = decimalLongitude, Lat = decimalLatitude, Sci_Name = scientificName)



write.csv(x = df_Mytilus_occurence, file = "Data/Mytilus_complex_occurence.csv" )


plot(df_Mytilus_occurence$Lon, df_Mytilus_occurence$Lat)



