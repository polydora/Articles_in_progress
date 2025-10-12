library(robis)
library(dplyr)

# species_list <- match_taxa("Dipolydora quadrilobata")

occurrences_with_depth <- occurrence(
  scientificname = "Nicomache minor",
  fields = c("scientificName", "decimalLongitude", "decimalLatitude",
             "depth", "minimumDepthInMeters", "maximumDepthInMeters",
             "year", "country", "datasetName")
)


occurrences_clean <- occurrences_with_depth %>%
  filter(!is.na(minimumDepthInMeters) | !is.na(maximumDepthInMeters)) %>%
  mutate(
    depth_mean = ifelse(!is.na(minimumDepthInMeters) & !is.na(maximumDepthInMeters),
                        (minimumDepthInMeters + maximumDepthInMeters) / 2,
                        coalesce(minimumDepthInMeters, maximumDepthInMeters))
  )

occurrences_clean$scientificName

occurrences_clean %>%
  filter(decimalLatitude > 66) %>%
  pull(depth) %>%
  median

OBIS_Nicomache <-
  occurrences_clean %>%
  mutate(Species = "Nicomache minor")

OBIS_Portlandia <-
  occurrences_clean %>%
  mutate(Species = "Portlandia arctica")



OBIS_Galathowenia <-
  occurrences_clean %>%
  mutate(Species = "Galathowenia oculata")


OBIS_Macoma <-
  occurrences_clean %>%
  mutate(Species = "Macoma balthica")



OBIS_Dipolydora <-
  occurrences_clean %>%
  mutate(Species = "Dipolydora quadrilobata")

OBIS_Mytilus <-
  occurrences_clean %>%
  mutate(Species = "Mytilus edulis")


OBIS_Mytilus <-
  OBIS_Mytilus %>%
  select(decimalLatitude,decimalLongitude,depth,depth_mean, Species)

OBIS_Dipolydora <-
  OBIS_Dipolydora %>%
  select(decimalLatitude,decimalLongitude,depth,depth_mean, Species)


OBIS_Macoma <-
  OBIS_Macoma %>%
  select(decimalLatitude,decimalLongitude,depth,depth_mean, Species)

OBIS_Nicomache <-
  OBIS_Nicomache %>%
  select(decimalLatitude,decimalLongitude,depth,depth_mean, Species)


OBIS_Portlandia <-
  OBIS_Portlandia %>%
  select(decimalLatitude,decimalLongitude,depth,depth_mean, Species)

occurences <-
rbind(OBIS_Mytilus, OBIS_Dipolydora, OBIS_Macoma, OBIS_Galathowenia, OBIS_Portlandia, OBIS_Nicomache)


write.table(x = occurences, file = "Data/occurences_depth.csv", sep = ";", row.names = F)
