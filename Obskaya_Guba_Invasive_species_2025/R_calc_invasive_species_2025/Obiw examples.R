# отработка принципов работы с базой Obis (морские виды)


library(robis)
library(dplyr)
library(ggplot2)


occ <- occurrence("Abra aequalis")
occ

ggplot(occ) +
  geom_bar(aes(date_year), stat = "count", width = 1)

occurrence("Abra alba", geometry = "POLYGON ((2.59689 51.16772, 2.62436 51.14059, 2.76066 51.19225, 2.73216 51.20946, 2.59689 51.16772))")


map_leaflet(occurrence("Senecella siberica"))

map_ggplot(occurrence("Mytilus trossulus")) + 
  theme_bw()


map_ggplot(occurrence("Eurytemora affinis")) + 
  theme_bw()


cl <- checklist("Semelidae")
cl

ggplot(cl %>% filter(!is.na(genus))) +
  geom_bar(aes(genus)) +
  coord_flip() +
  ylab("species count")


occ <- occurrence("Abra tenuis", mof = TRUE)

mof <- unnest_extension(occ, extension = "MeasurementOrFact", fields = c("scientificName", "decimalLongitude", "decimalLatitude"))
mof


occurrence("Abra tenuis", mof = TRUE, measurementtype = "biomass") %>%
  unnest_extension(extension = "MeasurementOrFact")



