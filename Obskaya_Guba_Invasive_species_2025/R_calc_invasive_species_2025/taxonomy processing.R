# install.packages("worms")

library(worms)

library(dplyr)

benthic_species <- read.csv("Data/benthos_occurence_all_final.csv")
benthic_species <- benthic_species[,-1] 

plancton_species <- read.csv("Data/plancton_occurence_all_final.csv")

benthic_species$Group <- "Benthos"
plancton_species$Group <- "Plancton"

all_species <- rbind(benthic_species, plancton_species)


species_list <- unique(PNIS_table_short$species)

taxonomy <- wormsbynames(taxon_names = species_list, marine_only = F)

str(taxonomy)

names(taxonomy)

taxonomy <- taxonomy %>% select(scientificname, authority, kingdom, phylum, class, order, family, genus )



write.csv(taxonomy,"Data/taxonomy_PNIS.csv", row.names = F )
