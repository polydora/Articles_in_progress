# В этом коде сводятся в единый список все PNIS, отловленные по базе https://www.iucngisd.org/gisd/ и выявленные в предыдущем отчете по "Топ 100"


library(readxl)
library(dplyr)

gisd <- read_excel("Data/export_gisd_PNIS.xlsx")


old_bethos <- read.table("Data/benthos_occurence_all_final.csv", sep = ",", header = T)
old_bethos<-
  old_bethos %>% 
  select(-X)


old_plancton <- read.table("Data/plancton_occurence_all_final.csv", sep = ",", header = T)


old_all <- rbind(old_bethos, old_plancton)

old_all_PNIS <- 
  old_all %>% 
  filter(Status == "PNIS") 


old_all_PNIS_species <-
  unique(old_all_PNIS$species)


gisd[!gisd$Species %in%  old_all_PNIS_species, ] %>% 
  write.table("clipboard", row.names = F, sep = "\t")
