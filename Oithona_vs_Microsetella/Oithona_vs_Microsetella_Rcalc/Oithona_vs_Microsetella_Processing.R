library(reshape2)
library(dplyr)


cop_init <- read.table("Data/Oithona_Microsetella.csv", sep = ",", header = T)

names(cop_init)



cop <- melt(cop_init, id.vars = c("Day","Month","Year","Day.of.year"), variable.name = "Stage", value.name = "N") 

