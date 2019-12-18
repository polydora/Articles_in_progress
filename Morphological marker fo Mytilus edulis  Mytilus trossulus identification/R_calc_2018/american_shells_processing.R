library(ggplot2)
library(dplyr)

sara_Z <- read.table("Data/Z_index_measurement_and_Sara_genetic_data.csv", sep = ",", header = T)
# Z <- read.table("Data/Z_index_measurement_of_Sara_data.csv", sep = ",", header = T)
# 
# Z$sample_ID[!unique(as.character(Z$sample_ID)) %in% unique(as.character(sara$sample_ID))]
# 
# 
# 
# sara_Z <- merge(sara, Z)
# 
# sara_Z[is.na(as.numeric(as.character(sara_Z$trossulus))), ]

# write.table(sara_Z, "Z_index_measurement_and_Sara_genetic_data.csv", sep = ",", row.names = F)

str(sara_Z)


sara_Z$Sp <- ifelse(sara_Z$trossulus > 0.5, "M.trossulus", "M.edulis")


prop_tros <- sara_Z %>% group_by(pop_ID) %>% summarise(Ptros = mean(trossulus > 0.5))

prop_tros$ord <- order(prop_tros$Ptros)


sara_Z[! sara_Z$pop_ID %in% c("CBSC", "CBCP"), ] %>% group_by(Sp, MorphotypE) %>% summarise(Prop = n())


sara_Z$Z <- with(sara_Z, a/l)

sara_Z2 <- merge(sara_Z, prop_tros)



ggplot(sara_Z2, aes(x = Z)) + geom_histogram(binwidth = 0.2) + facet_wrap(~Ptros, scales = "free_y" )

ggplot(sara_Z2, aes(x = L, y = l, color = Sp)) + geom_point() + geom_smooth(method = "lm")





