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

sara_Z$Sp <- ifelse(sara_Z$trossulus > 0.5, "M.trossulus", "M.edulis")

prop_tros <- sara_Z %>% group_by(pop_ID) %>% summarise(Ptros = mean(trossulus > 0.5))

prop_tros2 <- prop_tros[order(prop_tros$Ptros),] 

prop_tros2$order <- 1:nrow(prop_tros)



sara_Z %>% group_by(Sp, MorphotypE) %>% summarise(Prop = n())


sara_Z$Z <- with(sara_Z, a/l)

sara_Z2 <- merge(sara_Z, prop_tros)

sara_Z2$ind <- ifelse(sara_Z2$MorphotypE == "T",  1, 0)

sara_Z2$pop_ID <- factor(sara_Z2$pop_ID, levels = levels(sara_Z2$pop_ID)[order(prop_tros$Ptros)] )

levels(sara_Z2$pop_ID)[order(prop_tros$Ptros)]

included_in_paper <- c("BI", "PH", "VH", "CBE", "MDRE", "MDRW", "CBSC", "CBCP")

sara_Z2$In_Paper <- ifelse(sara_Z2$pop_ID %in% included_in_paper, "in paper", "not in paper")


# Распределение Z индекса в данных Сары

ggplot(sara_Z2, aes(y = Z, x = Sp)) + geom_violin(aes(fill = In_Paper)) + facet_wrap( ~ pop_ID, ncol = 3) + theme_bw() + theme(legend.title = element_blank()) + geom_text(data = prop_tros, aes(x = 1.5, y = 0.3, label = round(Ptros, 2) ) )





# ДАнные по Белому морю

myt_white <- read.table("Data/GDS_for_astred.csv", sep = ";", header = T)

myt_white$Sp <- ifelse(myt_white$structure > 0.5, "M.trossulus", "M.edulis")

prop_tros_white <- myt_white %>% group_by(population) %>% summarise(Ptros = mean(Sp == "M.trossulus")) 


myt_white$population <- factor(myt_white$population, levels = levels(myt_white$population)[order(prop_tros_white$Ptros)])

ggplot(myt_white, aes(y = z, x = Sp)) + geom_violin(fill = "gray") + theme_bw() + theme(legend.title = element_blank()) + facet_wrap(~population, ncol = 2) + ylim(0,1) + geom_text(data = prop_tros_white, aes(x = 1.5, y = 0.3, label = round(Ptros, 2) ) )

