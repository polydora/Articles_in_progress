
library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(readxl)


tuv <- read_excel("Data/TuMyt_2009_2010.xlsx")
str(tuv)

tuv_reduc <- tuv %>% filter(!is.na(PT))

tuv_demogr <- tuv_reduc %>% select(Age2, Age3,Age4, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, PT, N, W, OGP, max_L)



colSums(is.na(tuv_demogr))

tuv_predictors <-  tuv_reduc %>% select(Depth, Dist, Algae, Substrate)






tuv_cca <- cca(tuv_demogr ~ Substrate + Algae, data = tuv_predictors) 
vif.cca(tuv_cca)




vif.cca(tuv_cca)


plot(tuv_cca, display = c("cn", "sp"))

anova(tuv_cca)
anova(tuv_cca, by = "axis")
anova(tuv_cca, by = "margin")

plot(tuv_cca, display = c("cn", "site"))






tuv_cca_objects <- fortify(tuv_cca)

write.table(scores(tuv_cca)$species, "clipboard",  sep = "\t")
write.table(scores(tuv_cca)$sites, "clipboard",  sep = "\t")
write.table(scores(tuv_cca)$centroids, "clipboard",  sep = "\t")


tuv_cca_sites <- tuv_cca_objects %>% filter(Score == "constraints")

tuv_cca_sites$Sample <- tuv_reduc$Sample.ID


tuv_cca_sp <- tuv_cca_objects %>% filter(Score == "species")

tuv_cca_predictors  <- tuv_cca_objects %>% filter(Score == "biplot")




ggplot(data = tuv_cca_sites, aes(x = CCA1, y = CCA2)) +
  geom_point(aes(label = Sample, color = tuv_sites$Transect, size = tuv_demogr$PT), position = position_jitter(width = 0.1)) +
  geom_segment(data = tuv_cca_predictors,  aes(x = 0, y = 0, yend = CCA2, xend = CCA1), color = "blue", arrow = arrow()) +
  geom_text_repel(aes(label = tuv_predictors$Depth)) +
  geom_text_repel(data = tuv_cca_sp,  aes(x = CCA1, y = CCA2, label = Label), color = "red", size= 4)







tuv_demogr_r <- decostand(tuv_demogr, method = "total")

ord_tuv_domogr <- metaMDS(tuv_demogr_r)

plot(ord_tuv_domogr, display = c("sites"), type = "t")






tuv_ca <- cca(tuv_demogr ) 

envfitted <- envfit(tuv_ca ~ Depth + Dist, data = tuv_predictors)

plot(tuv_ca)
plot(envfitted)


gg_tuv_ca <- fortify(tuv_ca)
gg_tuv_ca_predictor <- fortify(envfitted)

gg_ord_tuv_ca_sites <- gg_tuv_ca %>% filter(Score == "sites") 
gg_ord_tuv_ca_species <- gg_tuv_ca %>% filter(Score == "species")


write.table(gg_tuv_ca_predictor, "clipboard",  sep = "\t")

write.table(gg_ord_tuv_ca_sites, "clipboard",  sep = "\t")


write.table(gg_ord_tuv_ca_species, "clipboard",  sep = "\t")



ggplot(gg_ord_tuv_domogr_sites, aes(x = NMDS1, y = NMDS2) ) + 
  geom_point(aes(shape = tuv_reduc$Transect), size = 6) + 
  geom_text_repel(aes(label = tuv_reduc$Depth)) +
  geom_segment(data = gg_ord_tuv_predictor,  aes(x = 0, y = 0, yend = NMDS2, xend = NMDS1), color = "blue", arrow = arrow()) + 
  geom_text_repel(data = gg_ord_tuv_predictor,  aes(x = NMDS1, y = NMDS2, label = Label), color = "blue", size= 10)




###### Анализ возрастной структуры 

tuv_demogr_age <- tuv_demogr %>% select(Age2, Age3,Age4, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12)

tuv_demogr_age2 <- decostand(tuv_demogr_age, method = "total" )

ca_age <- cca(tuv_demogr_age)


plot(ca_age, display = "sp")


tuv_demogr_age2$Site <- tuv_sites$Sample.ID

tuv_demogr_age_long <- melt(tuv_demogr_age2, id.vars = "Site")

tuv_demogr_age_long_CV <- tuv_demogr_age_long %>% group_by(Site) %>% summarise(CV = sd(value)/mean(value))


all <- merge(tuv_demogr_age_long, tuv_demogr_age_long_CV)



ggplot(all, aes(x = reorder(variable, CV), y = log(value+1) )) + geom_col() + facet_wrap(~ Site, ncol = 4)



tuv_demogr <- tuv_reduc %>% select(Age2, Age3,Age4, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12)

tuv_demogr_rel <- decostand(tuv_demogr, method = "total")

ord <- metaMDS(tuv_demogr_rel, mem)

plot(ord, type = "t")


write.table(fortify(ord), "clipboard",  sep = "\t")

autoplot(ord)


         