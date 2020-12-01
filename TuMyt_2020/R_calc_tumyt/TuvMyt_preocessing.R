
library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)
library(readxl)


tuv <- read_excel("Data/TuMyt_2009_2010.xlsx")
str(tuv)

tuv_reduc <- tuv %>% filter(!is.na(PT))

tuv_demogr <- tuv_reduc %>% select(Age2, Age3,Age4, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, PT, N, W, OGP, max_L) %>% as.data.frame()

row.names(tuv_demogr) <- tuv_reduc$Sample_ID

colSums(is.na(tuv_demogr))

tuv_predictors <-  tuv_reduc %>% select(Transect, Depth, Distance, Exposition, Fucus, Alaria, Cover, Slope, Width, Substrate)

tuv_predictors$Alaria <- factor(tuv_predictors$Alaria)
tuv_predictors$Fucus <- factor(tuv_predictors$Fucus)




tuv_cca <- cca(tuv_demogr ~ Distance + Depth + Slope  + Cover + Alaria, data = tuv_predictors) 
vif.cca(tuv_cca)


plot(tuv_cca, display = c("cn", "sp"))



plot(tuv_cca, display = c("cn", "sites"))


anova(tuv_cca)
anova(tuv_cca, by = "axis")
anova(tuv_cca, by = "margin")
anova(tuv_cca, by = "terms")



plot(tuv_cca, display = c("cn", "site"))







demogr_scores <- fortify(tuv_cca, scaling = "symmetric")

write.table(demogr_scores, "CCA results.csv", sep = ";", row.names = FALSE )

demogr_scores_site <- demogr_scores[demogr_scores$Score == "constraints", ] 
demogr_scores_site$Transect <- tuv_predictors$Transect 
demogr_scores_site$High <- -tuv_predictors$Depth 

demogr_scores_sp <- demogr_scores[demogr_scores$Score == "species", ] 


demogr_scores_cons <- demogr_scores[demogr_scores$Score == "biplot", ] 

ggplot(demogr_scores_site, aes(x = CCA1, y = CCA2)) + 
  geom_point(aes(fill = Transect, size = High ), shape = 21) + 
  scale_fill_manual(values = c("pink", "red", "green", "blue", "darkblue", "yellow", "orange", "black")) + 
  theme_bw() + 
  scale_size(range = c(3, 10)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0)  + 
  geom_segment(data = demogr_scores_cons, aes(x = 0, y = 0, xend = CCA1, yend = CCA2), color = "brown", arrow = arrow(type = "closed", angle = 10), size = 1) +
  geom_text_repel(data = demogr_scores_sp, aes(label = Label)) + 
  geom_text_repel(data = demogr_scores_cons, aes(label = Label), color = "black", size = 6,box.padding = 0.3)





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



ggplot(gg_ord_tuv_domogr_sites, aes(x = NMDS1, y = NMDS2) ) + 
  geom_point(aes(shape = tuv_reduc$Transect), size = 6) + 
  geom_text_repel(aes(label = tuv_reduc$Depth)) +
  geom_segment(data = gg_ord_tuv_predictor,  aes(x = 0, y = 0, yend = NMDS2, xend = NMDS1), color = "blue", arrow = arrow()) + 
  geom_text_repel(data = gg_ord_tuv_predictor,  aes(x = NMDS1, y = NMDS2, label = Label), color = "blue", size= 10)


