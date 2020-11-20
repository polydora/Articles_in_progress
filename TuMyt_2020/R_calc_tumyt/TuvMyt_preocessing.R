
library(vegan)
library(dplyr)
library(ggvegan)
library(ggrepel)
library(reshape2)



tuv <- read.csv("Data/TuMyt_2020.csv", header = T)
str(tuv)

tuv_reduc <- tuv %>% filter(!is.na(PT))

tuv_demogr <- tuv_reduc %>% select(Age2, Age3,Age4, Age5, Age6, Age7, Age8, Age9, Age10, Age11, Age12, PT, N, W, OGP, max_L)



colSums(is.na(tuv_demogr))

tuv_predictors <-  tuv_reduc %>% select(Depth, Position2)

tuv_sites <- tuv_reduc %>% select(Sample.ID, Transect, Position)






tuv_cca <- cca(tuv_demogr ~ ., data = tuv_predictors) 

plot(tuv_cca, display = c("cn", "sp"))

anova(tuv_cca)
anova(tuv_cca, by = "axis")
anova(tuv_cca, by = "margin")

plot(tuv_cca, display = c("cn", "site"))






tuv_cca_objects <- fortify(tuv_cca)

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

envfitted <- envfit(ord_tuv_domogr ~ Depth + N + OGP + W + PT, data = tuv_predictors)

plot(ord_tuv_domogr)
plot(envfitted)



gg_ord_tuv_predictor <- fortify(envfitted)
gg_ord_tuv_domogr <- fortify(ord_tuv_domogr)

gg_ord_tuv_domogr_sites <- gg_ord_tuv_domogr %>% filter(Score == "sites") 
gg_ord_tuv_domogr_sites$Site <- tuv_reduc$Sample.ID


gg_ord_tuv_domogr_sp <- gg_ord_tuv_domogr %>% filter(Score == "species")


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

         