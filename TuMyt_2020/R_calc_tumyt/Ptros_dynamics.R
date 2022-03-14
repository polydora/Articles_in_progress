library(vegan)
library(dplyr)
library(ggplot2)
library(mgcv)
library(readxl)



tuv_all <- read.table("Data/TuMyt_all_itog.csv", header = T, sep = ";")
str(tuv_all)

tuv_all$Habitat <- factor(tuv_all$Habitat)

# tuv_all$Fitros <- 2 * asin(sqrt(tuv_all$Ptros)) * 180/pi

tuv_all <- tuv_all %>% mutate(Age_4_7 = Age4 + Age5 + Age6 + Age7, Age_8_12 = Age8 + Age9 + Age10 + Age11 + Age12)



ptros_gen <- read_excel("Data/Ptros_gen.xlsx")


Mod_dynam <- gam(Ptros ~ s(Year, by = Habitat, k = 6) + Habitat, data = tuv_all) 





summary(Mod_dynam)


library(gratia)


draw(Mod_dynam) 





My_data <- tuv_all %>% group_by(Habitat) %>%  do(data.frame(Year = seq(min(.$Year), max(.$Year)), N = mean(tuv_all$N)))

Predicted <- predict(Mod_dynam, newdata = My_data, type = "response", se.fit = T)

My_data$Fit <- Predicted$fit 

My_data$SE <- Predicted$se.fit 




ggplot(My_data, aes(x = Year, y = Fit)) + 
  geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
  geom_line() +
  facet_wrap(~Habitat) +
  geom_point(data = tuv_all, aes(y = Ptros), position = position_jitter(width = 0.1))


# +   geom_point(data = ptros_gen, aes(y = Ptros), color = "blue")


tuv_all2 <- 
tuv_all %>% select(Sample_ID, Period, Year, Habitat, Age2_3, Age4_6, Age7_9, Age10_12, N, W, OGP, max_L, size_5) %>% as.data.frame() %>% filter(complete.cases(.)) 
 

tuv_ca <- cca(tuv_all2[,-c(1:4)])

scores(tuv_ca)$species


tuv_all2 <- tuv_all2 %>% mutate(CA1 = scores(tuv_ca)$sites[,1], CA2 = scores(tuv_ca)$sites[,2])


Mod_dynam_ca <- gam(CA1 ~ s(Year, by = Habitat, k = 5, bs="tp", m=2) + s(Habitat, bs="re", k=4), data = tuv_all2, method="REML") 



summary(Mod_dynam_ca)


draw(Mod_dynam_ca) 


My_data2 <- tuv_all2 %>% group_by(Habitat) %>%  do(data.frame(Year = seq(min(.$Year), max(.$Year))))

Predicted2 <- predict(Mod_dynam_ca, newdata = My_data2, type = "response", se.fit = T)

My_data2$Fit <- Predicted2$fit 

My_data2$SE <- Predicted2$se.fit 





ggplot(My_data2, aes(x = Year, y = Fit)) + 
  geom_ribbon(aes(ymin = Fit - 1.96*SE, ymax = Fit + 1.96*SE), alpha = 0.2) +
  geom_line() +
  facet_wrap(~Habitat, ncol = 1) +
  geom_point(data = tuv_all2, aes(y = CA1)) + 
  labs(x = "Year", y = "CA1")



