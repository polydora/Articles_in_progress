# The code bellow produce the assessment of salinity for each sampling sites

library(readxl)
library(ggplot2)
library(dplyr)

# Data on tide cycle
tide <- read_excel("data_Tyuva mussels.xlsx", sheet = "Tide")

# Data on salinity measured during observations in July 2009.
sal_tuv <-  read_excel("data_Tyuva mussels.xlsx", sheet = "salinity")


# Model describing the dependeny between salinity and tide hight and Distnce from upper part of Tyuva inlet (the river mouth) 
Mod_tide <- lm(Salinity ~ H*Distance, data = sal_tuv)


# Data on sampling sites
tuv <- read_excel("data_Tyuva mussels.xlsx", sheet = "Tyuva_09-10" )




# Calculation of mean and min salinity predicted by Model "Mod_tide" for each sampling sites

tuv$Mean_Salinity_predicted <- NA
tuv$Min_Salinity_predicted <- NA


for(i in 1:nrow(tuv)){
  d <- tide %>% filter(H >= tuv$Depth[i])
  d$Distance <- tuv$Dist[i]
  Sal_pred <- predict(Mod_tide, newdata = d)
  tuv$Mean_Salinity_predicted[i] <- mean(Sal_pred)
  tuv$Min_Salinity_predicted[i] <- min(Sal_pred)
}






# Data for Figure 1 D construction

tide_transect_R <- tide
tide_transect_R$Transect <- "R"
tide_transect_R$Distance <- 0


tide_transect_B <- tide
tide_transect_B$Transect <- "B"
tide_transect_B$Distance <- 250


tide_transect_Mid <- tide
tide_transect_Mid$Transect <- "Mid"
tide_transect_Mid$Distance <- 1750


tide_transect_Mon <- tide
tide_transect_Mon$Transect <- "Mon"
tide_transect_Mon$Distance <- 2550

tide_transect <- rbind(tide_transect_R, tide_transect_B, tide_transect_Mid, tide_transect_Mon)

tide_transect$Salinity_predicted <- predict(Mod_tide, newdata = tide_transect)

tide_transect$Transect <- factor(tide_transect$Transect, levels = c("R", "B", "Mid", "Mon"))
sal_tuv$Transect <- factor(sal_tuv$Transect, levels = c("R", "B", "Mid", "Mon"))


levels_salinity_0.5 <- tide_transect %>% filter(H>=0.5) %>% group_by(Transect) %>% summarise(Level = 0.5, Salinity_predicted = mean(Salinity_predicted))

levels_salinity_1 <- tide_transect %>% filter(H>=1) %>% group_by(Transect) %>% summarise(Level = 1, Salinity_predicted = mean(Salinity_predicted))

levels_salinity_1.5 <- tide_transect %>% filter(H>=1.5) %>% group_by(Transect) %>% summarise(Level = 1.5, Salinity_predicted = mean(Salinity_predicted))

levels_salinity_2 <- tide_transect %>% filter(H>=2) %>% group_by(Transect) %>% summarise(Level = 2, Salinity_predicted = mean(Salinity_predicted))



levels_salinity_subtidal <- tide_transect %>% filter(H>=0) %>% group_by(Transect) %>% summarise(Level = "subtidal", Salinity_predicted = mean(Salinity_predicted))



mean_salinity <- tide_transect %>% group_by(Transect) %>% summarise(Salinity_predicted = mean(Salinity_predicted))





tide_transect$Transect <- factor(tide_transect$Transect, levels = c("Mon", "Mid", "B", "R"), labels = c("MoS", "MidN", "BN", "R"))

mean_salinity$Transect <- factor(mean_salinity$Transect, levels = c("Mon", "Mid", "B", "R"), labels = c("MoS", "MidN", "BN", "R"))

sal_tuv$Transect <- factor(sal_tuv$Transect, levels = c("Mon", "Mid", "B", "R"), labels = c("MoS", "MidN", "BN", "R"))


ggplot(tide_transect, aes(x = Time, y = Salinity_predicted)) + 
  geom_line(size = 1, color = "blue") + 
  facet_wrap(~Transect, nrow = 4)+
  theme_bw() + 
  geom_point(data = sal_tuv, aes(x= Time, y = Salinity, shape = factor(Depth) ), size = 2, color = "blue") +
  scale_shape_manual(values = c(22, 24, 21)) +
  geom_hline(data = mean_salinity, aes(yintercept = Salinity_predicted), linetype = 2) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Date and Time", y = "Salinity ppt") +
  coord_flip() +
  guides(shape = "none")
  


# Sving the data for futher analysis

tuv_print <- tuv %>% select(Sample_ID, Min_Salinity_predicted, Mean_Salinity_predicted)

# write.table(tuv_print, "Salinity_predicted.csv", sep = ";", row.names = F)

