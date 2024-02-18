library(readxl)
library(mgcv)
library(ggplot2)
library(gratia)
library(reshape2)
library(dplyr)



zub <- read_excel("Data/Зубатка_питание2001-2023.xlsx", sheet = "С пищей")

str(zub)

zub$L <- as.numeric(zub$L)

names(zub)

zub2 <- melt(zub, id.vars = c("ID", "Date", "Year", "L", "W", "Sex"), variable.name = "Species",value.name = "Out" )

species_freq <- 
zub2 %>% group_by(Species) %>% summarise(Freq = mean(Out))


rare_species <-
  species_freq %>% filter(Freq < 0.05)

zub3 <- 
zub2 %>% filter(!Species %in% rare_species$Species) 


unique(zub3$Species)

zub3$Species <- factor(zub3$Species)
zub3$Sex <- factor(zub3$Sex)

str(zub3)


Mod_gam  <-  gam(Out ~ s(Year, by = Species) + Species, data = zub3, method = "REML", family = "binomial")

summary(Mod_gam)


my_data <- expand.grid(Species = unique(zub3$Species), Year = seq(min(zub3$Year), max(zub3$Year))  )

predicted <- predict(Mod_gam, newdata = my_data, se.fit = TRUE, type = "response")

my_data$fit <- predicted$fit

my_data$SE <- predicted$se.fit

Pl_Mod_gam <- 
ggplot(my_data, aes(x = Year, y = fit)) +
  geom_line(color = "blue") +
  facet_wrap(~Species, ncol = 3) +
  geom_ribbon(aes(ymin = fit - 1.96*SE, ymax = fit + 1.96*SE), alpha = 0.2)
  
species_freq3 <-
zub3 %>% group_by(Species, Year) %>% summarise(Freq = mean(Out)) 

Pl_Mod_gam +
  geom_point(data = species_freq3, aes(y = Freq), size = 1) +
  theme_bw() +
  labs(y = "Freq")




draw(Mod_gam)

library(broom)

sum_Mod_gam <- tidy(Mod_gam)

str(sum_Mod_gam)


library(stringr)
sum_Mod_gam$term_2 <- str_replace_all(sum_Mod_gam$term, "s\\(Year\\):Species" , "" ) 
sum_Mod_gam$term_2 <- str_replace_all(sum_Mod_gam$term_2, "\\." , " " ) 





