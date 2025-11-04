library(readxl)
library(dplyr)
library(reshape2)


dat <- read_excel("swans.xlsx")

dat2 <- 
dat %>% 
  select(Ice, Depth,  Dist,  Year, Date, Sp1, Sp2, Sp3 ) %>% 
  melt(id.vars = c("Ice", "Depth",  "Dist",  "Year", "Date" ), variable.name = "Species", value.name = "N")


dat2$Species <- factor(dat2$Species)


library(mgcv)


Mod_ice <- gam(N ~ s(Year, by = Species, k = 5) + s(Depth, Dist, by = Species, k = 5) + Species, data = dat2 %>% filter(Ice == 1), family = "nb")

summary(Mod_ice)

library(gratia)


draw(Mod_ice, select = 4:6)




Mod_no_ice <- gam(N ~ s(Year, by = Species, k = 5) + s(Depth, Dist, by = Species, k = 5) + Species, data = dat2 %>% filter(Ice == 2), family = "nb")

draw(Mod_no_ice, select = 4:6)
