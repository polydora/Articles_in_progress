library(readxl)
library(dplyr)
library(reshape2)


dat <- read_excel("Data/swans.xlsx")

dat2 <- 
dat %>% 
  select(Ice, Depth,  Dist,  Year, Date, Site, Sp1, Sp2, Sp3 ) %>% 
  melt(id.vars = c("Ice", "Depth",  "Dist",  "Year", "Date", "Site" ), variable.name = "Species", value.name = "N")


dat2$Species <- factor(dat2$Species)
dat2$Ice <- factor(dat2$Ice, labels = c("Ice_cover", "No_ice"))
dat2$Site <- factor(dat2$Site)


library(mgcv)

Mod <- gam(N ~ s(Year, by = Species, k = 5) +
             s(Dist, Depth,  by = interaction(Species, Ice), k = 10) +
             Species + Ice +
             s(Site, bs = "re", k = length(unique(dat2$Site))),
           data = dat2,
           family = nb(),
           method = "REML")


library(gratia)

appraise(Mod)

library(DHARMa)
simulateResiduals(Mod, plot = T)


library(ggplot2)

draw(Mod, select = 7) +
guides(fill = "none") +
scale_fill_gradient(low = "yellow", high = "red") +
ggtitle("Sp1 в безледный период", subtitle = NULL)


draw(Mod, select = 8) +
guides(fill = "none") +
scale_fill_gradient(low = "yellow", high = "red") +
ggtitle("Sp2 в безледный период", subtitle = NULL)


draw(Mod, select = 9) +
guides(fill = "none") +
scale_fill_gradient(low = "yellow", high = "red") +
ggtitle("Sp3 в безледный период", subtitle = NULL)





