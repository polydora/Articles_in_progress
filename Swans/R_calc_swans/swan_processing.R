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
             s(Dist, Depth,  by = interaction(Species, Ice), k = -1) +
             Species + Ice +
             s(Site, bs = "re", k = length(unique(dat2$Site))),
           data = dat2,
           family = nb(),
           method = "REML")



appraise(Mod)
library(DHARMa)
simulateResiduals(Mod, plot = T)
summary(Mod)
library(gratia)
draw(Mod)
draw(Mod, select = 5:9)
draw(Mod, select = 4:9)
sm <- smooth_estimates(Mod)  %>%
add_confint()
sm <-
sm %>%
mutate(.estimate_N = exp(.estimate + coef(Mod)["(Intercept)"]),
.lower_N = exp(.lower_ci + coef(Mod)["(Intercept)"]),
.upper_N = exp(.upper_ci +coef(Mod)["(Intercept)"]))
sm
names(sm)
unique(sm$.smooth)
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice")
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, fill = .estimate_N)) %>%
geom_tile()
library(ggplot2)
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, fill = .estimate_N)) %>%
geom_tile()
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, fill = .estimate_N)) +
geom_tile()
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, fill = .estimate_N)) +
geom_point()
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, color = .estimate_N)) +
geom_point()
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, fill = .estimate_N)) +
geom_tile() +
scale_fill_gradient(low = "yellow", y = "red")
sm %>%
filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)Sp1.No_ice") %>%
ggplot(aes(x = Dist, y = Depth, fill = .estimate_N)) +
geom_tile() +
scale_fill_gradient(low = "yellow", high = "red")
draw(Mod, select = 4:9)
draw(Mod, select = 7)
draw(Mod, select = 7) +
guides(fill = "none")
draw(Mod, select = 7) +
guides(fill = "none") +
scale_fill_gradient(low = "yellow", high = "red")
draw(Mod, select = 7) +
guides(fill = "none") +
scale_fill_gradient(low = "yellow", high = "red") +
ggtitle("Sp1 в безледный период")
draw(Mod, select = 7) +
guides(fill = "none") +
scale_fill_gradient(low = "yellow", high = "red") +
ggtitle("Sp1 в безледный период", subtitle = "")
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
library(knitr)
opts_chunk$set(echo = TRUE)
opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)





Mod_no_ice <- gam(N ~ s(Year, by = Species, k = 5) + s(Depth, Dist, by = Species, k = 5) + Species, data = dat2 %>% filter(Ice == 2), family = "nb")

draw(Mod_no_ice, select = 4:6)
