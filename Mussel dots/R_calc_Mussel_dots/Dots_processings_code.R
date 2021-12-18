library(ggmap)
library(dplyr)
library(readxl)
library(mgcv)


myt <- read_excel("data/Mussel from monitoring in Kandalaksha.xlsx", sheet = "Mussel dots")

myt$Morphotype <- factor(myt$Morphotype)

unique(myt$Site)

myt$Region <- factor(myt$Region)


# myt <- myt %>% filter(Site %in% c("Ovechiy", "Ryazhkov"))


# ggplot(myt, aes(x = Year,  y = N_dots, color = Region))  + geom_smooth() + scale_x_continuous(breaks = 2011:2021) + theme(axis.text.x = element_text(angle = 90))





                        
Mod_dot <- gam(N_dots ~ s(Year, by = Region ) + Region  + Morphotype, family = "nb", 
               data = myt %>%  filter(Year > 2010))

plot(Mod_dot, pages = 1)


library(gratia)

appraise(Mod_dot)

gam.check(Mod_dot)

summary(Mod_dot)


library(gratia)

Pl_Inner_dots <- draw(Mod_dot)[[1]] + theme_bw() + scale_x_continuous(breaks = 2000:2021) + ggtitle("Smoother for Inner region")

Pl_Outer_dots <- draw(Mod_dot)[[2]] + theme_bw() + scale_x_continuous(breaks = 2000:2021) + ggtitle("Smoother for Outer region")


########################
ntne <- read_excel("Data/nt_ne_monitoring_2002_2021.xlsx")

ntne <- ntne %>% mutate(PT = Nt/(Nt+Ne))

ntne$PT[ntne$PT == 1] <- 0.999

ntne <- ntne %>% filter(Site %in% c("Ovechiy","Lupchostrov", "Ryazhkov"))


ntne$Region <- factor(ntne$Region)

Mod_PT <- gam(PT ~ s(Year, by = Region, k = 8) + Region, family = "betar", 
               data = ntne %>% filter(Year > 2010))

plot(Mod_PT, pages = 1)

gam.check(Mod_PT)

appraise(Mod_PT)


Pl_Inner_PT <- draw(Mod_PT)[[1]] + theme_bw() + scale_x_continuous(breaks = 2011:2021) + ggtitle("Smoother for  PT Inner region")

Pl_Outer_PT <- draw(Mod_PT)[[2]] + theme_bw() + scale_x_continuous(breaks = 2011:2021) + ggtitle("Smoother for PT Inner region") 


library(cowplot)

plot_grid(Pl_Inner_dots, Pl_Outer_dots, Pl_Inner_PT, Pl_Outer_PT, ncol = 2)


# Анализ размеров

size <- read_excel("data/Mussel from monitoring in Kandalaksha.xlsx", sheet = "Mussel sizes")

dot_size <- merge(size, myt, all.x = T) %>% filter(complete.cases(.))

ggplot(dot_size, aes(x = Length, y = N_dots, color = Morphotype)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~Site)

Mod_size <- lm(log(N_dots+1) ~ Length * Morphotype, data = dot_size)
plot(Mod_size)
summary(Mod_size)



#### Модель связи количества точек с предикторами






