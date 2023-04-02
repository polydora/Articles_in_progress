library(dplyr)
library(nlme)



bys <- read.csv("data/byss_force_2021.csv")
str(bys)

table(bys$Label, bys$Morphotype)

# Убираю случаи, когда ожидаемый морфотип не совпал с наблюдаемым
bys2 <- bys %>% filter((Label == "l" & Morphotype == "t") | (Label == "r" & Morphotype == "e")) %>% filter(!is.na(Force))  

table(bys2$Label, bys2$Morphotype)

Mod_bys <- lme(Force ~ Morphotype + Weight, random =  ~1|Cage, data = bys2)

plot(Mod_bys)

summary(Mod_bys)



Mod_bys_n <- lme(log(N_byss + 1) ~ Morphotype + Weight, random =  ~1|Cage, data = bys2)

plot(Mod_bys_n)

summary(Mod_bys_n)

ggplot(bys2, aes(x = Morphotype, y = Force)) + geom_boxplot()
ggplot(bys2, aes(x = Morphotype, y = N_byss)) + geom_boxplot()

