library(ggplot2)
library(dplyr)
library(readxl)
library(nlme)


myt <- read_excel("Data/Mytred_2021_byssus_force.xlsx", na = "NA", col_names = T)
str(myt)


nrow(myt)

myt2 <- myt %>% filter( (Label == "r" & Morphotype == "e") | (Label == "l" & Morphotype == "t")) %>% filter(complete.cases(.))

nrow(myt2)

ggplot(myt2, aes(x = Type, y = Force, fill = Morphotype)) + geom_boxplot()


mod <- lme(log(Force+1) ~ Type*Morphotype + Weight, random = ~1|Cage, data = myt2)

plot(mod)

summary(mod)
