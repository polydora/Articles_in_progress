library(readxl)
library(reshape2)

opennes1 <- read_excel("Data/Mytred_2021_Salinity.xlsx", sheet = "Exp1_tidy") 

d1 <- melt(opennes, id.vars = c("Experiment",	"Tank",	"Plank", 	"Mussel_ID",	"Expected_Morph",	"True_Morph",	"Length",	"Www"), variable.name = "Observation", value.name = "Status")

d1$Experiment

opennes2 <- read_excel("Data/Mytred_2021_Salinity.xlsx", sheet = "Exp2_tidy") 

d2 <- melt(opennes2, id.vars = c("Experiment",	"Tank",	"Plank", 	"Mussel_ID",	"Expected_Morph",	"True_Morph",	"Length",	"Www"), variable.name = "Observation", value.name = "Status")

d2$Experiment

opennes3 <- read_excel("Data/Mytred_2021_Salinity.xlsx", sheet = "Exp3_tidy") 

d3 <- melt(opennes3, id.vars = c("Experiment",	"Tank",	"Plank", 	"Mussel_ID",	"Expected_Morph",	"True_Morph",	"Length",	"Www"), variable.name = "Observation", value.name = "Status")

d3$Experiment


d <- rbind(d1, d2, d3)






str(d)
d$Length <- as.numeric(d$Length)
d$Www <- as.numeric(d$Www)




exp_param <- read_excel("Data/Mytred_2021_Salinity.xlsx", sheet = "Exp_Salinity_Total")
unique(exp_param$Experiment)

unique(d$Experiment)

myt <- merge(exp_param, d)

unique(myt$Experiment)


myt$Experiment <- factor(myt$Experiment)
myt$True_Morph <- factor(myt$True_Morph)

unique(myt$Experiment)


library(lme4)

myt2 <- myt %>% filter(Tank != "C")


unique(myt$Experiment)

str(myt2)
unique(myt2$Status)

Mod <- glmer(Status ~ scale(Salinity) * Phase * True_Morph + (1| Mussel_ID) + (1|Experiment), data = myt2, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)) )

plot(Mod)
summary(Mod)

drop1(Mod)
 
Mod2 <- update(Mod, .~.-scale(Salinity):Phase:True_Morph  )
drop1(Mod2)

Mod3 <- update(Mod2, .~.-Phase:True_Morph  )
drop1(Mod3)


plot(Mod3)

summary(Mod3)




my_data <- myt2 %>% group_by(True_Morph, Phase) %>% do(data.frame(Salinity = seq(min(.$Salinity), max(.$Salinity), length.out = 100)))

my_data <- expand.grid(True_Morph = factor(unique(myt2$True_Morph)), Phase =  factor(unique(myt2$Phase)), Salinity = seq(min(myt2$Salinity), max(myt2$Salinity), length.out = 100))


library(effects)

Effects_salinity <- as.data.frame(allEffects(Mod3, xlevels = my_data))

str(Effects_salinity)


ggplot(Effects_salinity[[2]], aes(x = Salinity, y = fit, group = True_Morph)) + 
  geom_line(aes(color= True_Morph)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(10, 24, 1))



ggplot(Effects_salinity[[1]], aes(x = Salinity, y = fit, group = Phase)) + 
  geom_line(aes(color= Phase)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(10, 24, 1)) + 
  facet_wrap(~True_Morph)

ggplot(Effects_salinity[[1]], aes(x = Salinity, y = fit, group = True_Morph)) + 
  geom_line(aes(color= True_Morph)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  scale_color_manual(values = c("blue", "red")) +
  scale_x_continuous(breaks = seq(10, 24, 1)) + 
  facet_wrap(~Phase)

