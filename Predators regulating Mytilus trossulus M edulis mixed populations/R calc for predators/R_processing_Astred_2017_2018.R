# Часть 1. Эксперименты по выеданию мидий разного морфотипа в кормушках с разным соотношением морф

library(dplyr)
library(ggplot2)
library(gamm4)
library(mgcv)
library(readxl)

myt <- read_excel("data/Astred_2017_2018_myt.xls", sheet = "Astred All Mussel 2017 2018", na = "NA")
astr <- read_excel("data/Astred_2017_2018_myt.xls", sheet = "Atred Asterias 2017 2018", na = "NA")

myt$Exp <- factor(myt$Exp)
myt$Morph <- factor(myt$Morph)
astr$Exp <- factor(astr$Exp)

myt <- myt %>% filter(complete.cases(.))


# Вычисляем долю T-морфотипа и долю съеденных в садках

myt_1 <- myt %>% group_by(Year, Exp, Box) %>% summarise(P_T = mean(Morph == "t"), t = sum(Morph == "t"), e = sum(Morph == "e"), P_eaten = mean(Status == "eaten"))


astr_1 <- astr %>% group_by(Year, Exp, Box) %>% summarise(B_aster = sum(Weight), N_aster = n())



myt_aster <- merge(myt_1, astr_1) %>% filter(complete.cases(.))

myt_aster <- myt_aster %>% mutate(N_total = e + t)


myt <- myt %>% filter(!(Exp == "Exp2" & Box == 16))

myt_aster <- myt_aster %>% filter(!(Exp == "Exp2" & Box == 16))



myt_aster_full <- merge(myt, myt_aster) %>% filter(complete.cases(.))

myt_aster_full$Box2 <- paste(myt_aster_full$Box, "_", myt_aster_full$Exp, sep = "")

myt_aster_full <- myt_aster_full %>% mutate(Outcome = ifelse(Status == "eaten", 1, 0))

unique(myt_aster_full$Box2)

str(myt_aster_full)

myt_aster_full$Box2 <- factor(myt_aster_full$Box2)





myt_aster_full <- myt_aster_full %>% mutate(Composition = ifelse(P_T<0.5 & P_T > 0.2, "Mixed", "Pure"))

myt_aster_full$Composition <- factor(myt_aster_full$Composition)

myt_aster_full$Composition <- relevel(myt_aster_full$Composition, ref = "Pure")

myt_aster_full$N_consp <- ifelse(myt_aster_full$Morph == "e", myt_aster_full$e, myt_aster_full$t)


# Продолжительность экспозиции

myt_aster_full <- myt_aster_full %>% mutate(Dur = case_when(Exp == "Exp1" ~ 61,
                                                            Exp == "Exp2" ~ 121,
                                                            Exp == "Exp3" ~ 113))






# Задаем продолжительность экспериментов в часах
# myt_aster_full$Duration <- ifelse(myt_aster_full$Exp == "Exp1", 61, 121)


myt_aster_full_short <- myt_aster_full %>% filter(Exp != "Exp1") 


# myt_aster_full_short %>% group_by(Box2, Morph) %>% summarise(N_E = mean(e), N_T = mean(t), N_own = mean(N_own)) %>% View(.)





library(car)


Mod <- glmer(Outcome ~ Morph + scale(N_consp) + scale(P_T) + scale(Dur) + scale(N_total) + scale(L) + scale(B_aster) +   (1|Box2), family = binomial(link = "logit"), data = myt_aster_full )


vif(Mod)


mod_diagn <- fortify.merMod(Mod)

ggplot(mod_diagn, aes(.fitted, .scresid)) + geom_point() + geom_smooth()


ggplot(mod_diagn, aes(N_consp, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(P_T, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(L, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(B_aster, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(N_total, .scresid)) + geom_point() + geom_smooth()


ggplot(mod_diagn, aes(factor(Dur), .scresid)) + geom_boxplot() 


ggplot(mod_diagn, aes(Morph, .scresid)) + geom_boxplot() 

ggplot(mod_diagn, aes(myt_aster_full$Composition, .scresid)) + geom_boxplot() 

ggplot(mod_diagn, aes(myt_aster_full$Origin, .scresid)) + geom_boxplot() 

ggplot(mod_diagn, aes(myt_aster_full$Box2, .scresid)) + geom_boxplot() 

ggplot(mod_diagn, aes(myt_aster_full$Exp, .scresid)) + geom_boxplot() 




summary(Mod)


library(broom.mixed)

tidy(Mod)


# Модель отдельно для смешанных поселений

myt_aster_full_mix <- myt_aster_full %>% filter(Composition == "Mixed") 

myt_aster_full_pure <- myt_aster_full %>% filter(Composition == "Pure") 


Mod_mix <- glmer(Outcome ~ Morph  + scale(P_T) + scale(Dur) + scale(N_total) + scale(L) + scale(B_aster) + (1|Box2), family = binomial(link = "logit"), data = myt_aster_full_mix )

vif(Mod_mix)

summary(Mod_mix)


# Модель отдельно для чистых поселений

Mod_pure <- glmer(Outcome ~ Morph  + scale(P_T) + scale(Dur) + scale(N_total) + scale(L) + scale(B_aster) + (1|Box2), family = binomial(link = "logit"), data = myt_aster_full_pure )

vif(Mod_pure)

summary(Mod_pure)




# Возможные визуализации ####


# Зависимость смертности от количества конспецификов в садке 
myt_aster_full %>% group_by(Exp, Composition, Morph, Box2) %>% summarise(P_eaten = mean(Outcome), N_consp = mean(N_consp)) %>% 
  ggplot(., aes(x = N_consp, y = P_eaten, color = Composition)) + geom_point(size = 2) + facet_wrap(~Morph)



# Зависимость смертности от состава садка (Смешанные vs Чистые)
myt_aster_full %>% group_by(Exp, Composition, Box2) %>% summarise(P_eaten = mean(Outcome), P_T = mean(P_T)) %>% 
  ggplot(., aes(x = Composition, y = P_eaten)) + geom_boxplot() + facet_wrap(~Exp)

# Смертность морфотипов для разного происхождения.
myt_aster_full %>% group_by(Exp, Box2, Origin, Morph) %>% summarise(P_eaten = mean(Outcome)) %>% 
  ggplot(., aes(x = Morph, y = P_eaten)) + geom_boxplot() + facet_grid(Origin ~ Exp)




# Зависимость числа съеденных моллюсков от обилия звезд

ggplot(myt_aster, aes(x = B_aster, y = P_eaten)) + geom_point()  + facet_grid(~ Exp)




#############################################33
#### Часть 2. Регуляция таксономического состава



astfood_myt <- read_excel('Data/Astfood_2019.xlsx', sheet = "Astfood_2019", na = "NA")

astfood_aster <- read_excel('Data/Astfood_2019.xlsx', sheet = "Astfood_2019_Asterias", na = "NA")



asterias_abund <- astfood_aster %>% group_by(Site, Sample) %>% summarise(N_aster = n(), B_aster = sum(B))



all_ast_abund <- merge(astfood_myt, asterias_abund, all.x = T)

all_ast_abund[is.na(all_ast_abund)]<- 0

all_ast_abund <- all_ast_abund %>% mutate(Prop_dead = (N_T_dead + N_E_dead)/(N_T_dead + N_E_dead + N_T_alive + N_E_alive), P_T = N_T_alive/(N_T_alive + N_E_alive) )




ggplot(all_ast_abund, aes(x = Prop_dead, y = B_aster)) +
  geom_point() +
  facet_wrap(~Site) +
  labs(x = "Доля мертвых", y = "Биомасса морских звезд")


library(mgcv)

all_ast_abund$Site <- factor(all_ast_abund$Site)

all_ast_abund$Stage2 <- factor(all_ast_abund$Stage, labels = c("До нашествия",   "Во время нашествия", "После нашествия"))






Mod_ast_1 <- gam(B_aster ~ s(Prop_dead, by = Site) + Site, data = all_ast_abund)


new_dat_ast <- all_ast_abund %>% group_by(Site) %>% do(expand.grid(Prop_dead = seq(min(.$Prop_dead), max(.$Prop_dead), by = 0.01)))


new_dat_ast$B_ast_predicted <- predict(Mod_ast_1, newdata = new_dat_ast, se.fit = T)$fit
new_dat_ast$SE <- predict(Mod_ast_1, newdata = new_dat_ast, se.fit = T)$se.fit


ggplot(all_ast_abund, aes(x = Prop_dead)) +
  geom_point(aes (y = B_aster, color = Stage2), size = 4) +
  facet_wrap(~Site) +
  labs(x = "Доля мертвых", y = "Биомасса морских звезд", color = "") +
  geom_line(data = new_dat_ast, aes(y = B_ast_predicted)) +
  ylim(0, max(all_ast_abund$B_aster)) +
  scale_color_manual(values = c("blue", "red", "gray")) +
  theme_bw()+
  theme(legend.position = "bottom")


# +
#   geom_ribbon(data = new_dat_ast, aes(ymin = B_ast_predicted - 1.96*SE, ymax =  B_ast_predicted + 1.96*SE), alpha = 0.3)
#




Mod_PT <- gam(P_T ~ Prop_dead + Site , data = all_ast_abund, family=betar(link="logit"))

vif(Mod_PT)

plot(Mod_PT)

summary(Mod_PT)



# Визуализации ++++



# Зависимость доли мертвых от биомассы звезд


ggplot(all_ast_abund, aes(x = Prop_dead, y = B_aster, color = Stage2)) + geom_point()

ggplot(all_ast_abund, aes(x = Stage2, y = B_aster)) + geom_boxplot() + facet_wrap(~Site)


ggplot(all_ast_abund, aes(x = Stage2, y = P_T)) + geom_boxplot() + facet_wrap(~Site)



new_data <- all_ast_abund %>% group_by(Site) %>% do(data.frame(Prop_dead = seq(min(.$Prop_dead), max(.$Prop_dead), length.out = 100)))

  
predicted <- predict(Mod_PT, newdata = new_data, type = "response", se.fit = T)


new_data$Predicted <- predicted$fit
new_data$SE <- predicted$se.fit

ggplot(all_ast_abund, aes(x = Prop_dead)) +
  geom_point(data = all_ast_abund, aes(x = Prop_dead, y= P_T, color = Stage2), size = 3) +
  facet_wrap(~Site) +
  geom_ribbon(data = new_data, aes(ymin = Predicted - 1.96*SE, ymax = Predicted + 1.96*SE), alpha = 0.2) +
  geom_line(data = new_data, aes(x = Prop_dead, y = Predicted),  color = "blue", size = 1) +
  guides (fill = "none") +
  labs(x = "Доля мертвых", y = "Доля T-морфотипа среди живых", color = "") +
  ylim(0, 1) +
  scale_color_manual(values = c("blue", "red", "gray")) +
  theme_bw()+
  theme(legend.position = "bottom")


