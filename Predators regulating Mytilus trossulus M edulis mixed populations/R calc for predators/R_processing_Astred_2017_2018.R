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

myt %>% group_by(Origin) %>% summarise(PT = mean(Morph == "t"))


# Вероятность индивидуальной мидии быть Mt по уравнениям из Khaitov et al. 2021

myt <- myt %>% mutate(P_Mt = case_when(Origin == "vor" & Morph == "e" ~ 1-0.96,
                                Origin == "vor" & Morph == "t" ~ 0.63,
                                Origin == "tel" & Morph == "e" ~ 1-0.46,
                                Origin == "tel" & Morph == "t" ~ 0.94))





# Вычисляем долю T-морфотипа, долю съеденных в садках и показатедь смешанности поселений

myt_1 <- myt %>% group_by(Year, Exp, Box) %>% summarise(P_T = mean(Morph == "t"), t = sum(Morph == "t"), e = sum(Morph == "e"), P_eaten = mean(Status == "eaten"), P_pure = mean(P_Mt)*(1 - mean(P_Mt)))


hist(myt_1$P_pure)



ggplot(myt_1, aes(P_T,P_pure)) + geom_point() 


astr_1 <- astr %>% group_by(Year, Exp, Box) %>% summarise(B_aster = sum(Weight), N_aster = n(), Size_mean = mean(Diametr))



myt_aster <- merge(myt_1, astr_1) %>% filter(complete.cases(.))

myt_aster <- myt_aster %>% mutate(N_total = e + t)

ggplot(myt_aster, aes(x = P_T, y = Size_mean)) + geom_point()











myt <- myt %>% filter(!(Exp == "Exp2" & Box == 16)) # Удаляем садок с кривым количесвтом высаженных мидий

myt_aster <- myt_aster %>% filter(!(Exp == "Exp2" & Box == 16))



myt_aster_full <- merge(myt, myt_aster) %>% filter(complete.cases(.))

myt_aster_full$Box2 <- paste(myt_aster_full$Box, "_", myt_aster_full$Exp, sep = "")

myt_aster_full$Box2 <- factor(myt_aster_full$Box2)


myt_aster_full <- myt_aster_full %>% mutate(Outcome = ifelse(Status == "eaten", 1, 0))


myt_aster_full$Box2 <- factor(myt_aster_full$Box2)





myt_aster_full <- myt_aster_full %>% mutate(Composition = ifelse(P_T<0.5 & P_T > 0.2, "Mixed", "Pure"))

myt_aster_full$Composition <- factor(myt_aster_full$Composition)

myt_aster_full$Composition <- relevel(myt_aster_full$Composition, ref = "Pure")

myt_aster_full$N_consp <- ifelse(myt_aster_full$Morph == "e", myt_aster_full$e, myt_aster_full$t)

hist(myt_aster_full$N_consp)


myt_aster_full %>% group_by(Box2, Composition) %>% summarise(N_consp = mean(N_consp), P_pure = mean(P_pure)) %>% 
ggplot(., aes(Composition, N_consp)) + geom_boxplot()


# Продолжительность экспозиции

myt_aster_full <- myt_aster_full %>% mutate(Dur = case_when(Exp == "Exp1" ~ 61,
                                                            Exp == "Exp2" ~ 121,
                                                            Exp == "Exp3" ~ 113))






myt_aster_full_short <- myt_aster_full %>% filter(Exp != "Exp1") 


# myt_aster_full_short %>% group_by(Box2, Morph) %>% summarise(N_E = mean(e), N_T = mean(t), N_own = mean(N_own)) %>% View(.)





library(car)


Mod <- glmer(Outcome ~ scale(P_Mt) +  scale(N_consp) + scale(P_T)  + scale(N_total) + scale(L) + scale(B_aster) +   (1|Exp/Box2), family = binomial(link = "logit"), data = myt_aster_full )


# Mod_mixed <- glmer(Outcome ~ scale(P_Mt) +  scale(N_consp) + scale(P_T)  + scale(N_total) + scale(L) + scale(B_aster) +   (1|Exp/Box2), family = binomial(link = "logit"), data = myt_aster_full %>% filter(Composition == "Mixed") )


vif(Mod)


# vif(Mod_mixed)

# 
# library(glmmTMB)
# 
# Mod <- glmmTMB(Outcome ~ scale(P_Mt) +  scale(N_consp) + scale(P_T)  + scale(N_total) + scale(L) + scale(B_aster) +   (1|Exp/Box2), family = "binomial", data = myt_aster_full, ziformula = ~ Box2 )


summary(Mod)

# summary(Mod_mixed)


mod_diagn <- fortify.merMod(Mod)

ggplot(mod_diagn, aes(.fitted, .scresid)) + geom_point() + geom_smooth()


ggplot(mod_diagn, aes(N_consp, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(P_T, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(L, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(B_aster, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(N_total, .scresid)) + geom_point() + geom_smooth()

ggplot(mod_diagn, aes(P_Mt, .scresid)) + geom_point() + geom_smooth(method = "lm")


ggplot(mod_diagn, aes(factor(Dur), .scresid)) + geom_boxplot() 


ggplot(mod_diagn, aes(Morph, .scresid)) + geom_boxplot() 

ggplot(mod_diagn, aes(myt_aster_full$Composition, .scresid)) + geom_boxplot() 

ggplot(mod_diagn, aes(myt_aster_full$Origin, .scresid)) + geom_boxplot() 






library(broom.mixed)

tidy(Mod)



# Bизуализации ####


# Зависимость числа съеденных моллюсков от обилия звезд

theme_set(theme_bw())


Pl_P_eaten_B_aster <- 
  ggplot(myt_aster, aes(x = B_aster, y = P_eaten)) + 
  geom_point() + 
  # geom_smooth(method = "lm", se = F) + 
  labs(x = "Starfish biomass", y = "Proportion of eaten") 



Pl_P_eaten_P_Mt <- 
myt_aster_full %>% group_by(P_Mt) %>% summarise(P_eaten = mean(Outcome), N = n()) %>% 
  ggplot(., aes(x = P_Mt, y = P_eaten)) + geom_point(aes(size = N))  + 
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Probability to be M.trossulus", y = "Proportion of eaten") +
  guides(size = "none") +
  xlim(0,1)


Pl_P_eaten_N_total <- 
  myt_aster_full %>% group_by(Box2) %>% summarise(P_eaten = mean(Outcome), N_total = mean(N_total)) %>% 
  ggplot(., aes(x = N_total, y = P_eaten)) + geom_point()  + 
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Mussel amount", y = "Proportion of eaten") +
  guides(size = "none") 


Pl_P_eaten_L <- 
  myt_aster_full %>% mutate(L2 = ntile(L, 7)) %>%  group_by(L2) %>% summarise(P_eaten = mean(Outcome), N = n(), L3 = mean(L)) %>% 
  ggplot(., aes(x = L3, y = P_eaten)) + geom_point(aes(size = N))  + 
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Mussel size", y = "Proportion of eaten") +
  guides(size = "none") 



Pl_P_eaten_N_consp <- 
  myt_aster_full %>% group_by(Morph, N_consp) %>% summarise(P_eaten = mean(Outcome), N = n()) %>%  
  ggplot(., aes(x = N_consp, y = P_eaten)) + geom_point(aes(size = N, color = Morph))  + 
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Number of mussels of the same morphotype", y = "Proportion of eaten") +
  guides(size = "none") +
  scale_color_manual(values = c("blue", "red"), labels = c("E", "T"), name = "Morphotype") +
  theme (legend.position = c(0.75, 0.85), legend.box.background = element_rect(colour = "black", fill = NA))

Pl_P_eaten_P_T <- 
  myt_aster_full %>% group_by(P_T) %>% summarise(P_eaten = mean(Outcome)) %>% 
  ggplot(., aes(x = P_T, y = P_eaten)) + geom_point()  + 
  # geom_smooth(method = "lm", se = F) +
  labs(x = "Proportion of T-morphotype", y = "Proportion of eaten")



library(cowplot)

plot_grid(Pl_P_eaten_B_aster, Pl_P_eaten_P_Mt,  Pl_P_eaten_N_total,  Pl_P_eaten_L, Pl_P_eaten_N_consp, Pl_P_eaten_P_T, ncol = 2, labels = "AUTO")




ggplot(myt_aster, aes(x = P_eaten)) + geom_histogram(bins = 10) + facet_wrap(~Exp)

## Анализ зависимости размера мидий от состава экспериментального юнита


ggplot(myt_aster_full, aes(x = P_T, y = L)) + geom_point()

Mod_myt_L <- lme(L ~ P_T +  N_total,  random = ~1|Exp, method = "REML", data = myt_aster_full)

vif(Mod_myt_L)

summary(Mod_myt_L)


aster_size <- myt_aster %>% select(Year, Exp, Box, P_T, P_eaten, N_aster, N_total ) %>% merge(.,astr)

ggplot(aster_size, aes(x = P_T, y =Diametr)) + geom_point()

Mod_ast_siz <- lme(Diametr ~ P_T +  N_aster + N_total, random = ~1|Exp, method = "REML", data = aster_size)

vif(Mod_ast_siz)

summary(Mod_ast_siz)



#############################################33
#### Часть 2. Регуляция таксономического состава



astfood_myt <- read_excel('Data/Astfood_2019.xlsx', sheet = "Astfood_2019", na = "NA")

astfood_aster <- read_excel('Data/Astfood_2019.xlsx', sheet = "Astfood_2019_Asterias", na = "NA")



asterias_abund <- astfood_aster %>% group_by(Site, Sample) %>% summarise(N_aster = n(), B_aster = sum(B))



all_ast_abund <- merge(astfood_myt, asterias_abund, all.x = T)

all_ast_abund[is.na(all_ast_abund)]<- 0

all_ast_abund <- all_ast_abund %>% mutate(Prop_dead = (N_T_dead + N_E_dead)/(N_T_dead + N_E_dead + N_T_alive + N_E_alive), P_T = N_T_alive/(N_T_alive + N_E_alive), N_alive =  N_T_alive + N_E_alive)




ggplot(all_ast_abund, aes(x = Prop_dead, y = B_aster)) +
  geom_point() +
  facet_wrap(~Site) +
  labs(x = "Доля мертвых", y = "Биомасса морских звезд")



library(mgcv)

all_ast_abund$Site <- factor(all_ast_abund$Site)

all_ast_abund$Stage2 <- factor(all_ast_abund$Stage, labels = c("Intact",   "Starfish", "Dead"))






# Mod_ast_1 <- gam(B_aster ~ s(Prop_dead, by = Site) + Site, data = all_ast_abund)
# 
# 
# new_dat_ast <- all_ast_abund %>% group_by(Site) %>% do(expand.grid(Prop_dead = seq(min(.$Prop_dead), max(.$Prop_dead), by = 0.01)))
# 
# 
# new_dat_ast$B_ast_predicted <- predict(Mod_ast_1, newdata = new_dat_ast, se.fit = T)$fit
# new_dat_ast$SE <- predict(Mod_ast_1, newdata = new_dat_ast, se.fit = T)$se.fit
# 
# 
# ggplot(all_ast_abund, aes(x = Prop_dead)) +
#   geom_point(aes (y = B_aster, color = Stage2), size = 4) +
#   facet_wrap(~Site) +
#   labs(x = "Доля мертвых", y = "Биомасса морских звезд", color = "") +
#   geom_line(data = new_dat_ast, aes(y = B_ast_predicted)) +
#   ylim(0, max(all_ast_abund$B_aster)) +
#   scale_color_manual(values = c("blue", "red", "gray")) +
#   theme_bw()+
#   theme(legend.position = "bottom")


# +
#   geom_ribbon(data = new_dat_ast, aes(ymin = B_ast_predicted - 1.96*SE, ymax =  B_ast_predicted + 1.96*SE), alpha = 0.3)
#




Mod_PT <- gam(P_T ~ Stage2 + s(B_aster, bs = "cs") + s(Site, bs = "re"), data = all_ast_abund, family=betar(link="logit"))


summary(Mod_PT)

library(broom)
tidy(Mod_PT, parametric = T)



# Визуализации ++++


theme_set(theme_bw())

Pl_baster <- ggplot(all_ast_abund, aes(x = Stage2, y = B_aster)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + labs(y = "Starfish biomass")


Pl_N_alive <- ggplot(all_ast_abund, aes(x = Stage2, y = N_alive)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank()) + labs(y = "Abundance of alive mussels")
 

Pl_Prop_dead <- ggplot(all_ast_abund, aes(x = Stage2, y = Prop_dead)) + geom_boxplot() + theme(axis.title.x = element_blank(), axis.text.x = element_blank())+ labs(y = "Proportion of dead shells")


compar <- all_ast_abund %>% group_by(Stage2) %>% summarize(y = median(P_T) +0.03) %>% mutate(Lab = c("a", "b", "b"))

Pl_PT <- 
  ggplot(all_ast_abund, aes(x = Stage2, y = P_T)) + 
  geom_boxplot() +  
  theme(axis.title.x = element_blank()) + 
  labs(y = "Proportion of T-morphotype") +
  geom_text(data = compar, aes(x = Stage2, y = y, label = Lab))



library(cowplot)


plot_grid(Pl_baster, Pl_Prop_dead, Pl_PT, ncol = 1 , labels = "AUTO")


library(ggsignif)


# plot(Mod_PT)




library(multcomp)


contr <- matrix(0, nrow = 3, ncol = length(coef(Mod_PT)))
colnames(contr) <- names(coef(Mod_PT))
rownames(contr) <- c("Starfish crowd - Intact patch", "Dead shell patch - Intact patch", "Dead shell patch - Starfish crowd")
contr[, 2:3] <- rbind(c(1, 0), c(0, 1), c(-1, 1))



comparison <- glht(Mod_PT, linfct = contr)
summary(comparison)


# library(itsabug)
# 
# wald_gam(Mod_PT)
# 
# plot_parametric(Mod_PT, pred = list(Stage2 = levels(all_ast_abund$Stage2)), rm.ranef = F)

