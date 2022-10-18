# Часть 1. Эксперименты по выеданию мидий разного морфотипа в кормушках с разным соотношением морф

library(dplyr)
library(ggplot2)
library(gamm4)
library(mgcv)


myt <- read.table("Data/Astred_2017_myt.csv", sep = ",", header = T)
astr <- read.table("Data/Astred_2017_aster.csv", sep = ";", header = T)

myt$Exp <- factor(myt$Exp)
myt$Morph <- factor(myt$Morph)
astr$Exp <- factor(astr$Exp)


# Вычисляем долю T-морфотипа и долю съеденных в садках

myt_1 <- myt %>% group_by(Exp, Box) %>% summarise(P_T = mean(Morph == "t"), t = sum(Morph == "t"), e = sum(Morph == "e"), P_eaten = mean(Status == "eaten"))

ggplot(myt_1, aes(x = P_T, y = P_eaten)) + geom_point() + geom_smooth()


astr_1 <- astr %>% group_by(Exp, Box) %>% summarise(B_aster = sum(Weight), N_aster = n())


myt_aster <- merge(myt_1, astr_1) %>% filter(complete.cases(.))

ggplot(myt_aster, aes(x = B_aster, y = P_eaten)) + geom_point()



# names(myt)
#
# # Отбираем только чистые поселения
# myt2 <- myt %>% filter((Exp == "Exp1" & (Box %in% c(1:6, 22:24)) )|(Exp == "Exp2" & (Box %in% c(1:11, 25:36)) ))
#
#
# # Оставляем только морфотипы, которые представлены в большинстве в чистых поселениях
# myt2_claned <- myt2 %>% filter(((Exp == "Exp1" & (Box %in% c(1:6)) &  (Morph == "e"))|(Exp == "Exp1" & (Box %in% c(22:24)) &  (Morph == "t"))) | ((Exp == "Exp2" & (Box %in% c(1:11)) &  (Morph == "e"))|(Exp == "Exp2" & (Box %in% c(25:36)) &  (Morph == "t"))) )
#
#
# myt2_claned %>% group_by(Morph) %>% summarise(P_eaten = mean(Status == "eaten"))
#
# myt2_claned %>%  summarise(P_eaten = mean(Status == "eaten"))
#
#
#
# # Отбираем только смешанные поселения
#
# myt3 <- myt %>% filter((Exp == "Exp1" & !(Box %in% c(1:6, 22:24)) )|(Exp == "Exp2" & !(Box %in% c(1:11, 25:36)) ))
#
# myt3 %>% group_by(Morph) %>% summarise(P_eaten = mean(Status == "eaten"))
#
# myt3 %>% summarise(P_eaten = mean(Status == "eaten"))
#
#



myt_aster_full <- merge(myt, astr_1) %>% merge(., myt_1) %>% filter(complete.cases(.))

myt_aster_full$Box2 <- paste(myt_aster_full$Box, "_", myt_aster_full$Exp, sep = "")

myt_aster_full <- myt_aster_full %>% mutate(Outcome = ifelse(Status == "eaten", 1, 0))

unique(myt_aster_full$Box2)

str(myt_aster_full)

myt_aster_full$Box2 <- factor(myt_aster_full$Box2)


myt_aster_full$N_own <- ifelse(myt_aster_full$Morph == "e", myt_aster_full$e, myt_aster_full$t)

# Задаем продолжительность экспериментов в часах
myt_aster_full$Duration <- ifelse(myt_aster_full$Exp == "Exp1", 61, 121)



myt_aster_full %>% group_by(Box2, Morph) %>% summarise(N_E = mean(e), N_T = mean(t), N_own = mean(N_own)) %>% View(.)




# Mod_2 <- gam(Outcome ~ s(P_T, by = Morph, bs = "cs") + Morph + B_aster + s(Box2, k = 40, bs = "re" ), family = binomial(link = "logit"), data = myt_aster_full)
#
# summary(Mod_2)
#
# library(mgcViz)
#
# b <- getViz(Mod_2)
# check.gamViz(b)
#
# br <- plot(b, allTerms = T)
# print(br, pages = 1)



library(car)


Mod_3 <- glmer(Outcome ~ Morph + scale(P_T) + scale(N_own) + scale(L) + scale(B_aster) + scale(Duration) + (1|Box2), family = binomial(link = "logit"), data = myt_aster_full )

plot(Mod_3)

vif(Mod_3)

summary(Mod_3)


library(broom.mixed)

tidy(Mod_3)




#############################################33
#### Часть 2. Регуляция таксономического состава



ast_myt <- read.table('Data/Astfood_2019.csv', sep = ',', header = TRUE)

asterias <- read.table('Data/Astfood_Asterias_2019.csv', sep = ',', header = TRUE)



asterias_abund <- asterias %>% group_by(Site, Sample) %>% summarise(N_aster = n(), B_aster = sum(B))



all_ast_abund <- merge(ast_myt, asterias_abund, all.x = T)

all_ast_abund[is.na(all_ast_abund)]<- 0

all_ast_abund$Prop_dead <- with(all_ast_abund, (N_T_dead + N_T_dead)/(N_T_dead + N_T_dead + N_T_alive + N_T_alive))

all_ast_abund$N_dead <- with(all_ast_abund, (N_T_dead + N_T_dead))

all_ast_abund$P_T_alive <- with(all_ast_abund, N_T_alive/(N_T_alive + N_E_alive))



ggplot(all_ast_abund, aes(x = Prop_dead, y = B_aster)) +
  geom_point() +
  facet_wrap(~Site) +
  labs(x = "Доля мертвых", y = "Биомасса морских звезд")


library(mgcv)

all_ast_abund$Site <- factor(all_ast_abund$Site)

all_ast_abund$Stage2 <- factor(all_ast_abund$Stage, labels = c("До нашествия",   "Во время нашествия", "После нашествия"))



all_ast_abund$LogB_aster <-log(all_ast_abund$B_aster + 1)




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



Mod_PT <- glm(cbind(N_T_alive, N_E_alive) ~ Prop_dead * Site, data = all_ast_abund, family = "quasibinomial")



new_data <- all_ast_abund %>% group_by(Site) %>% do(data.frame(Prop_dead = seq(min(.$Prop_dead), max(.$Prop_dead), length.out = 100)))

predicted <- predict(Mod_PT, newdata = new_data, type = "response", se.fit = T)


new_data$Predicted <- predicted$fit
new_data$SE <- predicted$se.fit

ggplot(all_ast_abund, aes(x = Prop_dead)) +
geom_point(data = all_ast_abund, aes(x = Prop_dead, y= Prop_T_alive, color = Stage2), size = 3) +
  facet_wrap(~Site) +
  geom_ribbon(data = new_data, aes(ymin = Predicted - 1.96*SE, ymax = Predicted + 1.96*SE), alpha = 0.2) +
  geom_line(data = new_data, aes(x = Prop_dead, y = Predicted),  color = "blue", size = 1) +
  guides (fill = "none") +
  labs(x = "Доля мертвых", y = "Доля T-морфотипа среди живых", color = "") +
  geom_line(data = new_dat_ast, aes(y = B_ast_predicted/max(B_ast_predicted))) +
  ylim(0, 1) +
  scale_color_manual(values = c("blue", "red", "gray")) +
  theme_bw()+
  theme(legend.position = "bottom")



