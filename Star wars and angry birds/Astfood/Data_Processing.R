library(ggplot2)
library(dplyr)
library(car)
library(reshape2)
library(MASS)


myt <- read.table('Data/Astfood_2019.csv', sep = ';', header = TRUE)
ast <- read.table('Data/Astfood_Asterias_2019.csv', sep = ';', header = TRUE, dec = ',')

ast_abund <- ast %>% group_by(Sample) %>% summarise(N_aster = n(), B_aster = sum(B))


myt_ast <- merge(myt, ast_abund, by = 'Sample')

myt_ast$Site2 <- as.character(myt_ast$Site)
myt_ast$Site2[myt_ast$Site2 != "Kurichek"] <- "Luvenga" 

myt_ast$Prop_dead <- with(myt_ast, (N_T_dead + N_E_dead)/(N_T_dead + N_E_dead + N_T_alive + N_E_alive))

ggplot(myt_ast[myt_ast$Type == 'Main',], aes(x = B_aster, y = Prop_dead)) + geom_point() + geom_smooth(method = 'lm')


myt_ast$PropT_dead <- with(myt_ast, (N_T_dead)/(N_T_dead + N_E_dead))
myt_ast$PropT_alive <- with(myt_ast, (N_T_alive)/(N_T_alive + N_E_alive))

myt_ast$PropT <- with(myt_ast, (N_T_alive + N_T_dead)/(N_T_alive + N_E_alive+ N_T_dead+ N_E_dead))


myt_ast1 <- myt_ast[myt_ast$Type == 'Main',]

myt_ast2 <- myt_ast[myt_ast$Type == 'Z',]




ggplot(myt_ast1, aes(x = PropT_alive, y = PropT_dead)) + geom_point(color = "blue") + geom_smooth(method = 'lm') + geom_abline() + geom_point(data = myt_ast2, color = "black")
# 
# M <- lm(N_T_dead ~ N_T_alive + N_E_dead + N_E_alive + B_aster, data = myt_ast1)
# summary(M)
# 
# M <- glm(N_T_dead ~ N_T_alive + N_E_dead + N_E_alive + B_aster, data = myt_ast1, family = "quasipoisson")
# summary(M)

M <- glm.nb(N_T_dead ~ N_T_alive + N_E_dead + N_E_alive + B_aster + Site2 , data = myt_ast1)
summary(M)

plot(M)



Anova(M)

ggplot(myt_ast1, aes(x = B_aster, y = N_T_dead)) + geom_point(color = "blue") + geom_smooth(method = 'lm') + facet_wrap(~Site2) 

myt_melt <- melt(myt_ast1, id.vars = c("Sample", "B_aster", "N_aster", "Lon", "Lat", "Site", "Site2", "Type", "Prop_dead", "PropT_alive", "PropT_dead"), variable.name = "Status", value.name = "N")


ggplot(myt_melt[myt_melt$Status %in% c("N_T_dead", "N_E_dead"), ], aes(x = PropT_alive, y = N, color = Status)) + geom_point() + geom_smooth(method = "glm", method.args = list(family = "quasipoisson"))



Pdead_ET <- myt_ast2 %>% group_by(Site2) %>% summarise(P_dead_T = sum(N_T_dead) / (sum(N_T_dead) + sum(N_T_alive)), P_dead_E = sum(N_E_dead) / (sum(N_E_dead) + sum(N_E_alive)))



Bayes_probs <- expand.grid(PT = seq(0,1,0.01), Site2 = unique(Pdead_ET$Site2))

Bayes_probs2 <- merge(Pdead_ET, Bayes_probs )


Bayes_probs2$P_T_dead <- with(Bayes_probs2, (PT * P_dead_T)/(PT * P_dead_T + (1-PT) * P_dead_E ) )

ggplot(Bayes_probs2, aes(x = PT, y = P_T_dead)) + geom_line() + facet_wrap(~Site2) + geom_point(data = myt_ast1, aes(x = PropT, y = PropT_dead, size = B_aster)) + geom_smooth(data = myt_ast1, aes(x = PropT, y = PropT_dead) , method = "lm") 

