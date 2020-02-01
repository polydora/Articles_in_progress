library(ggplot2)
library(dplyr)
library(car)
library(reshape2)
library(MASS)
library(mgcv)
library(lme4)
library(gridExtra)




theme_set(theme_bw() + theme(axis.title = element_text(size = 30), legend.text =  element_text(size = 30)))







################ SEA STARS ##################################

# Размеры съеденных и живых ####

ast_size <- read.table('Data/Astfood_Length.csv', sep = ',', header = TRUE)
 

ggplot(ast_size, aes(x = Length, fill = Status)) + geom_density(alpha = 0.7) + scale_fill_manual(values = c("yellow", "black")) + labs(x = "Размер мидии", fill = "") + theme(legend.position = c(0.6, 0.8))



ggplot(ast_size, aes(x = Status, y = Length, fill = Status)) + geom_boxplot() + facet_wrap( ~ Morph) + scale_fill_manual(values = c("yellow", "darkgray")) + labs(x = "", y = "Размер мидии", fill = "") + guides(fill = "none")







ast_myt <- read.table('Data/Astfood_2019.csv', sep = ',', header = TRUE)

asterias <- read.table('Data/Astfood_Asterias_2019.csv', sep = ',', header = TRUE)



# ast_myt <- ast_myt[ast_myt$Sample != "Astfood_8", ]
# asterias <- asterias[asterias$Sample != "Astfood_8", ]
# 
# 
# ast_N <- ast_myt %>% group_by(Site, Type_2, Sample) %>% summarise(N_alive_T = sum(Status == "alive" & Morph == "T" ), N_dead_T = sum(Status == "dead" & Morph == "T" ), N_alive_E = sum(Status == "alive" & Morph == "E" ), N_dead_E = sum(Status == "dead" & Morph == "E" ), N_alive_total = sum(Status == "alive"), Prop_dead = mean(Status == "dead"), Prop_T_total = mean(Morph == "T")) 

asterias_abund <- asterias %>% group_by(Site, Sample) %>% summarise(N_aster = n(), B_aster = sum(B))



all_ast_abund <- merge(ast_myt, asterias_abund, all.x = T)

all_ast_abund[is.na(all_ast_abund)]<- 0 

all_ast_abund[all_ast_abund$Stage == "Stage_3", c("N_T_alive", "N_T_dead", "N_E_alive", "N_E_dead", "N_aster", "B_aster")] <- all_ast_abund[all_ast_abund$Stage == "Stage_3", c("N_T_alive", "N_T_dead", "N_E_alive", "N_E_dead", "N_aster", "B_aster")] * 20  

all_ast_abund[all_ast_abund$Stage != "Stage_3", c("N_T_alive", "N_T_dead", "N_E_alive", "N_E_dead", "N_aster", "B_aster")] <- all_ast_abund[all_ast_abund$Stage != "Stage_3", c("N_T_alive", "N_T_dead", "N_E_alive", "N_E_dead", "N_aster", "B_aster")] * 182  


all_ast_abund$Prop_dead <- with(all_ast_abund, (N_T_dead + N_T_dead)/(N_T_dead + N_T_dead + N_T_alive + N_T_alive))


all_ast_abund$N_dead <- with(all_ast_abund, (N_T_dead + N_T_dead))

all_ast_abund$Prop_T_alive <- with(all_ast_abund, N_T_alive/(N_T_alive + N_E_alive))


all_ast_abund$Prop_T_total <- with(all_ast_abund, (N_T_alive + N_T_dead)/(N_T_alive + N_T_dead + N_E_alive + N_E_dead) )

all_ast_abund$Prop_T_dead <- with(all_ast_abund, (N_T_dead)/(N_T_dead + N_E_dead) )



# Связи с долей съеденных #####


ggplot(all_ast_abund, aes(x = N_dead, y = B_aster)) + geom_point() + facet_wrap(~Site)


ggplot(all_ast_abund, aes(x = N_dead, y = Prop_T_alive)) + geom_point() + facet_wrap(~Site) + geom_smooth(method = "lm")


ggplot(all_ast_abund, aes(x = Stage, y = Prop_T_alive)) + geom_boxplot()  + facet_wrap(~Site)


# Модель, описывающее поведение обилия звезд

Mod_ast_1 <- gam((B_aster) ~ s(Prop_dead, by = Site) + Site, data = all_ast_abund)

summary(Mod_ast_1)

plot(Mod_ast_1)


new_dat_ast <- all_ast_abund %>% group_by(Site) %>% do(expand.grid(levels = seq(from = 0,to = 1,by = 0.01), Prop_dead = seq(min(.$Prop_dead), max(.$Prop_dead), by = 0.01)))


new_dat_ast$Predict <- predict(Mod_ast_1, newdata = new_dat_ast)




Pl_aster_abund <- ggplot(new_dat_ast, aes(x = Prop_dead)) + geom_raster(aes(y = levels, fill = Predict), alpha = 0.6) + facet_wrap(~Site) + scale_fill_gradient(low = "white", high = "red")



# Модель, описывающая связь дол Т морфотипа среди живых и долм мертвых моллсков

Mod_1 <- glm(cbind(N_T_alive, N_E_alive) ~ Prop_dead * Site, data = all_ast_abund, family = "quasibinomial")

drop1(Mod_1, test = "Chi")

Mod_2 <- update(Mod_1, . ~ . - Prop_dead:Site)

drop1(Mod_2, test = "Chi")


new_data <- all_ast_abund %>% group_by(Site) %>% do(data.frame(Prop_dead = seq(min(.$Prop_dead), max(.$Prop_dead), length.out = 100)))

predicted <- predict(Mod_2, newdata = new_data, type = "response", se.fit = T)


new_data$Predicted <- predicted$fit 
new_data$SE <- predicted$se.fit



Pl_aster_abund + geom_line(data = new_data, aes(x = Prop_dead, y = Predicted),  color = "blue", size = 1.5) + facet_wrap(~Site)  


Pl_aster_abund + geom_point(data = all_ast_abund, aes(x = Prop_dead, y= Prop_T_alive)) + facet_wrap(~Site) + geom_ribbon(data = new_data, aes(ymin = Predicted - 1.96*SE, ymax = Predicted + 1.96*SE), alpha = 0.2) +  geom_line(data = new_data, aes(x = Prop_dead, y = Predicted),  color = "blue", size = 1) + guides (fill = "none") + labs(x = "Относительное обилие мертвых моллюсков", y = "Доля M.trossulus среди живых")



ggplot(data = all_ast_abund, aes(x = Prop_dead, y= Prop_T_alive, group = Site)) + 
  geom_point(size = 4, shape = 21,  aes(fill = Site)) +
  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), size = 1.5) +
  labs(x = "Относительное обилие мертвых моллюсков", y = "Доля M.trossulus среди живых") + theme(axis.title = element_text(size = 19)) +
  guides(fill = "none")





Pl_aster_abund + geom_point(data = all_ast_abund, aes(x = Prop_dead, y= (N_T_alive + N_E_alive)/ max((N_T_alive + N_E_alive))  ))  + guides(fill = "none") + labs(y = "Плотность поселения живых мидий", x = "Относительное обилие мертвых створок")  + theme(axis.title = element_text(size = 15))

ggplot(all_ast_abund, aes(x = N_dead)) + geom_point(aes(y = N_T_alive + N_E_alive)) + facet_wrap(~ Site)


ggplot(all_ast_abund, aes(x = B_aster, y = N_dead)) + geom_point(size = 4, shape = 21, fill = "red")+  labs(x = "Биомасса звезд", y = "Обилие мертвых мидий") + theme(axis.title = element_text(size = 20))




ggplot(all_ast_abund, aes(x = Prop_T_alive, y =  Prop_T_dead)) + geom_point() + facet_wrap(~ Site) + geom_abline() + xlim(0,1) + ylim(0,1) + labs(x = "Доля M.trossulus среди живых", y = "Доля M.trossulus среди мертвых")



# Pay charts 
total_N <- melt(all_ast_abund %>% filter (Stage == "Stage_3" | Stage == "Stage_2" ) %>% summarise(N_T_dead = sum(N_T_dead), N_E_dead = sum(N_E_dead),  N_T_alive = sum(N_T_alive),  N_E_alive= sum(N_E_alive)) )

total_N$Status <- c("dead", "dead", "alive", "alive")

total_N <- dcast( variable ~ Status, data = total_N, value.var = "value")

Pl_alive <- ggplot(total_N, aes(x ="", y = alive, fill = variable)) + geom_bar(stat= "identity", width=1, color = "black") + coord_polar("y", start=0)  + theme(axis.text = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), axis.title = element_blank())+ guides(fill="none")

Pl_dead <- ggplot(total_N, aes(x ="", y = dead, fill = variable)) + geom_bar(stat= "identity", width=1, color = "black") + coord_polar("y", start=0) + theme(axis.text = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), axis.title = element_blank()) + guides(fill="none")


grid.arrange(Pl_alive, Pl_dead, ncol = 2)



################ OYSTERCATCHERS #################


hem <- read.table("Data/Hotred_Length.csv", sep = ",", header = T)

hem <- hem[hem$Sample != "C", ]


## Size-structure of shells #####

ggplot(hem, aes(x = Length, fill = Status)) + geom_density(alpha = 0.7) + scale_fill_manual(values = c("yellow", "black")) + labs(x = "Размер мидии", fill = "") + theme(legend.position = c(0.6, 0.8))
 

## Зависимость от доли мертвых у куликов #####

hem_abund <- hem %>% group_by(Site, Square) %>% summarise(N_T_alive = sum(Status == "alive" & Morph == "T")/3*182, N_E_alive = sum(Status == "alive" & Morph == "E")/3*182, N_T_dead = sum(Status == "dead" & Morph == "T"), N_E_dead = sum(Status == "dead" & Morph == "E"))
  



hem_abund$N_dead <- with(hem_abund, N_T_dead + N_E_dead)

hem_abund$N_alive <- with(hem_abund, N_T_alive + N_E_alive)

hem_abund$Prop_T_alive <- with(hem_abund, N_T_alive/(N_T_alive + N_E_alive))  

hem_abund$Prop_dead <- with(hem_abund, N_dead/(N_dead + N_alive ))  
hem_abund$Prop_T_dead <- with(hem_abund, N_T_dead/(N_T_dead + N_E_dead))  

hem_abund$Prop_T <- with(hem_abund,  N_T_alive / (N_T_alive + N_E_alive))



ggplot(hem_abund, aes(x = N_dead, y = Prop_T)) + geom_point()  


Mod_hem_1 <- glmer(Prop_T ~ scale(N_dead) + (1|Site), data = hem_abund, family = "binomial", weights = N_alive)  

summary(Mod_hem_1)



hem_abund_bank <- hem %>% group_by(Site) %>% summarise(N_T_alive = sum(Status == "alive" & Morph == "T"), N_E_alive = sum(Status == "alive" & Morph == "E"), N_T_dead = sum(Status == "dead" & Morph == "T"), N_E_dead = sum(Status == "dead" & Morph == "E"), n = length(unique(Square)))


hem_abund_bank$N_dead <- with(hem_abund_bank, N_T_dead + N_E_dead)/hem_abund_bank$n 

hem_abund_bank$N_alive <- with(hem_abund_bank, N_T_alive + N_E_alive)/hem_abund_bank$n / 3 * 182


hem_abund_bank$Prop_T <- with(hem_abund_bank,  N_T_alive / (N_T_alive + N_E_alive))

hem_abund_bank$Prop_dead <- with(hem_abund_bank, N_dead/N_alive)


ggplot(hem_abund_bank, aes(x = N_dead, y = Prop_T)) + geom_point(size = 4, color = "red") + labs(x = "Среднее количество съеденных на кв. метр", y = "Доля M.trossulus  среди живых") + geom_point(data =  hem_abund) +  geom_smooth(method = "glm", method.args = list(family = "quasibinomial"), size = 1.5) + theme(axis.title = element_text(size = 19))


ggplot(hem_abund, aes(x = Prop_T_alive, y = Prop_T_dead)) + geom_point() + geom_abline() + xlim(0,1) + ylim(0,1) + labs(x = "Доля M.trossulus среди живых", y = "Доля M.trossulus среди мертвых")


# Pay charts 
total_N <- melt(hem_abund_bank  %>% summarise(N_T_dead = sum(N_T_dead), N_E_dead = sum(N_E_dead),  N_T_alive = sum(N_T_alive),  N_E_alive= sum(N_E_alive)) )

total_N$Status <- c("dead", "dead", "alive", "alive")

total_N <- dcast( variable ~ Status, data = total_N, value.var = "value")

Pl_alive <- ggplot(total_N, aes(x ="", y = alive, fill = variable)) + geom_bar(stat= "identity", width=1, color = "black") + coord_polar("y", start=0)  + theme(axis.text = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), axis.title = element_blank())+ guides(fill="none")

Pl_dead <- ggplot(total_N, aes(x ="", y = dead, fill = variable)) + geom_bar(stat= "identity", width=1, color = "black") + coord_polar("y", start=0) + theme(axis.text = element_blank(), panel.grid = element_blank(), panel.border = element_blank(), axis.title = element_blank()) + guides(fill="none")


grid.arrange(Pl_alive, Pl_dead, ncol = 2)




