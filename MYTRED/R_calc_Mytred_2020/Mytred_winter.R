library(readxl)
library(ggplot2)
library(dplyr)
library(nlme)
library(car)
library(lme4)




#Adding data
winter <- read_excel('Data/Mytred_Growth_Winter_2021.xlsx', sheet = 'Winter installeation')

nrow(winter)

winter <- winter %>% filter( (Sample_site == "Kanal" & Morphotype == "t") | (Sample_site == "2R" & Morphotype == "e")) 


#Only Alive mussels allowed 

winter1 <- winter %>% filter(Status == 'Alive')
nrow(winter1)

str(winter1)

# View(winter1)

#Adding FON data


library(reshape2)

exp_mussel <- winter1 %>%  group_by(Cage_ID, Morphotype, Type) %>% summarise(N_alive = n()) %>% dcast(., Cage_ID ~ Morphotype ) %>% filter(! (Cage_ID %in% c("K1", "K2", "K3", "K4")) )

winter_f <- read_excel('Data/Mytred_Growth_Winter_2021.xlsx', sheet = 'Fone_mussel')

winter_f <- winter_f[order(winter_f$Cage_ID), ]

winter_f$E_alive[1:20] <- winter_f$E_alive[1:20] + exp_mussel$e
winter_f$T_alive[1:20] <- winter_f$T_alive[1:20] + exp_mussel$t

winter_f <- winter_f %>% mutate(Init_N = 0)
winter_f$Init_N[1:9] <- 62
winter_f$Init_N[10:20] <- 102
winter_f$Init_N[21:24] <- 22

winter_f <- winter_f %>% mutate(Dens = NA)
winter_f$Dens[1:9] <- "Medium"
winter_f$Dens[10:20] <- "High"
winter_f$Dens[21:24] <- "Control"







winter_f <- winter_f %>% mutate(N_total = E_alive + T_alive, P_Tr = T_alive /(T_alive + E_alive ), P_dead = (T_dead + E_dead)/(T_dead + E_dead + N_total), P_dead2 = N_total/Init_N)

ggplot(winter_f, aes(x = Type, y = P_Tr)) + geom_boxplot()
ggplot(winter_f, aes(x = Type, y = P_dead2)) + geom_boxplot()
ggplot(winter_f, aes(x = Type, y = P_dead)) + geom_boxplot()






winter2 <- merge(winter1, winter_f, by = 'Cage_ID')


winter2$Init_size2 <- as.numeric(winter2$Init_size)
winter2$Fin_size2 <- as.numeric(winter2$Fin_size)



winter2$Growth <- winter2$Fin_size2 - winter2$Init_size2


winter2$Growth2 <- winter2$Growth
winter2$Growth2[winter2$Growth2 < 0] <- 0


#Growth
ggplot(data = winter2, aes(x = P_Tr, y = Growth2, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))


#Condition Index

winter2$W_tis <- as.numeric(winter2$W_tis)
winter2$W_sh <- as.numeric(winter2$W_sh)

winter2$Con_Ind <- winter2$W_tis/(winter2$W_tis + winter2$W_sh)



# View(winter2)

ggplot(data = winter2, aes(x = P_Tr, y = Con_Ind, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))

ggplot(data = winter2, aes(x = P_Tr, y = W_tis, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))

ggplot(data = winter2, aes(x = P_Tr, y = W_sh, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))


ggplot(data = winter2, aes(x = P_Tr, y = Fin_size2, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))

ggplot(winter_f, aes(x= P_Tr, y = N_total)) + geom_point()


ggplot(winter_f, aes(x= P_Tr, y = P_dead)) + geom_point() + geom_smooth(method = "lm")

ggplot(winter_f, aes(x= N_total, y = P_dead)) + geom_point() + geom_smooth(method = "lm")


ggplot(data = winter2, aes(x = Dens, y = Growth2, fill = Type.y)) + geom_boxplot() 




#models
#GROWTH

growth_mod_dummy <- glm(data = winter2, Growth2 ~ Morphotype + P_Tr + N_total + Init_size2 + P_dead2)
vif(growth_mod_dummy)


growth_mod <- gls(data = winter2, Growth2  ~ Morphotype * P_Tr + N_total + Init_size2 + P_dead2)

growth_mod_var1 <- update(growth_mod, weights = varExp(form = ~ N_total))

AIC(growth_mod, growth_mod_var1)

plot(growth_mod, which = 1)
plot(growth_mod_var1, which = 1)

summary(growth_mod_var1)

growth_mod_var1_ML <- update(growth_mod_var1, method = "ML")
drop1(growth_mod_var1_ML, test = "Chi")

growth_mod_var1_ML2 <- update(growth_mod_var1_ML, . ~ . - Morphotype:P_Tr )
drop1(growth_mod_var1_ML2, test = "Chi")

growth_mod_var1_2 <- update(growth_mod_var1_ML2, method = "REML")
plot(growth_mod_var1_2)

summary(growth_mod_var1_2)

growth_mod1 <- update(growth_mod, .~.-Morphotype:P_Tr)
drop1(growth_mod1, test = "Chi")

summary(growth_mod1)

plot(growth_mod1, which = 1)




#CONDITION INDEX


conind_mod <- glm(data = winter2, Con_Ind ~ Morphotype * P_Tr + N_total + Fin_size2 + P_dead2)
drop1(conind_mod, test = "Chi")


conind_mod2 <- update(conind_mod, . ~ . - Morphotype:P_Tr)
drop1(conind_mod2, test = "Chi")

plot(conind_mod2, which = 1)
summary(conind_mod)



#Growth again
coe_growth <- coef(growth_mod1)

mydata_growth <- winter2 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total), Init_size2 = mean(.$Init_size2)))

# View(mydata_growth)

X_growth <- model.matrix(~ Morphotype + P_Tr + N_total + Init_size2, data = mydata_growth)
mydata_growth$predicted_growth <- X_growth%*%coe_growth


View(mydata_growth)

ggplot(mydata_growth, aes(x = P_Tr, y = predicted_growth, colour = Morphotype)) + geom_line()

vcov(growth_mod1)

mydata_growth$se_growth <- sqrt(diag(X_growth%*%vcov(growth_mod1)%*%t(X_growth)))

ggplot(mydata_growth, aes(x = P_Tr, y = predicted_growth, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_growth -1.96*se_growth, ymax = predicted_growth + 1.96*se_growth, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red')) + ylab('Прирост, мм') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид')

control_growth <- winter %>% filter(Type == 'K')

str(control_growth)


control_growth$Growth_num <- as.numeric(control_growth$Fin_size) - as.numeric(control_growth$Init_size)

control_growth1 <- control_growth %>% filter(Status == 'Alive')
# View(control_growth1)

control_growth2 <- control_growth1[complete.cases(control_growth1$Growth_num),]
# View(control_growth2)

control_growth3 <- control_growth2 %>% group_by(Morphotype) %>% summarise(Mean_Growth = mean(Growth_num), SE = sd(Growth_num)/sqrt(n()))
head(control_growth3)


winter2 %>% filter(Type == "K") %>% 
ggplot(. , aes(x = Morphotype, y = Growth2)) + geom_boxplot(aes(fill = Cage_ID))


ggplot(control_growth3, aes(x = Morphotype, y = Mean_Growth)) + geom_col()

# View(control_growth3)

ggplot(mydata_growth, aes(x = P_Tr, y = predicted_growth, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_growth -1.96*se_growth, ymax = predicted_growth + 1.96*se_growth, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red'))  + ylab('Прирост, мм') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид') + geom_hline(data = control_growth3, aes(yintercept = Mean_Growth, color = Morphotype), linetype = 2)




#Condition_Index

coe_conind <- coef(conind_mod)

mydata_conin <- winter2 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total), Fin_size2 = mean(.$Fin_size2)))

# View(mydata_conin)

X_conin <- model.matrix(~ Morphotype * P_Tr + N_total + Fin_size2, data = mydata_conin)
mydata_conin$predicted_conin <- X_conin%*%coe_conind



ggplot(mydata_conin, aes(x = P_Tr, y = predicted_conin, colour = Morphotype)) + geom_line()


mydata_conin$se_conind <- sqrt(diag(X_conin%*%vcov(conind_mod)%*%t(X_conin)))

ggplot(mydata_conin, aes(x = P_Tr, y = predicted_conin, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_conin -1.96*se_conind, ymax = predicted_conin + 1.96*se_conind, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red'))+ geom_point(data = winter2, aes(x = P_Tr, y = Con_Ind)) + ylab('Condition index') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид')


control_conind <- winter %>% filter(Type == 'K')
View(control_conind)

str(control_conind)


control_conind$Conind_num <- as.numeric(control_growth$W_tis) / (as.numeric(control_conind$W_tis) + as.numeric(control_conind$W_sh))

control_conind1 <- control_conind %>% filter(Status == 'Alive')
View(control_conind1)

control_conind2 <- control_conind1[complete.cases(control_conind1$Conind_num),]
View(control_conind2)

control_conind3 <- control_conind2 %>% group_by(Morphotype) %>% summarise(Mean_Conind = mean(Conind_num), SE = sd(Conind_num)/sqrt(n()))
head(control_conind3)
View(control_conind3)

ggplot(mydata_conin, aes(x = P_Tr, y = predicted_conin, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_conin -1.96*se_conind, ymax = predicted_conin + 1.96*se_conind, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red')) + geom_point(data = winter2, aes(x = P_Tr, y = Conind_num)) + ylab('Condition index') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид') + geom_hline(data = control_conind3, aes(yintercept = Mean_Conind, color = Morphotype), linetype = 2)





#######################
# Try to use categorical predictor

M_growth <- lme(Growth2 ~ Morphotype * Type + N_total + Init_size2, random =  ~1|Cage_ID, data = winter2)

M_growth_ML <- update(M_growth, method = "ML")

plot(M_growth)

drop1(M_growth_ML, test = "Chi")

M_growth_ML2 <- update(M_growth_ML, . ~ . - Morphotype:Type  )
drop1(M_growth_ML2)

M_growth_ML3 <- update(M_growth_ML2, . ~ . - Init_size2  )
drop1(M_growth_ML3)

M_growth_ML4 <- update(M_growth_ML3, . ~ . - N_total  )
drop1(M_growth_ML4)

M_growth3 <- update(M_growth_ML3, method = "REML")
summary(M_growth3)


ggplot(winter_f, aes(x = Fon_type, y = N_total)) + geom_point()




ggplot(winter2, aes(x = Type, y = Growth2)) + geom_boxplot()





#########################################
#Model of deadness

winter3 <- winter %>% mutate(Status2 = ifelse(Status == "Alive", 0, 1)) %>% merge(., winter_f, by = "Cage_ID")

winter3$Init_size2 <- as.numeric(winter3$Init_size)



M_dead <- glm(Status2 ~ Morphotype * P_Tr + N_total, data = winter3, family = "binomial" ) 

drop1(M_dead)

plot(M_dead)



my_data_deadness <- winter3 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total)))

my_data_deadness$Predicted <- predict(M_dead, newdata = my_data_deadness, type = "response")


logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

X <- model.matrix(~ Morphotype * P_Tr + N_total , data = my_data_deadness)
b <- coef(M_dead)

my_data_deadness$fit_eta <- X %*% b

my_data_deadness$se_eta <- sqrt(diag(X %*% vcov(M_dead) %*% t(X))) #Ошибки в шкале логитов


# Границы доверительного интервала
my_data_deadness$lwr <- logit_back(my_data_deadness$fit_eta - 1.96 * my_data_deadness$se_eta)
my_data_deadness$upr <- logit_back(my_data_deadness$fit_eta + 1.96 * my_data_deadness$se_eta)



dead_sum <- winter3 %>% group_by(Cage_ID, Morphotype) %>% summarise(P_Tr = mean(P_Tr), P_dead = mean(Status2))


ggplot(my_data_deadness, aes(x = P_Tr, y = Predicted)) + geom_line(aes(color = Morphotype)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) + scale_color_manual(values = c("blue", "red")) + geom_point(data = dead_sum, aes(y = P_dead, color = Morphotype))


