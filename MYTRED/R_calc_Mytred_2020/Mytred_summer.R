library(readxl)
library(ggplot2)
library(dplyr)
library(nlme)
library(car)
library(lme4)
library(betareg)




#Adding data
summer <- read_excel('Data/Mytred_Growth_Summer_2021_Anton_modified.xlsx', sheet = 'Growth_data')

nrow(summer)

# удаляем тех меченных особей, которые имеют морфотипы не соотвествующие месту взятия мидий
summer_pure <- summer %>% filter( (Sample_site == "Tr" & Morphotype == "t") | (Sample_site == "Ed" & Morphotype == "e")) 


#Only Alive mussels allowed 

summer1 <- summer_pure %>% filter(Status == 'Alive')
# View(summer1)

#Adding FON data


library(reshape2)

exp_mussel <- summer %>%  group_by(Cage_ID, Morphotype, Type, Status) %>% summarise(N_alive = n()) %>% dcast(., Cage_ID ~ Morphotype + Status ) 

exp_mussel[is.na(exp_mussel)] <- 0



summer_f <- read_excel('Data/Mytred_Growth_Summer_2021_Anton_modified.xlsx', sheet = 'Fone_mussel')


# Добавляем к численности фоновых мидий численность меченных 
summer_f <- summer_f[order(summer_f$Cage_ID), ]

summer_f$E_alive[1:12] <- summer_f$E_alive[1:12] + exp_mussel$e_Alive[1:12]
summer_f$T_alive[1:12] <- summer_f$T_alive[1:12] + exp_mussel$t_Alive[1:12]

summer_f$E_dead[1:12] <- summer_f$E_dead[1:12] + exp_mussel$e_Dead[1:12]
summer_f$T_dead[1:12] <- summer_f$T_dead[1:12] + exp_mussel$t_Dead[1:12]




# Пересчитываем Dead_ratio с учетом добавления меченных мидий
summer_f$Dead_ratio <- with(summer_f, (E_dead + T_dead) / (E_alive + E_dead + T_alive + T_dead))


summer_f <- summer_f %>% mutate(N_total = E_alive + T_alive)

##############################################################################
# Зависимость доли погибших мидий (фон+экспериментальные) от структуры поселения
ggplot(summer_f, aes(x = P_Tr, y = Dead_ratio)) + geom_point(aes(color = Fon_type), size = 3) + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F)


# Датафрейм с общей характеристикой условий в садках
total_condition <- summer_f %>% mutate(dead_T = T_dead/(T_alive + T_dead), dead_E = E_dead/(E_alive + E_dead))


# Переводим в длинный формат
total_condition2 <- melt(total_condition, id.vars = c("Cage_ID", "Experiment", "E_alive", "E_dead", "T_alive", "T_dead",   "P_Tr", "Dead_ratio", "Fon_type", "N_total"), variable.name = "Dead_morph", value.name = "P_dead_morph")


ggplot(total_condition2, aes(x = P_Tr, y = P_dead_morph, color = Dead_morph)) + geom_point(size = 3) + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = F)



# Строим модель на основе beta-distribution
total_condition2$P_dead_morph[total_condition2$P_dead_morph == 0] = 0.001 #Это нужно для того, чтобы удовлетоворить требование бета-распределения, которое определено на интервале ]0,1[
total_condition2$P_dead_morph[total_condition2$P_dead_morph == 1] = 0.999


model.beta <- betareg(P_dead_morph ~ P_Tr*Dead_morph + N_total, data = total_condition2, link = "logit")


plot(model.beta, which = 1)



# model.beta_dummy <- betareg(P_dead_morph ~ P_Tr + Dead_morph + N_total, data = total_condition2, link = "logit")

# vif(model.beta_dummy)

summary(model.beta)

my_data <- expand.grid(Dead_morph = c("dead_E", "dead_T"), P_Tr = seq(from = min(total_condition2$P_Tr), to = max(total_condition2$P_Tr), length.out = 100))


# my_data$fit <- predict(model.beta, newdata = my_data, type = "response")

# Нашел пакетик, который делает расчеты для визуализации более простыми
library(effects)
Effects <- as.data.frame(allEffects(model.beta, xlevels=my_data))



# определяем доверительные интервалы для маргинальных эффектов P_tr по фактору Dead_morph
library(emmeans )

Dead_ratio_trends_by_Dead_morph <- emtrends(model.beta, specs = ~ Dead_morph, var="P_Tr")
summary(Dead_ratio_trends_by_Dead_morph) 

pairs(Dead_ratio_trends_by_Dead_morph)




ggplot(Effects[[2]], aes(x = P_Tr, y = fit)) + geom_line(aes(color = Dead_morph)) + geom_point(data = total_condition2, aes(y = P_dead_morph, color = Dead_morph, shape = Fon_type), size = 3) + geom_ribbon(data = Effects[[2]], aes(ymin = lower, ymax = upper, group = Dead_morph), alpha=0.2) + scale_color_manual(values = c("blue", "red"))

ggplot(total_condition, aes(x = P_Tr, y = N_total)) +  geom_point(aes(shape = Fon_type), size = 3, position = position_jitter(width = 0.1)) + ylim(0, 100)





##################################################





summer2 <- merge(summer1, summer_f, by = 'Cage_ID')



# summer2 <- summer2 %>% filter(Type != "K")

# View(summer2)

#Calculating growth
str(summer2)

#It seems dat Init size and Fin size are not numeric but charrecters 

summer2$Init_size2 <- as.numeric(summer2$Init_size)
summer2$Fin_size2 <- as.numeric(summer2$Fin_size)
is.na(summer2$Fin_size2)


summer2 <- summer2 %>% filter(!is.na(Fin_size2))


summer2$Growth <- summer2$Fin_size2 - summer2$Init_size2

summer2 <- summer2 %>% filter(Growth > -4)

summer2$Growth2 <- summer2$Growth
summer2$Growth2[summer2$Growth2 < 0] <- 0


#Growth
ggplot(data = summer2, aes(x = P_Tr, y = Growth2, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))


#Condition Index

summer2$W_tis <- as.numeric(summer2$W_tis)
summer2$W_sh <- as.numeric(summer2$W_sh)

summer2$Con_Ind <- summer2$W_tis/(summer2$W_tis + summer2$W_sh)



# View(summer2)

ggplot(data = summer2, aes(x = P_Tr, y = Con_Ind, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))

ggplot(data = summer2, aes(x = P_Tr, y = W_tis, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))

ggplot(data = summer2, aes(x = P_Tr, y = W_sh, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))


ggplot(data = summer2, aes(x = P_Tr, y = Fin_size2, colour = Morphotype)) + geom_point() + geom_smooth(method = 'lm') + scale_color_manual(values = c('blue' , 'red'))




#models
#GROWTH

growth_mod_dummy <- gls(data = summer2, Growth2 ~ Morphotype + P_Tr + N_total + Init_size2)
vif(growth_mod_dummy)


growth_mod1 <- gls(data = summer2, Growth2 ~ Morphotype * P_Tr + N_total + Init_size2, weights = varPower(form = ~ N_total))

growth_mod2 <- gls(data = summer2, Growth2 ~ Morphotype * P_Tr + N_total + Init_size2, weights = varPower(form = ~ N_total|Morphotype))

AIC(growth_mod1, growth_mod2)

plot(growth_mod2)


growth_mod2_ML <- update(growth_mod2, method = "ML")
drop1(growth_mod2_ML, test = "Chi") 

# Здесь надо подумать!!! ТАк как после сокращения модели будет считаться, что оба морфотипа реагируют на P_Tr одинаково, что, кажется, не так
# 
# growth_mod2_ML_2 <- update(growth_mod2_ML,. ~ . - Morphotype:P_Tr)
# drop1(growth_mod2_ML_2, test = "Chi") 
# 
# growth_mod2_ML_3 <- update(growth_mod2_ML_2,. ~ . - Init_size2)
# drop1(growth_mod2_ML_3, test = "Chi") 
# 
# growth_mod3 <- update(growth_mod2_ML_3, method = "REML")

summary(growth_mod2)

AIC(growth_mod, growth_mod2, growth_mod3)

library(broom.mixed)
tidy(growth_mod3)


##### Визуализация модели прироста ########

mydata_growth <- summer2 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total), Init_size2 = mean(.$Init_size2)))


Effects_growth <- as.data.frame(allEffects(growth_mod2, xlevels=mydata_growth))

ggplot(Effects_growth[[3]], aes(x = P_Tr, y = fit)) + geom_line(aes(color = Morphotype))  + geom_ribbon(data = Effects_growth[[3]], aes(ymin = lower, ymax = upper, group = Morphotype), alpha=0.2) + scale_color_manual(values = c("blue", "red")) + geom_point(data = summer2, aes(y = Growth2, color = Morphotype))

# install.packages("devtools")
# devtools::install_github("cardiomoon/ggiraphExtra")


# определяем доверительные интервалы для маргинальных эффектов P_tr по фактору Morphotype
library(emmeans )

Growth2_trends_morph <- emtrends(growth_mod2, specs = ~ Morphotype, var="P_Tr", mode = "df.error")
summary(Growth2_trends_morph) 

pairs(Growth2_trends_morph)













##########################################################

growth_mod1 <- update(growth_mod, .~.-Morphotype:P_Tr)
drop1(growth_mod1, test = "Chi")




growth_mod2 <- update(growth_mod1, . ~ . - Init_size2) 
drop1(growth_mod2, test = "Chi")

Anova(growth_mod2)

summary(growth_mod2)

plot(growth_mod2, which = 1)




#CONDITION INDEX

summer3 <- summer2 %>% filter(!is.na(Fin_size2))


summer2 %>% filter(is.na(Morphotype))


conind_mod <- gls(data = summer3, Con_Ind ~ Morphotype * P_Tr + N_total + Fin_size2)
plot(mod1, which = 1)

drop1(conind_mod, test = "Chi")


plot(conind_mod, which = 1)
summary(conind_mod)



#Growth again
coe_growth <- coef(growth_mod1)

mydata_growth <- summer2 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total), Init_size2 = mean(.$Init_size2)))

# View(mydata_growth)

X_growth <- model.matrix(~ Morphotype + P_Tr + N_total + Init_size2, data = mydata_growth)
mydata_growth$predicted_growth <- X_growth%*%coe_growth


# View(mydata_growth)



vcov(growth_mod1)

mydata_growth$se_growth <- sqrt(diag(X_growth%*%vcov(growth_mod1)%*%t(X_growth)))

ggplot(mydata_growth, aes(x = P_Tr, y = predicted_growth, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_growth -1.96*se_growth, ymax = predicted_growth + 1.96*se_growth, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red')) + ylab('Прирост, мм') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид')

control_growth <- summer %>% filter(Type == 'K')

str(control_growth)


control_growth$Growth_num <- as.numeric(control_growth$Fin_size) - as.numeric(control_growth$Init_size)

control_growth1 <- control_growth %>% filter(Status == 'Alive')
# View(control_growth1)

control_growth2 <- control_growth1[complete.cases(control_growth1$Growth_num),]
# View(control_growth2)

control_growth3 <- control_growth2 %>% group_by(Morphotype) %>% summarise(Mean_Growth = mean(Growth_num), SE = sd(Growth_num)/sqrt(n()))
head(control_growth3)


summer2 %>% filter(Type == "K") %>% 
ggplot(. , aes(x = Morphotype, y = Growth2)) + geom_boxplot(aes(fill = Cage_ID))


ggplot(control_growth3, aes(x = Morphotype, y = Mean_Growth)) + geom_col()


# View(control_growth3)

ggplot(mydata_growth, aes(x = P_Tr, y = predicted_growth, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_growth -1.96*se_growth, ymax = predicted_growth + 1.96*se_growth, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red'))  + ylab('Прирост, мм') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид') + geom_hline(data = control_growth3, aes(yintercept = Mean_Growth, color = Morphotype), linetype = 2)








#Condition_Index

coe_conind <- coef(conind_mod)

mydata_conin <- summer2 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total), Fin_size2 = mean(.$Fin_size2)))

# View(mydata_conin)

X_conin <- model.matrix(~ Morphotype * P_Tr + N_total + Fin_size2, data = mydata_conin)
mydata_conin$predicted_conin <- X_conin%*%coe_conind



ggplot(mydata_conin, aes(x = P_Tr, y = predicted_conin, colour = Morphotype)) + geom_line()


mydata_conin$se_conind <- sqrt(diag(X_conin%*%vcov(conind_mod)%*%t(X_conin)))

ggplot(mydata_conin, aes(x = P_Tr, y = predicted_conin, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_conin -1.96*se_conind, ymax = predicted_conin + 1.96*se_conind, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red'))+ geom_point(data = summer2, aes(x = P_Tr, y = Con_Ind)) + ylab('Condition index') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид')


control_conind <- summer %>% filter(Type == 'K')
# View(control_conind)

str(control_conind)


control_conind$Conind_num <- as.numeric(control_growth$W_tis) / (as.numeric(control_conind$W_tis) + as.numeric(control_conind$W_sh))

control_conind1 <- control_conind %>% filter(Status == 'Alive')
# View(control_conind1)

control_conind2 <- control_conind1[complete.cases(control_conind1$Conind_num),]
# View(control_conind2)

control_conind3 <- control_conind2 %>% group_by(Morphotype) %>% summarise(Mean_Conind = mean(Conind_num), SE = sd(Conind_num)/sqrt(n()))
head(control_conind3)
# View(control_conind3)

ggplot(mydata_conin, aes(x = P_Tr, y = predicted_conin, colour = Morphotype)) + geom_line() + geom_ribbon(aes(ymin = predicted_conin -1.96*se_conind, ymax = predicted_conin + 1.96*se_conind, group = Morphotype), alpha = 0.2, colour = 'grey') + scale_color_manual(values = c('blue', 'red')) + geom_point(data = summer2, aes(x = P_Tr, y = Conind_num)) + ylab('Condition index') + xlab('Доля M.trossulus в садке') + labs(color = 'Вид') + geom_hline(data = control_conind3, aes(yintercept = Mean_Conind, color = Morphotype), linetype = 2)





#######################
# Try to use categorical predictor

M_growth <- lme(Growth2 ~ Morphotype * Type + N_total + Init_size2, random =  ~1|Cage_ID, data = summer2)

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


ggplot(summer_f, aes(x = Fon_type, y = N_total)) + geom_point()




ggplot(summer2, aes(x = Type, y = Growth2)) + geom_boxplot()





#########################################
#Model of deadness

summer3 <- summer %>% mutate(Status2 = ifelse(Status == "Alive", 0, 1)) %>% merge(., summer_f, by = "Cage_ID")

summer3$Init_size2 <- as.numeric(summer3$Init_size)



M_dead <- glm(Status2 ~ Morphotype * P_Tr + N_total, data = summer3, family = "binomial" ) 

drop1(M_dead)

plot(M_dead)



my_data_deadness <- summer3 %>% group_by(Morphotype) %>% do(data.frame(P_Tr = seq(min(.$P_Tr), max(.$P_Tr), length.out = 10), N_total = mean(.$N_total)))

my_data_deadness$Predicted <- predict(M_dead, newdata = my_data_deadness, type = "response")


logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

X <- model.matrix(~ Morphotype * P_Tr + N_total , data = my_data_deadness)
b <- coef(M_dead)

my_data_deadness$fit_eta <- X %*% b

my_data_deadness$se_eta <- sqrt(diag(X %*% vcov(M_dead) %*% t(X))) #Ошибки в шкале логитов


# Границы доверительного интервала
my_data_deadness$lwr <- logit_back(my_data_deadness$fit_eta - 1.96 * my_data_deadness$se_eta)
my_data_deadness$upr <- logit_back(my_data_deadness$fit_eta + 1.96 * my_data_deadness$se_eta)



dead_sum <- summer3 %>% group_by(Cage_ID, Morphotype) %>% summarise(P_Tr = mean(P_Tr), P_dead = mean(Status2))


ggplot(my_data_deadness, aes(x = P_Tr, y = Predicted)) + geom_line(aes(color = Morphotype)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = Morphotype), alpha = 0.2) + scale_color_manual(values = c("blue", "red")) + geom_point(data = dead_sum, aes(y = P_dead, color = Morphotype))


