library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(car)
library(doBy)
library(pROC)
library(betareg)
library(lmtest)
library(broom)
library(ggpubr)

myt_am <- read.table("american_bm.csv", header = T, sep = ";")

# Оставляем только мидий, у которых есть оценка морфотипа
myt_am2 <- myt_am[!is.na(myt_am$ind), ]
str(myt_am2)

# Вводим обозначения для видов-факторов
myt_am2$Sp2 <- ifelse(myt_am2$sp == 1, "M. trossulus", "M. edulis")
myt_am2$Sp2 <- factor(myt_am2$Sp2)

# Correct identification
myt_am2$congr <- ifelse((myt_am2$ind == 1 & myt_am2$Sp2 == "M. trossulus") | (myt_am2$ind == 0 & myt_am2$Sp2 == "M. edulis"), 1, 0   )

# Частота M.trossulus в популяции
freq_MT <- myt_am2 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp))
myt_am2 <- merge(myt_am2, freq_MT)
freq_T <- myt_am2 %>% group_by(pop_ID) %>% summarise(freq_T = mean(ind))
myt_am2 <- merge(myt_am2, freq_T)

# считаем с оутлаерами
#Модель 1
Model_1 <- glm(ind ~  freq_MT, data = myt_am2, family = binomial(link = "logit"))

summary(Model_1)

new_data <- myt_am2 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(freq_MT) ) %>%  do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 10)))

predicted <- predict(Model_1, newdata = new_data,  type="response", se.fit = T)

new_data$fit <- predicted$fit

new_data$SE <- predicted$se.fit 

theme_set(theme_bw())

# врисовываем первичные данные
link_over_T <- myt_am2 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp), freq_T = mean(ind), N_MT = sum(sp == 1),  N_ME = sum(sp == 0))

Pl_mod2 <-  ggplot(new_data, aes(x = freq_MT, y = fit)) + geom_line(linetype = 2, color = "red", size = 1)  + xlim(0, 1) + ylim(0, 1) + geom_abline() + geom_point(data=link_over_T, aes(x=freq_MT, y = freq_T, size=(N_MT+N_ME)), fill="black", shape=21) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") + labs(y =  "     \n  Proportion of T-morphotype", x = "Proportion of M. trossulus", fill = "")  + geom_ribbon(aes(ymin = fit - 1.96*SE, ymax = fit + 1.96*SE), alpha = 0.1)

# Модель2
str(myt_am2)
Model_pp <- glmer(ind ~ Sp2 * freq_MT + (1 | pop_ID), data = myt_am2,  family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

Model_pp_rs <- glmer(ind ~ Sp2 * freq_MT + (1 + freq_MT | pop_ID), data = myt_am2,  family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))



AIC(Model_pp, Model_pp_rs)


overdisp(Model_pp)

drop1(Model_pp, k=log(nrow(myt_am2)))


summary(Model_pp)
Anova(Model_pp)
vif(Model_pp)




Model_pp2 <- update(Model_pp, .~.-Sp2:freq_MT)

vif(Model_pp2)

drop1(Model_pp2, k=log(nrow(myt_am2)))

Mod_fT_congr_fin <- Mod_fT_congr2

AIC(Model_pp, Model_pp2)

BIC(Model_pp, Model_pp2)


# рисуем
newdata2 <- myt_am2 %>% group_by(Sp2, pop_ID) %>% summarise(freq_MT=mean(freq_MT)) %>% group_by(Sp2) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале логитов
newdata2$fit_eta <- predict(Model_pp, newdata = newdata2, re.form = NA) 
summary(Model_pp)

# Вычисление доверительного интеравала

X <- model.matrix(  ~ Sp2 * freq_MT, data = newdata2) #Модельная матрица для визуализации

# Ошибки в шкале логитов
newdata2$se_eta <- sqrt(diag(X %*% vcov(Model_pp) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata2$lwr <- logit_back(newdata2$fit_eta - 2 * newdata2$se_eta)
newdata2$upr <- logit_back(newdata2$fit_eta + 2 * newdata2$se_eta)

# Предсказанные значеня в шкале вероятностей
newdata2$fit <- predict(Model_pp, newdata = newdata2, type = "response", re.form = NA) 

pops_over_M <- myt_am2 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp), N_MT = sum(sp == 1),  N_T_MT = sum(sp == 1 & ind == 1), N_E_MT = sum(sp == 1 & ind == 0), N_ME = sum(sp == 0), N_E_ME = sum(sp == 0 & ind == 0), N_T_ME = sum(sp == 0 & ind == 1)) 

pops_over_M$P_T_MT <- with(pops_over_M, N_T_MT / N_MT)

pops_over_M$P_E_MT <- with(pops_over_M, N_E_MT / N_MT)

pops_over_M$P_E_ME <- with(pops_over_M, N_E_ME / N_ME)

pops_over_M$P_T_ME <- with(pops_over_M, N_T_ME / N_ME)

# рисуем 
T_sp1 <- ggplot(pops_over_M, aes(x = freq_MT))  + geom_segment(aes(x = freq_MT, y = (1-P_E_ME), xend = freq_MT, yend = P_T_MT), color="darkgrey") + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(y = (1-P_E_ME), size= N_ME), fill = "white", shape = 21) + geom_point(aes(y = P_T_MT, size=N_MT), fill = "black", shape = 21) + xlim(0,1)+ theme_bw() + labs(y =  "Proportion of T-morphotype \n among  M. edulis  and  M. trossulus", x = "Proportion of M. trossulus", fill = "")+ ylim(0,1) + xlim(0,1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") + geom_line(data=newdata2, aes(x= freq_MT, y= fit, color = Sp2), linetype = 2, size=1)   + scale_color_manual(values=c("blue", "red")) + geom_ribbon(data=newdata2, aes(ymin = lwr, ymax = upr, linetype=NA, group=Sp2), alpha = 0.1)

# модель без взаимодействия
newdata2a <- myt_am2 %>% group_by(Sp2, pop_ID) %>% summarise(freq_MT=mean(freq_MT)) %>% group_by(Sp2) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале логитов
newdata2a$fit_eta <- predict(Model_pp2, newdata = newdata2a, re.form = NA)
summary(Model_pp)
# Вычисление доверительного интеравала

X <- model.matrix(  ~ Sp2 + freq_MT, data = newdata2a) #Модельная матрица для визуализации

# Ошибки в шкале логитов
newdata2a$se_eta <- sqrt(diag(X %*% vcov(Model_pp2) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata2a$lwr <- logit_back(newdata2a$fit_eta - 2 * newdata2a$se_eta)
newdata2a$upr <- logit_back(newdata2a$fit_eta + 2 * newdata2a$se_eta)

# Предсказанные значеня в шкале вероятностей
newdata2a$fit <- predict(Model_pp2, newdata = newdata2a, type = "response", re.form = NA)

T_sp2 <- ggplot(pops_over_M, aes(x = freq_MT))  + geom_segment(aes(x = freq_MT, y = (1-P_E_ME), xend = freq_MT, yend = P_T_MT), color="darkgrey") + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(y = (1-P_E_ME), size= N_ME), fill = "white", shape = 21) + geom_point(aes(y = P_T_MT, size=N_MT), fill = "black", shape = 21) + xlim(0,1)+ theme_bw() + labs(y =  "Proportion of T-morphotype \n among  M. trossulus  and  M. edulis", x = "Proportion of M. trossulus", fill = "")+ ylim(0,1) + xlim(0,1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") + geom_line(data=newdata2a, aes(x= freq_MT, y= fit, color = Sp2), linetype = 2, size=1)   + scale_color_manual(values=c("blue", "red"))  + geom_ribbon(data=newdata2a, aes(ymin = lwr, ymax = upr, linetype=NA, group=Sp2), alpha = 0.1)

# Модель3
Mod_fT_congr <- glmer(congr ~ morph * freq_MT + (1 | pop_ID), data = myt_am2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

drop1(Mod_fT_congr, test = "Chi")

Mod_fT_congr_fin <- Mod_fT_congr

summary(Mod_fT_congr_fin)

newdata3 <- myt_am2 %>% group_by(morph, pop_ID) %>% summarise(freq_MT=mean(freq_MT)) %>%  group_by(morph) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале логитов
newdata3$fit_eta <- predict(Mod_fT_congr_fin, newdata = newdata3, re.form = NA) 

# Предсказанные значеня в шкале вероятностей
newdata3$fit <- predict(Mod_fT_congr_fin, newdata = newdata3, type = "response", re.form = NA) 

# Вычисление доверительного интеравала

X <- model.matrix(  ~ morph * freq_MT, data = newdata3) #Модельная матрица для визуализации

# Ошибки в шкале логитов
newdata3$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr_fin) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata3$lwr <- logit_back(newdata3$fit_eta - 2 * newdata3$se_eta)
newdata3$upr <- logit_back(newdata3$fit_eta + 2 * newdata3$se_eta)

pr_value_M <- myt_am2 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp), N_T = sum(ind == 1),  N_T_MT = sum(sp == 1 & ind == 1), N_E_MT = sum(sp == 1 & ind == 0), N_E = sum(ind == 0), N_E_ME = sum(sp == 0 & ind == 0), N_T_ME = sum(sp == 0 & ind == 1))

pr_value_M$PMT_T <- with(pr_value_M, N_T_MT / N_T)

pr_value_M$PMT_E <- with(pr_value_M, N_E_MT / N_T)

pr_value_M$PME_E <- with(pr_value_M, N_E_ME / N_E)

pr_value_M$PME_T <- with(pr_value_M, N_T_ME / N_E)

pr_value_plot_M <- ggplot(pr_value_M, aes(x = freq_MT)) + geom_segment(aes(x = freq_MT, y = PME_E, xend = freq_MT, yend = PMT_T), color="darkgrey") + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(y = PME_E, size= N_E), fill = "white", shape = 21) + geom_point(aes(y = PMT_T, size=N_T), fill = "black", shape = 21) + xlim(0,1)+ theme_bw() + labs(y =  "Proportion of correct species \n identification by morphotypes", x = "Proportion of M. trossulus", fill = "")+ ylim(0,1) + xlim(0,1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "")  + geom_line(data=newdata3,aes(x = freq_MT, y = fit, color=morph), linetype = "dashed", size=1)+ scale_color_manual(values=c("blue", "red")) + geom_ribbon(data=newdata3, aes(ymin = lwr, ymax = upr, linetype=NA, group=morph), alpha = 0.1)

# объединяем три графика
mod_1 <- ggarrange(Pl_mod2, T_sp1, pr_value_plot_M, ncol = 1)

mor_1_w <- ggarrange(Pl_mod2, T_sp2, pr_value_plot_M, ncol = 1)

mod_with_out <- ggarrange(mod_1, mor_1_w, nrow=1)


# ##########################################################################################
# считаем без оутлаеров
myt_am4 <- read.table("american_bm2.csv", header = T, sep = ";")

# Оставляем только мидий, у которых есть оценка морфотипа
myt_am4 <- myt_am4[!is.na(myt_am4$ind), ]
str(myt_am4)

# Вводим обозначения для видов-факторов
myt_am4$Sp2 <- ifelse(myt_am4$sp == 1, "M. trossulus", "M. edulis")
myt_am4$Sp2 <- factor(myt_am4$Sp2)

#Correct identification
myt_am4$congr <- ifelse((myt_am4$ind == 1 & myt_am4$Sp2 == "M. trossulus") | (myt_am4$ind == 0 & myt_am4$Sp2 == "M. edulis"), 1, 0   )

# Частота M.trossulus в популяции
freq_MT <- myt_am4 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp))
myt_am4 <- merge(myt_am4, freq_MT)
freq_T <- myt_am4 %>% group_by(pop_ID) %>% summarise(freq_T = mean(ind))
myt_am4 <- merge(myt_am4, freq_T)

#Модель 1
Model_1w <- glm(ind ~  freq_MT, data = myt_am4, family = binomial(link = "logit"))

summary(Model_1w)

new_data <- myt_am4 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(freq_MT) ) %>%  do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 10)))

predicted <- predict(Model_1w, newdata = new_data,  type="response", se.fit = T)

new_data$fit <- predicted$fit

new_data$SE <- predicted$se.fit 

theme_set(theme_bw())

# врисовываем первичные данные
link_over_T <- myt_am4 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp), freq_T = mean(ind), N_MT = sum(sp == 1),  N_ME = sum(sp == 0))

Pl_mod2 <-  ggplot(new_data, aes(x = freq_MT, y = fit)) + geom_line(linetype = 2, color = "red", size = 1)  + xlim(0, 1) + ylim(0, 1) + geom_abline() + geom_point(data=link_over_T, aes(x=freq_MT, y = freq_T, size=(N_MT+N_ME)), fill="black", shape=21) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") + labs(y =  "     \n  Proportion of T-morphotype", x = "Proportion of M. trossulus", fill = "") + geom_ribbon(aes(ymin = fit - 1.96*SE, ymax = fit + 1.96*SE), alpha = 0.1)

# Модель2
Model_ppw <- glmer(ind ~ Sp2 * freq_MT + (1 | pop_ID), data = myt_am4,  family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

drop1(Model_ppw, test = "Chi")

overdisp(Model_ppw)

summary(Model_ppw)
Anova(Model_ppw)

Model_ppw2 <- update(Model_ppw, .~.-Sp2:freq_MT)
drop1(Model_ppw2, test = "Chi")
Anova(Model_ppw2)
AIC(Model_ppw, Model_ppw2)

# рисуем пока со взаимодействием
newdata2 <- myt_am4 %>% group_by(Sp2, pop_ID) %>% summarise(freq_MT=mean(freq_MT)) %>% group_by(Sp2) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале логитов
newdata2$fit_eta <- predict(Model_ppw, newdata = newdata2, re.form = NA) 

# Вычисление доверительного интеравала
X <- model.matrix(  ~ Sp2 * freq_MT, data = newdata2) #Модельная матрица для визуализации

# Ошибки в шкале логитов
newdata2$se_eta <- sqrt(diag(X %*% vcov(Model_ppw) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata2$lwr <- logit_back(newdata2$fit_eta - 2 * newdata2$se_eta)
newdata2$upr <- logit_back(newdata2$fit_eta + 2 * newdata2$se_eta)

# Предсказанные значеня в шкале вероятностей
newdata2$fit <- predict(Model_ppw, newdata = newdata2, type = "response", re.form = NA) 

pops_over_M <- myt_am4 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp), N_MT = sum(sp == 1),  N_T_MT = sum(sp == 1 & ind == 1), N_E_MT = sum(sp == 1 & ind == 0), N_ME = sum(sp == 0), N_E_ME = sum(sp == 0 & ind == 0), N_T_ME = sum(sp == 0 & ind == 1)) 

pops_over_M$P_T_MT <- with(pops_over_M, N_T_MT / N_MT)

pops_over_M$P_E_MT <- with(pops_over_M, N_E_MT / N_MT)

pops_over_M$P_E_ME <- with(pops_over_M, N_E_ME / N_ME)

pops_over_M$P_T_ME <- with(pops_over_M, N_T_ME / N_ME)

# рисуем 
T_sp1 <- ggplot(pops_over_M, aes(x = freq_MT))  + geom_segment(aes(x = freq_MT, y = (1-P_E_ME), xend = freq_MT, yend = P_T_MT), color="darkgrey") + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(y = (1-P_E_ME), size= N_ME), fill = "white", shape = 21) + geom_point(aes(y = P_T_MT, size=N_MT), fill = "black", shape = 21) + xlim(0,1)+ theme_bw() + labs(y =  "Proportion of T-morphotype \n among  M. edulis  and  M. trossulus", x = "Proportion of M. trossulus", fill = "")+ ylim(0,1) + xlim(0,1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") + geom_line(data=newdata2, aes(x= freq_MT, y= fit, color = Sp2), linetype = 2, size=1)   + scale_color_manual(values=c("blue", "red")) + geom_ribbon(data=newdata2, aes(ymin = lwr, ymax = upr, linetype=NA, group=Sp2), alpha = 0.1)

# рисуем модель без взаимодействия
newdata2a1 <- myt_am4 %>% group_by(Sp2, pop_ID) %>% summarise(freq_MT=mean(freq_MT)) %>% group_by(Sp2) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

summary(Model_ppw2)

# Предсказанные значеня в шкале логитов
newdata2a1$fit_eta <- predict(Model_ppw2, newdata = newdata2a1, re.form = NA)

# Вычисление доверительного интеравала
X <- model.matrix(  ~Sp2 + freq_MT, data = newdata2a1) #Модельная матрица для визуализации

# Ошибки в шкале логитов
newdata2a1$se_eta <- sqrt(diag(X %*% vcov(Model_ppw2) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata2a1$lwr <- logit_back(newdata2a1$fit_eta - 2 * newdata2a1$se_eta)
newdata2a1$upr <- logit_back(newdata2a1$fit_eta + 2 * newdata2a1$se_eta)

# Предсказанные значеня в шкале вероятностей
newdata2a1$fit <- predict(Model_ppw2, newdata = newdata2a1, type = "response", re.form = NA)

T_sp2 <- ggplot(pops_over_M, aes(x = freq_MT))  + geom_segment(aes(x = freq_MT, y = (1-P_E_ME), xend = freq_MT, yend = P_T_MT), color="darkgrey") + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(y = (1-P_E_ME), size= N_ME), fill = "white", shape = 21) + geom_point(aes(y = P_T_MT, size=N_MT), fill = "black", shape = 21) + xlim(0,1)+ theme_bw() + labs(y =  "Proportion of T-morphotype \n among  M. trossulus  and  M. edulis", x = "Proportion of M. trossulus", fill = "")+ ylim(0,1) + xlim(0,1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") + geom_line(data=newdata2a1, aes(x= freq_MT, y= fit, color = Sp2), linetype = 2, size=1)   + scale_color_manual(values=c("blue", "red")) + geom_ribbon(data=newdata2a1, aes(ymin = lwr, ymax = upr, linetype=NA, group=Sp2), alpha = 0.1)

# Модель3
Mod_fT_congr <- glmer(congr ~ morph * freq_MT + (1 | pop_ID), data = myt_am4, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

drop1(Mod_fT_congr, test = "Chi")

Mod_fT_congr_fin <- Mod_fT_congr

summary(Mod_fT_congr_fin)

newdata3a <- myt_am4 %>% group_by(morph, pop_ID) %>% summarise(freq_MT=mean(freq_MT)) %>%  group_by(morph) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале логитов
newdata3a$fit_eta <- predict(Mod_fT_congr_fin, newdata = newdata3, re.form = NA) 

# Предсказанные значеня в шкале вероятностей
newdata3a$fit <- predict(Mod_fT_congr_fin, newdata = newdata3, type = "response", re.form = NA) 

# Вычисление доверительного интеравала

X <- model.matrix(  ~ morph * freq_MT, data = newdata3a) #Модельная матрица для визуализации

# Ошибки в шкале логитов
newdata3a$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr_fin) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata3a$lwr <- logit_back(newdata3a$fit_eta - 2 * newdata3a$se_eta)
newdata3a$upr <- logit_back(newdata3a$fit_eta + 2 * newdata3a$se_eta)

pr_value_M <- myt_am4 %>% group_by(pop_ID) %>% summarise(freq_MT = mean(sp), N_T = sum(ind == 1),  N_T_MT = sum(sp == 1 & ind == 1), N_E_MT = sum(sp == 1 & ind == 0), N_E = sum(ind == 0), N_E_ME = sum(sp == 0 & ind == 0), N_T_ME = sum(sp == 0 & ind == 1))

pr_value_M$PMT_T <- with(pr_value_M, N_T_MT / N_T)

pr_value_M$PMT_E <- with(pr_value_M, N_E_MT / N_T)

pr_value_M$PME_E <- with(pr_value_M, N_E_ME / N_E)

pr_value_M$PME_T <- with(pr_value_M, N_T_ME / N_E)

pr_value_plot_M <- ggplot(pr_value_M, aes(x = freq_MT)) + geom_segment(aes(x = freq_MT, y = PME_E, xend = freq_MT, yend = PMT_T), color="darkgrey") + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(y = PME_E, size= N_E), fill = "white", shape = 21) + geom_point(aes(y = PMT_T, size=N_T), fill = "black", shape = 21) + xlim(0,1)+ theme_bw() + labs(y =  "Proportion of correct species \n identification by morphotypes", x = "Proportion of M. trossulus", fill = "")+ ylim(0,1) + xlim(0,1) + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "")  + geom_line(data=newdata3a,aes(x = freq_MT, y = fit, color=morph), linetype = "dashed", size=1)+ scale_color_manual(values=c("blue", "red"))  + geom_ribbon(data=newdata3a, aes(ymin = lwr, ymax = upr, linetype=NA, group=morph), alpha = 0.1)

# объединяем три графика
mod_1a <- ggarrange(Pl_mod2, T_sp1, pr_value_plot_M, ncol = 1)

mor_1a_w <- ggarrange(Pl_mod2, T_sp2, pr_value_plot_M, ncol = 1)

moda_with_out <- ggarrange(mod_1a, mor_1a_w, nrow=1)

