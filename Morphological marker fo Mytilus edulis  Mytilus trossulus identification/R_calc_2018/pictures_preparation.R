library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(car)
library(doBy)
library(pROC)
library(betareg)


# Подготовка данных
myt <- read.table("data_salinity3.csv", header = T, sep = ",")

nrow(myt)

table(myt$pop)


myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)


# Оставляем только мидий, у которых есть оценка морфотипа
myt2 <- myt[!is.na(myt$ind), ]
myt2 <- myt2[!is.na(myt2$size), ]

nrow(myt2)



# Вводим обозначения для морфотипов
myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)

#Оставляем только данные, на основе, которых строится модель
myt3 <- myt2[myt2$dataset == "testing", ]
myt2 <- myt2[myt2$dataset == "training", ]

nrow(myt2)

myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )




# Частота M.trossulus в популяции, вычисленная как срденее значение structure
freq_MT <- summaryBy( str ~ pop, data = myt2)
names(freq_MT) <- c("pop", "freq_MT")


# Частота T-морфотипов в каждой  популяции

Prop_T <- summaryBy( ind ~ pop, data = myt2)
names(Prop_T) <- c("pop", "Prop_T")


Prop_T_freq_MT <- merge(Prop_T, freq_MT )


####### Частоты T морфотипа в каждой совокупности

Freq_T <- myt2 %>% group_by(Sp, sea, sal_place) %>% summarise(Prop_T = mean(ind))

Freq_T$Location <- paste(Freq_T$sea,"_", Freq_T$sal_place, sep = "") 

Freq_T$Location <- factor(Freq_T$Location, labels = c("BF", "BN", "WF", "WN"))


###########################


myt2 <- merge(myt2, freq_MT)

myt2$Sp2 <- ifelse(myt2$Sp == "M.trossulus", 1, 0)


myt2 <- merge(myt2, Prop_T)


myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") 

myt3$Location <- paste(myt3$sea,"_", myt3$sal_place, sep = "") 







##################### Картинка с частотам  T морфотипа среди разных видов


Model_T_MT <- glmer(ind ~ sea*sal_place*Sp + (1|pop), data = myt2,  family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

drop1(Model_T_MT, test = "Chi")

summary(Model_T_MT)


# newdata <- expand.grid(Sp = levels(myt2$Sp), sea = levels(myt2$sea), sal_place = levels(myt2$sal_place))
# 
# # Предсказанные значеня в шкале вероятностей
# newdata$fit <- predict(Model_T_MT, newdata = newdata, type = "response", re.form = NA)
# 
# # Предсказанные значеня в шкале логитов
# newdata$fit_eta <- predict(Model_T_MT, newdata = newdata, re.form = NA)
# 
# # Вычисление доверительного инеравала
# 
# # formula((Mod_fT_congr_fin))
# 
# X <- model.matrix(  ~  Sp * sea*sal_place , data = newdata) #Модельная матрица для визуализации
# 
# 
# # Ошибки в шкале логитов
# newdata$se_eta <- sqrt(diag(X %*% vcov(Model_T_MT) %*% t(X)))
# 
# logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация
# 
# # Границы доверительных интервалов в масштабах вероятностей
# newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)
# 
# 
# 
# 
# ggplot(newdata, aes(x = Sp, y = fit)) + geom_col(fill = "darkgray", color = "black") + facet_grid(sea ~ sal_place) + geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2)+ theme_bw() + labs(y = "Proportion of T-morphotype", x = "Species") + theme(legend.position = "bottom")


# Более легкий способ получения визуализации модели для дискретных предикторов 
library(emmeans)

Model_T_MT_predicted<- as.data.frame(emmeans(Model_T_MT, list(pairwise ~ Sp * sea*sal_place ), adjust = "tukey")[1])

emmeans(Model_T_MT, list(pairwise ~ Sp * sea*sal_place ), adjust = "tukey")[2]


emmeans(Model_T_MT, list(pairwise ~ sea), adjust = "tukey")[2]


names(Model_T_MT_predicted) <- c("Sp", "sea", "sal_place", "emmean", "SE", "df", "LCL", "UCL")



means_melt <- melt(Model_T_MT_predicted, id = c("Sp", "sea", "sal_place"))

Model_T_MT_predicted <- dcast(means_melt, formula = Sp+ sea + sal_place ~  variable)

Model_T_MT_predicted$Location <- paste(Model_T_MT_predicted$sea,"_", Model_T_MT_predicted$sal_place, sep = "") 

Model_T_MT_predicted$Location <- factor(Model_T_MT_predicted$Location, labels = c("BF", "BN", "WF", "WN"))




Pl_prediction <- ggplot(Model_T_MT_predicted, aes(x = Sp, y =logit_back(emmean))) + geom_col(fill = "darkgray", color = "black") + facet_grid( sal_place ~ sea ) + geom_errorbar(aes(ymin = logit_back(UCL), ymax = logit_back(LCL)), width = 0.2)+ theme_bw() + labs(y = "Proportion of T-morphotype", x = "Species") + theme(legend.position = "bottom")



Pl_prediction2 <- ggplot(Model_T_MT_predicted, aes(x = Location, y =logit_back(emmean), group = Sp)) + geom_col(aes(fill = Sp), color = "black", position = position_dodge2())  + geom_errorbar(aes(ymin = logit_back(UCL), ymax = logit_back(LCL)), width = 0.2, position = position_dodge(width = 0.9))+ theme_bw() + labs(y = "Probability of being T-morphotype") + theme(axis.title.x = element_blank()) + scale_fill_manual(values = c("blue", "red")) + guides(fill = "none") + geom_point(data = Freq_T, aes(x = Location, y = Prop_T), position = position_dodge(width = 0.9), shape = 21, fill = "yellow", size = 2)


# Картинка по ROC-анализу

ROC_white_fresh <- roc(Sp ~ ind, data = myt2[myt2$Location =="white_fresh", ], ci = TRUE)
ROC_curve_white_fresh <- data.frame(sensitivity = ROC_white_fresh$sensitivities, specif = ROC_white_fresh$specificities, sea = "white", sal_place = "fresh") 




ROC_white_normal <- roc(Sp ~ ind, data = myt2[myt2$Location =="white_normal", ], ci = TRUE)
ROC_curve_white_normal <- data.frame(sensitivity = ROC_white_normal$sensitivities, specif = ROC_white_normal$specificities, sea = "white", sal_place = "normal") 




ROC_barents_normal <- roc(Sp ~ ind, data = myt2[myt2$Location =="barents_normal", ], ci = TRUE)
ROC_curve_barents_normal <- data.frame(sensitivity = ROC_barents_normal$sensitivities, specif = ROC_barents_normal$specificities, sea = "barents", sal_place = "normal") 


ROC_barents_fresh <- roc(Sp ~ ind, data = myt2[myt2$Location =="barents_fresh", ], ci = TRUE)
ROC_curve_barents_fresh <- data.frame(sensitivity = ROC_barents_fresh$sensitivities, specif = ROC_barents_fresh$specificities, sea = "barents", sal_place = "fresh") 



ROC_barents_fresh_test <- roc(Sp ~ ind, data = myt3[myt3$Location =="barents_fresh", ], ci = TRUE)


ROC_barents_normal_test <- roc(Sp ~ ind, data = myt3[myt3$Location =="barents_normal", ], ci = TRUE)


# Датафрейм со значениями AUC и 95% CI для всех локаций

AUC <- data.frame(Location = c("BF", "BN", "WF", "WN"), CIlow = NA, AUC = NA, CIupr = NA)

AUC[1,2:4] <- as.vector(ROC_barents_fresh$ci)
AUC[2,2:4] <- as.vector(ROC_barents_normal$ci)
AUC[3,2:4] <- as.vector(ROC_white_fresh$ci)
AUC[4,2:4] <- as.vector(ROC_white_normal$ci)


BF_BN_test <- roc.test(ROC_barents_fresh,ROC_barents_normal, paired = FALSE, method = "venkatraman")

BF_WF_test <- roc.test(ROC_barents_fresh, ROC_white_fresh, paired = FALSE, method = "venkatraman")

BF_WN_test <- roc.test(ROC_barents_fresh, ROC_white_normal, paired = FALSE, method = "venkatraman")

BN_WF_test <- roc.test(ROC_barents_normal, ROC_white_fresh, paired = FALSE, method = "venkatraman")

BN_WN_test <- roc.test(ROC_barents_normal, ROC_white_normal, paired = FALSE, method = "venkatraman")

WN_WF_test <- roc.test(ROC_white_normal, ROC_white_fresh, paired = FALSE, method = "venkatraman")



Pl_AUC <- ggplot(AUC, aes(x = Location, y = AUC)) + geom_col(fill = "gray", color = "black") + geom_errorbar(aes(ymin = CIlow, ymax = CIupr), width = 0.1) + theme_bw() + geom_text(aes(y = CIupr+0.1, label = c("a", "b", "a", "a")))+ geom_hline(yintercept = 0.5, linetype = 2) + ylim(0,1)

library(gridExtra)

grid.arrange(Pl_prediction2,Pl_AUC, ncol = 1)



##########################################


# Картинка с зависимсотями встречаемости MT от встречаемости T-морфотипа

#Объединяем Белое море

myt2$Location2 <- ifelse(myt2$Location == "white_fresh" | myt2$Location == "white_normal", "White", myt2$Location) 

myt2$Location2 <- factor(myt2$Location2, labels = c("BF", "BN", "W"))



Model_T_MT <- glmer(Sp2 ~ Prop_T * Location2 + (1|pop), data = myt2,  family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


drop1(Model_T_MT, test = "Chi")


Model_T_MT2 <- update(Model_T_MT, .~. - Prop_T:Location2 )

Model_T_MT_final <- Model_T_MT2

summary(Model_T_MT_final)


newdata <- myt2 %>% group_by(Location2) %>% do(data.frame(Prop_T = seq(min(.$Prop_T), max(.$Prop_T), length.out = 100)))

# Предсказанные значеня в шкале вероятностей
newdata$fit <- predict(Model_T_MT_final, newdata = newdata, type = "response", re.form = NA) 

# Предсказанные значеня в шкале логитов
newdata$fit_eta <- predict(Model_T_MT_final, newdata = newdata, re.form = NA) 

# Вычисление доверительного инеравала

# formula((Mod_fT_congr_fin)) 

X <- model.matrix(  ~ Prop_T + Location2 , data = newdata) #Модельная матрица для визуализации


# Ошибки в шкале логитов
newdata$se_eta <- sqrt(diag(X %*% vcov(Model_T_MT_final) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)

# newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)

ggplot(newdata, aes(x = Prop_T, y = fit, group = Location2))+ geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) + geom_line(aes(color = Location2), size=1)   +   scale_fill_manual(values = c("blue", "red"))+ theme_bw() + xlim(0,1) + geom_point(data = myt2, aes(x = Prop_T, y = freq_MT, color = Location2)) +scale_color_manual(values= c("black", "darkgray", "blue")) + geom_abline(linetype = 2) + labs(x = "Proportion of T-morphotype in population", y = "Probability of being M.trossulus", color = "Location")





#### Картинка с Баесовскими кривыми и congr

# Модель для саязи congr и доли MT и выдела


Mod_fT_congr <- glmer(congr ~ morph * freq_MT*Location2 + (1 | pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

drop1(Mod_fT_congr, test = "Chi")

# Упрощение модели

Mod_fT_congr2 <- update(Mod_fT_congr, . ~. -morph:freq_MT:Location2)
drop1(Mod_fT_congr2, test = "Chi")

Mod_fT_congr_fin <- Mod_fT_congr2 


AIC(Mod_fT_congr,Mod_fT_congr_fin)


Anova(Mod_fT_congr_fin)

summary(Mod_fT_congr_fin)




##Диагностика модели

Mod_fT_congr_diagn <- fortify(Mod_fT_congr_fin)

ggplot(data = Mod_fT_congr_diagn, aes(x = .fitted, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "loess") # Идеально!


# Переменные из модели

ggplot(Mod_fT_congr_diagn, aes(x = morph, y = .scresid)) + geom_boxplot() #Норм!

ggplot(Mod_fT_congr_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() #Норм!

ggplot(Mod_fT_congr_diagn, aes(x = sal_place, y = .scresid)) + geom_boxplot() #Норм!

ggplot(Mod_fT_congr_diagn, aes(x = freq_MT, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "loess") # Идеально!

ggplot(Mod_fT_congr_diagn, aes(x = sal_place, y = .scresid)) + geom_boxplot() #Норм!


# Переменные не включенные в модель

ggplot(Mod_fT_congr_diagn, aes(x = size, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "loess") #Приемлемо

ggplot(Mod_fT_congr_diagn, aes(x = str, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "gam") #Не очень хорошо! Полагаю это результат плавающей границы между видами. Не везде ее надо проводить по значению str = 0.5.

ggplot(Mod_fT_congr_diagn, aes(x = Sp, y = .scresid)) + geom_boxplot() #Есть слабый паттерн, но он опять же, я думаю, связан с тем, что граница между видами в разных популяциях должна проводиться по разным значениям structure

ggplot(Mod_fT_congr_diagn, aes(x =Location, y = .scresid)) + geom_boxplot()  #Терпимо!

ggplot(Mod_fT_congr_diagn, aes(x =Location2, y = .scresid)) + geom_boxplot()  #Терпимо!





##Визуализация модели

newdata <- myt2 %>% group_by(morph, Location2) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале вероятностей
newdata$fit <- predict(Mod_fT_congr_fin, newdata = newdata, type = "response", re.form = NA) 

# Предсказанные значеня в шкале логитов
newdata$fit_eta <- predict(Mod_fT_congr_fin, newdata = newdata, re.form = NA) 

# Вычисление доверительного инеравала

# formula((Mod_fT_congr_fin)) 

X <- model.matrix(  ~ morph + freq_MT + Location2 +  morph:freq_MT +  
                      morph:Location2 + freq_MT:Location2, data = newdata) #Модельная матрица для визуализации


# Ошибки в шкале логитов
newdata$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr_fin) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

# Границы доверительных интервалов в масштабах вероятностей
newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)

# newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)


Pl_fit <- ggplot(newdata, aes(x = freq_MT, y = fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.2)  + geom_line(aes(color = morph), size=1) + facet_grid( ~ Location2)   + scale_color_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1)  + labs(y = "Probability of correct identification", x = "Proportion of M.trossulus", color = "Morphotype") + theme(legend.position = "bottom")

correct_prop <- summaryBy(congr + freq_MT  ~ pop + morph + Location2, data = myt2)


Pl_fit_init_data <- Pl_fit + geom_point(data = correct_prop, aes(x = freq_MT.mean, y = congr.mean, color = morph), size = 2)

Pl_fit_init_data


#######Байесовские кривые

# Функция для вычисления условных вероятностей

probs_calc <- function(variable = "Location2", place = "W") {
  n <- which(names(myt2) == variable)
  d <- myt2[myt2[,n] == place, ]
  dd <- melt(table(d$Sp, d$morph))
  freq_dd <- dcast(data = dd, formula = Var2 ~ Var1)
  
  P_T_MT <- round(with(data = freq_dd, M.trossulus[2] /(M.trossulus[2] + M.trossulus[1])), 2)
  P_T_ME <- round(with(data = freq_dd, M.edulis[2] /(M.edulis[2] + M.edulis[1])), 2)
  # P_ME <- round(with(data = freq_dd, sum(M.edulis) /(sum(freq_dd[,-1]))), 2)
  
  P_E_ME <- round(with(data = freq_dd, M.edulis[1] /(M.edulis[1] + M.edulis[2])), 2)
  P_E_MT <- round(with(data = freq_dd, M.trossulus[1] /(M.trossulus[1] + M.trossulus[2])), 2)
  # P_MT <- round(with(data = freq_dd, sum(M.trossulus) /(sum(freq_dd[,-1]))), 2)
  
  
  probs <- data.frame(P_MT = seq(0, 1, 0.01), P_MT_T = NA, P_ME_E = NA)
  
  probs$P_MT_T <- with(probs, (P_T_MT*P_MT)/(P_T_MT*P_MT + P_T_ME*(1-P_MT)))
  probs$P_ME_E <- with(probs, (P_E_ME*(1-P_MT))/(P_E_ME*(1-P_MT) + P_E_MT*P_MT)  )
  
  probs[,4] <- place
  names(probs)[4] <- variable
  probs
  
}


probs_all <- rbind(probs_calc(place = "W"),  probs_calc(place = "BF"), probs_calc(place = "BN") )


########################### Вычисление таблицы со всеми показателями для регионов

probs_calc_3 <- function(variable = "Location2", place = "W", P_MT = 0.5) {
  n <- which(names(myt2) == variable)
  d <- myt2[myt2[,n] == place, ]
  dd <- melt(table(d$Sp, d$morph))
  freq_dd <- dcast(data = dd, formula = Var2 ~ Var1)
  
  P_T_MT <- round(with(data = freq_dd, M.trossulus[2] /(M.trossulus[2] + M.trossulus[1])), 2)
  P_T_ME <- round(with(data = freq_dd, M.edulis[2] /(M.edulis[2] + M.edulis[1])), 2)
  # P_ME <- round(with(data = freq_dd, sum(M.edulis) /(sum(freq_dd[,-1]))), 2)
  
  P_E_ME <- round(with(data = freq_dd, M.edulis[1] /(M.edulis[1] + M.edulis[2])), 2)
  P_E_MT <- round(with(data = freq_dd, M.trossulus[1] /(M.trossulus[1] + M.trossulus[2])), 2)
  # P_MT <- round(with(data = freq_dd, sum(M.trossulus) /(sum(freq_dd[,-1]))), 2)
  
  
  P_MT_T <- (P_T_MT*P_MT)/(P_T_MT*P_MT + P_T_ME*(1-P_MT))
  P_ME_E <- (P_E_ME*(1-P_MT))/(P_E_ME*(1-P_MT) + P_E_MT*P_MT)
  
  c(P_MT_T, P_ME_E)
}



freq_calc2 <- function(variable = "Location2", place = "BF"){
  n <- which(names(myt2) == variable)
  d <- myt2[myt2[,n] == place, ]
  dd <- melt(table(d$Sp, d$morph))
  freq_dd <- dcast(data = dd, formula = Var2 ~ Var1)
  
  #Вероятность встретить M,trossulus среди Т морфотипа оценка условной веротяности P_MT|T
  P_MT_T <- freq_dd$M.trossulus[2]/(freq_dd$M.trossulus[2] + freq_dd$M.edulis[2])
  
  #Вероятность встретить M.edulis среди E морфотипа
  P_ME_E <- freq_dd$M.edulis[1]/(freq_dd$M.edulis[1] + freq_dd$M.trossulus[1])
  
  #Вероятность встретить M.trossulus среди E морфотипа
  P_MT_E <- freq_dd$M.trossulus[1]/(freq_dd$M.edulis[1] + freq_dd$M.trossulus[1])
  
  #Вероятность встретить M.edulis среди T морфотипа
  P_ME_T <- freq_dd$M.edulis[2]/(freq_dd$M.edulis[2] + freq_dd$M.trossulus[2])
  
  #Вероятность встретить T морфотп среди M.trossulus морфотипа
  P_T_MT <- freq_dd$M.trossulus[2]/(freq_dd$M.trossulus[2] + freq_dd$M.trossulus[1])
  
  #Вероятность встретить E морфотп среди M.trossulus морфотипа
  P_E_MT <- freq_dd$M.trossulus[1]/(freq_dd$M.trossulus[2] + freq_dd$M.trossulus[1])
  
  #Вероятность встретить T морфотп среди M.edulis морфотипа
  P_T_ME <- freq_dd$M.edulis[2]/(freq_dd$M.edulis[2] + freq_dd$M.edulis[1])
  
  #Вероятность встретить E морфотп среди M.edlis морфотипа
  P_E_ME <- freq_dd$M.edulis[1]/(freq_dd$M.edulis[2] + freq_dd$M.edulis[1])
  
  #Вероятность встретить M.trossulus любого морфотипа
  P_MT   <- sum(freq_dd$M.trossulus)/(sum(freq_dd$M.trossulus) + sum(freq_dd$M.edulis))
  
  #Вероятность встретить M.edulis любого морфотипа
  P_ME   <- sum(freq_dd$M.edulis)/(sum(freq_dd$M.trossulus) + sum(freq_dd$M.edulis))
  
  #Вероятность встретить M.trossulus T морфотипа
  P_MT_of_T   <- (freq_dd$M.trossulus[2])/(sum(freq_dd$M.trossulus) + sum(freq_dd$M.edulis))
  
  #Вероятность встретить M.trossulus E морфотипа
  P_MT_of_E   <- (freq_dd$M.trossulus[1])/(sum(freq_dd$M.trossulus) + sum(freq_dd$M.edulis))
  
  #Вероятность встретить M.edulis T морфотипа
  P_ME_of_T   <- (freq_dd$M.edulis[2])/(sum(freq_dd$M.trossulus) + sum(freq_dd$M.edulis))
  
  #Вероятность встретить M.edulis E морфотипа
  P_ME_of_E   <- (freq_dd$M.edulis[1])/(sum(freq_dd$M.trossulus) + sum(freq_dd$M.edulis))
  
  #Вероятность встретить  E морфотип
  P_E   <- sum(freq_dd[1, -1])/sum(freq_dd[,-1])
  
  #Вероятность встретить  T морфотип
  P_T   <- sum(freq_dd[2, -1])/sum(freq_dd[,-1])
  
  
  props <- c(P_MT_T, P_ME_E, P_MT_E, P_ME_T, P_T_MT, P_E_MT, P_T_ME,  P_E_ME, P_MT, P_ME, P_MT_of_T, P_MT_of_E, P_ME_of_T, P_ME_of_E, P_E, P_T)
  
  names(props) <- c("P_MT_T", "P_ME_E", "P_MT_E", "P_ME_T", "P_T_MT", "P_E_MT", "P_T_ME",  "P_E_ME", "P_MT", "P_ME", "P_MT_of_T", "P_MT_of_E", "P_ME_of_T", "P_ME_of_E", "P_E", "P_T")
  
  props
}


All_freq <- data.frame(barents_normal = freq_calc2(place = "BN"), 
                       barents_fresh = freq_calc2(place = "BF"),
                       white_normal = freq_calc2(place = "W"))


tAll_freq <- as.data.frame(t(All_freq))
tAll_freq$Location <- rownames(tAll_freq)








Pl_fit_init_data + geom_line(data = probs_all, aes(x = P_MT,  y = P_MT_T), color = "red", linetype = 2) + geom_line(data = probs_all, aes(x = P_MT,  y = P_ME_E), color = "blue", linetype = 2)

