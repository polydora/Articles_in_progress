# Этот код написан для анализа возможности определения видов, определенных по генетическим данным, за счет использованя морфологического признакак  Золоторева 


library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(car)
library(doBy)



# Подготовка данных

myt <- read.table("data_salinity3.csv", header = T, sep = ",")

myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)


names(myt)

# myt$sal_place <- factor(myt$sal_place, levels = c("low", "meadle", "high")) 
# myt$sal_place2 <- ifelse(myt$sal_place == "low", "fresh", "normal") 


# 
# colSums(is.na(myt))
# # 30 мидий не имеют размеров. Удаляем их из датасета
# 
# myt2 <- myt[!is.na(myt$size), ]
# 
# 
# colSums(is.na(myt2))
# 14 мидий не имеют измерений язычка. Сломаны чтоли? Удаляем их датасета.

myt2 <- myt[!is.na(myt$ind), ]

myt2 <- myt2[myt2$dataset == "training", ]


# переменная congr - это событие правильного определения 1 если T-морфотип совпадает с MT и E-морфотип совпадает с ME, 0 - если не свопадает

myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )


## Частота мидий T-морфотипа в отдельных поселеинях
ind_pop <-summaryBy(ind ~ pop, data = myt2) 

names(ind_pop) <- c("pop", "freq_Tmorph")


names(myt2)
myt2 <- merge(myt2, ind_pop, by = "pop")


myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)

# myt2$place2 <- factor(myt2$place2, levels = c( "kand_cut" , "kand_main", "kola_cut"  ,  "kola_tuva", "kola_dz"))

# levels(myt2$place2)


freq_MT <- summaryBy( str ~ pop, data = myt2)

names(freq_MT) <- c("pop", "freq_MT")

myt2 <- merge(myt2, freq_MT)





### Модель

Mod_fT_congr <- glmer(congr ~ morph * freq_MT*sea*sal_place + (1 | pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# Mod_fT_congr_clog <- glmer(congr ~ morph * freq_MT*sea + (1 | pop), data = myt2, family = binomial(link = "cloglog"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# Mod_fT_congr_prob <- glmer(congr ~ morph * freq_MT*sea + (1 | pop), data = myt2, family = binomial(link = "probit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


# 
# 
# Mod_fT_congr <- glm(congr ~ morph * freq_MT*sea, data = myt2, family = binomial(link = "logit"))
# 
# Mod_fT_congr_clog <- glm(congr ~ morph * freq_MT*sea , data = myt2, family = binomial(link = "cloglog"))
# 
# Mod_fT_congr_prob <- glm(congr ~ morph * freq_MT*sea, data = myt2, family = binomial(link = "probit"))
# 



# AIC(Mod_fT_congr, Mod_fT_congr_clog, Mod_fT_congr_prob)

# Mod_fT_congr <- Mod_fT_congr_prob

# overdisp(Mod_fT_congr)


drop1(Mod_fT_congr, test = "Chi")

Mod_fT_congr2 <- update(Mod_fT_congr, .~.-morph:freq_MT:sea:sal_place)
drop1(Mod_fT_congr2, test = "Chi")

Mod_fT_congr3 <- update(Mod_fT_congr2, .~.-morph:freq_MT:sea )
drop1(Mod_fT_congr3, test = "Chi")

Mod_fT_congr4 <- update(Mod_fT_congr3, .~.-freq_MT:sea:sal_place )
drop1(Mod_fT_congr4, test = "Chi")

Mod_fT_congr5 <- update(Mod_fT_congr4, .~.-morph:freq_MT:sal_place )
drop1(Mod_fT_congr5, test = "Chi")

Mod_fT_congr6 <- update(Mod_fT_congr5, .~.- freq_MT:sal_place  )
drop1(Mod_fT_congr6, test = "Chi")


# 


#Диагностика

Mod_fT_congr_diagn <- fortify(Mod_fT_congr6)

ggplot(data = Mod_fT_congr_diagn, aes(x = .fitted, y = .scresid, color = sea)) + geom_point() + geom_smooth()


ggplot(Mod_fT_congr_diagn, aes(x = size, y = .scresid, color = sea)) + geom_point() + geom_smooth()


ggplot(Mod_fT_congr_diagn, aes(x = str, y = .scresid, color = sea)) + geom_point() + geom_smooth()


ggplot(Mod_fT_congr_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = Sp, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = place, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = sal_place, y = .scresid)) + geom_boxplot() 


ggplot(Mod_fT_congr_diagn, aes(x = morph, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = sal_low, y = .scresid, color = sea)) + geom_point() + geom_smooth()

ggplot(Mod_fT_congr_diagn, aes(x = freq_MT, y = .scresid, color = sea)) + geom_point() + geom_smooth()



ggplot(Mod_fT_congr_diagn, aes(sample=.resid))+stat_qq() + geom_abline(intercept = 0) + facet_wrap(~sea)  



summary(Mod_fT_congr6)

exp(fixef(Mod_fT_congr6))

Anova(Mod_fT_congr6)

icc(Mod_fT_congr6)


#Визуализация

newdata <- myt2 %>% group_by(sea, morph, sal_place) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

newdata$fit <- predict(Mod_fT_congr6, newdata = newdata, type = "response", re.form = NA) 

newdata$fit_eta <- predict(Mod_fT_congr6, newdata = newdata, re.form = NA) 


summary(Mod_fT_congr6)

X <- model.matrix(  ~ morph + freq_MT + sea + sal_place + morph:freq_MT + morph:sea + freq_MT:sea + morph:sal_place + sea:sal_place + morph:sea:sal_place , data = newdata)



b <- fixef(Mod_fT_congr6)



newdata$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr6) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация


# probit_back <- function(x) {library(VGAM); probit(x, inverse = TRUE)}

newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)

# newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)


Pl_fit <- ggplot(newdata, aes(x = freq_MT, y = fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.2)  + geom_line(aes(color = morph), size=2) + facet_grid(sea ~ sal_place) + scale_color_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1) + geom_hline(yintercept = 0.5, linetype = 2 ) + labs(y = "Probability of correct identification", x = "Proportion of M.trossulus") + theme(legend.position = "bottom")




# Первичные данные на графике предсказаний

correct_prop <- summaryBy(congr + freq_MT  ~ pop + sea + morph + sal_place, data = myt2)



Pl_fit_init_data <- Pl_fit + geom_point(data = correct_prop, aes(x = freq_MT.mean, y = congr.mean, color = morph), size = 2)





# 
# probs_calc <- function(variable = "Location", place = "white") {
#   n <- which(names(myt2) == variable)
#   d <- myt2[myt2[,n] == place, ]
#   dd <- melt(table(d$Sp, d$morph))
#   freq_dd <- dcast(data = dd, formula = Var.2 ~ Var.1)
#   
#   P_T_MT <- round(with(data = freq_dd, M.trossulus[2] /(M.trossulus[2] + M.trossulus[1])), 2)
#   P_T_ME <- round(with(data = freq_dd, M.edulis[2] /(M.edulis[2] + M.edulis[1])), 2)
#   P_ME <- round(with(data = freq_dd, sum(M.edulis) /(sum(freq_dd[,-1]))), 2)
#   
#   P_E_ME <- round(with(data = freq_dd, M.edulis[1] /(M.edulis[1] + M.edulis[2])), 2)
#   P_E_MT <- round(with(data = freq_dd, M.trossulus[1] /(M.trossulus[1] + M.trossulus[2])), 2)
#   P_MT <- round(with(data = freq_dd, sum(M.trossulus) /(sum(freq_dd[,-1]))), 2)
#   
#   
#   probs <- data.frame(P_T = seq(0, 1, 0.01), P_MT_T = NA, P_ME_E = NA)
#   
#   probs$P_MT_T <- with(probs, (P_T_MT*P_T)/(P_T_MT*P_T + P_T_ME*P_ME))
#   probs$P_ME_E <- with(probs, (P_E_ME*(1-P_T))/(P_E_ME*(1-P_T) + P_E_MT*P_MT)  )
#   
#   probs[,4] <- place
#   names(probs)[4] <- variable
#   probs
# 
# }
# 
# 







probs_calc <- function(variable = "Location", place = "white") {
  n <- which(names(myt2) == variable)
  d <- myt2[myt2[,n] == place, ]
  dd <- melt(table(d$Sp, d$morph))
  freq_dd <- dcast(data = dd, formula = Var1 ~ Var2)
  freq_dd$P_E <- round(with(data = freq_dd, E_m/(E_m + T_m)), 2)
  freq_dd$P_T <- round(with(data = freq_dd, T_m/(E_m + T_m)), 2)
  probs <- data.frame(P_MT = seq(0, 1, 0.01), P_MT_T = NA, P_ME_E = NA)

  probs$P_MT_T <- with(probs, (freq_dd$P_T[2]*P_MT)/(freq_dd$P_T[2]*P_MT + freq_dd$P_T[1]*(1-P_MT)))

  probs$P_ME_E <- with(probs, (freq_dd$P_E[1]*(1-P_MT))/(freq_dd$P_E[1]*(1-P_MT) + freq_dd$P_E[2]*P_MT))

  probs[,4] <- place
  names(probs)[4] <- variable
  probs

}




myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") 


probs_all <- rbind(probs_calc(place = "white_normal"), probs_calc(place = "white_fresh"), probs_calc(place = "barents_fresh"), probs_calc(place = "barents_normal") )

probs_all$sea <- ifelse(probs_all$Location == "white_normal" | probs_all$Location == "white_fresh", "white", "barents") 

probs_all$sal_place <- ifelse(probs_all$Location == "barents_normal" | probs_all$Location == "white_normal", "normal", "fresh") 



################## Зависмость вероятности правильного определения от доли M.trossulus в популяции ############

Pl_fit_init_data + geom_line(data = probs_all, aes(x = P_MT,  y = P_MT_T), color = "red", linetype = 2) + geom_line(data = probs_all, aes(x = P_MT,  y = P_ME_E), color = "blue", linetype = 2)

##############



# 
# 
# ## Бутстрепы
# 
# 
# # confint(Mod_fT_congr4, nsim = 50)
# 
# 
# newdata_boot <- newdata[,1:4]
# 
# newdata_boot$group <- 0
# 
# 
# 
# for (i in 1:100) {
#   
#   d <- newdata[,1:4]
# 
#   y<- unlist(simulate(Mod_fT_congr3))
#   M_boot <- refit(Mod_fT_congr4, y )
#   d$fit <- predict(M_boot, newdata = d, type = "response", re.form = NA) 
#   d$group <- i
#   newdata_boot <-rbind(newdata_boot, d)
#   
#   print(i)  
# 
# }
# 
# 
# 
# 
# newdata_boot$freq_MT_2 <- round(newdata_boot$freq_MT, 1)
# 
# boot_quantiles2 <- newdata_boot %>% group_by(sea, morph, freq_MT) %>% summarise(lwr = quantile(fit, probs = c(0.025)), upr = quantile(fit, probs = c(0.975)) )
# 
# 
# Pl_fit + geom_line(data = boot_quantiles2, aes(x = freq_MT, y = lwr, color = morph), linetype = 1) + geom_line(data = boot_quantiles2, aes(x = freq_MT, y = upr, color = morph), linetype = 1) + geom_line(data = probs_total, aes(x = P_MT,  y = P_ME_E), color = "blue", linetype = 2)  + geom_line(data = probs_total, aes(x = P_MT,  y = P_MT_T), color = "red", linetype = 2)
# 
# Pl_fit2 +   geom_ribbon(data = boot_quantiles, aes(x = freq_MT_2, ymin = lwr, ymax = upr, group = morph))   + geom_line(data = probs_all, aes(x = P_MT,  y = P_ME_E), color = "blue", linetype = 2)  + geom_line(data = probs_all, aes(x = P_MT,  y = P_MT_T), color = "red", linetype = 2)
# 
# 
# 
# 

#########Модель в зависимости от частоты T мофотипа  ##############################

# 
# ### Модель
# 
# Mod_fT_congr <- glmer(congr ~ Sp * freq_Tmorph.x*sea*sal_place + (1 | pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# 
# Mod_fT_congr_ris <- glmer(congr ~ Sp * freq_Tmorph.x*sea + (1 + freq_Tmorph.x| pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
# 
# 
# 
# AIC(Mod_fT_congr, Mod_fT_congr_ris)
# 
# # Mod_fT_congr <- Mod_fT_congr_prob
# 
# # overdisp(Mod_fT_congr)
# 
# 
# drop1(Mod_fT_congr, test = "Chi")
# 
# Mod_fT_congr2 <- update(Mod_fT_congr, .~.-Sp:freq_Tmorph.x:sea:sal_place )
# drop1(Mod_fT_congr2, test = "Chi")
# 
# Mod_fT_congr3 <- update(Mod_fT_congr2, .~.-freq_Tmorph.x:sea:sal_place )
# drop1(Mod_fT_congr3, test = "Chi")
# 
# Mod_fT_congr4 <- update(Mod_fT_congr3, .~.-Sp:sea:sal_place )
# drop1(Mod_fT_congr4, test = "Chi")
# 
# Mod_fT_congr5 <- update(Mod_fT_congr4, .~.-Sp:freq_Tmorph.x:sea )
# drop1(Mod_fT_congr5, test = "Chi")
# 
# 
# Mod_fT_congr6 <- update(Mod_fT_congr5, .~.-Sp:sea )
# drop1(Mod_fT_congr6, test = "Chi")
# 
# 
# Mod_fT_congr7 <- update(Mod_fT_congr6, .~.-freq_Tmorph.x:sea  )
# drop1(Mod_fT_congr7, test = "Chi")
# 
# 
# # Mod_fT_congr4 <- update(Mod_fT_congr3, .~.-morph:sea)
# # drop1(Mod_fT_congr4, test = "Chi")
# 
# 
# 
# 
# #Диагностика
# 
# Mod_fT_congr_diagn <- fortify(Mod_fT_congr7)
# 
# # Mod_fT_congr_diagn <- fortify(Mod_fT_congr_ris)
# 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = .fitted, y = .scresid, color = sea)) + geom_point() + geom_smooth()
# 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = size, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")
# 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = str, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")
# 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = Sp, y = .scresid)) + geom_boxplot() 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = place, y = .scresid)) + geom_boxplot() 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = sal_place, y = .scresid)) + geom_boxplot() 
# 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = morph, y = .scresid)) + geom_boxplot() 
# 
# ggplot(Mod_fT_congr_diagn, aes(x = sal_low, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")
# 
# ggplot(Mod_fT_congr_diagn, aes(x = freq_MT, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")
# 
# 
# 
# ggplot(Mod_fT_congr_diagn, aes(sample=.resid))+stat_qq() + geom_abline(intercept = 0) + facet_wrap(~sea)  
# 
# 
# 
# summary(Mod_fT_congr7)
# 
# exp(fixef(Mod_fT_congr7))
# 
# Anova(Mod_fT_congr7)
# 
# icc(Mod_fT_congr7)
# 
# 
# #Визуализация
# 
# newdata <- myt2 %>% group_by(sea, Sp, sal_place) %>% do(data.frame(freq_Tmorph.x = seq(min(.$freq_Tmorph.x), max(.$freq_Tmorph.x), length.out = 100)))
# 
# newdata$fit <- predict(Mod_fT_congr7, newdata = newdata, type = "response", re.form = NA) 
# 
# newdata$fit_eta <- predict(Mod_fT_congr7, newdata = newdata, re.form = NA) 
# 
# 
# summary(Mod_fT_congr7)
# 
# X <- model.matrix(  ~ Sp + freq_Tmorph.x + sea + sal_place + Sp:freq_Tmorph.x + Sp:sal_place + freq_Tmorph.x:sal_place + sea:sal_place + Sp:freq_Tmorph.x:sal_place, data = newdata)
# 
# 
# 
# b <- fixef(Mod_fT_congr7)
# 
# 
# 
# newdata$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr7) %*% t(X)))
# 
# logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация
# 
# 
# # probit_back <- function(x) {library(VGAM); probit(x, inverse = TRUE)}
# 
# newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)
# 
# # newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# # newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)
# 
# 
# Pl_fit <- ggplot(newdata, aes(x = freq_Tmorph.x, y = fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = Sp), alpha = 0.2)  + geom_line(aes(color = Sp), size=2) + facet_grid( sal_place ~ sea) + scale_color_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1) + geom_hline(yintercept = 0.5, linetype = 2 ) + labs(y = "Probability of correct identification", x = "Proportion of T-morphotype") + theme(legend.position = "bottom") 
# 
# 
# 
# 
# # Первичные данные на графике предсказаний
# 
# correct_prop <- summaryBy(congr + freq_Tmorph.x  ~ pop + sea + Sp + sal_place, data = myt2)
# 
# 
# Pl_fit_init_data <- Pl_fit + geom_point(data = correct_prop, aes(x = freq_Tmorph.x.mean, y = congr.mean, color = Sp), size = 2)
# 
# 
# 
# 
# 
# # Условные вероятности присутствия язычка, если мы знаем, что мидия относится к определенному виду
# 
# 
# myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") 
# 
# probs_calc <- function(variable = "Location", place = "white_fresh") {
#   n <- which(names(myt2) == variable)
#   d <- myt2[myt2[,n] == place, ]
#   dd <- melt(table(d$morph, d$Sp))
#   freq_dd <- dcast(data = dd, formula = Var1 ~ Var2)
#   freq_dd$P_ME <- round(with(data = freq_dd, M.edulis/(M.edulis + M.trossulus)), 2)
#   freq_dd$P_MT <- round(with(data = freq_dd, M.trossulus/(M.edulis + M.trossulus)), 2)
#   probs <- data.frame(P_T = seq(0, 1, 0.01), P_T_MT = NA, P_E_ME = NA)
#   
#   probs$P_T_MT <- with(probs, (freq_dd$P_MT[2]*P_T)/(freq_dd$P_MT[2]*P_T + freq_dd$P_MT[1]*(1-P_T)))
#   
#   probs$P_E_ME <- with(probs, (freq_dd$P_ME[1]*(1-P_T))/(freq_dd$P_ME[1]*(1-P_T) + freq_dd$P_ME[2]*P_T))
#   
#   probs[,4] <- place
#   names(probs)[4] <- variable
#   probs
#   
# }
# 
# 
# 
# 
# probs_all <- rbind(probs_calc(place = "white_normal"), probs_calc(place = "white_fresh"), probs_calc(place = "barents_fresh"), probs_calc(place = "barents_normal") )
# 
# probs_all$sea <- ifelse(probs_all$Location == "white_normal" | probs_all$Location == "white_fresh", "white", "barents") 
# 
# probs_all$sal_place <- ifelse(probs_all$Location == "barents_normal" | probs_all$Location == "white_normal", "normal", "fresh") 
# 
# 
# 
# Pl_fit_init_data + geom_line(data = probs_all, aes(x = P_T, y = P_T_MT), color = "darkred", linetype = 2) + geom_line(data = probs_all, aes(x = P_T, y = P_E_ME), color = "darkblue", linetype = 2)
# 





######### Модель в зависимости от частоты T мофотипа, но в качестве предиктора берем не Sp, а morp  ##############################


### Модель

Mod_fT_congr_morph <- glmer(congr ~ morph * freq_Tmorph*sea*sal_place + (1 | pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


# Mod_fT_congr_ris <- glmer(congr ~ Sp * freq_Tmorph.x*sea + (1 + freq_Tmorph.x| pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))



# AIC(Mod_fT_congr, Mod_fT_congr_ris)

# Mod_fT_congr <- Mod_fT_congr_prob

# overdisp(Mod_fT_congr)


drop1(Mod_fT_congr_morph, test = "Chi")


# Mod_fT_congr4 <- update(Mod_fT_congr3, .~.-morph:sea)
# drop1(Mod_fT_congr4, test = "Chi")




#Диагностика

Mod_fT_congr_diagn <- fortify(Mod_fT_congr_morph)

# Mod_fT_congr_diagn <- fortify(Mod_fT_congr_ris)


ggplot(Mod_fT_congr_diagn, aes(x = .fitted, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "loess")


ggplot(Mod_fT_congr_diagn, aes(x = size, y = .scresid, color = sea)) + geom_point() + geom_smooth()


ggplot(Mod_fT_congr_diagn, aes(x = str, y = .scresid, color = sea)) + geom_point() + geom_smooth()


ggplot(Mod_fT_congr_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = Sp, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = place, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = sal_place, y = .scresid)) + geom_boxplot() 


ggplot(Mod_fT_congr_diagn, aes(x = morph, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = sal_low, y = .scresid, color = sea)) + geom_point() + geom_smooth()

ggplot(Mod_fT_congr_diagn, aes(x = freq_MT, y = .scresid, color = sea)) + geom_point() + geom_smooth()



ggplot(Mod_fT_congr_diagn, aes(sample=.resid))+stat_qq() + geom_abline(intercept = 0) + facet_wrap(~sea)  



summary(Mod_fT_congr_morph)

Anova(Mod_fT_congr_morph)

icc(Mod_fT_congr_morph)


#Визуализация

newdata <- myt2 %>% group_by(sea, morph, sal_place) %>% do(data.frame(freq_Tmorph = seq(min(.$freq_Tmorph), max(.$freq_Tmorph), length.out = 100)))

newdata$fit <- predict(Mod_fT_congr_morph, newdata = newdata, type = "response", re.form = NA) 

newdata$fit_eta <- predict(Mod_fT_congr_morph, newdata = newdata, re.form = NA) 


summary(Mod_fT_congr)

X <- model.matrix(  ~ morph * freq_Tmorph * sea * sal_place, data = newdata)



b <- fixef(Mod_fT_congr_morph)



newdata$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr_morph) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация


# probit_back <- function(x) {library(VGAM); probit(x, inverse = TRUE)}

newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)

# newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)


Pl_fit <- ggplot(newdata, aes(x = freq_Tmorph, y = fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.2)  + geom_line(aes(color = morph), size=2) + facet_grid( sea ~ sal_place ) + scale_color_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1) + geom_hline(yintercept = 0.5, linetype = 2 ) + labs(y = "Probability of correct identification", x = "Proportion of T-morphotype") + theme(legend.position = "bottom")




# Первичные данные на графике предсказаний

correct_prop <- summaryBy(congr + freq_Tmorph  ~ pop + sea + morph + sal_place, data = myt2)


Pl_fit_init_data <- Pl_fit + geom_point(data = correct_prop, aes(x = freq_Tmorph.mean, y = congr.mean, color = morph), size = 2)


myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") 


probs_calc2 <- function(variable = "Location", place = "white_fresh") {
  n <- which(names(myt2) == variable)
  d <- myt2[myt2[,n] == place, ]
  dd <- melt(table(d$Sp, d$morph))
  freq_dd <- dcast(data = dd, formula = Var.2 ~ Var.1)
  
  P_T_MT <- round(with(data = freq_dd, M.trossulus[2] /(M.trossulus[2] + M.trossulus[1])), 2)
  P_T_ME <- round(with(data = freq_dd, M.edulis[2] /(M.edulis[2] + M.edulis[1])), 2)
  P_ME <- round(with(data = freq_dd, sum(M.edulis) /(sum(M.edulis) + sum(M.trossulus))), 2)
  
  P_E_ME <- round(with(data = freq_dd, M.edulis[1] /(M.edulis[1] + M.edulis[2])), 2)
  P_E_MT <- round(with(data = freq_dd, M.trossulus[1] /(M.trossulus[1] + M.trossulus[2])), 2)
  P_MT <- round(with(data = freq_dd, sum(M.trossulus) /(sum(M.edulis) + sum(M.trossulus))), 2)
  
  
  
  probs <- data.frame(P_T = seq(0, 1, 0.01), P_MT_T = NA, P_ME_E = NA)
  
  probs$P_MT_T <- with(probs, (P_T_MT*P_T)/(P_T_MT*P_T + P_T_ME*P_ME))
  probs$P_ME_E <- with(probs, (P_E_ME*(1-P_T))/(P_E_ME*(1-P_T) + P_E_MT*P_MT)  )

  probs[,4] <- place
  names(probs)[4] <- variable
  probs
  
}

probs_calc2()


# myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") 


probs_all <- rbind(probs_calc2(place = "white_normal"), probs_calc2(place = "white_fresh"), probs_calc2(place = "barents_fresh"), probs_calc2(place = "barents_normal") )

probs_all$sea <- ifelse(probs_all$Location == "white_normal" | probs_all$Location == "white_fresh", "white", "barents") 

probs_all$sal_place <- ifelse(probs_all$Location == "barents_normal" | probs_all$Location == "white_normal", "normal", "fresh") 



Pl_fit_init_data + geom_line(data = probs_all, aes(x = P_T, y = P_MT_T), color = "darkred", linetype = 2) + geom_line(data = probs_all, aes(x = P_T, y = P_ME_E), color = "darkblue", linetype = 2)





