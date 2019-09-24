# Этот код написан для анализа возможности определения видов, определенных по генетическим данным, за счет использованя морфологического признакак  Золоторева 


library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(car)
library(doBy)



# Подготовка данных

myt <- read.table("data_salinity2.csv", header = T, sep = ",")

myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)


myt$sal_place <- factor(myt$sal_place, levels = c("low", "meadle", "high")) 
myt$sal_place2 <- ifelse(myt$sal_place == "low", "fresh", "normal") 



colSums(is.na(myt))
# 30 мидий не имеют размеров. Удаляем их из датасета

myt2 <- myt[!is.na(myt$size), ]


colSums(is.na(myt2))
# 14 мидий не имеют измерений язычка. Сломаны чтоли? Удаляем их датасета.

myt2 <- myt2[!is.na(myt2$ind), ]


# переменная congr - это событие правильного определения 1 если T-морфотип совпадает с MT и E-морфотип совпадает с ME, 0 - если не свопадает

myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )



ind_pop <-summaryBy(ind ~ pop, data = myt2) 

names(ind_pop) <- c("pop", "freq_Tmorph")

myt2 <- merge(myt2, ind_pop, by = "pop")


myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)

myt2$place2 <- factor(myt2$place2, levels = c( "kand_cut" , "kand_main", "kola_cut"  ,  "kola_tuva", "kola_dz"))

levels(myt2$place2)


freq_MT_class <- ntile(myt2$freq_Tmorph, 2)
myt2$freq_MT_class <- freq_MT_class

table(myt2$place2, myt2$freq_MT_class)

table(myt2$place2, myt2$morph)


d <- myt2[myt2$place2 == "kand_cut", ]

d <- myt2[myt2$place2 == "kand_main", ]

d <- myt2[myt2$place2 == "kola_cut", ]

d <- myt2[myt2$place2 == "kola_tuva", ]


table(d$Sp, d$morph)



### Модель

Mod_fT_congr <- glmer(congr ~ morph * place2 * freq_Tmorph  + (1 | pop), data = myt2, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

overdisp(Mod_fT_congr)


drop1(Mod_fT_congr, test = "Chi")






#Диагностика

Mod_fT_congr_diagn <- fortify(Mod_fT_congr)


ggplot(Mod_fT_congr_diagn, aes(x = .fitted, y = .scresid, color = sea)) + geom_point() + geom_smooth()


ggplot(Mod_fT_congr_diagn, aes(x = size, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")


ggplot(Mod_fT_congr_diagn, aes(x = str, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")


ggplot(Mod_fT_congr_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = Sp, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = place2, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = sal_place, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = morph, y = .scresid)) + geom_boxplot() 

ggplot(Mod_fT_congr_diagn, aes(x = sal_low, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")

ggplot(Mod_fT_congr_diagn, aes(x = freq_MT, y = .scresid, color = sea)) + geom_point() + geom_smooth(method = "lm")


summary(Mod_fT_congr)

Anova(Mod_fT_congr)

#Визуализация

newdata <- myt2 %>% group_by(place2, morph) %>% do(data.frame(freq_Tmorph = seq(min(.$freq_Tmorph), max(.$freq_Tmorph), length.out = 100)))

newdata$fit <- predict(Mod_fT_congr, newdata = newdata, type = "response", re.form = NA) 

newdata$fit_eta <- predict(Mod_fT_congr, newdata = newdata, re.form = NA) 


X <- model.matrix(  ~ morph *place2 * freq_Tmorph , data = newdata)



b <- fixef(Mod_fT_congr)



newdata$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr) %*% t(X)))

logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация



newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)



Pl_fit <- ggplot(newdata, aes(x = freq_Tmorph, y = fit)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.2)  + geom_line(aes(color = morph)) + facet_wrap( ~ place2) + scale_color_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1) + geom_hline(yintercept = 0.5, linetype = 2 ) + labs(y = "Probability of correct identification") + theme(legend.position = "bottom")

# Первичные данные на графике предсказаний

correct_prop <- summaryBy(congr + freq_Tmorph  ~ pop + place2 + morph, data = myt2)

props <- summaryBy(freq_Tmorph + freq_MT  ~ pop + place2, data = myt2)

Pl_fit + geom_point(data = correct_prop, aes(x = freq_Tmorph.mean, y = congr.mean, color = morph))


Pl_fit + geom_point(data = props, aes(x = freq_Tmorph.mean, y = freq_MT.mean))









stat_sum_df <- function(fun, geom="crossbar", ...) {
  stat_summary(fun.data = fun, geom = geom, width = 0.2, ...)
}

ggplot(myt2, aes(x = place, y = sal_low)) + stat_sum_df("median_hilow", mapping = aes(group = place)) + theme_bw() + labs(x = "Area", y = "Salinity") + geom_point(color ="blue")
  

ggplot(myt2, aes(x = place2, y = freq_Tmorph)) + stat_sum_df("median_hilow", mapping = aes(group = place2)) + theme_bw() + labs(x = "Area", y = "Salinity") + geom_point(color ="blue")

