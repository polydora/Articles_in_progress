# В этом скрипте приведена обработка данных, включающих все георафические данные

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
library(MuMIn)
library(gridExtra)
library(mgcv)






#### Data reading and initial preparation #####

myt <- read.table("data_salinity3.csv", header = T, sep = ",")

# Оставляем только мидий, у которых есть оценка морфотипа
myt2 <- myt[!is.na(myt$ind), ]


# Подразделяем данные на сабсеты

myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "fresh"] <- "BL" 
myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "normal"] <- "BH" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "normal"] <- "W" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "fresh"] <- "W" 

myt2$Subset[myt2$sea == "Baltic"] <- "BALT" 
myt2$Subset[myt2$sea == "GOM"] <- "GOM" 
myt2$Subset[myt2$sea == "Norway"] <- "NORW" 
myt2$Subset[myt2$sea == "Scotland"] <- "SCOT" 


myt2$Subset <- factor(myt2$Subset, levels = c("W", "BL", "BH", "NORW", "BALT", "SCOT", "GOM" ))

levels(myt2$Subset)



# Вводим обозначения 

myt2$Sp [myt2$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt2$Sp [myt2$str <= 0.5] <- "M.edulis"
myt2$Sp <- factor(myt2$Sp)



# Вводим обозначения для морфотипов
myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)



# Бинарное обозначение видов
myt2$Sp2 <- ifelse(myt2$Sp == "M.trossulus", 1, 0)


#Correct identification
myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )


# Частота M.trossulus в популяции

freq_MT <- myt2 %>% group_by(pop) %>% summarise(freq_MT = mean(Sp2))

myt2 <- merge(myt2, freq_MT)


# Частота T-морфотипа в популяции

Prop_T <- myt2 %>% group_by(pop) %>% summarise(Prop_T = mean(ind))

myt2 <- merge(myt2, Prop_T)


# Разделяем на тестовые и моделинговые датасеты


# 
# # Извлекаем из беломорского материала тестовую выборку 
# #В формальную тестовую выборку  попадают точки наиболее близкие к 20%, 40%, 60% и 80% freq_MT
# 
# selected_pop <- myt2[myt2$Subset == "W", ] %>% group_by(Subset, pop) %>% summarise(freq_MT = mean(freq_MT)) %>% group_by(Subset) %>% arrange(freq_MT, .by_group = TRUE) %>% mutate(dif_20 = (freq_MT - 0.2)^2, dif_40 = (freq_MT - 0.4)^2, dif_60 = (freq_MT - 0.6)^2, dif_80 = (freq_MT - 0.8)^2)  %>% group_by(Subset)  %>% summarize (n_pop =n(), q_20_pop = nth(pop, which.min(dif_20)), q_40_pop = nth(pop, which.min(dif_40)), q_60_pop = nth(pop, which.min(dif_60)), q_80_pop = nth(pop, which.min(dif_80))) 
# 
# selected_pop <- melt(selected_pop, id.vars = c("Subset", "n_pop"))$value
# 


# testing data set
myt3 <- myt2[myt2$dataset == "testing" | myt2$pop %in% c("kovda", "rya", "chupa", "umba_pil"),  ]

#modelling data set
myt2 <- myt2[! myt2$pop %in% unique(myt3$pop), ]

levels(myt2$Subset)


# Модели для сравнения geographical datasets 

myt2_reduced <- myt2[myt2$Subset %in% c("W", "BL", "BH", "GOM", "BALT"), ]

myt2_reduced$Subset <- factor(myt2_reduced$Subset)
levels(myt2_reduced$Subset)



library("optimx")


Model_4_full_geogr <- glmer(congr ~ morph * freq_MT * Subset + (1 | pop), data = myt2_reduced, family = binomial(link = "logit"), control=glmerControl(optimizer = "optimx", calc.derivs = FALSE, optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))



overdisp_fun(Model_4_full_geogr)
summary(Model_4_full_geogr)

r.squaredGLMM(Model_4_final)

drop1(Model_4_full_geogr)

Model_4_full_geogr2 <- update(Model_4_full_geogr, . ~ . - morph:freq_MT:Subset)

drop1(Model_4_full_geogr2)


Model_4_final <- Model_4_full_geogr2 





new_data4 <- myt2_reduced %>% group_by(Subset, morph) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))



# Предсказанные значеня в шкале вероятностей
new_data4$fit <- predict(Model_4_final, newdata = new_data4, type = "response", re.form = NA)

# Предсказанные значеня в шкале логитов
new_data4$fit_eta <- predict(Model_4_final, newdata = new_data4, re.form = NA)

# Вычисление доверительного инеравала
formula(Model_4_final)

X <- model.matrix(  ~ morph + freq_MT + Subset + morph:freq_MT + morph:Subset + freq_MT:Subset, data = new_data4) #Модельная матрица для визуализации


# Ошибки в шкале логитов
new_data4$se_eta <- sqrt(diag(X %*% vcov(Model_4_final) %*% t(X)))

new_data4$lwr <- logit_back(new_data4$fit_eta - 1.96 * new_data4$se_eta)

new_data4$upr <- logit_back(new_data4$fit_eta + 1.96 * new_data4$se_eta)





Pl_mod4 <- ggplot(new_data4, aes(x = freq_MT)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.1)  +
  geom_line(aes(y = fit, color = morph), size=1, linetype = 2) +
  geom_rug(data = myt2_reduced, inherit.aes = FALSE,  aes(x = freq_MT), size = 0.1) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red"))  +
  xlim(0,1)  +
  facet_wrap( ~ Subset)




pr_value_M <- myt2 %>% group_by(Subset, pop) %>% summarise(freq_MT = mean(freq_MT), N_T = sum(ind == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_E = sum(ind == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1))

pr_value_M$PMT_T <- with(pr_value_M, N_T_MT / N_T)
pr_value_M$PMT_E <- with(pr_value_M, N_E_MT / N_T)
pr_value_M$PME_E <- with(pr_value_M, N_E_ME / N_E)
pr_value_M$PME_T <- with(pr_value_M, N_T_ME / N_E)


Pl_mod4_with_initial_data <- Pl_mod4 + geom_segment(data = pr_value_M, aes(x = freq_MT, y = PME_E, xend = freq_MT, yend = PMT_T), color="darkgrey") +
  geom_hline(data = pr_value_M, aes(yintercept=0.5), color="black") +
  geom_point(data = pr_value_M, aes(y = PME_E), fill = "white", shape = 21) +
  geom_point(data = pr_value_M, aes(y = PMT_T), fill = "black", shape = 21) +
  labs(y =  "Proportions of correct species \n identification by morphotypes", x = "Proportion of M. trossulus", fill = "")+
  ylim(0,1) +
  xlim(0,1) + 
  theme_bw()



Model_4_final_diag <- fortify(Model_4_final)


ggplot(Model_4_final_diag, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()



#######################

ptop_T_MT <- myt2_reduced %>% group_by(Subset, pop) %>% summarize(Prop_T = mean(Prop_T), MT = sum(Sp2), N = n())

# ptop_T_MT <- ptop_T_MT[! ptop_T_MT$pop %in% c("Limh88", "CBCP"), ]


Model_5_full <- glm(cbind(MT, (N-MT)) ~  Prop_T * Subset, data = ptop_T_MT, family = binomial(link = "logit"))

# 
# Model_5_full <- glm(Sp2 ~  Prop_T * Subset, data = myt2_reduced, family = binomial(link = "logit"))
#  overdisp_fun(Model_5_full)

# drop1(Model_5_full, test = "Chi")

# Model_5_1 <- update(Model_5_full, . ~ . - Prop_T:Subset)

# drop1(Model_5_1, test = "Chi")

Model_5_final <- Model_5_full 


new_data5 <- myt2_reduced %>% group_by(Subset, pop) %>% summarise(Prop_T = mean(Prop_T) ) %>% group_by(Subset) %>%  do(data.frame(Prop_T = seq(min(.$Prop_T), max(.$Prop_T), length.out = 10)))

predicted5 <- predict(Model_5_final, newdata = new_data5,  type="response", se.fit = T)

new_data5$fit <- predicted5$fit

new_data5$SE <- predicted5$se.fit 




Pl_mod5 <- ggplot(new_data5, aes(x = Prop_T, y = fit)) + geom_line(linetype = 2, color = "red", size = 1) + facet_wrap(~Subset) + geom_ribbon(aes(ymin = fit - 1.96*SE, ymax = fit + 1.96*SE), alpha = 0.1) + xlim(0, 1) + ylim(0, 1) +  geom_rug(data = myt2, inherit.aes = FALSE,  aes(x = Prop_T), size = 0.1) + geom_abline()

init_data_Model_5 <- myt2 %>% group_by(Subset, pop) %>% summarise(Prop_T = mean(morph == "T_m"),  freq_MT = mean(Sp == "M.trossulus"), N = n())

# init_data_Model_5 <- init_data_Model_5[init_data_Model_5$pop %in% c("Limh88", "CBCP"),  ]

Pl_mod5_with_initial_data <- Pl_mod5 + geom_point(data = init_data_Model_5, aes( y = freq_MT), shape = 21 ) + scale_fill_continuous(low = "white", high = "black") + labs(x = "Proportion of mussels with T-morphotype", y = "Proportion of M.trossulus \n") + theme_bw()


Model_5_final_diag <- fortify(Model_5_final)

Model_5_final_diag$pop <- ptop_T_MT$pop 

Model_5_final_diag[order(Model_5_final_diag$.cooksd, decreasing = T), c("pop", ".cooksd")]


qplot(y =ptop_T_MT$pop, x = Model_5_final_diag$.cooksd)


ggplot(Model_5_final_diag, aes(x = .fitted, y = .stdresid)) + geom_point() + geom_smooth()

