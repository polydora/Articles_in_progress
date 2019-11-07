#### Packages #####



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






#### Data reading and initial preparation #####

myt <- read.table("data_salinity3.csv", header = T, sep = ";")

myt <- myt[myt$dataset != "overseas", ]



myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)

# Оставляем только мидий, у которых есть оценка морфотипа
myt2 <- myt[!is.na(myt$ind), ]


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


# Подразделяем дмнные на три сабсета

myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "fresh"] <- "BL" 
myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "normal"] <- "BH" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "normal"] <- "W" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "fresh"] <- "W" 

myt2$Subset <- factor(myt2$Subset, levels = c("W", "BL", "BH"))


#Оставляем только данные, на основе, которых строится модель
myt3 <- myt2[myt2$dataset == "testing", ]
myt2 <- myt2[myt2$dataset == "training", ]


### Functions #####


# Функция для вычисления P_T_MT и P_T_ME в заданном датасете (БУБЛИК) ####
donat <- function(df){
  P_MT <- sum(df$Sp == "M.trossulus")
  P_T_MT <- sum(df$Sp == "M.trossulus" & df$morph == "T_m")/P_MT
  
  P_ME <- sum(df$Sp == "M.edulis")
  P_T_ME <- sum(df$Sp == "M.edulis" & df$morph == "T_m")/P_ME
  c(P_T_MT, P_T_ME)
}



#Функция для "ленивого" калькулятора №1 который строит зависимость Ptros от P_T  

# На входе параметры бублика

calc1 <- function(P_T_MT, P_T_ME){
  result <- data.frame(P_T = seq(0, 1, 0.01))
  result$Ptros <- (result$P_T - P_T_ME)/(P_T_MT - P_T_ME)
  result <- result[result$P_T <= P_T_MT & result$P_T >= P_T_ME, ]
  result
}



# Функция для вычисления значений Ptros по заданным значениям парамтеров логистического уравнения

logist_empir <- function(b0 = -2.5, b1 = 5.2, P_T_vector){
  exp(b0 + b1*P_T_vector)/(1+exp(b0 + b1*P_T_vector))
}




# Функция для сравннения результатов предесказания Ptros по P_T в соответствии с "ленивым" калькулятором и в соответсвии с эмпирической моделью 


perms2 <- function(df = myt2[myt2$facet == "W", ], ...) {
  require(dplyr)
  df$pop <- as.character(df$pop)
  perm_pairs <- expand.grid(First = unique(df$pop), Second = unique(df$pop))
  perm_pairs <- perm_pairs[perm_pairs$First != perm_pairs$Second,]
  
  perm_pairs$First <- as.character(perm_pairs$First)
  perm_pairs$Second <- as.character(perm_pairs$Second)
  perm_pairs$Delta <- NA
  for(i in 1:nrow(perm_pairs)){
    df_selected <- df[df$pop %in% c(perm_pairs$First[i], perm_pairs$Second[i]),] 
    
    means <- df_selected %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT))
    # perm_pairs$Delta[i] <- abs(means$freq_MT[1] - means$freq_MT[2])
    perm_pairs$Delta[i] <- max(c(means$freq_MT[1],means$freq_MT[2])) *(1 - min(c(means$freq_MT[1],means$freq_MT[2])))
    W <- donat(df_selected)
    calc1_predict_W <- calc1(W[1], W[2])
    logist_empir_predict_W <- logist_empir(P_T_vector = calc1_predict_W$P_T )
    perm_pairs$Goodness[i] <- 1/(mean((logist_empir_predict_W - calc1_predict_W$Ptros)^2))
    perm_pairs$pop1[i] <- unique(as.character(df_selected$pop))[1]
    perm_pairs$pop2[i] <- unique(as.character(df_selected$pop))[2]
    
  }
  perm_pairs
}




# Функция для вычисления баесовских вероятностей по данным из бублика

calc2 <- function(P_T_MT, P_T_ME){
  result <- data.frame(freq_MT = seq(0, 1, 0.01))
  result$P_MT_T <- (P_T_MT * result$freq_MT)/(P_T_MT * result$freq_MT + P_T_ME*(1-result$freq_MT))
  result$P_ME_E <- ((1 - P_T_ME) * (1 - result$freq_MT))/(1 - P_T_ME + result$freq_MT * (P_T_ME - P_T_MT))
  result
}


# Функция для обраного пробит преобразования
probit_back <- function (eta) 
{
  thresh <- -qnorm(.Machine$double.eps)
  eta <- pmin(pmax(eta, -thresh), thresh)
  pnorm(eta)
}



logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация




overdisp_fun <- function(model) {
  rdf <- df.residual(model)  # Число степеней свободы N - p
  if (inherits(model, 'negbin')) rdf <- rdf - 1 ## учитываем k в NegBin GLMM
  rp <- residuals(model,type='pearson') # Пирсоновские остатки
  Pearson.chisq <- sum(rp^2) # Сумма квадратов остатков, подчиняется Хи-квадрат распределению
  prat <- Pearson.chisq/rdf  # Отношение суммы квадратов остатков к числу степеней свободы
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE) # Уровень значимости
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)        # Вывод результатов
}




 ######################################

# Model 1. P_T ~ Ptros*subset (GLM)

# Model 2. P_T ~ Ptros*subset*Sp (GLMM)

# Model 3. Accuracy ~ Ptros*subset (GLM)

# Model 4. Congr ~ Ptros*subset*Morph (GLMM, probit)

# Model 5. Ptros ~ P_T*subset (GLM)



theme_set(theme_bw() + theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "")  )




# Рисунок с первичными данными и моделями 1, 2, 4 



#Модель 1
###################################################

names(myt2)

myt2$Prop_T_adj <- myt2$Prop + 0.00001 

Model_T_MT <- glm(ind ~  freq_MT * Subset, data = myt2, family = binomial(link = "logit"))

Model_T_MT_probit <- glm(ind ~  freq_MT * Subset, data = myt2, family = binomial(link = "probit"))

AIC(Model_T_MT, Model_T_MT_probit)



overdisp_fun(Model_T_MT_probit)

drop1(Model_T_MT_probit, test = "Chi")


Model_T_MT_final <- Model_T_MT_probit 

summary(Model_T_MT_final)

r.squaredGLMM(Model_T_MT_final)




new_data <- myt2 %>% group_by(Subset, pop) %>% summarise(freq_MT = mean(freq_MT) ) %>% group_by(Subset) %>%  do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 10)))


predicted <- predict(Model_T_MT_final, newdata = new_data,  type="response", se.fit = T)

new_data$fit <- predicted$fit

new_data$SE <- predicted$se.fit 




Pl_mod1 <- ggplot(new_data, aes(x = freq_MT, y = fit)) + geom_line(linetype = 2, color = "red", size = 1) + facet_wrap(~Subset) + geom_ribbon(aes(ymin = fit - 1.96*SE, ymax = fit + 1.96*SE), alpha = 0.1) + xlim(0, 1) + ylim(0, 1) +  geom_rug(data = myt2, aes(x = freq_MT, y = 0.05))


# иллюстрация с точками
link_over_M <- myt2 %>% group_by(Subset, pop) %>% summarise(freq_MT = mean(Sp2), freq_Tmorph = mean(ind), N_MT = sum(Sp2 == 1),  N_ME = sum(Sp2 == 0))

Pl_mod1_with_initial_data <- Pl_mod1 + geom_point(data = link_over_M, aes(y = freq_Tmorph, size = (N_MT+N_ME), fill = (freq_MT)), shape = 21) + scale_fill_continuous(high = "black", low = "white" ) + geom_abline() + labs(x =  "Proportion of M. trossulus", y = "Proportion of T-morphotype \n") 

Pl_mod1_with_initial_data



#Модель 2
######################################

Model_2_full <- glmer(ind ~  freq_MT * Subset * Sp + (1|pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

Model_2 <- glmer(ind ~  freq_MT * Subset * Sp + (1|pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


Model_2_reduced <- glmer(ind ~  freq_MT + Subset + Sp + Subset : Sp + (1|pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


Model_2_probit <- glmer(ind ~  freq_MT * Subset * Sp + (1|Subset:pop), data = myt2, family = binomial(link = "probit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


summary(Model_2_full)


AIC(Model_2_probit, Model_2_full)

drop1(Model_2_probit, test = "Chi")

new_data2 <- myt2 %>% group_by(Subset,  Sp) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))



new_data2$eta <- predict(Model_2_probit, newdata = new_data2,  re.form = NA) 

X <- model.matrix(~freq_MT * Subset * Sp , data = new_data2)



new_data2$SE_eta <- sqrt(diag(X %*% vcov(Model_2_probit) %*% t(X)))

new_data2$fit <- probit_back(new_data2$eta)

new_data2$lwr <- probit_back(new_data2$eta -  1.96 *new_data2$SE_eta)

new_data2$upr <- probit_back(new_data2$eta +  1.96 *new_data2$SE_eta)

Pl_mod2 <-  ggplot(new_data2, aes(x = freq_MT, y = fit, group = Sp)) + geom_line(linetype = 2,  size = 1, aes(color = Sp)) + geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.1) + facet_wrap(~Subset)  + xlim(0, 1) + ylim(0, 1) + scale_color_manual(values=c("blue", "red")) + guides(color = "none") +  geom_rug(data = myt2, aes(x = freq_MT, y = 0.05))



pops_over_M <- myt2 %>% group_by(Subset, pop) %>% summarise(freq_MT = mean(freq_MT), N_MT = sum(Sp2 == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_ME = sum(Sp2 == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1))
 
pops_over_M$P_T_MT <- with(pops_over_M, N_T_MT / N_MT)
pops_over_M$P_E_MT <- with(pops_over_M, N_E_MT / N_MT)
pops_over_M$P_E_ME <- with(pops_over_M, N_E_ME / N_ME)
pops_over_M$P_T_ME <- with(pops_over_M, N_T_ME / N_ME)


Pl_mod2_with_initial_data <- Pl_mod2 +   geom_segment(data = pops_over_M, aes(x = freq_MT, y = (1-P_E_ME), xend = freq_MT, yend = P_T_MT, group = 1), color = "darkgray")+ 
  geom_hline(aes(yintercept=0.5), color="black") + 
  geom_point(data = pops_over_M, aes(y = (1-P_E_ME), size= N_ME, group =1), fill = "white", shape = 21)+
  geom_point(data = pops_over_M, aes(y = P_T_MT, size=N_MT, group =1), fill = "black", shape = 21)  + xlim(0,1)+ 
  labs(y =  "Proportion of T-morphotype \n among  M. trossulus  and  M. edulis", x = "Proportion of M. trossulus", fill = "") + 
  ylim(0,1) + xlim(0,1) + 
  theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12), axis.text= element_text(size = 11), legend.position = "") 


Pl_mod2_with_initial_data






#Модель 4
#####################################


Mod_fT_congr <- glmer(congr ~ morph * freq_MT*Subset + (1 | pop), data = myt2, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

Mod_fT_congr_probit <- glmer(congr ~ morph * freq_MT*Subset + (1 | pop), data = myt2, family = binomial(link = "probit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AIC(Mod_fT_congr, Mod_fT_congr_probit)

drop1(Mod_fT_congr_probit)


Mod_fT_congr2 <- update(Mod_fT_congr_probit, .~.-morph:freq_MT:Subset )

drop1(Mod_fT_congr2)

Mod_fT_congr_fin <- Mod_fT_congr2


new_data3 <- myt2 %>% group_by(Subset, morph) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале вероятностей
new_data3$fit <- predict(Mod_fT_congr_fin, newdata = new_data3, type = "response", re.form = NA) 

# Предсказанные значеня в шкале логитов
new_data3$fit_eta <- predict(Mod_fT_congr_fin, newdata = new_data3, re.form = NA) 

# Вычисление доверительного инеравала

X <- model.matrix(  ~ morph + freq_MT + Subset + morph:freq_MT + 
                      morph:Subset + freq_MT:Subset, data = new_data3) #Модельная матрица для визуализации


# Ошибки в шкале логитов
new_data3$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr_fin) %*% t(X)))

new_data3$lwr <- probit_back(new_data3$fit_eta - 1.96 * new_data3$se_eta)

new_data3$upr <- probit_back(new_data3$fit_eta + 1.96 * new_data3$se_eta)



Pl_mod4 <- ggplot(new_data3, aes(x = freq_MT)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.1)  + geom_line(aes(y = fit, color = morph), size=1) + facet_wrap( ~ Subset) + geom_rug(data = myt2) + scale_color_manual(values = c("blue", "red")) + scale_fill_manual(values = c("blue", "red"))  + xlim(0,1)  + labs(y = "Probability of correct identification \n PPV and NPV", x = "M.trossulus prevalence \n (MTprev)") 





pr_value_M <- myt2 %>% group_by(Subset, pop) %>% summarise(freq_MT = mean(freq_MT), N_T = sum(ind == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_E = sum(ind == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1))
pr_value_M$PMT_T <- with(pr_value_M, N_T_MT / N_T)
pr_value_M$PMT_E <- with(pr_value_M, N_E_MT / N_T)
pr_value_M$PME_E <- with(pr_value_M, N_E_ME / N_E)
pr_value_M$PME_T <- with(pr_value_M, N_T_ME / N_E)


Pl_mod4_with_initial_data <- Pl_mod4 + geom_segment(data = pr_value_M, aes(x = freq_MT, y = PME_E, xend = freq_MT, yend = PMT_T), color="darkgrey") + 
  geom_hline(data = pr_value_M, aes(yintercept=0.5), color="black") + 
  geom_point(data = pr_value_M, aes(y = PME_E, size= N_E), fill = "white", shape = 21) + 
  geom_point(data = pr_value_M, aes(y = PMT_T, size=N_T), fill = "black", shape = 21) + 
  labs(y =  "Proportions of correct species \n identification by morphotypes", x = "Proportion of M. trossulus", fill = "")+ 
  ylim(0,1) + 
  xlim(0,1) 

Pl_mod4_with_initial_data







#объединяем графики
grid.arrange(Pl_mod1_with_initial_data, Pl_mod2_with_initial_data, Pl_mod4_with_initial_data, ncol = 1)







