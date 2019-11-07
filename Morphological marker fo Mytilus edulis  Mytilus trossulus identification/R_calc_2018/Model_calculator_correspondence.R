# В этом коде мы пытаемся поять какой из способов формироания калибровочной выборки наиболее приемлем для построения "ленивого" кальткулятора


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



# Частота M.trossulus в популяции, вычисленная как срденее значение structure

freq_MT <- myt2 %>% group_by(pop) %>% summarise(freq_MT = mean(Sp2))

myt2 <- merge(myt2, freq_MT)

# Подразделяем дмнные на три сабсета


myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "fresh"] <- "BL" 
myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "normal"] <- "BH" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "normal"] <- "W" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "fresh"] <- "W" 

myt2$Subset <- factor(myt2$Subset, levels = c("W", "BL", "BH"))




#Оставляем только данные, на основе, которых строится модель
myt3 <- myt2[myt2$dataset == "testing", ]
myt2 <- myt2[myt2$dataset == "training", ]



# Функция для вычисления P_T_MT и P_T_ME в заданном датасете
donat <- function(df){
  P_MT <- sum(df$Sp == "M.trossulus")
  P_T_MT <- sum(df$Sp == "M.trossulus" & df$morph == "T_m")/P_MT
  
  P_ME <- sum(df$Sp == "M.edulis")
  P_T_ME <- sum(df$Sp == "M.edulis" & df$morph == "T_m")/P_ME
  c(P_T_MT, P_T_ME)
}


#Функция для калькулятора №1

calc1 <- function(P_T_MT, P_T_ME){
  result <- data.frame(P_T = seq(0, 1, 0.01))
  result$Ptros <- (result$P_T - P_T_ME)/(P_T_MT - P_T_ME)
  result <- result[result$P_T <= P_T_MT & result$P_T >= P_T_ME, ]
  result
}

calc1(0.5, 0.1)


#Функция для получения Ptros из эмпирических логистических кривых

logist_empir <- function(b0 = -2.5, b1 = 5.2, P_T_vector){
  exp(b0 + b1*P_T_vector)/(1+exp(b0 + b1*P_T_vector))
}





perms2 <- function(df = myt2[myt2$Subset == "W", ], ...) {
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
    
    perm_pairs$Delta[i] <- max(c(means$freq_MT[1],means$freq_MT[2])) *(1 - min(c(means$freq_MT[1],means$freq_MT[2])))
    W <- donat(df_selected)
    
    calc1_predict_W <- calc1(W[1], W[2])
    
    names(calc1_predict_W) <- c("Prop_T", "Ptros_predicted" )
    
    Model_prediction <- expand.grid(Subset = unique(df_selected$Subset), Prop_T = seq(0, 1, 0.01))
    
    Model_prediction$Predict <- predict(Model_5_final, newdata = Model_prediction, type = "response")
    
    all_prediction <- merge(calc1_predict_W, Model_prediction, by = c("Prop_T"))
    
    perm_pairs$Goodness[i] <- 1/(mean((all_prediction$Predict - all_prediction$Ptros_predicted)^2))
    
  }
  perm_pairs
}






ggplot(perms2(df = myt2[myt2$Subset %in% c("BL"), ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth(se = F, span = 2 )




# Модель для Congr

myt2_reduced <- myt2


Mod_fT_congr <- glmer(congr ~ morph * freq_MT*Subset + (1 | pop), data = myt2_reduced, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

Mod_fT_congr_probit <- glmer(congr ~ morph * freq_MT*Subset + (1 | pop), data = myt2_reduced, family = binomial(link = "probit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AIC(Mod_fT_congr, Mod_fT_congr_probit)




Mod_fT_congr2 <- update(Mod_fT_congr_probit, .~.-morph:freq_MT:Subset )

drop1(Mod_fT_congr2)

Mod_fT_congr_fin <- Mod_fT_congr2


newdata <- myt2_reduced %>% group_by(Subset, morph) %>% do(data.frame(freq_MT = seq(min(.$freq_MT), max(.$freq_MT), length.out = 100)))

# Предсказанные значеня в шкале вероятностей
newdata$fit <- predict(Mod_fT_congr_fin, newdata = newdata, type = "response", re.form = NA) 

# Предсказанные значеня в шкале логитов
newdata$fit_eta <- predict(Mod_fT_congr_fin, newdata = newdata, re.form = NA) 

# Вычисление доверительного инеравала

# formula((Mod_fT_congr_fin)) 

X <- model.matrix(  ~ morph + freq_MT + Subset + morph:freq_MT + 
                      morph:Subset + freq_MT:Subset, data = newdata) #Модельная матрица для визуализации


# Ошибки в шкале логитов
newdata$se_eta <- sqrt(diag(X %*% vcov(Mod_fT_congr_fin) %*% t(X)))



logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация


# binomial(link="probit")$linkinv

probit_back <- function (eta) 
{
  thresh <- -qnorm(.Machine$double.eps)
  eta <- pmin(pmax(eta, -thresh), thresh)
  pnorm(eta)
}




# Границы доверительных интервалов в масштабах вероятностей
# newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)

newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)



# newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)


Pl_fit <- ggplot(newdata, aes(x = freq_MT)) + geom_ribbon(aes(ymin = lwr, ymax = upr, group = morph), alpha = 0.2)  + geom_line(aes(y = fit, color = morph), size=1) + facet_wrap( ~ Subset) + geom_rug(data = myt2) + scale_color_manual(values = c("blue", "red")) + scale_fill_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1)  + labs(y = "Probability of correct identification \n PPV and NPV", x = "M.trossulus prevalence \n (MTprev)", color = "Morphotype") + theme(legend.position = "bottom")

Pl_fit





# Функция для вычисления баесовских вероятностей по данным из бублика

calc2 <- function(P_T_MT, P_T_ME){
  result <- data.frame(freq_MT = seq(0, 1, 0.01))
  result$P_MT_T <- (P_T_MT * result$freq_MT)/(P_T_MT * result$freq_MT + P_T_ME*(1-result$freq_MT))
  result$P_ME_E <- ((1 - P_T_ME) * (1 - result$freq_MT))/(1 - P_T_ME + result$freq_MT * (P_T_ME - P_T_MT))
  result
}






perms3 <- function(df = myt2[myt2$facet == "W", ], ...) {
  require(dplyr)
  require(reshape2)
  df$pop <- as.character(df$pop)
  perm_sample <- data.frame(pop = unique(df$pop))
  perm_sample$Delta <- NA
  
  for(i in 1:nrow(perm_sample)){
    
    df_selected <- df[df$pop %in% perm_sample$pop[i], ] 
    perm_sample$Delta[i] <- mean(df_selected$freq_MT)
    
    W <- donat(df_selected)
    
    calc2_predict_W <- calc2(W[1], W[2])
    names(calc2_predict_W) <- c("freq_MT", "T_m",  "E_m")
    
    calc2_predict_W <- melt(calc2_predict_W, id.vars = "freq_MT" )
    names(calc2_predict_W) <- c("freq_MT", "morph", "Bayes_predict") 
    
    Model_prediction <- expand.grid(Subset = unique(df_selected$Subset),  morph = levels(df_selected$morph), freq_MT = seq(0, 1, 0.01))
    
    Model_prediction$Predict <- predict(Mod_fT_congr_fin, newdata = Model_prediction, type = "response",  re.form = NA )
    
    all_prediction <- merge(calc2_predict_W, Model_prediction, by = c("freq_MT", "morph"))
    
    
    perm_sample$Goodness[i] <- 1/mean((all_prediction$Bayes_predict - all_prediction$Predict)^2, na.rm = T)
    perm_sample$pop[i] <- unique(as.character(df_selected$pop))
    
  }
  perm_sample
}




perms4 <- function(df = myt2[myt2$facet == "W", ], ...) {
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
    
    calc2_predict_W <- calc2(W[1], W[2])
    names(calc2_predict_W) <- c("freq_MT", "T_m",  "E_m")
    
    calc2_predict_W <- melt(calc2_predict_W, id.vars = "freq_MT" )
    names(calc2_predict_W) <- c("freq_MT", "morph", "Bayes_predict") 
    
    Model_prediction <- expand.grid(Subset = unique(df_selected$Subset),  morph = levels(df_selected$morph), freq_MT = seq(0, 1, 0.01))
    
    Model_prediction$Predict <- predict(Model_4_final, newdata = Model_prediction, type = "response",  re.form = NA )
    
    all_prediction <- merge(calc2_predict_W, Model_prediction, by = c("freq_MT", "morph"))
    
    
    perm_pairs$Goodness[i] <- 1/mean((all_prediction$Bayes_predict - all_prediction$Predict)^2, na.rm = T)
    perm_pairs$pop[i] <- unique(as.character(df_selected$pop))
    
  }
  perm_pairs
}








resamplings2 <- perms4(df = myt2[myt2$facet == "W", ])

ggplot(perms4(df = myt2), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth(se = F) + xlim(0,1) 


ggplot(perms4(df = myt2[myt2$Subset == "BL", ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth(se = F)+ xlim(0,1)

ggplot(perms4(df = myt2[myt2$Subset == "BH", ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth(se = F)+ xlim(0,1)

ggplot(perms4(df = myt2[myt2$Subset %in%  c("BL", "W"), ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth(se = F) + xlim(0,1)





# Подбираем наилучшую выборку для оценки баесовских кривых, как самую смешанную выборку
W <- donat(df = myt2[myt2$Subset == "W", ])
BF <- donat(df = myt2[myt2$Subset == "BF", ])
BN <- donat(df = myt2[myt2$Subset == "BN", ])


# W <- donat(df = myt2[myt2$pop %in% (myt2 %>% group_by(facet) %>% filter(facet == "W") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% mutate(diff = (freq_MT - 0.5)) %>% filter(abs(diff) < (min(abs(diff)) + sd(diff)) ) %>% .$pop), ])      



W <- donat(df = myt2[myt2$pop %in% (myt2 %>% group_by(Subset) %>% filter(facet == "W") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% filter(freq_MT < 0.7 & freq_MT > 0.3 ) %>% .$pop), ])      



unique(df$pop)



# BF <- donat(df = myt2[myt2$pop %in% (myt2 %>% group_by(facet) %>% filter(facet == "BF") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% mutate(diff = (freq_MT - 0.5)) %>% filter(abs(diff) < (min(abs(diff)) + sd(diff)) ) %>% .$pop), ])      


BL <- donat(df = myt2[myt2$pop %in% (myt2 %>% group_by(Subset) %>% filter(facet == "BF") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>%filter(freq_MT < 0.7 & freq_MT > 0.3 ) %>% .$pop), ])



# BN <- donat(df = myt2[myt2$pop %in% (myt2 %>% group_by(facet) %>% filter(facet == "BN") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% mutate(diff = (freq_MT - 0.5)) %>% filter(abs(diff) < (min(abs(diff)) + sd(diff)) ) %>% .$pop), ])      

BH <- donat(df = myt2[myt2$pop %in% (myt2 %>% group_by(Subset) %>% filter(facet == "BN") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>%filter(freq_MT < 0.7 & freq_MT > 0.3 ) %>% .$pop), ])






Bayes_prediction <- rbind(data.frame(Subset = "W", calc2(W[1], W[2])), data.frame(Subset = "BL", calc2(BL[1], BL[2])), data.frame(Subset = "BH", calc2(BH[1], BH[2])))

Pl_fit + geom_line(data = Bayes_prediction, aes(y =  P_MT_T), color = "red", linetype = 2) + geom_line(data = Bayes_prediction, aes(y =  P_ME_E), color = "blue", linetype = 2)


