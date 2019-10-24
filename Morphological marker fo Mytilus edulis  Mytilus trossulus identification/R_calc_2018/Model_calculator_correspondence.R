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

myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "fresh"] <- "Barents_fresh" 
myt2$Subset[myt2$sea == "barents" & myt2$sal_place == "normal"] <- "Barents_normal" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "normal"] <- "White" 
myt2$Subset[myt2$sea == "white" & myt2$sal_place == "fresh"] <- "White" 



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




#Функция для получения Ptros из эмпирических логистических кривых

logist_empir <- function(b0 = -2.5, b1 = 5.2, P_T_vector){
  exp(b0 + b1*P_T_vector)/(1+exp(b0 + b1*P_T_vector))
}

# logist_empir_predict_W <- logist_empir(P_T_vector = calc1_predict_W$P_T )
# 
# qplot(x = logist_empir_predict_W, y = calc1_predict_W$Ptros) + geom_abline() + xlim(0,1)
# 
# mean((logist_empir_predict_W - calc1_predict_W$Ptros)^2)



#Функция, которая вычисляет предсказания эмпирической модели для данных из калькулятора

# perms <- function(df = myt2[myt2$facet == "W", ], nperm = 1000,...)
# {
#   require(dplyr)
#   perm_pairs <- data.frame(Delta = rep(NA, nperm))
#   for(i in 1:nrow(perm_pairs)){
#     pop_nums <- length(unique(df$pop))
#     select_nums <- sample(1:pop_nums, 2)
#     pop_selected <- unique(df$pop)[select_nums]
#     df_selected <- df[df$pop %in% pop_selected,] 
#     
#     means <- df_selected %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT))
#     perm_pairs$Delta[i] <- abs(means$freq_MT[1] - means$freq_MT[2])
#       
#     W <- donat(df_selected)
#     calc1_predict_W <- calc1(W[1], W[2])
#     logist_empir_predict_W <- logist_empir(P_T_vector = calc1_predict_W$P_T )
#     perm_pairs$Goodness[i] <- 1/(mean((logist_empir_predict_W - calc1_predict_W$Ptros)^2))
#     perm_pairs$pop1[i] <- unique(as.character(df_selected$pop))[1]
#     perm_pairs$pop2[i] <- unique(as.character(df_selected$pop))[2]
#     print(i)
#   }
#   unique(perm_pairs)
# }
# 


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
    perm_pairs$Delta[i] <- abs(means$freq_MT[1] - means$freq_MT[2])
    
    W <- donat(df_selected)
    calc1_predict_W <- calc1(W[1], W[2])
    logist_empir_predict_W <- logist_empir(P_T_vector = calc1_predict_W$P_T )
    perm_pairs$Goodness[i] <- 1/(mean((logist_empir_predict_W - calc1_predict_W$Ptros)^2))
    perm_pairs$pop1[i] <- unique(as.character(df_selected$pop))[1]
    perm_pairs$pop2[i] <- unique(as.character(df_selected$pop))[2]

  }
  perm_pairs
}





resamplings <- perms2(df = myt2[myt2$facet == "W", ], b0 = -2.5, b1 = 5.2)


ggplot(resamplings, aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth()


# Модель для Congr

myt2_reduced <- myt2


Mod_fT_congr <- glmer(congr ~ morph * freq_MT*Subset + (1 | pop), data = myt2_reduced, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

Mod_fT_congr2 <- update(Mod_fT_congr, .~.-morph:freq_MT:Subset )

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

# Границы доверительных интервалов в масштабах вероятностей
newdata$lwr <- logit_back(newdata$fit_eta - 2 * newdata$se_eta)
newdata$upr <- logit_back(newdata$fit_eta + 2 * newdata$se_eta)

# newdata$lwr <- probit_back(newdata$fit_eta - 2 * newdata$se_eta)
# newdata$upr <- probit_back(newdata$fit_eta + 2 * newdata$se_eta)


Pl_fit <- ggplot(newdata, aes(x = freq_MT)) + geom_ribbon(aes(y = fit, ymin = lwr, ymax = upr, group = morph), alpha = 0.2)  + geom_line(aes(y = fit, color = morph), size=1) + facet_wrap( ~ Subset) + geom_rug(data = myt2) + scale_color_manual(values = c("blue", "red")) + scale_fill_manual(values = c("blue", "red")) + theme_bw() + xlim(0,1)  + labs(y = "Probability of correct identification \n PPV and NPV", x = "M.trossulus prevalence \n (MTprev)", color = "Morphotype") + theme(legend.position = "bottom")

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
    
    
    perm_sample$Goodness[i] <- 1/sum((all_prediction$Bayes_predict - all_prediction$Predict)^2, na.rm = T)
    perm_sample$pop[i] <- unique(as.character(df_selected$pop))
    
  }
  perm_sample
}


resamplings2 <- perms3(df = myt2[myt2$facet == "W", ])

ggplot(perms3(df = myt2[myt2$facet == "W", ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth()


ggplot(perms3(df = myt2[myt2$facet == "BF", ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth()

ggplot(perms3(df = myt2[myt2$facet == "BN", ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth()

ggplot(perms3(df = myt2[myt2$facet %in%  c("BF", "W"), ]), aes(x = Delta, y = Goodness)) + geom_point() + geom_smooth()





# Подбираем наилучшую выборку для оценки баесовских кривых, как самую смешанную выборку
W <- donat(df = myt2[myt2$facet == "W", ])
BF <- donat(df = myt2[myt2$facet == "BF", ])
BN <- donat(df = myt2[myt2$facet == "BN", ])


W <- donat(df = myt2[myt2$pop == (myt2 %>% group_by(facet) %>% filter(facet == "W") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% mutate(diff = (freq_MT-0.5)^2) %>% filter(diff == min(diff)) %>% .$pop), ])      

BF <- donat(df = myt2[myt2$pop == (myt2 %>% group_by(facet) %>% filter(facet == "BF") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% mutate(diff = (freq_MT-0.5)^2) %>% filter(diff == min(diff)) %>% .$pop), ])      

BN <- donat(df = myt2[myt2$pop == (myt2 %>% group_by(facet) %>% filter(facet == "BN") %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT)) %>% mutate(diff = (freq_MT-0.5)^2) %>% filter(diff == min(diff)) %>% .$pop), ])      





Bayes_prediction <- rbind(data.frame(Subset = "White", calc2(W[1], W[2])), data.frame(Subset = "Barents_fresh", calc2(BF[1], BF[2])), data.frame(Subset = "Barents_normal", calc2(BN[1], BN[2])))

Pl_fit + geom_line(data = Bayes_prediction, aes(y =  P_MT_T), color = "red", linetype = 2) + geom_line(data = Bayes_prediction, aes(y =  P_ME_E), color = "blue", linetype = 2)


