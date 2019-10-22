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

perms <- function(df = myt2[myt2$facet == "W", ], nperm = 1000,...)
{
  require(dplyr)
  perm_pairs <- data.frame(Delta = rep(NA, nperm))
  for(i in 1:nrow(perm_pairs)){
    pop_nums <- length(unique(df$pop))
    select_nums <- sample(1:pop_nums, 2)
    pop_selected <- unique(df$pop)[select_nums]
    df_selected <- df[df$pop %in% pop_selected,] 
    
    means <- df_selected %>% group_by(pop) %>% summarise(freq_MT = mean(freq_MT))
    perm_pairs$Delta[i] <- abs(means$freq_MT[1] - means$freq_MT[2])
      
    W <- donat(df_selected)
    calc1_predict_W <- calc1(W[1], W[2])
    logist_empir_predict_W <- logist_empir(P_T_vector = calc1_predict_W$P_T )
    perm_pairs$Goodness[i] <- 1/(mean((logist_empir_predict_W - calc1_predict_W$Ptros)^2))
    perm_pairs$pop1[i] <- unique(as.character(df_selected$pop))[1]
    perm_pairs$pop2[i] <- unique(as.character(df_selected$pop))[2]
    print(i)
  }
  unique(perm_pairs)
}



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

