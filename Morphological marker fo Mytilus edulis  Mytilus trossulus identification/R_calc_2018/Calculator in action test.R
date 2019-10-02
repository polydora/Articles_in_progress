library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(car)
library(doBy)
library(pROC)
library(betareg)
library(knitr)




myt <- read.table("data_salinity3.csv", header = T, sep = ";")

myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)

# Оставляем только мидий, у которых есть оценка морфотипа
myt2 <- myt[!is.na(myt$ind), ]


# Вводим обозначения для морфотипов
myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)

#Оставляем только данные, на основе, которых строится модель
myt3 <- myt2[myt2$dataset == "testing", ]
myt2 <- myt2[myt2$dataset == "training", ]


myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )


# Частота M.trossulus в популяции, вычисленная как срденее значение structure
freq_MT <- summaryBy( str ~ pop, data = myt2)
names(freq_MT) <- c("pop", "freq_MT")

myt2 <- merge(myt2, freq_MT)

myt2$Sp2 <- ifelse(myt2$Sp == "M.trossulus", 1, 0)

myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") #Перемменная для кодирования четырех выделов

myt3$Location <- paste(myt3$sea,"_", myt3$sal_place, sep = "") #Перемменная для кодирования четырех выделов


# Регрессионная модель, описывающая связь между долей Т-морфотипа вероятностью встретить M.trossulus в популяции


prop_T_MT <- summaryBy(Sp2 + ind  ~ pop + sea + sal_place, data = myt2, keep.names = T)
names(prop_T_MT)[4:5] <- c("Prop_MT", "Prop_T")

prop_T_MT$Prop_MT_adj <- ifelse(prop_T_MT$Prop_MT == 0, 0.000001, prop_T_MT$Prop_MT)


prop_T <- summaryBy(ind  ~ pop, data = myt2, keep.names = T)
names(prop_T) <- c("pop", "Prop_T")

myt4 <- merge(myt2, prop_T, by = "pop")

Model_T_MT <- betareg(Prop_MT_adj ~ Prop_T + sea*sal_place, data = prop_T_MT)


new_data <- prop_T_MT %>% group_by(sea, sal_place) %>% do(data.frame(Prop_T = seq(min(.$Prop_T), max(.$Prop_T), length.out = 100)))


predicted <- predict(Model_T_MT, newdata = new_data,  type="response")

new_data$fit <- predicted



probs_calc_3 <- function(variable = "Location", place = "white_normal", P_MT = 0.5) {
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



freq_calc2 <- function(df = myt2, pop=NULL, variable = "Location", place = "barents_fresh"){
  require(dplyr)
  n <- which(names(df) == variable)
  d <- df[df[,n] == place, ]
  
  if(!is.null(pop)) d <- df[df[,n] == place & df$pop == pop, ]
  if(is.null(place)) d <- df[df$pop == pop, ]
  
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




# Характерстики вероятностей для каждой популяции

pop_freq <- data.frame((myt2 %>% group_by(Location, pop) %>% summarize(sea = unique(sea), sal_place =unique(sal_place))),  P_MT_T = NA, P_ME_E = NA, P_MT_E = NA, P_ME_T = NA,     P_T_MT = NA,   P_E_MT = NA,  P_T_ME = NA,     P_E_ME = NA, P_MT = NA,  P_ME = NA,  P_MT_of_T = NA,  P_MT_of_E = NA,  P_ME_of_T=NA,  P_ME_of_E = NA, P_E =NA, P_T = NA)


i <- 1
for(pop in unique(pop_freq$pop)){
  pop_freq[i,5:20] <- round(freq_calc2(df = myt2, place = NULL, pop = pop), 3) 
  i<-i+1
}



pop_freq #Датафрейм с частотами по каждой отдельной популяции



pop_freq_test <- data.frame((myt3 %>% group_by(Location, pop) %>% summarize(sea = unique(sea), sal_place =unique(sal_place))),  P_MT_T = NA, P_ME_E = NA, P_MT_E = NA, P_ME_T = NA,     P_T_MT = NA,   P_E_MT = NA,  P_T_ME = NA,     P_E_ME = NA, P_MT = NA,  P_ME = NA,  P_MT_of_T = NA,  P_MT_of_E = NA,  P_ME_of_T=NA,  P_ME_of_E = NA, P_E =NA, P_T = NA)


i <- 1
for(pop in unique(pop_freq_test$pop)){
  pop_freq_test[i,5:20] <- round(freq_calc2(df = myt3, place = NULL, pop = pop), 3) 
  i<-i+1
}



pop_freq_test #Датафрейм с частотами по каждой отдельной популяции из тестового датасета




pop_freq_modelling <- data.frame((myt2 %>% group_by(Location, pop) %>% summarize(sea = unique(sea), sal_place =unique(sal_place))),  P_MT_T = NA, P_ME_E = NA, P_MT_E = NA, P_ME_T = NA,     P_T_MT = NA,   P_E_MT = NA,  P_T_ME = NA,     P_E_ME = NA, P_MT = NA,  P_ME = NA,  P_MT_of_T = NA,  P_MT_of_E = NA,  P_ME_of_T=NA,  P_ME_of_E = NA, P_E =NA, P_T = NA)


i <- 1
for(pop in unique(pop_freq_modelling$pop)){
  pop_freq_modelling[i,5:20] <- round(freq_calc2(df = myt2, place = NULL, pop = pop), 3) 
  i<-i+1
}








All_freq <- data.frame(barents_normal = freq_calc2(place = "barents_normal"),
                       barents_fresh = freq_calc2(place = "barents_fresh"),
                       white_normal = freq_calc2(place = "white_normal"),
                       white_fresh = freq_calc2(place = "white_fresh"))



All_freq <- as.data.frame(t(All_freq))
All_freq$Location <- row.names(All_freq)








# Предскаания по Стрелкову
calc_1 <- function(P_T, P_T_ME, P_T_MT) (P_T - P_T_ME)/(P_T_MT - P_T_ME)  



# Предсказания по Хайтову
calc_2 <- function(P_T,P_MT_E, P_ME_T) P_T + (1-P_T)*P_MT_E - P_T*P_ME_T

calc_3 <- function(P_T,P_MT_of_E, P_ME_of_T) P_T + (P_MT_of_E - P_ME_of_T)

calc_4 <- function(P_T,P_MT_T, P_MT_E) P_T*P_MT_T + P_MT_E*(1 - P_T)


calculators <- expand.grid(P_T = seq(0, 1, 0.01), sea = c("barents", "white"), sal_place = c("fresh", "normal"))

calculators$Location = with(calculators, paste(sea,"_", sal_place, sep = ""))



for(i in 1:nrow(calculators)){
  calculators$P_MT_predicted_PPS [i] <-  calc_1(calculators$P_T[i], All_freq$P_T_ME[All_freq$Location == calculators$Location[i]], All_freq$P_T_MT[All_freq$Location == calculators$Location[i]])
  calculators$P_MT_predicted_VMK [i] <-  calc_2(calculators$P_T[i], All_freq$P_MT_E[All_freq$Location == calculators$Location[i]], All_freq$P_ME_T[All_freq$Location == calculators$Location[i]])
  calculators$P_MT_predicted_VMK2 [i] <-  calc_3(calculators$P_T[i], All_freq$P_MT_of_E[All_freq$Location == calculators$Location[i]], All_freq$P_ME_of_T[All_freq$Location == calculators$Location[i]])
  calculators$P_MT_predicted_VMK3 [i] <-  calc_4(calculators$P_T[i], All_freq$P_MT_T[All_freq$Location == calculators$Location[i]], All_freq$P_MT_E[All_freq$Location == calculators$Location[i]])
}

  

# calculators$P_MT_predicted_mean <- NA
# calculators$P_MT_predicted_mean  <- (calculators$P_MT_predicted_PPS + calculators$P_MT_predicted_VMK)/2  


ggplot(calculators) + 
  geom_line(aes(x = P_T, y = P_MT_predicted_PPS), color = "red") +  
  geom_line(aes(x = P_T, y = P_MT_predicted_VMK),  color = "blue")+  
  geom_line(aes(x = P_T, y = P_MT_predicted_VMK2), linetype = 3, color = "blue")+
  geom_point(aes(x = P_T, y = P_MT_predicted_VMK3),  color = "yellow", size =0.01)+
  facet_grid( sea ~ sal_place) + geom_point(data = pop_freq, aes(x = P_T, y = P_MT)) + geom_hline(yintercept = c(0,1), linetype = 2) + ylim(0,1) + labs(x = "Proportion of T-morphotype", y = "Proportion of M.trossulus") + geom_point(data = pop_freq_test, aes(x = P_T, y = P_MT), shape = 21, fill = "yellow", size = 3)




