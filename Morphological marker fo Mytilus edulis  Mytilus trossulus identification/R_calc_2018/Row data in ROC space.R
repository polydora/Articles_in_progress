library(ggplot2)
library(dplyr)



myt <- read.table("data_salinity3.csv", header = T, sep = ",")

unique(myt$pop)

# myt <- myt[!(myt$pop %in% c("kuvsh", "banka")), ] #Удаляем kuvsh по просьбе ППС и banka по аналогии




myt$Sp [myt$str > 0.5] <- "M.trossulus" #Лучше обозначать так!
myt$Sp [myt$str <= 0.5] <- "M.edulis"
myt$Sp <- factor(myt$Sp)


myt$sal_place <- factor(myt$sal_place)

str(myt$sal_place)



# Оставляем только мидий, у которых есть оценка морфотипа
myt2 <- myt[!is.na(myt$ind), ]


# Вводим обозначения для морфотипов
myt2$morph <- ifelse(myt2$ind == 1, "T_m", "E_m")
myt2$morph <- factor(myt2$morph)

#Оставляем только данные, на основе, которых строится модель
myt3 <- myt2[myt2$dataset == "testing", ]
myt2 <- myt2[myt2$dataset == "training", ]


myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )

names(myt2)


# Частота M.trossulus в популяции, вычисленная как срденее значение structure
freq_MT <- myt %>% group_by(sal_place, pop) %>% summarise(freq_MT = mean(str))


  
myt2 <- merge(myt2, freq_MT)

myt2$Sp2 <- ifelse(myt2$Sp == "M.trossulus", 1, 0)

myt2$Location <- paste(myt2$sea,"_", myt2$sal_place, sep = "") #Перемменная для кодирования четырех выделов

myt3$Location <- paste(myt3$sea,"_", myt3$sal_place, sep = "") #Перемменная для кодирования четырех выделов


# переменная congr - это событие правильного определения 1 если T-морфотип совпадает с MT и E-морфотип совпадает с ME, 0 - если не свопадает

myt2$congr <- ifelse((myt2$ind == 1 & myt2$Sp == "M.trossulus") | (myt2$ind == 0 & myt2$Sp == "M.edulis"), 1, 0   )


## Убираем данные testing

myt2 <- myt2[!(myt2$dataset %in% c("testing")), ]

########################

# Рисуок с первичными данными в ROC-пространстве

names(myt2)
pops <- myt2 %>% group_by(Location, pop) %>% summarise(freq_MT = mean(str), N_MT = sum(Sp2 == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_ME = sum(Sp2 == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1))


pops$P_T_MT <- with(pops, N_T_MT / N_MT)

pops$P_E_MT <- with(pops, N_E_MT / N_MT)

pops$P_E_ME <- with(pops, N_E_ME / N_ME)

pops$P_T_ME <- with(pops, N_T_ME / N_ME)

pops$AUC <- with(pops, (P_T_MT+P_E_ME)/2)


ggplot(pops, aes(x = (1-P_E_ME), y = P_T_MT)) + geom_point(aes(size = sqrt(N_MT+N_ME), fill = (freq_MT)), shape = 21)  + geom_abline() + facet_wrap(~Location) + scale_fill_continuous(high = "black", low = "white" ) + geom_point(data = pops[pops$N_MT == 0, ], aes(y=-0.1, x = (1-P_E_ME) ))


ggplot(pops [pops$pop != "banka", ], aes(x = freq_MT, y = AUC)) + facet_wrap(~Location) + geom_text(aes(label = pop)) + geom_smooth(method= "lm")



