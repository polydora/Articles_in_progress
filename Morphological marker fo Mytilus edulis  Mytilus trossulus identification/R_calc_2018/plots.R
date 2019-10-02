# грузим кучу пакетов (они тут далеко не все нужны, просто кочуют из скрипта в скрипт)
library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)
library(Hmisc)
library(doBy)
library(car)
library(pROC)
library(emmeans)
library(gridExtra)
library(ggpubr)

# читаем данные, добавлен заморский датасет
myt <- read.table("data_salinity3.csv", header = T, sep = ";")

myt$Sp [myt$str > 0.5] <- "M.trossulus" 
myt$Sp [myt$str <= 0.5] <- "M.edulis"

myt$Sp2 [myt$str > 0.5] <- 1 
myt$Sp2 [myt$str <= 0.5] <- 0

myt$Sp <- factor(myt$Sp)

myt$morph <- ifelse(myt$ind == 1, "T_m", "E_m")
myt$morph <- factor(myt$morph)

colSums(is.na(myt))

myt <- myt[!is.na(myt$ind), ]

str(myt)

# добавляем переменные
myt$congr <- ifelse((myt$ind == 1 & myt$Sp == "M.trossulus") | (myt$ind == 0 & myt$Sp == "M.edulis"), 1, 0 )

# средняя стракча в выборке
meanstr_pop <-summaryBy(str ~ pop, data = myt) 
names(meanstr_pop) <- c("pop", "Freq_MT")
myt <- merge(myt, meanstr_pop, by = "pop")

# доля язычковых в выборке
ind_pop <-summaryBy(ind ~ pop, data = myt) 
names(ind_pop) <- c("pop", "freq_Tmorph")
myt <- merge(myt, ind_pop, by = "pop")

str(myt)

# рисуем заморских мидий!
# для отрисовки в файл добавила предикторы с говорящим названием)) facet - чтобы разбить на 4 столбика, color - чтобы покрасить границы точек
levels(myt$facet)

# поменяем местами уровни фактора
myt$facet <- factor(myt$facet, levels = c("W", "BF", "BN", "overseas"))

pops_over <- myt %>% group_by(facet, color, pop) %>% summarise(freq_MT = mean(str), N_MT = sum(Sp2 == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_ME = sum(Sp2 == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1)) 

pops_over$P_T_MT <- with(pops_over, N_T_MT / N_MT)

pops_over$P_E_MT <- with(pops_over, N_E_MT / N_MT)

pops_over$P_E_ME <- with(pops_over, N_E_ME / N_ME)

pops_over$P_T_ME <- with(pops_over, N_T_ME / N_ME)

pops_over$accuracy <- with(pops_over, (P_T_MT+P_E_ME)/2)
levels(myt$color)

pops_over_plot <- ggplot(pops_over, aes(x = (1-P_E_ME), y = P_T_MT))+ geom_abline()  + geom_point(aes(size = (N_MT+N_ME), fill = (freq_MT), color = color, stroke = 2), shape = 21) + facet_wrap(~facet, nrow=1) + scale_color_manual(values= c("green", "yellow", "brown", "blue", "red", "black"))+ scale_fill_continuous(high = "black", low = "white" )+ theme_bw()  + theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text= element_text(size = 11), legend.text= element_text(size = 15)) + labs(y =  "Sensitivity", x = "1 - Specificity", fill = "")+ ylim(0,1) 
# + geom_text(aes(label = pop, color = sea)) 

# связь частоты морфотипа и фенотипа
link_over <- myt %>% group_by(facet, color, pop) %>% summarise(freq_MT = mean(str), freq_Tmorph = mean(ind), N_MT = sum(Sp2 == 1),  N_ME = sum(Sp2 == 0))

# рисуем 
link_over_plot <- ggplot(link_over, aes(y = freq_Tmorph, x = freq_MT))+ geom_abline() + geom_point(aes(size = (N_MT+N_ME), fill = (freq_MT), color=color), shape = 21, stroke = 2) + facet_wrap(~facet, nrow=1) + scale_color_manual(values= c("green", "yellow", "brown", "blue", "red", "black")) + scale_fill_continuous(high = "black", low = "white" )+ theme_bw() + ylim(0,1) + theme( axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text= element_text(size = 11), legend.text= element_text(size = 15)) + labs(y =  "Frequency of T", x = "Frequency of MT", fill = "") + xlim(0,1)
# + geom_text(aes(label = pop, color = sea))
# legend.position = "",

# рисуем ауки
acc_over_plot <- ggplot(pops_over, aes(x = freq_MT, y = accuracy)) + geom_hline(aes(yintercept=0.5), color="black") + geom_point(aes(size = (N_MT+N_ME), fill = (freq_MT), color=color), shape = 21, stroke = 2) + facet_wrap(~facet, nrow=1) + scale_color_manual(values= c("green", "yellow", "brown", "blue", "red", "black")) + scale_fill_continuous(high = "black", low = "white" )  + theme_bw() + xlim(0,1) + ylim(0,1) + theme( axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text= element_text(size = 11), legend.text= element_text(size = 15)) + labs(y =  "AUC", x = "Frequency of MT", fill = "") 

# рисуем ppv и npv 
pr_value <- myt %>% group_by(facet, color, pop) %>% summarise(Freq_MT = mean(str), N_T = sum(ind == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_E = sum(ind == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1))


pr_value$PMT_T <- with(pr_value, N_T_MT / N_T)

pr_value$PMT_E <- with(pr_value, N_E_MT / N_T)

pr_value$PME_E <- with(pr_value, N_E_ME / N_E)

pr_value$PME_T <- with(pr_value, N_T_ME / N_E)






ggplot(pr_value, aes(x = N_T_MT, y = N_E_ME)) + geom_point()

ggplot(pr_value, aes(x = Freq_MT, color = color)) + 
  geom_segment(aes(x = Freq_MT, y = PME_E, xend = Freq_MT, yend = PMT_T), color="darkgrey") +
  geom_point(aes(y = PME_E), fill = "white", shape = 21) +
  geom_point(aes(y = PMT_T), fill = "black", shape = 21) +
  geom_hline(aes(yintercept=0.5), color="black") + 
  facet_wrap(~facet, nrow=1) 


ggplot(pr_value, aes(x = Freq_MT, y = (PME_E - PMT_T))) + 
  geom_point() +
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0.5) + 
  geom_smooth(method = "lm")+
  facet_wrap(~facet, nrow=1) 



# делаю второй датасет, тк ошибки выдавал..
pr_value2 <- myt %>% group_by(facet, color, pop) %>% summarise(Freq_MT = mean(str), N_T = sum(ind == 1),  N_T_MT = sum(Sp2 == 1 & ind == 1), N_E_MT = sum(Sp2 == 1 & ind == 0), N_E = sum(ind == 0), N_E_ME = sum(Sp2 == 0 & ind == 0), N_T_ME = sum(Sp2 == 0 & ind == 1))

pr_value2$PMT_T <- with(pr_value2, N_T_MT / N_T)

pr_value <- ggplot(data = pr_value, aes(x = Freq_MT, y = PME_E, group = color)) + geom_hline(aes(yintercept=0.5), color="black") + facet_wrap(~facet, nrow=1) + geom_segment(aes(xend = Freq_MT, yend = PMT_T), color="darkgrey", size = 0.5)+ geom_point(aes(fill=color, size=N_E), position = position_jitter(width=0, height=0), shape=21)  + theme_bw() + geom_point(data = pr_value2, aes(x = Freq_MT, y = PMT_T, group = color, fill = color, size=N_T), shape=21) + scale_fill_manual(values= c("green", "yellow", "brown", "blue", "red", "black")) + scale_color_manual(values= c("green", "yellow", "brown", "blue", "red", "black"))+ xlim(0,1) + theme(axis.title.x = element_text(size = 13), axis.title.y = element_text(size = 13), axis.text= element_text(size = 11)) + labs(y =  "Predictive values", x = "Frequency of MT", fill = "") 

# объединяем 4 графика
ggarrange(link_over_plot, pops_over_plot, acc_over_plot, pr_value, ncol = 1, common.legend = TRUE, legend="right")


