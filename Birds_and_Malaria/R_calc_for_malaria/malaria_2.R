
library(ggplot2)
library(mgcv)
library(dplyr)
library(readxl)



finch_il <- read_excel("Data/Spinus_2020_parasitemia_IL6_survival_final.xlsx", na = "NA", sheet = "Spinus_2020_parasitemia_IL6_sur")
finch_il$Experiment <- factor(finch_il$Experiment)
finch_il$Ring <- factor(finch_il$Ring)

n_obs_il <- finch_il %>% group_by(Ring) %>% summarise(N_il_obs = n())


finch <- read_excel("Data/Spinus_2020_parasitemia_IL6_survival_final.xlsx", na = "NA", sheet = "RMR")
finch$Experiment <- factor(finch$Experiment)
finch$Ring <- factor(finch$Ring)

# finch <- finch %>% filter(Bad !=1)

ggplot(finch_il, aes(x = Date, y = Ring)) + 
  geom_point() + 
  facet_wrap(~Experiment, scales = "free") + 
  geom_point(data = finch,  color = "blue", shape = 4)


## -------------------------------------------------------------------------------------------------------------------------
ggplot(finch, aes(x = DPI, y = log(RMR), group = Ring)) + geom_line() + facet_wrap(~Experiment)


## -------------------------------------------------------------------------------------------------------------------------
ggplot(finch_il, aes(x = DPI, y = Parasitemia, group = Ring)) + geom_line() + facet_wrap(~Experiment)



## -------------------------------------------------------------------------------------------------------------------------
finch_il <- finch_il %>% group_by(Ring) %>% arrange(Date) %>% mutate(Cum_Parasitemia = cumsum(Parasitemia), Cum_IL = cumsum(IL6_pg_ml)) %>% arrange(Ring, Date)


ggplot(finch_il, aes(x = DPI, y = Cum_Parasitemia, group = Ring)) + geom_line() + facet_wrap(~Experiment)


## -------------------------------------------------------------------------------------------------------------------------

# Код для совмещения данных по RMR и измерениям паразитемии и интерлейкинов
finch$Parasitemia <- NA
finch$IL <- NA
finch$Cum_Parasitemia <- NA
finch$Dif_Date <- NA
finch$Survival <- NA
finch$Status <- NA


for(i in 1:nrow(finch)){
  df <- finch_il %>% filter(Ring %in% finch$Ring[i])
  
  dif_dat1 <- (difftime(df$Date, finch$Date[i]))
  dif_dat1[dif_dat1 < 0 ] <- NA
  time1 <- df$Date[which.min(dif_dat1)]
  
  finch$Parasitemia[i] <- df$Parasitemia[df$Date == time1]
  finch$IL[i] <- df$IL6_pg_ml[df$Date == time1]
  finch$Cum_Parasitemia[i] <- df$Cum_Parasitemia[df$Date == time1]
  finch$Survival[i] <- df$Survival[df$Date == time1]
  finch$Status[i] <- df$Status[df$Date == time1]
  

  finch$Dif_Date[i] <- as.numeric(difftime(time1, finch$Date[i], units = "day"))
}



ggplot(finch, aes(x = Cum_Parasitemia, y = RMR, group = Ring)) + geom_line() + facet_wrap(~Experiment)


# ggplot(finch, aes(y = Cum_Parasitemia, x = DPI, group = Ring)) + geom_line() + facet_wrap(~Experiment)


## -------------------------------------------------------------------------------------------------------------------------

finch$Experiment <- factor(finch$Experiment)
finch$Ring <- factor(finch$Ring)
finch$Survival <- factor(finch$Survival)
finch$Room <- factor(finch$Room)






# M1_gam <- gam(log(RMR) ~ s(Cum_Parasitemia, by = Experiment, k = 10) + s(IL, by = Experiment) +  Experiment + log(Mass) ,  method = "REML", data = finch)



library(gamm4)
M2_gam <- gamm(log(RMR) ~ s(Cum_Parasitemia,  by = Experiment) + IL+ Experiment + log(Mass),  random = list(Ring = ~1), method = "REML", data = finch)


plot(M2_gam$gam)


concurvity(M2_gam$gam, full = T )

library(gratia)

draw(M2_gam)


## -------------------------------------------------------------------------------------------------------------------------
summary(M1_gam)



## -------------------------------------------------------------------------------------------------------------------------
library(mgcViz)
b <- getViz(M1_gam)
check.gamViz(b)



## -------------------------------------------------------------------------------------------------------------------------
library(gratia)
library(cowplot)


Pl_Cum_Paras_Exp1 <- draw(M1_gam, select = c(2), scales = "fixed") + geom_hline(yintercept = 0)

Pl_Cum_Paras_Exp2 <- draw(M1_gam, select = c(3), scales = "fixed") + geom_hline(yintercept = 0)





## -------------------------------------------------------------------------------------------------------------------------

Pl_IL_Exp1 <- draw(M1_gam, select = c(8), scales = "fixed") + geom_hline(yintercept = 0)

Pl_IL_Exp2 <- draw(M1_gam, select = c(9), scales = "fixed") + geom_hline(yintercept = 0)



Pl_Parasitemia_Exp1 <- draw(M1_gam, select = c(5), scales = "fixed") + geom_hline(yintercept = 0)

Pl_Parasitemia_Exp2 <- draw(M1_gam, select = c(6), scales = "fixed") + geom_hline(yintercept = 0)



library(patchwork)
plot_grid(Pl_Cum_Paras_Exp1, Pl_Cum_Paras_Exp2, nrow = 1) / plot_grid(Pl_IL_Exp1, Pl_IL_Exp2, nrow = 1)/plot_grid(Pl_Parasitemia_Exp1, Pl_Parasitemia_Exp2, nrow = 1)


# Моделируем вероятность гибели


M2_gam <- gam(Status ~ s(Cum_Parasitemia, by = Experiment) + s(Parasitemia, by = Experiment) + Experiment + log(Mass)  + s(Ring, k = 53, bs = "re"),  method = "REML", data = finch, family = "binomial")


summary(M2_gam)
gam.check(M2_gam)

plot(M2_gam)

