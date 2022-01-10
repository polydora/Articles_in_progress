library(readxl)
library(ggplot2)
library(dplyr)
library(nlme)
library(car)
library(lme4)
library(emmeans)

byss_s <- read_excel('Data/Mytred_Byss_Salinity_2021.xlsx', sheet = 'list1')

str(byss_s)

byss_ss <- select(byss_s, -Comment)
byss_ss$Force <- as.numeric(byss_ss$Force)
str(byss_ss)
byss_ss <- na.omit(byss_ss)
str(byss_ss)

byss_ss$Wtotal <- as.numeric(byss_ss$Wtotal)
byss_ss$Salinity <- as.numeric(byss_ss$Salinity)

# ggplot(data = byss_ss, aes(x = Salinity, y = Force, colour = Morph_type)) + geom_boxplot()  + scale_color_manual(values = c('blue' , 'red'))

byss_ss$Force_true <- byss_ss$Force - byss_ss$Wtotal/1000*9.8 

byss_ss$Force_true[byss_ss$Force_true<0] <- 0


byss_s_mod <- lm(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal)

plot(byss_s_mod, which = 1)





byss_s_mod <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal)

byss_s_mod_2 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal, weights = varPower(form = ~Wtotal))

byss_s_mod_3 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal, weights = varPower(form = ~Wtotal|Morph_type))

byss_s_mod_4 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal, weights = varExp(form = ~Wtotal|Morph_type))


byss_s_mod_5 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal, weights = varComb(varExp(form = ~Wtotal|Morph_type), varExp(form=~Salinity|Morph_type)))

byss_s_mod_6 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal, weights = varComb(varExp(form = ~Wtotal|Morph_type), varIdent(form=~1|Morph_type)))


byss_s_mod_7 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type + Wtotal, weights = varComb(varExp(form = ~Wtotal), varIdent(form=~1|Morph_type)))


AIC(byss_s_mod, byss_s_mod_2, byss_s_mod_3, byss_s_mod_4, byss_s_mod_5, byss_s_mod_6, byss_s_mod_7)

plot(byss_s_mod_7)


anova(byss_s_mod_4)

summary(byss_s_mod_7)
anova(byss_s_mod_7)

byss_s_mod_7_ML <- update(byss_s_mod_7, method = "ML")

drop1(byss_s_mod_7_ML, test = "Chi")


summary(byss_s_mod_7)

byss_ss$Salinity <- factor(byss_ss$Salinity)


ppp_mod <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type * Length)

plot(ppp_mod)


ppp_mod2 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type * Length, weights = varComb(varExp(form = ~Wtotal), varIdent(form=~1|Morph_type)))

ppp_mod3 <- gls(data = byss_ss, Force_true ~ Salinity * Morph_type * Length, weights = varComb(varExp(form = ~Wtotal|Morph_type), varIdent(form=~1|Morph_type)))

AIC(ppp_mod, ppp_mod2, ppp_mod3)


Anova(ppp_mod2)

plot(ppp_mod2)

# 


force_trends <- emtrends(ppp_mod2, specs = ~ Salinity * Morph_type, var= "Length", mode = "df.error")
summary(force_trends) 

library(broom)

tidy(force_trends)

pairs(force_trends)


## adadaadw


my_data <- expand.grid(Morph_type = c("t", "e"), Length = seq(from = min(byss_ss$Length), to = max(byss_ss$Length), length.out = 10))


# Нашел пакетик, который делает расчеты для визуализации более простыми
library(effects)
Effects_salinity <- as.data.frame(allEffects(ppp_mod2, xlevels=my_data))

Effects_salinity[[1]]

Effects_salinity[[2]]


ggplot(Effects_salinity[[1]], aes(x = Length, y = fit, group = Morph_type)) + 
  geom_line(aes(color = Morph_type)) +
  facet_wrap(~Salinity) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  scale_color_manual(values = c("blue", "red")) + 
  geom_point(data = byss_ss, aes(y = Force_true, color = Morph_type)) + ylab('Сила прикрепления, Н') + xlab('Соленость, ‰') + labs(color = 'Вид') + scale_x_continuous(breaks = c(12, 16, 20, 24))




#####################

byss_ss$Log_Force <- log(byss_ss$Force_true+1)

byss_ss_mod <- gls(data = byss_ss, Log_Force ~ Salinity * Morph_type * Wtotal)

byss_ss_mod_2 <- gls(data = byss_ss, Log_Force ~ Salinity * Morph_type * Wtotal, weights = varComb(varExp(form = ~Wtotal), varIdent(form=~1|Morph_type)))

# byss_ss$Force_true2 <- byss_ss$Force_true
# byss_ss$Force_true2[byss_ss$Force_true == 0] <- 0.0001



AIC(byss_ss_mod, byss_ss_mod_2)

plot(byss_ss_mod_2)

# byss_ss_mod_2_ML <- update(byss_ss_mod_2, method = "ML")

drop1(byss_ss_mod_2_ML)




my_data <- byss_ss %>% group_by(Salinity, Morph_type) %>% do(data.frame(Wtotal = seq(min(.$Wtotal), max(.$Wtotal), length.out = 10)))

Effects_salinity_weight <- as.data.frame(allEffects(byss_ss_mod_2, xlevels = my_data))

str(Effects_salinity_weight)


ggplot(Effects_salinity_weight[[1]], aes(x = Wtotal, y = fit, group = Morph_type)) + 
  geom_line(aes(color = Morph_type)) + 
  facet_wrap(~Salinity, nrow = 1) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_point(data = byss_ss, aes(y = log(Force_true + 1), color = Morph_type)) +scale_color_manual(values = c("blue", "red")) + ylab('Log(Сила прикрепления, Н)') + xlab('Масса мидии, г') + labs(color = 'Вид')







####################

ggplot(data = byss_ss, aes(x = Wtotal, y = Force, colour = Morph_type)) + geom_point() + geom_smooth(method = 'lm') + facet_wrap(byss_ss$Salinity) + scale_color_manual(values = c('blue' , 'red')) + ylab('Сила прикрепления, Н') + xlab('Масса мидии, г') + labs(color = 'Вид')

exposure_types <- c('12' = '12‰', '16' = '16‰', '20' = '20‰', '24' = '24‰')






#Посмотрим контрольный эксперимент

byss_c <- read_excel('Data/Mytred_Byss_Control_2021.xlsx', sheet = 'list1')
str(byss_c)
#View(byss_c)
byss_cc <- byss_c %>% filter(comment != 'Mistake')

str(byss_cc)
byss_cc$Weight <- as.numeric(byss_cc$Weight)
byss_cc$Force <- as.numeric(byss_cc$Force)

byss_cc$Force_true <- byss_cc$Force - byss_cc$Weight/1000*9.8

byss_cc$Force_true[byss_cc$Force_true < 0] <- 0 

which(byss_cc$Force_true > 2)

 byss_cc[19,]
 
 
byss_cc$Log_Force <- log(byss_cc$Force_true +1 )
 
byss_cc <- byss_cc %>% filter(!is.na(Log_Force))

byss_c_mod <- gls(data = byss_cc, Log_Force ~ Morph_type * Length)

byss_c_mod_2 <- gls(data = byss_cc, Log_Force ~ Morph_type * Weight, weights = varComb(varExp(form = ~Weight), varIdent(form=~1|Morph_type)))

AIC(byss_c_mod, byss_c_mod_2)

plot(byss_c_mod_2, which = 1)


my_data <- byss_cc %>% group_by(Morph_type) %>% do(data.frame(Weight = seq(min(.$Weight), max(.$Weight), length.out = 10)))

Effects_weight <- as.data.frame(allEffects(byss_c_mod_2, xlevels = my_data))

ggplot(Effects_weight[[1]], aes(x = Weight, y = fit, group = Morph_type)) + 
  geom_line(aes(color = Morph_type)) + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) + 
  geom_point(data = byss_cc, aes(y = log(Force_true + 1), color = Morph_type)) +scale_color_manual(values = c("blue", "red")) + ylab('Log(Сила прикрепления, Н)') + xlab('Масса мидии, г') + labs(color = 'Вид')

summary(byss_c_mod_2)
anova(byss_c_mod_2)

