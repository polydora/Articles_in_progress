#Пакеты
library(ggplot2)
library(lme4)
library(multcomp)
library(nlme)
library(car)
library(gamm4)
library(dplyr)


# Читаем данные
shel <- read.table("Shell.csv", header = T, sep = ";")

nem <- read.table("Nemert 2016.csv", header = T, sep = ";")

nem <- nem %>% filter(Exp != "Pilot")

# Анализ связи вес мидии с раковиной и без раковины.

ggplot(shel, aes(x = Shell, y = Soft)) + geom_point() + theme_bw() + geom_smooth(method = "lm") + labs(x = "Вес мидии с раковиной", y = "Вес мягких тканей")

Mod_shell <- lm(Soft ~ Shell, data = shel)


# Пересчет веса с раковиной в вес мягких тканей.

nem$W <- nem$W_signal

nem$W[nem$Signal_type == "Mytilus"] <-   coef(Mod_shell)[1] + coef(Mod_shell)[2] *  nem$W[nem$Signal_type == "Mytilus"]

# доля заползших в контейнер с сигналом
# nem$P <- nem$N_in_signal/(nem$N_in_signal + nem$N_in_zero)

table(nem$Signal_type)


nem$O2_depl <- (nem$O2_in - nem$O2_out)/nem$O2_in

nem$Signal_type <- factor(nem$Signal_type)

nem$Signal_type <- relevel(nem$Signal_type, ref = "Water")



# Потери кислорода в садках разного типа

nem %>% 
  ggplot(aes(x = Signal_type , y = O2_depl)) + geom_boxplot(notch = T, varwidth = T)










names(nem)

nem$P <- nem$N_in_signal/(nem$N_in_signal + nem$N_in_zero)


nem$Signal_type <-as.factor(nem$Signal_type)

# nem <- nem[-c(16,19), ]

# nem <- nem[!(nem$Exp %in% c("Nemert2")),  ]

# Модель для выбора немертинами сигнала



overdisp_fun <- function(model) {
  ## number of variance parameters in
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}



### Модель, в которой включается тип экспермента и тип сигнала ####

# nem <- nem[-c(9,12), ] #Удаляем садки с очень высокой биомассой сигнальных животных.


# nem2 <- nem[-c(12), ]


M_sign_pois <- glmer.nb( N_in_signal ~   log(W+1) * Signal_type + (1 |Exp), data = nem)


drop1(M_sign_pois, test = "Chi")


M_sign_pois <- glmer.nb( N_in_signal ~   log(W+1) + Signal_type + (1 |Exp), data = nem)

vif(M_sign_pois)

drop1(M_sign_pois, test = "Chi")


summary(M_sign_pois)


ggplot(nem, aes(x = Signal_type, y = log(W+1))) + geom_boxplot() 


ggplot(nem, aes(x = Signal_type, y = N_in_signal)) + geom_boxplot() 


ggplot(nem, aes(x = log(W+1), y = N_in_signal)) + geom_point() + geom_smooth(method = "lm")




# install.packages("stargazer")
library(stargazer)

# распечатка таблицы summary латеха
stargazer(M_sign_pois)



plot(M_sign_pois)
Anova(M_sign_pois)

overdisp_fun(M_sign_pois)




M_sign_pois_tuk <- glht(M_sign_pois, linfct = mcp(Signal_type = "Tukey"))


summary(M_sign_pois_tuk)

plot(M_sign_pois_tuk)


# Диагностика модели

M_foo <- glm(N_in_signal ~   log(W + 1)  + Signal_type, data = nem)

vif(M_foo)




M_sign_pois_diagn <- fortify(M_sign_pois)

ggplot(M_sign_pois_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth(method = "lm")



ggplot(M_sign_pois_diagn, aes(x = O2_depl, y = .scresid)) + geom_point() + geom_smooth(method = "lm")


ggplot(M_sign_pois_diagn, aes(x =  T_air , y = .scresid)) + geom_point() + geom_smooth(method = "lm")



ggplot(M_sign_pois_diagn, aes(x =  Signal , y = .scresid)) + geom_boxplot()



ggplot(M_sign_pois_diagn, aes(x =  Signal_type , y = .scresid)) + geom_boxplot()


ggplot(M_sign_pois_diagn, aes(x =  Day , y = .scresid)) + geom_point() + geom_smooth()








#Визуализация модели

Mydata <- data.frame(W = c(median(nem2$W[nem$Signal_type == "Mytilus"]), median(nem2$W[nem$Signal_type == "Oligochaeta"] ), 0), Signal_type = levels(nem2$Signal_type))


Mydata <- data.frame(W = mean(nem$W_signal), Signal_type = levels(nem$Signal_type))


Mydata$Predicted <- predict(M_sign_pois, newdata = Mydata, type = "response", re.form = NA)


ggplot() + geom_boxplot(data = nem2, aes(x= Signal_type, y = N_in_signal)) + geom_bar(data = Mydata, aes(x= Signal_type, y = Predicted), stat = "identity", fill = "gray", color = "black", alpha = 0.5) + theme_bw()


ggplot()  + geom_bar(data = Mydata, aes(x= Signal_type, y = Predicted), stat = "identity", fill = "gray", color = "black") + theme_bw() + geom_point(data = nem2, aes(x= Signal_type, y = N_in_signal), position = position_jitter(width = 0.05)) + stat_summary(data = nem2, aes(x= Signal_type, y = N_in_signal), fun.y = "mean", geom = "point", color = "red", size = 6) + xlab("Тип сигнала") + ylab("Количество червей, \nзаползших в сигнальный контейнер") + geom_text(aes(x = (c(3,1,2) + 0.2), y = (Mydata$Predicted + 1), label = c("B", "A", "A")), size = 6)





# Связь с кислородом
Mod_O2 <- lme(O2_depl ~  log(W+1) + Signal_type, random = ~1 |Exp, data = nem [!nem$Exp %in% c("Pilot", "Nemert 1"), ] )

summary(Mod_O2)

Anova(Mod_O2, test = "Chi")

stargazer(Mod_O2)


library(doBy)

nem$Resid_W [nem$Signal_type == "Water"] <- residuals(lm(N_in_signal ~ log(W+1) -1, data = nem[nem$Signal_type == "Water", ])) 

nem$Resid_W [nem$Signal_type == "Oligochaeta"] <- residuals(lm(N_in_signal ~ log(W+1) -1, data = nem[nem$Signal_type == "Oligochaeta", ])) 

nem$Resid_W [nem$Signal_type == "Mytilus"] <- residuals(lm(N_in_signal ~ log(W+1)-1, data = nem[nem$Signal_type == "Mytilus", ])) 


nem$Fit_W [nem$Signal_type == "Water"] <- fitted(lm(N_in_signal ~ log(W+1), data = nem[nem$Signal_type == "Water", ])) 

nem$Fit_W [nem$Signal_type == "Oligochaeta"] <- fitted(lm(N_in_signal ~ log(W+1) -1, data = nem[nem$Signal_type == "Oligochaeta", ])) 

nem$Fit_W [nem$Signal_type == "Mytilus"] <- fitted(lm(N_in_signal ~ log(W+1)-1, data = nem[nem$Signal_type == "Mytilus", ])) 





M_sign_resid <- lmer( N_in_signal ~  Signal_type + Fit_W + (1 |Exp), data = nem)

summary(M_sign_resid)
plot(M_sign_resid)
Anova(M_sign_resid)



#############################################

library(dplyr)

ggplot(nem, aes(x = Exp, y = P) ) + 
  geom_boxplot() +
  geom_hline(yintercept = 0.5)



nem %>% filter(Exp != "Pilot") %>%  
  ggplot(., aes(x = Signal_type, y = P) ) + 
  geom_boxplot() +
  geom_hline(yintercept = 0.5)


nem %>% filter(Exp != "Pilot") %>%  
  ggplot(., aes(x = Signal_type, y = N_in_signal) ) + 
  geom_boxplot() +
  geom_hline(yintercept = 0.5)


library(glmmTMB)

nem3 <- nem %>% filter(Exp != "Pilot")  


model <- glmmTMB(cbind(N_in_signal, N_in_zero ) ~ Signal_type  + (1|Exp),  data = nem3, family = betabinomial(link = "logit"))


summary(model)




Mydata <- data.frame(W = mean(nem$W_signal), Signal_type = levels(nem$Signal_type))


Mydata$Predicted <- predict(model, newdata = Mydata, type = "response", re.form = ~0)


ggplot() + geom_boxplot(data = nem2, aes(x= Signal_type, y = P)) + geom_bar(data = Mydata, aes(x= Signal_type, y = Predicted), stat = "identity", fill = "gray", color = "black", alpha = 0.5) + theme_bw()


ggplot()  + geom_bar(data = Mydata, aes(x= Signal_type, y = Predicted), stat = "identity", fill = "gray", color = "black") + theme_bw() + geom_point(data = nem2, aes(x= Signal_type, y = N_in_signal), position = position_jitter(width = 0.05)) + stat_summary(data = nem2, aes(x= Signal_type, y = N_in_signal), fun.y = "mean", geom = "point", color = "red", size = 6) + xlab("Тип сигнала") + ylab("Количество червей, \nзаползших в сигнальный контейнер") + geom_text(aes(x = (c(3,1,2) + 0.2), y = (Mydata$Predicted + 1), label = c("B", "A", "A")), size = 6)





