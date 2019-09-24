library(lme4)
library(ggplot2)
library(reshape2)
library(sjstats)
library(dplyr)

myt <- read.table("data_salinity2.csv", header = T, sep = ";")
str(myt)
myt$Sp [myt$str > 0.5] <- 1
myt$Sp [myt$str <= 0.5] <- 0
myt$Sample_type[is.na(myt$substrate)] <- "chaotic" 
myt$Sample_type[!is.na(myt$substrate)] <- "systematic" 
myt$standart<-factor(myt$standart)
myt$dist<-factor(myt$dist)
myt$pop.for.dist<-factor(myt$pop.for.dist)
myt$ind<-factor(myt$ind)

# язычки
myt1<- myt[myt$standart == "1",]
myt1 <- myt1[!is.na(myt1$size), ]
myt1 <- myt1[!is.na(myt1$ind), ]


myt2 <- read.table("data_salinity.csv", header = T, sep = ",")

table(myt2$pop)

ggplot(myt2, aes(x = place, y = sal_low)) + geom_boxplot()


myt2$sal_qual [myt2$sal_low <= 15] <- "freshened"

myt2$sal_qual [myt2$sal_low > 15] <- "normal"

table(myt2$sal_qual, myt2$pop)






# боклсплоты по язычкам (просто смотрим первичные данные)
# ПП очень нравится эта картинка
ggplot(myt1, aes(x = place, y = str)) + geom_boxplot(aes(fill=ind))

ggplot(myt1, aes(x = ind, y = str)) + geom_violin(aes(fill=ind)) + facet_wrap(~place)

# а как поменять цвет заливки местами? не помню((

# модель по язычкам без добавленного белого моря 
# (только с тремя стандартными точками - чупа,умба,ряжков)
# ПП сказал остальные убирать..

# ind больше не фактор, не забываем)
myt1$sal_scale <- scale(myt1$sal_low)
myt1$size_scale <- scale(myt1$size)
str(myt1)

M1_ri <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 |sea:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M1_rs <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 + str|sea:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M2_rs <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 + size_scale|sea:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M3_rs <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 + str + size_scale|sea:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AIC(M1_ri, M1_rs, M2_rs, M3_rs)

drop1(M3_rs, test = "Chi")


M3_rs1 <- update(M3_rs, .~.-size_scale:sal_scale)

drop1(M3_rs1, test = "Chi")

M3_rs2 <- update(M3_rs1, .~. -sal_scale:sea )

drop1(M3_rs2, test = "Chi")

M3_rs3 <- update(M3_rs2, .~. -str:sea)

drop1(M3_rs3, test = "Chi")

M3_rs4 <- update(M3_rs3, .~. -size_scale:str)

drop1(M3_rs4, test = "Chi")

M3_rs5 <- update(M3_rs4, .~. -sal_scale:str)

drop1(M3_rs5, test = "Chi")
# если ориентироваться на хи-квадрат, то дропаем дальше и 
# получаем итоговую модель M3_rs7
M3_rs6 <- update(M3_rs5, .~. -size_scale:sea)

drop1(M3_rs6, test = "Chi")

M3_rs7 <- update(M3_rs6, .~. -sea)

drop1(M3_rs7, test = "Chi")

AIC(M3_rs7, M3_rs5)
BIC(M3_rs7, M3_rs5)
# работаем с M3_rs7

overdisp(M3_rs7)

summary(M3_rs7)

icc(M3_rs7, adjusted = TRUE)

# с этим набором данных, где в баренцево море 
# включена опресненная часть кольского залива,
# разделение по морям у нас улетает

# диагностика

M3_rs7_diagn <- data.frame(fortify(M3_rs7), myt1)

ggplot(M3_rs7_diagn, aes(x = place, y = .scresid)) + geom_boxplot() 

ggplot(M3_rs7_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

ggplot(M3_rs7_diagn, aes(x = Sample_type, y = .scresid)) + geom_boxplot() 

ggplot(M3_rs7_diagn, aes(x = freq_MT, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs7_diagn, aes(x = size, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs7_diagn, aes(x = str, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs7_diagn, aes(x = sal_low, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs7_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 

ggplot(M3_rs7_diagn, aes(x = substrate, y = .scresid)) + geom_boxplot() 

# рисуем
new_data <- myt1 %>% group_by(sea, pop) %>% do(
  data.frame(str = seq(min(.$str), max(.$str), length.out = 100),
             size_scale = 0,
             sal_scale = seq(min(.$sal_scale), max(.$sal_scale), length.out = 4)))
summary(M3_rs7)

new_data$fit <- predict(M3_rs7, newdata = new_data, type = "response")

pops <- new_data %>% group_by(sea, pop) %>% do(data.frame(fit = min(.$fit), sal_scale = mean(.$sal_scale))) 
pops$str <- 0.01

# соленость
ggplot(new_data, aes(x = str, y = fit, group = pop, color = sal_scale)) + geom_line() + facet_wrap(~sea) + scale_color_gradient(high = "red", low = "yellow") + geom_text(data = pops, aes(x= str, y = fit, label = pop), color = "black") + xlim(-0.2, 1)

# размер
new_data2 <- myt1 %>% group_by(sea, pop) %>% do(
     data.frame(str = seq(min(.$str), max(.$str), length.out = 100),
               sal_scale = 0,
            size_scale = seq(min(.$size_scale), max(.$size_scale), length.out = 4)))

new_data2$fit <- predict(M3_rs7, newdata = new_data2, type = "response")

pops2 <- new_data2 %>% group_by(sea, pop) %>% do(data.frame(fit = min(.$fit), size_scale = mean(.$size_scale))) 
pops2$str <- -0.1

ggplot(new_data2, aes(x = str, y = fit, group = size_scale, color = size_scale)) + geom_line() + facet_grid(pop~sea) + scale_color_gradient(high = "red", low = "yellow") + geom_text(data = pops2, aes(x= str, y = fit, label = pop), color = "black") + xlim(-0.2, 1)

# в Бар море почти во всех выборках чем ты крупнее, тем меньше вероятность, что у тебя будет язычок
# но! надо грамотно выделить самим размерные классы. мне кажется,
# тк есть выборки с мидиями-гигантами, а есть без мелких


# сделаю модель не по sea, а по place

str(myt1)
M1a_ri <- glmer(ind ~ (str + place + sal_scale + size_scale)^2 + (1 |place:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M1a_rs <- glmer(ind ~ (str + place + sal_scale + size_scale)^2 + (1 + str|place:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M2a_rs <- glmer(ind ~ (str + place + sal_scale + size_scale)^2 + (1 + size_scale|place:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M3a_rs <- glmer(ind ~ (str + place + sal_scale + size_scale)^2 + (1 + str + size_scale|place:pop), data = myt1, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

AIC(M1a_ri, M1a_rs, M2a_rs, M3a_rs)

drop1(M3a_rs, test = "Chi")
# что за сингулар фит? вроде всё равно считает. или эти расчеты неправильны?
M3a_rs1 <- update(M3a_rs, .~.-str:place)

drop1(M3a_rs1, test = "Chi")

M3a_rs2 <- update(M3a_rs1, .~. -str:sal_scale)

drop1(M3a_rs2, test = "Chi")

M3a_rs3 <- update(M3a_rs2, .~. -str:size_scale)

drop1(M3a_rs3, test = "Chi")

M3a_rs4 <- update(M3a_rs3, .~. -sal_scale:place)

drop1(M3a_rs4, test = "Chi")

M3a_rs5 <- update(M3a_rs4, .~. -sal_scale:size_scale)

drop1(M3a_rs5, test = "Chi")
#солёность уходит, ого-го
M3a_rs6 <- update(M3a_rs5, .~. -sal_scale)

drop1(M3a_rs6, test = "Chi")
summary(M3a_rs6)
# ну по-честному последнее взаимодействее тоже надо убрать..
M3a_rs7 <- update(M3a_rs6, .~. -size_scale:place)

drop1(M3a_rs7, test = "Chi")
summary(M3a_rs7)

overdisp(M3a_rs7)

icc(M3a_rs7)
# всего 5? хм.
M3a_rs7_diagn <- data.frame(fortify(M3a_rs7), myt1)

ggplot(M3a_rs7_diagn, aes(x = place, y = .scresid)) + geom_boxplot() 

ggplot(M3a_rs7_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

ggplot(M3a_rs7_diagn, aes(x = Sample_type, y = .scresid)) + geom_boxplot() 

ggplot(M3a_rs7_diagn, aes(x = freq_MT, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3a_rs7_diagn, aes(x = size, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3a_rs7_diagn, aes(x = str, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3a_rs7_diagn, aes(x = sal_low, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3a_rs7_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 

ggplot(M3a_rs7_diagn, aes(x = substrate, y = .scresid)) + geom_boxplot()

# рисуем
new_dataa <- myt1 %>% group_by(place, pop) %>% do(
  data.frame(str = seq(min(.$str), max(.$str), length.out = 100),
             sal_scale = 0,
             size_scale = seq(min(.$size_scale), max(.$size_scale), length.out = 2)))


new_dataa$fit <- predict(M3a_rs7, newdata = new_dataa, type = "response")

pops <- new_dataa %>% group_by(place, pop) %>% do(data.frame(fit = min(.$fit), size_scale = mean(.$size_scale))) 
pops$str <- -0.1
ggplot(new_dataa, aes(x = str, y = fit, group = size_scale, color = size_scale)) + geom_line() + facet_grid(pop~place) + scale_color_gradient(high = "red", low = "yellow") + geom_text(data = pops, aes(x= str, y = fit, label = pop), color = "black") + xlim(-0.2, 1)
# вот, солёность ушла скорее всего потому, что у меня в тюве проставлен градиент 
# солености от кута к устью (15-25-30)
# да и вообще, всё это считалось с сингулар фит. может так вообще нельзя?


# субстраты
myt2<- myt[myt$Sample_type == "systematic",]
str(myt2)
myt2 <- myt2[!is.na(myt2$substrate), ]
myt2$substrate<-factor(myt2$substrate)
myt3<- myt2[myt2$sea == "white",]
str(myt3)
myt3<-merge(myt1, myt3)
myt3$ind<-factor(myt3$ind)
myt3$dist<-factor(myt3$dist)

myt3 <- myt3[!is.na(myt3$size), ]
myt3 <- myt3[!is.na(myt3$ind), ]

ggplot(myt2, aes(x = pop, y = str)) + geom_boxplot(aes(fill=substrate))

# модель по субстратам
Mod1_ri <- glmer(Sp ~ sea*substrate*size + (1 |sea:pop), data = myt2, family = "binomial")

Mod2_rs <- glmer( Sp ~ sea*substrate*size + (1 + size|sea:pop), data = myt2, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


AIC(Mod1_ri, Mod2_rs)

drop1(Mod2_rs, test = "Chi")
summary(Mod2_rs)
# диагностика
Mod2_rs_diagn <- data.frame(fortify(Mod2_rs), myt2)



ggplot(Mod2_rs_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

ggplot(Mod2_rs_diagn, aes(x = freq_MT, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(Mod2_rs_diagn, aes(x = size, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

# отрисую модель
install.packages("dplyr")
library(dplyr)

mean_str<-summaryBy(str~sea+substrate+size+pop, myt2)

new_data <- myt2 %>% group_by(sea, substrate, pop) %>% do(data.frame(size = seq(min(.$size), max(.$size), length.out = 100)))


new_data$fit <- predict(Mod2_rs, newdata = new_data, type = "response")

ggplot(new_data, aes(x = size, y = fit, group=pop, color = substrate), size=1) + geom_line() + facet_wrap(~sea)
# не получается короче нарисовать, пойду спать


+ geom_point(data=mean_str, aes(x= size, y=str.mean, color=substrate))





library(sjstats)
overdisp(Mod2_rs)



# №№№№№№№№№№№№№№№№№№№№№№№№№№№№№№№ модели по солености №№№№№№№№№№№№№№№№№№№№3

M1_ri <- glmer(ind ~ (str + sea + Sample_type + sal + size)^2 + (1 |sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M1_rs <- glmer(ind ~ (str + sea + Sample_type + sal + size)^2 + (1 + str|sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M2_rs <- glmer(ind ~ (str + sea + Sample_type + sal_scale + size_scale)^2 + (1 + size_scale|sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))



AIC(M1_ri, M1_rs, M2_rs)


drop1(M1_rs)

M2_rs <- update(M1_rs, .~.-sal_scale:size_scale)

drop1(M2_rs)

M3_rs <- update(M2_rs, .~. - Sample_type:sal_scale)

drop1(M3_rs)

M4_rs <- update(M3_rs, .~. - sea:Sample_type )

drop1(M4_rs)


M5_rs <- update(M4_rs, .~. - str:size_scale )

drop1(M5_rs)


M6_rs <- update(M5_rs, .~. - str:sal_scale )

drop1(M6_rs)

M7_rs <- update(M6_rs, .~. - Sample_type:size_scale  )

drop1(M7_rs)





library(sjstats)
overdisp(M7_rs)


summary(M7_rs)


### РќРµ СѓС‡РёС‚С‹РІР°РµРј С‚РёРї СЃР±РѕСЂР° РјР°С‚РµСЂРёР°Р»Р°


M1_ri <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 |sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M1_rs <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 + str|sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

M2_rs <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 + size_scale|sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


M3_rs <- glmer(ind ~ (str + sea + sal_scale + size_scale)^2 + (1 + str + size_scale|sea:pop), data = myt, family = "binomial", control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))




AIC(M1_ri, M1_rs, M2_rs, M3_rs)


drop1(M3_rs, test = "Chi")

M3_rs1 <- update(M3_rs, .~.-str:sal_scale)

drop1(M3_rs1, test = "Chi")

M3_rs2 <- update(M3_rs1, .~. -sal_scale:size_scale )

drop1(M3_rs2, test = "Chi")


M3_rs3 <- update(M3_rs2, .~. -str:size_scale  )

drop1(M3_rs3, test = "Chi")


overdisp(M3_rs3)

summary(M3_rs3)

icc(M3_rs3)




M3_rs3_diagn <- data.frame(fortify(M3_rs3), myt)

 

ggplot(M3_rs3_diagn, aes(x = .fitted, y = .scresid)) + geom_point() + geom_smooth()

ggplot(M3_rs3_diagn, aes(x = Sample_type, y = .scresid)) + geom_boxplot() 

ggplot(M3_rs3_diagn, aes(x = freq_MT, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs3_diagn, aes(x = size, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs3_diagn, aes(x = str, y = .scresid)) + geom_point() + geom_smooth(method = "loess")

ggplot(M3_rs3_diagn, aes(x = sal_low, y = .scresid)) + geom_point() + geom_smooth(method = "loess")


ggplot(M3_rs3_diagn, aes(x = sea, y = .scresid)) + geom_boxplot() 

ggplot(M3_rs3_diagn, aes(x = substrate, y = .scresid)) + geom_boxplot() 


library(dplyr)


new_data <- myt %>% group_by(sea, pop) %>% do(
  data.frame(str = seq(min(.$str), max(.$str), length.out = 100),
  size_scale = 0,
  sal_scale = seq(min(.$sal_scale), max(.$sal_scale), length.out = 4)))


new_data$fit <- predict(M3_rs3, newdata = new_data, type = "response")


pops <- new_data %>% group_by(sea, pop) %>% do(data.frame(fit = min(.$fit), sal_scale = mean(.$sal_scale))) 
pops$str <- -0.1




new_data_2 <- myt %>% group_by(sea) %>% do(
  data.frame(str = seq(min(.$str), max(.$str), length.out = 100),
             size_scale = 0,
             sal_scale = mean(.$sal_scale)))

new_data_2$fit_total <- predict(M3_rs3, newdata = new_data_2, type = "response", re.form = ~0)


freq_MT <-  myt %>% group_by(sea, pop) %>% do(
  data.frame(freq_MT = mean(.$freq_MT)))

new_data <- merge(new_data, freq_MT)

ggplot(new_data, aes(x = str, y = fit, group = pop, color = freq_MT)) + geom_line() + facet_grid(size_scale~sea) + scale_color_gradient(high = "red", low = "yellow")  + geom_line(data = new_data_2, aes(x= str, y = fit_total, group = sal_scale), color = "blue", size = 2)




# 
# 
# new_data_3 <- myt %>% group_by(sea, pop) %>% do(
#   data.frame(str = seq(min(.$str), max(.$str), length.out = 100),
#              sal_scale = 0,
#              size_scale = seq(min(.$size_scale), max(.$size_scale), length.out = 2)))
# 
# new_data_3$fit2 <- predict(M4_rs, newdata = new_data_3, type = "response")
# 
# ggplot(new_data_3, aes(x = str, y = fit2, group = size_scale, color = size_scale)) + geom_line() + facet_wrap(~sea) + scale_color_gradient(high = "red", low = "yellow") 

# 
# 
# + geom_text(data = pops, aes(x= str, y = fit, label = pop), color = "black") + xlim(-0.2, 1)


