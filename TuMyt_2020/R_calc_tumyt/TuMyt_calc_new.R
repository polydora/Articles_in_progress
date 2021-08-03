library(readxl)
library(ggpubr)
library(MuMIn)
library(car)
library(ggplot2)
library(dplyr)

# идея - делать новый калькулятор

# выборки для нового калькулятора по Тюве - 6 выборок из язычковой статьи, которые входили в датасет BH, + 3 старых выборки начала 2000-х годов из опубликованных работ (Буфалова и др., 2005; Vainola, Strelkov, 2011), + 6 новых выборок (сборы 2018 года). Брали только особей в возрасте от 4 до 7 лет включительно! 
# чтобы построить новый калькулятор, мы подумали, что лучше это делать не просто "описанием" 15-ти Тювских выборок, а с добавлением выборок из язычковой статьи (12 выборок из BL и 8 оставшихся выборок из BH). Брали особей всех возрастов

# получился вот такой файл
tuv_calc <- read_excel("Data/Tu_myt_calculator.xlsx")
str(tuv_calc)

tuv_calc$Depth <- as.numeric(tuv_calc$Depth)
tuv_calc$period <- factor(tuv_calc$period)
tuv_calc$dataset <- factor(tuv_calc$dataset, levels = c("BL", "TV", "BH"))
tuv_calc$Transect <- factor(tuv_calc$Transect)
tuv_calc$sp <- ifelse(tuv_calc$str <= 0.5, 0, 1)

PT3 <- tuv_calc %>% group_by(dataset, pop) %>% summarise(PT=mean(ind==1), Ptros=mean(sp==1))
str(PT3)

calculator3 <- merge(tuv_calc, PT3)
str(calculator3)

# считаем вот такую модель
Model3 <- glm(sp~PT*dataset, family = "binomial", data = calculator3)

summary(Model3)
Anova(Model3)

plot(Model3)

r.squaredGLMM(Model3)

# выведенные формулы
# 
# BH: exp(-3.8 + 4.8*PT)/(1 + exp(-3.8 + 4.8*PT)))
# 
# BL: <- with(MyData, exp(-2.3 + 5.6*PT)/(1 + exp(-2.3 + 5.6*PT)))
# 
# TV: <- with(MyData, exp(-2.3 + 3.3*PT)/(1 + exp(-2.3 + 3.3*PT)))

# график
MyData <- calculator3 %>% group_by(dataset) %>% do(data.frame(PT=seq(0, 1, length.out = 100), Ptros=seq(0, 1, length.out = 100)))

Model3_Predict <- predict(Model3, newdata = MyData, type = "response", se.fit=TRUE)

MyData$fit <- Model3_Predict$fit

MyData$se <- Model3_Predict$se.fit

df <- data.frame(x1 = 0, x2 = 1, y1 = 0, y2 = 1)

# отобразим все три линии на одном графике
calc3 <- ggplot(MyData, aes(x = PT, y = fit, color = dataset)) + geom_line(size = 2) + theme_bw() + geom_ribbon(aes(ymin = fit-1.96*se, ymax = fit+1.96*se), fill = "gray", alpha=0.3) + ylim(0, 1) + labs(y="Ptros") + annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1) + geom_point(data=PT3, aes(y=Ptros, x=PT, shape=dataset, color=dataset), size = 2) + scale_color_manual(values = c("grey", "darkviolet", "black"))

# для тювы линия - промежуточная между BL и BH, а также там самая высокая изменчивость. Важные выводы для местообитаний с пограничной, около 30ppt, солёностью?

# теперь нарисуем для каждого датасета свой график. график по тюве будет оформлен более подробно: цвет - период, форма значка - трансекта, размер значка - глубина. оформительские идеи перекочевали из прошлых кодов и из аспирантской вкр
# 
# датасет TV
MyData_TV <-  MyData %>%  filter(dataset %in% "TV")
str(MyData_TV)

tuv_calc_TV <- tuv_calc %>%  filter(dataset %in% "TV")

tuv_calc_TV$Depth <- factor(tuv_calc_TV$Depth)

PT4 <- tuv_calc_TV %>% group_by(dataset, period, Transect, Depth, pop) %>% summarise(PT=mean(ind==1), Ptros=mean(sp==1))

PT4$Depth <- as.numeric(PT4$Depth)

calc_TV <- ggplot(data = MyData_TV, aes(x=PT, y = fit)) + geom_line(color="darkviolet", size=2) + geom_ribbon(data=MyData_TV, aes(ymin = fit-1.96*se, ymax = fit+1.96*se), fill = "gray", alpha=0.3)+ ylim(0, 1) + theme_bw() + geom_point(data = PT4, aes(x = PT, y = Ptros, color = period, fill = period, shape = Transect, stroke = 2.5, size = Depth)) + ylim(0, 1) + annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1)  + scale_fill_manual(values = c("red", "blue2", "aquamarine3")) + scale_color_manual(values = c("red", "blue2", "aquamarine3")) + scale_shape_manual(values = c(21, 23, 25, 24)) + scale_size(range = c(1, 3)) + labs(y="Ptros") + theme(legend.position = "") 

# датасет BL
MyData_BL <-  MyData %>%  filter(dataset %in% "BL")
tuv_calc_BL <- tuv_calc %>%  filter(dataset %in% "BL")
PT5 <- tuv_calc_BL %>% group_by(pop) %>% summarise(PT=mean(ind==1), Ptros=mean(sp==1))

calc_BL <- ggplot(data = MyData_BL, aes(x=PT, y = fit)) + theme_bw() + geom_line(color="grey", size=2) + geom_ribbon(data=MyData_BL, aes(ymin = fit-1.96*se, ymax = fit+1.96*se), fill = "gray", alpha=0.3) + ylim(0, 1) + theme_bw() + geom_point(data = PT5, aes(x = PT, y = Ptros), color = "grey", shape = 4, stroke = 2.5, size = 2) + annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1) + labs(y="Ptros")

# датасет BH
MyData_BH <-  MyData %>%  filter(dataset %in% "BH")
tuv_calc_BH <- tuv_calc %>%  filter(dataset %in% "BH")
PT6 <- tuv_calc_BH %>% group_by(pop) %>% summarise(PT=mean(ind==1), Ptros=mean(sp==1))

calc_BH <- ggplot(data = MyData_BH, aes(x=PT, y = fit)) + theme_bw() + geom_line(color="black", size=2) + geom_ribbon(data=MyData_BH, aes(ymin = fit-1.96*se, ymax = fit+1.96*se), fill = "gray", alpha=0.3) + ylim(0, 1) + theme_bw() + geom_point(data = PT6, aes(x = PT, y = Ptros), color = "black", shape = 4, stroke = 2.5, size = 2) + annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1) + labs(y="Ptros")

# собираем три графика вместе
calc_comp_3 <- ggarrange(calc_BL, calc_TV, calc_BH, nrow = 1)

# тут на графике для Тювы хорошо видна динамика Ptros на прямых генетических данных. и динамика PT, кстати тоже. 