
####версия 26.04


library(readxl)
library(dplyr)

library(vegan)


library(dplyr)
library(ggrepel)

install.packages("patchwork")
library(patchwork)
library(ggplot2)

dataframe <- read_excel("C:/shrimp/stat/Sd_morph3.xlsx", na = "NA")

# рассмотрим наши данные - названия стобцов, есть N, 73 строки
str(dataframe)
nrow(dataframe)



### убираем ювенилей 

dataframe <- dataframe %>% filter(length >= 5)

## остается 69 особей
nrow(dataframe)

###заменяем NA средними значениями
#1. берем только численные столбцы и находим среднее в каждом из них
means <- dataframe %>% select(-c( sample, coordinates_Lat, coordinates_Long, location, haplotype, sex)) %>%  summarise_all(.funs = function(x) mean(x, na.rm = T))

### берем только численные столбцы в новый датафрейм
dataframe %>% select(-c( sample, coordinates_Lat, coordinates_Long, location, haplotype, sex))

###заменяем NA средними значениями по каждому столбцу

dataframe2 <- dataframe %>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))    

## дальше смотрим только на численные столбцы (пока)

dataframe3 <- (dataframe2) %>% select(-c(1:9)) %>% select ( - length, - height)

###Стандартизация

dataframe4 <- decostand(dataframe3, method = "total", na.rm = TRUE)

### сделаем RDA для выявления зависимости между матрицей измеренных параметров ( кроме 9) и предикторами (длиной и высотой)

dataframe_rda <- rda(dataframe3 ~ length + height, data = dataframe2 )

anova.cca(dataframe_rda, permutations = 9999)

### нашли основные канонические оси - по RDA1 это длина (в первую очередь) и высота карапакса (во вторую очередь)

head(summary(dataframe_rda))
s <- summary(dataframe_rda)

### выходит, канонические оси (длина и высота) объясняют только 37 % изменчивости, а все остальные 63% (Unconstrained) приходится на неканонические оси. Из канонич. высота (соответствующая в основном RDA1) объясняет 35 %, а длина (RDA2 - 2 %). Остальная изменчивость объясняется неканоническими осями.

#####1######
plot(dataframe_rda, display = c("species", "cn"), type = "p")




###### А как ведут себя различные признаки в зависимости от длины?###
### 1. высота от длины - прямая зависимость
p <- ggplot(dataframe2, aes(x = length, y = height), xlab("Длина рострума (мм)") + ylab("Высота рострума (мм)")) + 
  geom_point()+
  geom_smooth(method = "lm")
library(ggplot2)

p_mod <- p +
  theme_bw() +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Зависимость высоты рострума от его длины") +
  xlab("Длина рострума (мм)") + ylab("Высота рострума (мм)") + 
  geom_point()+
  geom_smooth(method = "lm")

p_mod

  
### 2. высота от зубцов на 5 сомите с левой стороны (som_4_ser_left) - тоже прямая зависимость, хотя и не такая же четкая

p2 <- ggplot(dataframe2, aes(x = length, y = som_4_ser_left)) + 
  geom_point()+
  geom_smooth(method = "lm")

p2_mod <- p +
  theme_bw() +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Зависимость высоты рострума \n от количества зубцов с левой стороны \n пятого сомита") +
  xlab("Длина рострума (мм)") + ylab("Количество зубцов") + 
  geom_point()+
  geom_smooth(method = "lm")

p2_mod

### 3. а если рассмотреть график зависимости кол-ва дорзальных зубцов на роструме (d_teeth) от длины, то мы не увидим зависимости, т.е. признак независим от длины

p3 <- ggplot(dataframe2, aes(x = length, y = d_teeth)) + 
  geom_point() +
  geom_smooth(method = "lm")

p3_mod <- p3 +
  theme_bw() +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Зависимость количества дорзальных зубцов \n на роструме от его длины") +
  xlab("Длина рострума (мм)") + ylab("Количество зубцов") + 
  geom_point()+
  geom_smooth(method = "lm")

p3_mod

### слабая корреляция есть


### 4. график зависимости кол-ва посторбитальных дорзальных зубцов на роструме от длины, то мы не увидим зависимости, т.е. признак независим от длины

p4 <- ggplot(dataframe2, aes(x = length, y = post_d_teeth)) + geom_point()

p4_mod <- p4 +
  theme_bw() +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Зависимость количества посторбитальных дорзальных зубцов \n на роструме от его длины") +
  xlab("Длина рострума (мм)") + ylab("Количество зубцов") + 
  geom_point()+
  geom_smooth(method = "lm")

p4_mod


p_mod + p2_mod + p3_mod + p4_mod

#### связи с длиной нет

### Для некоторых признаков есть связь с длиной, нам нужно это вл-е убрать, поэтому дальше работаетм с PCa

## какие вклады в неканонические оси разных признаков, если не учитывать длину и высоту? как будто дорзальные зубцы в RDA1 тоже вложились, причем собственное число даже больше, чем для PC1???

anova(dataframe_rda)
### значимы, но нас интересуют остатки



#### !!!Уберем RDA1 RDA2, работаем теперь с неканоническими осями


scores_traits <- as.data.frame (scores(dataframe_rda,choices = 3:4)$species)
scores_traits

scores_individuals <- as.data.frame (scores(dataframe_rda,choices = 3:8)$sites)


##имена образцов = их номера 
scores_individuals$names <- dataframe2$sample
scores_traits$names <- dataframe2$sites


## включим в новый датафрейм и другие переменные

scores_individuals <- scores_individuals %>% mutate(location = dataframe2 $location, sex = dataframe2$sex, haplotype = dataframe2$haplotype, maturity = dataframe2$maturity)

### смотрим, есть ли зависимость между PC1 и гаплотипами, PC1 и локацией (по соответствию линейной модели??? aov)


summary(aov(PC1 ~ location, data = scores_individuals))

##да, зависимость между PC1 и локацией есть, и она статистически значима. Занчит, PC1 связана с географией
summary(aov(PC2 ~ location, data = scores_individuals))
##нет, зависимости между PC2 и локацией нет, либо она статистически не значима


summary(aov(PC1 ~ haplotype, data = scores_individuals))
##нет, зависимости между PC1 и гаплотипами (наличием мутаций) нет, либо она статистически не значима

summary(aov(PC2 ~ haplotype, data = scores_individuals))
##нет, зависимости между PC2 и гаплотипами (наличием мутаций) нет, либо она статистически не значима



library(ggplot2)

## посмотрим на разброс вкладов признаков в изменчивость неканонических осей (так говорят???) 
ggplot(scores_traits, aes(PC1, PC2)) + geom_point()
## отскакивают (далеко от 0) точки - это признаки. какие же?

## тут смотрим, отскакивающие особи по PC1 и PC2 - это самки с яйцами? 
ggplot(scores_individuals, aes(PC1, PC2)) + 
  geom_point(aes(color = maturity)) + 
  geom_vline(xintercept = 0) + 
  geom_hline(yintercept = 0)
##вроде, нет




######## Таким образом, мы можем сказать с уверенностью, что наша модель, полученная методом RDA отражает морфологическую изменчивость, при этом на эти признаки (в форме некононических осей) не влияют длина и высота! То есть мы проверили гипотезу о наличии аллометрии (непропорционального роста частей тела и изменения признаков в онтогенезе) и опровергли ее, а также избавились от влияния размера на значения признаков. НО! для этого сначала пришлось исключить из анализа ювенилей - особей с длиной карапакса меньше 5 мм. С ними аллометрия наблюдалась, и это влияло на результаты анализа (КАК?? показать картинки или хотя бы привести значения в тех же анализах)

# Посмотрим, зависят ли значения PC1 и PC2 от локации сбора?
scores_individuals$location <- factor(scores_individuals$location, levels = c("North_Atlantic", "South_Atlantic", "Indian"))

### установим цвета
group.colors <- c(Indian = "violet", South_Atlantic = "palegreen", North_Atlantic = "lightskyblue3")
#####3######

 ggplot(scores_individuals, aes(x= location, y = PC1)) +
  geom_boxplot(aes(fill = location)) + 
  theme_bw() +
  scale_fill_manual(values=group.colors) +
  theme(legend.position="none", title = element_text(size = 19), axis.text.y  = element_text(size = 20), axis.text.x = element_text(size = 20)) +
  labs(x = "")
###theme(legend.position="none", title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15))+
  labs(x = "")
### по PC1 Сев. Атлантика от Индийского отличается, а Южная Атлантика больше похожа на Индийский, хотя по 5 особям сложно (и не нужно) судить

boxplot2 <- ggplot(scores_individuals, aes(x= location, y = PC2)) +
  geom_boxplot(aes(fill = location)) +
  theme_bw() +
  scale_fill_manual(values=group.colors)
# по PC2 вариации не наблюдается


###Объединим картинки

library(patchwork)
boxplot1 + boxplot2






##Посмотрим, зависят ли значения PC1 и PC2 от наличия мутаций?

ggplot(scores_individuals, aes(x= haplotype, y = PC1)) +
  geom_boxplot(lwd=1, fill = "gray")+
  theme_bw() 

ggplot(scores_individuals, aes(x= haplotype, y = PC1)) +
  geom_violin(aes(fill = location)) + 
  theme_bw() +
  scale_fill_manual(values=group.colors) +
  theme(legend.position="none")

ggplot(scores_individuals, aes(x= haplotype, y = PC1)) +
  geom_boxplot(aes(fill = location)) + 
  theme_bw() +
  scale_fill_manual(values=group.colors) 


######4#####
group.colors2 <- c("основная гаплогруппа" = "green", "есть мутации" = "yellow")

 ggplot(scores_individuals, aes(x= location, y = PC1)) +
  geom_boxplot(aes(fill = haplotype)) + 
  theme_bw() +
  scale_fill_manual(values = group.colors2) +
   xlab("")+
   theme(legend.position="bottom", legend.title = element_blank(), legend.text = element_text(size = 19),  title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15))
 
 

o#### взрослые в ЮжАтл - мутанты, у мутантов более размазанные PC1
ggplot(scores_individuals, aes(x= haplotype, y = PC1)) +
  geom_boxplot()

# по PC1 вариация минимальная, но различия есть

ggplot(scores_individuals, aes(x= haplotype, y = PC2)) +
  geom_boxplot()
# по PC2 вариации не наблюдается

#### Проверим t-тестом, насколько значими отличия
t.test(PC1 ~ haplotype, data = scores_individuals)
t.test(PC2 ~ haplotype, data = scores_individuals)
# Связь PC1 и PC2 с гаплотипами не значима статистически


# Подпишем имена признаков, наиболее и наименее значимых (чтобы на графике они не мешались)
traits <- scores_traits %>% filter(PC1< -0.8 | PC1 > 0.5) %>% mutate(Name = row.names(.))


traits <- scores_traits %>% mutate (name = rownames(.)) %>% mutate(Name = case_when (name == "pereopod_4_merus_post_spines" ~ "переопод 4 мерус зад ряд, щетинок", 
                                                                                     name == "pereopod_4_merus_ant_spines" ~ "переопод 4 мерус пер ряд, щетинок", 
                                                                                     name == "pereopod_3_merus_post_spines" ~ "переопод 3 мерус зад ряд щетинок", 
                                                                                     name == "som_4_ser_left" ~ "боковых зубчиков на 4 сомите (лев)"))
                                                                   #####  c("боковых зубчиков на 4 сомите (прав)", "боковых зубчиков на 4 сомите (лев)", "переопод 3 merus зад ряд, щетинок","переопод 4 ischium зад ряд, щетинок", "переопод 4 merus пер ряд, щетинок", "переопод 4 merus зад ряд, щетинок")) 

# ординация с подписанными признаками, находящимися на расстоянии от 0 (чтобы было видно названия)
######2#####
ggplot(scores_traits, aes(x = PC1, y = PC2)) + 
  geom_point(aes(colour = "blue", size = PC1 > 0.5)) + 
  scale_size_manual(values=c (3, 6)) + 
  scale_color_manual(values = "aquamarine") +
  theme_bw() + 
  ggtitle("Ординация признаков в пространстве неканонических осей PC1 и PC 2") +
  geom_vline(xintercept = 0, colour = "blue") +
  geom_hline(yintercept = 0, colour = "blue") +
  geom_text_repel(data = traits, aes(label = Name), colour = "black", size = 6)+
  theme(legend.position="none", legend.title = element_blank(), legend.text = element_text(size = 19),  title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15)) +
  geom_point(data = scores_traits %>% filter(PC1 < -0.8), color = "red", size = 4) 


#добавим в датафрейм названия предикторов

scores_individuals$coordinates_Lat <- dataframe2$coordinates_Lat
scores_individuals$location <- dataframe2$location
scores_individuals$haplotype <- dataframe2$haplotype

# ординация разброса значений собственных чисел по PC1 и PC2 по локациям
ggplot(scores_individuals, aes(PC1, PC2, color = factor (haplotype))) +
  geom_point(size = 3) + 
  theme_bw() + 
  facet_wrap(~location) + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  labs(color = "") +
  theme(legend.text = element_text(size = 20), legend.position = "bottom", strip.text = element_text(size = 17))
  

### облака точек в пространстве наиболее изменчивых по PC1 признаков

dataframe2$location <- as.factor(dataframe2$location)



# Basic scatter plot
plot01 <- ggplot(dataframe2, aes(x= pereopod_3_merus_post_spines, y = pereopod_4_merus_post_spines, color = location)) + 
  geom_point(size = 5) +
  theme (plot.title = element_text(hjust = 0.7)) +
  ggtitle("Количество щетинок на merus в заднем ряду\n3 и 4 переоподы") +
  xlab("Количество щетинок, 3 переопода") + ylab("Количество щетинок, 4 переопода") + 
  theme_bw() +
  scale_color_manual(values=group.colors) +
  theme(legend.position="none")


plot02 <- ggplot(dataframe2, aes(x= pereopod_3_merus_post_spines, y = som_4_ser_left, color = location)) + 
  geom_point(size = 5) +
  theme (plot.title = element_text(hjust = 0.7)) +
  ggtitle("Количество щетинок на merus в заднем ряду\n 3 и на 4 сомите с левой стороны") +
  xlab("Количество щетинок, 3 переопода") + ylab("Количество щетинок, 4 сомит") + 
  theme_bw() +
  scale_color_manual(values=group.colors)




plot01 + plot02




# гистограммы по разным признакам
plot1 <- ggplot(dataframe2, aes(x=pereopod_4_merus_post_spines, fill = location)) +
  geom_histogram(alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Количество щетинок на merus в заднем ряду \n 4 переоподы") +
  xlab("Количество щетинок") + ylab("Количество особей") + 
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none")

plot2 <- ggplot(dataframe2, aes(x=pereopod_3_merus_post_spines, fill = location)) +
  geom_histogram(alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Количество щетинок на merus в заднем ряду \n 3 переоподы") +
  xlab("Количество щетинок") + ylab("Количество особей") + 
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) 

plot3 <- ggplot(dataframe2, aes(x=pereopod_4_merus_ant_spines, fill = location)) +
  geom_histogram(alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.7)) +
  ggtitle("Количество щетинок на merus в переднем ряду \n 4 переоподы") +
  xlab("Количество щетинок") + ylab("Количество особей") + 
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none")

plot4 <- ggplot(dataframe2, aes(x=som_4_ser_left, fill = location)) +
  geom_histogram(alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.7)) +
  ggtitle("Количество боковых зубчиков с левой стороны 4 сомита") +
  xlab("Количество щетинок") + ylab("Количество особей") + 
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none")

plot1 + plot2 + plot3 + plot4        

###боксплоты
plot1 <- ggplot(dataframe2, aes(x = location, y=pereopod_4_merus_post_spines, fill = location)) +
  geom_boxplot(alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Количество щетинок на merus \nв заднем ряду 4 переоподы") +
  ylab("Количество") + 
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none", title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15))+
  labs(x = "")
plot1

plot2 <- ggplot(dataframe2, aes(x = location, y=pereopod_3_merus_post_spines, fill = location)) +
  geom_boxplot (alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.5)) +
  ggtitle("Количество щетинок на merus \n в заднем ряду 3 переоподы") +
  ylab("Количество") +
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none", title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15))+
  labs(x = "")

plot3 <- ggplot(dataframe2, aes(x = location, y =pereopod_4_merus_ant_spines, fill = location)) +
  geom_boxplot (alpha=0.8) +
  theme (plot.title = element_text(hjust = 0.7)) +
  ggtitle("Количество щетинок на merus \n в переднем ряду 4 переоподы") +
  ylab("Количество") +
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none", title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15))+
  labs(x = "")

plot4 <- ggplot(dataframe2, aes(x = location, y=som_4_ser_left, fill = location)) +
  geom_boxplot(alpha=0.8) +
  ggtitle("Количество боковых зубчиков\n с левой стороны 4 сомита", ) +
  theme (plot.title = element_text(hjust = 0.5)) +
  ylab("Количество") +
  theme_bw() +
  scale_fill_manual(values=c("violet", "lightskyblue3", "lightgreen")) +
  theme(legend.position="none", title = element_text(size = 19), axis.text.y  = element_text(size = 15), axis.text.x = element_text(size = 15))+
  labs(x = "")
plot4

(plot1 + plot2)/(plot3 + plot4)   

