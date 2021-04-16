library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(reshape2)
library(vegan)
library(mgcv)

cam <- read_excel("Data/All_length_age_year.xlsx") 





lefts <- cam %>% filter(Orient == "Left") 

rights <- cam %>% filter(Orient == "Right") 


left_w <- lefts %>%  count(Age, Bay, Year) %>% spread(Age, n, fill = 0) %>% select(-c( Year, "1")) %>% select(-Bay)

right_w <- rights %>%  group_by(Bay) %>% count(Age, Year) %>% spread(Age, n, fill = 0) %>% select(-"1")



left_w_onega <- left_w %>% filter(right_w$Bay == "Onega") %>% mutate(Old = `8`+`9`+`10`+`11`+`12`+`13` ) %>% select(-c(`8`,`9`,`10`,`11`,`12`,`13`))


right_w_onega <- right_w %>% filter(Bay == "Onega") %>% mutate(Old = `8`+`9`+`10`+`11`+`12`+`13` ) %>% select(-c(`8`,`9`,`10`,`11`,`12`,`13`))




left_onega_cca <- cca(left_w_onega ~ `2`  + `3`   + `5` +  `6`     + Old, data = right_w_onega)


vif.cca(left_onega_cca)

anova(left_onega_cca)
anova(left_onega_cca, by = "axis")

anova(left_onega_cca, by = "margin")


plot(left_onega_cca)





left_stages_onega <- left_w_onega %>% mutate(Young = `2`+`3`, Med = `4`+`5`, Old = Old + `6` + `7`) %>% select(Young, Med, Old) %>% mutate(Orient = "Left")

rigth_stages_onega <- right_w_onega %>% mutate(Young = `2`+`3`, Med = `4`+`5`, Old = Old + `6` + `7`) %>% select(Young, Med, Old, Year) %>% mutate(Orient = "Right")

stages_onega <- rbind(rigth_stages_onega, left_stages_onega) 

stages_onega$Bay <- "Onega"

stages_onega$Year <- rep(stages_onega$Year[!is.na(stages_onega$Year)], 2) 


ggplot(stages_onega, aes(x= Year, y = Old, color = Orient)) + geom_line(size = 2)




left_young_mature <- left_w %>% mutate(Young = `2`+`3`+`4` + `5`, Mature =  `6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`) %>% select(Young, Mature)

right_young_mature <- right_w %>% mutate(Young = `2`+`3`+`4` + `5` , Mature =  `6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`) %>% select(Young, Mature, Bay, Year)


young_mature <- 
data.frame(Prop_left = left_young_mature$Young/(left_young_mature$Young+right_young_mature$Young),
           Year = right_young_mature$Year,
           Bay = right_young_mature$Bay,
           Prop_young = right_young_mature$Young/(right_young_mature$Mature + right_young_mature$Young))



Mod <- lm(Prop_left ~ Prop_young + Bay, data = young_mature )

summary(Mod)


# Строим модель динамики доли левых

cam <- cam %>% mutate(Orient2 = ifelse(Orient == "Left", 1, 0))

cam <- cam %>% mutate(Stage = ifelse(Age <= 3 , "Young", "Adult"))


Mod <- gam(Orient2 ~ s(Year, by = interaction(Stage, Bay)) + Stage * Bay, data = cam, family = "binomial")

plot(Mod)

gam.check(Mod)

summary(Mod)

mydata <- cam %>% group_by(Bay) %>% do(expand.grid(Stage = c("Young", "Adult"), Year = seq(min(.$Year), max(.$Year), 1)))


predicted <- predict(Mod, newdata = mydata, se.fit = T, type = "response")


mydata$Fit = predicted$fit

mydata$SE = predicted$se.fit


left_freq <- cam %>%  group_by(Bay, Year, Stage) %>% summarise(Left_prop = mean(Orient2))

ggplot(mydata, aes(x = Year, y = Fit, color = Stage, fill = Stage)) + geom_line(size = 1) + facet_wrap(~Bay) + geom_point(data = left_freq, aes(y = Left_prop, color = Stage))

anova(Mod)



cam$Bay <- factor(cam$Bay)

Mod2 <- gam(Orient2 ~ s(Year, by = Bay) +  Bay, data = cam, family = "binomial")

# Mod2 <- glm(Orient2 ~ Year*Bay, data = cam, family = "binomial")


plot(Mod2)

gam.check(Mod2)

summary(Mod2)

mydata <- cam %>% group_by(Bay) %>% do(data.frame(Year = seq(min(.$Year), max(.$Year), 1)))


predicted <- predict(Mod2, newdata = mydata, se.fit = T, type = "response")


mydata$Fit = predicted$fit

mydata$SE = predicted$se.fit


left_freq <- cam %>%  group_by(Bay, Year) %>% summarise(Left_prop = mean(Orient2))

ggplot(mydata, aes(x = Year, y = Fit)) + geom_line(size = 1) + facet_wrap(~Bay) + geom_point(data = left_freq, aes(y = Left_prop)) 
