library(mgcv)
library(ggplot2)
library(dplyr)
library(broom)

kam <- read.table("Data/kambala.csv", sep = ";", header = T, dec = ",")
str(kam)

kam$Outcome <- ifelse(kam$Orient == "Left", 1, 0)

Mod <- glm(Outcome ~  L * Sex, data = kam, family = "binomial")
summary(Mod)

tidy(Mod)

# plot(Mod, pages = 1, pages = 1)
# hist(kam$L)

new_data <- kam %>% group_by(Sex) %>% do(data.frame(L = seq(min(.$L), max(.$L), length.out = 100) ))

predicted <- predict(Mod, newdata = new_data, se.fit = T, type = "response")

new_data$Predicted <- predicted$fit
new_data$SE <- predicted$se.fit

Left_prop <- kam %>% mutate(Size_class = ntile(x = L, n = 10)) %>% group_by(Sex, Size_class) %>% summarise(L_mean = mean(L), Left_prop = mean(Orient == "Left"))


ggplot(new_data, aes(x = L, y = Predicted)) + geom_line(aes(color = Sex), size = 1) +  geom_ribbon(aes(ymin = Predicted - 1.96*SE, ymax = Predicted + 1.96*SE, fill = Sex), alpha = 0.5 ) + geom_point(data = Left_prop, aes(x = L_mean, y = Left_prop, fill = Sex), shape = 21, size = 4) + labs(x = "Size", y = "Probability to find Left oriented flounder") + theme_bw()
