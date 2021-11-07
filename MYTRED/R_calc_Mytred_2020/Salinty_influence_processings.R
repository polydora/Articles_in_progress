library(readxl)
library(dplyr)
library(reshape2)
library(lme4)
library(ggplot2)

myt <- read_excel("Data/Mytred_2021_Salinity.xlsx", sheet = "Exp1_tidy")

myt <- as.data.frame(myt)

salinity <- read_excel("Data/Mytred_2021_Salinity.xlsx", sheet = "Exp1_Salinity")

salinity <- as.data.frame(salinity)

str(salinity)

#Ход солености

ggplot(salinity, aes(x = Duration, y = Salinity)) + geom_line()



# Удвляем мидий, у которых ожидаемый морфотип не соответствует наблюдаемому
myt2 <- myt %>% filter(Expected_Morph == True_Morph) 

myt2_long <- melt(myt2, id.vars = c("Tank",	"Plank", 	"Mussel_ID",	"Expected_Morph",	"True_Morph",	"Length",	"Www"), value.name = "Status", variable.name = "Observation" ) %>% filter(Tank != "C")


myt2_sal_long <- merge(myt2_long, salinity)


myt2_sal_summary <- myt2_sal_long %>% group_by(Tank, Observation, True_Morph) %>% summarize(Salinity = mean(Salinity), Prop_Open = mean(Status == 1)) %>% as.data.frame()


ggplot(myt2_sal_summary, aes(x = Salinity, y = Prop_Open, color = True_Morph)) + geom_point() + geom_smooth(se = F) + facet_wrap(~Tank)

str(myt2_sal_long)

myt2_sal_long$Status <- as.numeric(myt2_sal_long$Status)
myt2_sal_long$True_Morph <- factor(myt2_sal_long$True_Morph)




Model1 <- glmer(Status ~ Salinity*True_Morph + Duration + (1|Mussel_ID) + (1|Tank), data = myt2_sal_long, family = binomial(link = "logit"), control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(Model1)


# library(mgcv)
# Model2 <- gamm(Status ~ s(Salinity, by = True_Morph) + True_Morph + Duration, random = list(Mussel_ID = ~1, Tank = ~1) , data = myt2_sal_long, family = binomial(link = "logit"))

# Model2 <- gamm(Status ~ s(Salinity, by = True_Morph) + True_Morph + Duration , data = myt2_sal_long, family = binomial(link = "logit"))



# summary(Model2$gam)


# plot(Model2$gam)




MyData <- expand.grid(Salinity = seq(min(myt2_sal_long$Salinity), max(myt2_sal_long$Salinity), 0.1), True_Morph = c("t", "e"), Duration = mean(myt2_sal_long$Duration))


MyData$Fit <- predict(Model1, newdata = MyData, re.form = NA)


logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация

X <- model.matrix(~  Salinity*True_Morph + Duration, data = MyData)
b <- fixef(Model1)

MyData$fit_eta <- X %*% b 
MyData$se_eta <- sqrt(diag(X %*% vcov(Model1) %*% t(X)))

MyData$fit_pi <- logit_back(MyData$fit_eta)
MyData$lwr <- logit_back(MyData$fit_eta - 1.96 * MyData$se_eta)
MyData$upr <- logit_back(MyData$fit_eta + 1.96 * MyData$se_eta)






ggplot(MyData, aes(x = Salinity, y = fit_pi)) + 
  geom_line(aes(, color = True_Morph), size = 1) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr, group = True_Morph), alpha = 0.2) +
  geom_point(data = myt2_sal_summary, aes(x = Salinity, y = Prop_Open, color = True_Morph), position = position_jitter(width = 0.1, height = 0.05)) +
  scale_color_manual(values = c("red", "blue"))



