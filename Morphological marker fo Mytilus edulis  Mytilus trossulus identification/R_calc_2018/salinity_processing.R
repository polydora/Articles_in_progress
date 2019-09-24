library(ggplot2)
library(reshape2)


sal <- read.table("sal_new.csv", header = T, sep = ";")
sal <- sal[, -c(6:10)]

ggplot(sal, aes(x = Low_Tide, y = High_Tide)) + geom_point()

ggplot(sal, aes(x = Dist, y = Low_Tide)) + geom_point() + geom_smooth()



ggplot(sal, aes(x = Lon, y = Lat, size = Low_Tide)) + geom_point()

sal2 <- melt(sal, id.vars = c( "Site",      "Lat",      "Lon",  "Dist"))

sal2$variable <- factor(sal2$variable)

ggplot(sal2, aes(x = Dist, y = value, color = variable)) + geom_point() + geom_smooth(method = lm)


sal_model <- lm(value ~ Dist*variable, data = sal2)

plot(sal_model)

summary(sal_model)
drop1(sal_model, test = "F")

sal_model2 <- lm(value ~ Dist + variable, data = sal2)
drop1(sal_model2, test = "F")

summary(sal_model2)

predict(sal_model2, newdata = data.frame(Dist = 12.5, variable = "High_Tide"))


