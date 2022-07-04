library(readxl)
library(mgcv)
library(ggplot2)
library(dplyr)



long_B <- read_excel("Data/long_term_dyn.xlsx", sheet = "B")
long_B$B <- as.numeric(long_B$B)
long_B$LogB <- log10(long_B$B)

long_B2 <- long_B %>% filter(year > 1950)


long_T <- read_excel("Data/long_term_dyn.xlsx", sheet = "T")
# long_T <- long_T %>% filter(year >= min(long_B$year) )





Mod_B <- gam(LogB ~ s(year, k = 3), data = long_B2)

plot(Mod_B)

mydata <- data.frame(year = seq(min(long_B2$year), max(long_B2$year), by = 1)) 

predicted <- predict(Mod_B, newdata = mydata, se.fit = T)

mydata$Fit <- predicted$fit
mydata$SE <- predicted$se.fit








ylim.T <- range(long_T$Temp)
ylim.LogB <- range(long_B2$LogB)

# mydata$Fit

b <- diff(ylim.T)/diff(ylim.LogB)
a <- b*(ylim.T[1] - ylim.LogB[1])



ggplot(long_T, aes(x = year)) +
  geom_line(aes(y = Temp), size = 1, color = "red") +
  geom_line(data = mydata, aes(x = year, y = a + Fit*b), size = 2, color = "blue") +
  geom_ribbon(data = mydata, aes(x = year, ymax = a + (Fit + 1.96*SE)*b, ymin = a + (Fit - 1.96*SE)*b), alpha = 0.2) +
  geom_point(data =long_B, aes(y = a + LogB*b), size = 2) + 
  scale_y_continuous("Mean Water Temperature", sec.axis = sec_axis(~ (. - a)/b, name = "lg(B)")) + 
  theme_bw()



all_assessment <- merge(long_B, long_T)


ggplot(all_assessment, aes(x = Temp, y = LogB)) + geom_point()


ggplot(mydata, aes(x=year)) +
  geom_line(aes(y=Fit), col="blue") +
  geom_line(data = long_T, aes(y = Temp * scaleFactor), col="red") +
  scale_y_continuous(name="Temperature", sec.axis=sec_axis(~.*scaleFactor, name="Log(B)")) 




ggplot(long_T, aes(x=year)) +
  geom_line(aes(y=Temp), col="blue") +
  geom_line(data = mydata, aes(y = Fit * scaleFactor), col="red") +
  scale_y_continuous(name="Temperature", sec.axis=sec_axis(~./scaleFactor, name="Log(B)")) 



+
  theme(
    axis.title.y.left=element_text(color="blue"),
    axis.text.y.left=element_text(color="blue"),
    axis.title.y.right=element_text(color="red"),
    axis.text.y.right=element_text(color="red")
  )