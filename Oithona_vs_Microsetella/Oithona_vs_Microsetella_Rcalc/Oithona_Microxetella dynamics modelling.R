# В этом скрипте анализируется многолетняя динамика Микросетеллы и Оитоны

library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)
library(mgcv)
library(gridExtra)
library(broom)
library(tidyr)




cop_init <- read.table("Data/Oithona_Microsetella.csv", sep = ";", header = T)


cop <- melt(cop_init, id.vars = c("Day","Month","Year","Day.of.year"), variable.name = "Stage", value.name = "N") 

cop$Depth[str_detect(as.character(cop$Stage), "_1")] <- "0_10"  
cop$Depth[str_detect(as.character(cop$Stage), "_25")] <- "0_25"  
cop$Depth[str_detect(as.character(cop$Stage), "_3")] <- "25_bottom" 
cop$Depth[str_detect(as.character(cop$Stage), "_65")] <- "0_bottom" 

cop$Depth[is.na(cop$Depth)] <- "10_25" 

# unique(cop$Depth)

cop <- cop %>% filter(Depth %in% c("0_10", "10_25", "25_bottom")) 


cop$Stage2[str_detect(as.character(cop$Stage), "_F_")] <- "Female"
cop$Stage2[str_detect(as.character(cop$Stage), "_M_")] <- "Male"
cop$Stage2[str_detect(as.character(cop$Stage), "_C_")] <- "Cop."
cop$Stage2[str_detect(as.character(cop$Stage), "_J_")] <- "Juv."
cop$Stage2[str_detect(as.character(cop$Stage), "_A_")] <- "Adult"
cop$Stage2[str_detect(as.character(cop$Stage), "_N_")] <- "Nauplii"
cop$Stage2[is.na(cop$Stage2)] <- "Total"

cop$Sp[str_detect(as.character(cop$Stage), "OITO")] <- "Oithona"
cop$Sp[str_detect(as.character(cop$Stage), "MIC")] <- "Microsetella"


#Remove all cases with NA in Abundance
cop <- cop[! is.na(cop$N), ]


#Construct the variable vith date of sample
cop$Date_text <- with(cop, paste(Day,Month,Year, sep = "-"))

cop$Date <- as.POSIXct(as.Date(cop$Date_text, format = "%d-%m-%Y"))



#Combine Males and Females in one category 
cop$Stage3 <- cop$Stage2

cop$Stage3[cop$Stage3 == "Male" | cop$Stage3 == "Female"] <- "Adult"

cop$Stage3[cop$Stage3 == "Juv." | cop$Stage3 == "Nauplii"] <- "Juv."


# unique(cop$Stage3)




#Ordering stages 

cop$Stage3 <- factor(cop$Stage3, levels = c("Adult", "Juv.", "Cop.", "Total") )

#Constructing date of sample as Day from yar beginning

cop$DOY <- as.numeric(strftime(cop$Date, format = "%j"))




total <- cop %>% filter(Depth == "0_10") %>% group_by(Year, DOY, Sp, Stage3) %>% 
  summarise(Log_N = log(mean(N) +1) ) %>% 
  filter(Stage3 != "Total") %>% as.data.frame() 

total$Sp <- factor(total$Sp)



m <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3 , data = total)


summary(m$gam)



layout(matrix(1:9, ncol = 3))
plot(m$gam, scale = 0)
layout(1)



layout(matrix(1:2, ncol = 2))
acf(resid(m$lme), lag.max = 36, main = "ACF")
pacf(resid(m$lme), lag.max = 36, main = "pACF")
layout(1)



ctrl <- list(niterEM = 0, msVerbose = TRUE, optimMethod="L-BFGS-B", maxIter = 150, msMaxIter = 150)

m1 <- gamm(Log_N ~s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 1), control = ctrl)

m2 <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 2), control = ctrl)


m3 <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 3), control = ctrl)


m4 <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 4), control = ctrl)

m5 <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 5), control = ctrl)

m6 <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 6), control = ctrl)


m7 <- gamm(Log_N ~ s(DOY, bs = "cc", by = interaction(Sp, Stage3) ) + s(Year, by = interaction(Sp, Stage3)) + Sp*Stage3, data = total, correlation = corARMA(form = ~ 1|Year, p = 7), control = ctrl)


AIC(m$lme, m1$lme, m2$lme, m3$lme, m4$lme, m5$lme, m6$lme, m7$lme)


anova(m$lme, m1$lme, m2$lme, m3$lme, m4$lme, m5$lme, m6$lme, m7$lme)



layout(matrix(1:12, ncol = 4))
plot(m6$gam, scale = 0)
layout(1)



layout(matrix(1:2, ncol = 2))
res <- resid(m6$lme, type = "normalized")
acf(res, lag.max = 36, main = "ACF - AR(2) errors")
pacf(res, lag.max = 36, main = "pACF- AR(2) errors")
layout(1)



