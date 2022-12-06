library(dplyr)
library(readxl)
library(ggplot2)
library(mgcv)


theme_set(theme_bw() + theme(legend.key = element_blank()))


# Все данные
myt_old_new <- read_excel("Data/Mytilus old and new samples morphotype count 2022.xlsx", na = "NA")


myt_old_new <- myt_old_new %>% filter(Site != "ZRS_subtidal")


monitoring <- read_excel("Data/nt_ne_monitoring_2002_2022.xlsx", na = "NA")



# Карта точек 


# Мониторинговые точки
monitoring_points <- data.frame(Site = c("Лупчостров", "Малый", "Овечий", "Ряжков"), Lat = c(67.147790, 67.117923,67.094935,67.020064 ) , Lon =c(32.355051, 32.406971, 32.452459, 32.570831))



Kand_x <- c(32.2, 34)
Kand_y <- c(66.2, 67.12)


ggWhite_Sea <- read.csv("Data/ggWhite_Sea.csv")


Plot_Kand_upper <-
  ggplot(ggWhite_Sea, aes(x=long, y=lat, group=group)) + 
  geom_polygon(fill = "gray70", colour = "black") + 
  coord_map(xlim = Kand_x, ylim = Kand_y) + 
  theme(axis.ticks=element_blank(), axis.title.x =element_blank(),  axis.title.y= element_blank(), plot.background = element_rect(fill = NULL), panel.grid = element_blank()) +
  theme(axis.text = element_blank())
  # annotate (x= 32.410973,  y = 67.154371, geom =  "point", size = 5, shape = 21, color = "black", fill = "black") 
  # annotate (x= 32.83,  y = 67.154371, geom = "text", label =  "Кандалакша") +



points <- myt_old_new %>% group_by(Site) %>% summarise(Lat = mean(Lat), Lon = mean(Lon))  
matrenin <- points %>% filter(Site == "Matrenin") 

Pl_historical_points <- 
Plot_Kand_upper + 
  geom_point(data = points, aes(x = Lon, y = Lat, group = 1), size = 4, shape = 21, fill = "yellow") +
  geom_point(data = matrenin, aes(x = Lon, y = Lat, group = 1), size = 4, shape = 21, fill = "red") +
  geom_point(data = monitoring_points, aes(x = Lon, y = Lat, group = 1), size = 3, shape = 24, fill ="blue") 
  # ggtitle("Расположение 'исторических' и \nмониторинговых точек сбора")


# Динамика на о.Матренин

matr <- myt_old_new %>% filter(Site == "Matrenin") %>% group_by(Year, Period ) %>% summarise(N_T = sum(N_T), N_E = sum(N_E)) %>% mutate(PropT = N_T / (N_T + N_E))

matr$Year <- as.numeric(matr$Year)

pss_matr <- data.frame(Year = c(2002, 2022), PSS = c(0.03, 0.42))

matr$Period <- factor(matr$Period, labels = c("Переописания", "Исторические сборы") )

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) as.character(round(x,decimals))
}



Pl_matr <- 
ggplot(matr, aes(x = Year, y = PropT)) + 
  geom_segment(data = pss_matr, aes(x = Year, xend = Year, y = 0,  yend = PSS), color = "gray", size = 5) + 
  geom_point(aes(fill = Period), shape = 21,  size = 4) +
  scale_fill_manual(values = c("yellow", "gray20"))+
  labs(fill = "", x = "Годы сбора", y = "Частота T-морфотипа / Значение PSS") +
  scale_x_continuous(breaks = seq(min(matr$Year), max(matr$Year), 3)) + 
  scale_y_continuous(labels = fmt_dcimals(1))+
  theme(legend.position = c(0.2, 0.85)) 


# Остальные точки исторических сборов 

myt_old_new2 <- myt_old_new %>% filter(Site != "Matrenin") %>% group_by(Site, Year, Period) %>% summarise(N_T = sum(N_T), N_E = sum(N_E)) %>% mutate(PropT = N_T / (N_T + N_E))

myt_old_new2$Period <- factor(myt_old_new2$Period, labels = c("Переописания", "Исторические сборы") )

Pl_histor_new <- 
ggplot(myt_old_new2, aes(x = Year, y = PropT)) + 
  geom_point(aes(fill = Period), shape = 21, size = 4, position = position_jitter(width = 0.4)) + 
  labs(fill = "", x = "Годы сбора", y = "Частота T-морфотипа") +
  scale_fill_manual(values = c("yellow", "gray20")) + 
  theme(legend.position = c(0.2, 0.85)) +
  scale_x_continuous(breaks = seq(min(myt_old_new2$Year), max(myt_old_new2$Year), 5))+
  scale_y_continuous(labels = fmt_dcimals(1))


# Динамика в точках мониторинга

monitoring <- monitoring %>% mutate(PropT = Nt / (Nt + Ne))  

monitoring <- monitoring %>% mutate(PropT_adj = case_when(
PropT == 0 ~ 0.000001,
PropT == 1 ~ 0.999999,
PropT != 1 & PropT != 0 ~ PropT))
 
monitoring$Site = factor(monitoring$Site)

Mod_monitor_1 <- gam(PropT_adj ~ s(Year, by = Site, bs = "cr", k = 5) + Site, family = betar(link = "logit"), data = monitoring)



MyData2 <-  monitoring %>% group_by(Site) %>% do(data.frame(Year = seq(min(.$Year), max(.$Year), 1)))
  # monitoring %>% group_by(Site) %>% do(data.frame(Year = seq(min(.$Year), max(.$Year), 1)))   
  # unique(monitoring[,1:2])
  # expand.grid(Year = seq(2002, max(monitoring$Year, 1)), Site = levels(monitoring$Site))

  
MyData2$Predict <- predict(Mod_monitor_1, newdata = MyData2, type = "response", se = T)$fit

MyData2$SE <- predict(Mod_monitor_1, newdata = MyData2, type = "response", se = T)$se.fit

# MyData2[MyData2$Site == "Malij",]
# MyData2[MyData2$Site == "Ovech",]
# MyData2[MyData2$Site == "Ryashkov",]



islands <- as_labeller(c(
  "Lupch" = "остров Б.Лупчостров ",
  "Malij" = "остров Малый",
  "Ovech" = "остров Овечий",
  "Ryashkov" = "остров Ряжков"
))

Pl_monitor <-  
  ggplot(MyData2, aes(x = Year, y = Predict)) + 
  facet_wrap(~ Site, labeller = islands) + 
  geom_point(data = monitoring, aes(x = Year, y = PropT )) + 
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = Predict - 1.96*SE, ymax = Predict + 1.96*SE), alpha = 0.2)+  
  theme_bw()  + 
  labs(y= "Частота Т-морфотипа", x = "Годы") + 
  theme(title = element_text(size =10)) +
  scale_x_continuous(breaks = seq(min(monitoring$Year), max(monitoring$Year), 4)) +
  scale_y_continuous(labels = fmt_dcimals(1))



library(cowplot)


plot_grid(Pl_historical_points, Pl_matr, Pl_histor_new, Pl_monitor,  labels = c("А", "Б", "В", "Г"))
  

########################## Рак в Магадане ###############################

library(betareg)
library(car)

scam_param$Prop_ill[scam_param$Prop_ill == 0] <-0.00001 


Mod <- betareg(Prop_ill ~   scale(N)  + scale(S) + scale(Fetch) + scale(Dist), data = scam_param )

library(performance)

check_collinearity(Mod)

summary(Mod)

tidy(Mod)






library(reshape2)

site_param <-read_excel("Data/Magadan_data.xlsx", na = "NA")
site_param$Fetch <- as.numeric(site_param$Fetch) 

size <- read_excel("Data/Ecological data on mussels.xlsx", na = "NA")

size$L <- round(as.numeric(size$L), 0)

size <- 
  size %>% mutate(L_class = case_when(
    L<=5 ~ "L3",
    L>5 & L<=10 ~ "L8",
    L>10 & L<=15 ~ "L13",
    L>15 & L<=20 ~ "L18",
    L>20 & L<=25 ~ "L23",
    L>25 & L<=30 ~ "L28",
    L>30 & L<=35 ~ "L33",
    L>35 & L<=40 ~ "L38",
    L>40  ~ "L43"
  ))






# size$L_class <- ntile(size$L, n = 15)

scam <-
  size %>% filter(!is.na(L)) %>% group_by(Site, L_class) %>% summarise(N = n(), L_mean = round(mean(L), 0)) %>% dcast(., Site ~ L_class, value.var = "N") 

scam[is.na(scam)] <- 0



# dd <- size %>% filter(!is.na(L)) %>% group_by(L_class) %>% summarise(N = n(), L_mean = round(mean(L), 0)) 
# 
# names(scam) <- c("Site", paste("L_", unique(dd$L_mean), sep = ""))
# 


scam_param <- merge(site_param, scam)

scam_param$Prop_ill <- scam_param$N_ill/scam_param$Sample_size

scam <- scam_param %>% select(L13, L18, L23, L28,  L3, L33, L38, L43,  L8)




library(vegan)


scam_rel <- decostand(scam, method = "hellinger") 

scam_rel_rda <- rda(scam_rel)
summary(scam_rel_rda)




library(ggvegan)
library(ggrepel)


autoplot(scam_rel_rda)

PC <- fortify(scam_rel_rda)
 
PC$Label[(ncol(scam)+1):(ncol(scam) + nrow(scam))] <- scam_param$Site


PC_site <- PC %>% filter(Score == "sites")
PC_site$Prop_ill <- scam_param$Prop_ill
PC_site$Prop_ill[PC_site$Prop_ill == 0] <- 0.00001 
PC_site$Fetch <- scam_param$Fetch

PC_size_class <- PC %>% filter(Score == "species")



Pl_PC_site <-
ggplot(PC_site, aes(PC1, PC2)) + 
  geom_point(aes(size = Prop_ill),color = "red") + 
  geom_hline(yintercept = 0, linetype = 2) + 
  geom_vline(xintercept = 0, linetype = 2) +
  guides(size = "none") +
  labs(x = "PC1 (47%)", y = "PC2 (35%)") + 
  geom_segment(data = PC_size_class, aes(x = 0, y = 0, xend = PC1, yend = PC2), arrow = arrow(type = "closed", angle = 5), color = "blue") +
  geom_text_repel(data = PC_size_class, aes(label = Label),  point.padding = 2)




size$Site <- factor(size$Site, levels = PC_site$Label[order(PC_site$Prop_ill, decreasing = T)])

PC_site$Site = PC_site$Label

PC_site$Site <- factor(PC_site$Site, levels = PC_site$Label[order(PC_site$Prop_ill, decreasing = T)])

ggplot(size, aes(x = L)) + 
  geom_histogram() + 
  facet_wrap(~Site, ncol = 4) + 
  geom_text(data = PC_site, aes(x = 30, y= 65, label = paste("BTN:", round(PC_site$Prop_ill, 3))), size = 3 )


# 
# 
# Pl_PC1_min <- 
#   size %>% filter(Site %in% PC_site$Site[which.min(PC_site$PC1)]) %>% 
#   ggplot(., aes(x= L)) + geom_histogram(binwidth = 5) + xlim(0, max(size$L, na.rm = T)) +
#   labs(x = "Размер", y = "Частота")
# 
# 
# Pl_PC1_max <- 
#   size %>% filter(Site %in% PC_site$Site[which.max(PC_site$PC1)]) %>% 
#   ggplot(., aes(x= L)) + geom_histogram(binwidth = 5)+ xlim(0, max(size$L, na.rm = T)) + 
#   labs(x = "Размер", y = "Частота")
# 
# 


plot_grid(Pl_PC1_min, Pl_PC_site, Pl_PC1_max, nrow = 1, rel_widths  = c(0.5, 1, 0.5), labels = c("A", "Б", "B")) 









Mod_PC <- betareg(Prop_ill ~   PC1 + PC2, data = PC_site )

library(performance)

check_collinearity(Mod_PC)

summary(Mod_PC)





