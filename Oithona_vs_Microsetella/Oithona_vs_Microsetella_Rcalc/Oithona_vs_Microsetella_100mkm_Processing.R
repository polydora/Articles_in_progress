library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)
library(mgcv)
library(gridExtra)

cop_init_100 <- read.table("Data/Oithona_Microsetella_100_mkm.csv", sep = ",", header = T)


cop100 <- melt(cop_init_100, id.vars = c("Day","Month","Year","Date"), variable.name = "Stage", value.name = "N") 

unique(cop100$Stage)


cop100$Depth[str_detect(as.character(cop100$Stage), "_1")] <- "0_10"  
cop100$Depth[str_detect(as.character(cop100$Stage), "_25")] <- "0_25"  
cop100$Depth[str_detect(as.character(cop100$Stage), "_3")] <- "25_bottom" 
cop100$Depth[is.na(cop100$Depth)] <- "10_25" 

cop100$Stage2[str_detect(as.character(cop100$Stage), "_F_")] <- "Female"
cop100$Stage2[str_detect(as.character(cop100$Stage), "_M_")] <- "Male"
cop100$Stage2[str_detect(as.character(cop100$Stage), "_C_")] <- "Cop."
cop100$Stage2[str_detect(as.character(cop100$Stage), "_J_")] <- "Juv."
cop100$Stage2[str_detect(as.character(cop100$Stage), "_A_")] <- "Adult"
cop100$Stage2[str_detect(as.character(cop100$Stage), "_N_")] <- "Nauplii"
cop100$Stage2[is.na(cop100$Stage2)] <- "Total"

table(cop100$Stage2)


cop100$Sp[str_detect(as.character(cop100$Stage), "OITO")] <- "Oithona"
cop100$Sp[str_detect(as.character(cop100$Stage), "MIC")] <- "Microsetella"


#Remove all cases with NA in Abundance
cop100 <- cop100[! is.na(cop100$N), ]




#Construct the variable vith date of sample
cop100$Date_text <- with(cop100, paste(Day,Month,Year, sep = "-"))

cop100$Date <- as.POSIXct(as.Date(cop100$Date_text, format = "%d-%m-%Y"))



#Combine Males and Females in one category 
cop100$Stage3 <- cop100$Stage2

cop100$Stage3[cop100$Stage3 == "Male" | cop100$Stage3 == "Female"] <- "Adult"

unique(cop100$Stage3)


#Ordering stages 
cop100$Stage3 <- factor(cop100$Stage3, levels = c("Adult", "Nauplii", "Juv.", "Cop.", "Total") )

unique(cop100$Stage3)




#Constructing date of sample as Day from yar beginning

cop100$DOY <- as.numeric(strftime(cop100$Date, format = "%j"))


# Log transformation for abundance

cop100$Log_N <- log(cop100$N + 1)




# Model fitting  for Oithona


cop_Oit <- cop100 %>% filter(Sp == "Oithona", Stage3 != "Total")

Mod_Oit <- gam(Log_N ~ s(Year, DOY, by = Stage3) + Stage3*Depth, data = cop_Oit)



new_data_Oit <- expand.grid(Stage3 = unique(cop_Oit$Stage3), Depth =unique(cop_Oit$Depth),  DOY = 1:365, Year = seq(min(cop_Oit$Year), max(cop_Oit$Year), 1))

new_data_Oit$Predicted <- predict(Mod_Oit, newdata = new_data_Oit)



mean_Oit <- cop_Oit %>% group_by(Stage3) %>% summarise(Mean = mean(Log_N))

new_data_Oit2 <- merge(new_data_Oit, mean_Oit)

new_data_Oit2$Anom <- ifelse(new_data_Oit2$Predicted >= new_data_Oit2$Mean, 1, -1)

ggplot(new_data_Oit2, aes(x = Year, y = DOY)) + geom_tile(aes(fill = Anom)) + facet_grid(Depth~Stage3) + scale_fill_gradient(low = "white", high = "red") + geom_point(data = cop_Oit, aes(x = Year, y = DOY), size = 0.1) + guides(fill = "none") + theme(axis.text.x = element_text(angle = 90)) + labs(y = "Day of Year")



ggplot(new_data_Oit, aes(x = Year, y = DOY)) + geom_tile(aes(fill = Predicted)) + facet_grid(Depth ~ Stage3) + scale_fill_gradient(low = "white", high = "red") + geom_point(data = cop_Oit, aes(x = Year, y = DOY), size = 0.1) + guides(fill = "none") + theme(axis.text.x = element_text(angle = 90)) + labs(y = "Day of Year")


df_Oit <- new_data_Oit2 %>% filter(Anom == 1) 
ggplot(df_Oit, aes(x = Stage3, y =  DOY)) + geom_boxplot() + facet_wrap(~Depth)





#Modelfittng for Microsetella


cop_Mic <- cop100 %>% filter( Sp == "Microsetella", Stage3 != "Total")

Mod_Mic <- gam(Log_N ~ s(Year, DOY, by = Stage3) + Stage3*Depth, data = cop_Mic, family = "gaussian")

# plot(Mod_Mic, pages = 1)

new_data_Mic <- expand.grid(Stage3 = unique(cop_Mic$Stage3),  Depth =unique(cop_Mic$Depth),  DOY = 1:365, Year = seq(min(cop_Mic$Year), max(cop_Mic$Year), 1))

new_data_Mic$Predicted <- predict(Mod_Mic, newdata = new_data_Mic, type = "response")


mean_Mic <- cop_Mic %>% group_by(Stage3) %>% summarise(Mean = mean(Log_N))

new_data_Mic2 <- merge(new_data_Mic, mean_Mic)

new_data_Mic2$Anom <- ifelse(new_data_Mic2$Predicted >= new_data_Mic2$Mean, 1, -1)


ggplot(new_data_Mic2, aes(x = Year, y = DOY)) + geom_tile(aes(fill = Anom)) + facet_grid(Depth ~ Stage3) + scale_fill_gradient(low = "white", high = "red") + geom_point(data = cop_Mic, aes(x = Year, y = DOY), size = 0.1) + guides(fill = "none") + theme(axis.text.x = element_text(angle = 90)) + labs(y = "Day of Year") 

ggplot(new_data_Mic, aes(x = Year, y = DOY)) + geom_tile(aes(fill = Predicted)) + facet_grid(Depth ~ Stage3) + scale_fill_gradient(low = "white", high = "red") + geom_point(data = cop_Mic, aes(x = Year, y = DOY), size = 0.1) + guides(fill = "none") + theme(axis.text.x = element_text(angle = 90)) + labs(y = "Day of Year") 



df_Mic <- new_data_Mic2 %>% filter(Anom == 1) 
ggplot(df_Mic, aes(x = Stage3, y =  DOY)) + geom_boxplot() + facet_wrap(~Depth)





## Distributios of DOY for species and Stages

mean_N <- cop100 %>% group_by(Sp, Stage3, Depth) %>% summarize(Mean_N = mean(N))

df <- merge(cop100, mean_N)

df$Anom <- ifelse(df$N >= df$Mean_N, 1, -1)

ggplot(df[df$N > quantile(df$N,probs = 0.75 ) , ], aes(x = Stage3, y = DOY)) + geom_violin() + facet_wrap(~Sp)


