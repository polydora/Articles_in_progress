library(reshape2)
library(dplyr)
library(stringr)
library(ggplot2)
library(mgcv)




cop_init <- read.table("Data/Oithona_Microsetella.csv", sep = ",", header = T)

names(cop_init)



cop <- melt(cop_init, id.vars = c("Day","Month","Year","Day.of.year"), variable.name = "Stage", value.name = "N") 

unique(cop$Stage)



cop$Depth[str_detect(as.character(cop$Stage), "_1")] <- "0_10"  
cop$Depth[str_detect(as.character(cop$Stage), "_25")] <- "0_25"  
cop$Depth[str_detect(as.character(cop$Stage), "_3")] <- "25_bottom" 
cop$Depth[is.na(cop$Depth)] <- "10_25" 

cop$Stage2[str_detect(as.character(cop$Stage), "_F_")] <- "Female"
cop$Stage2[str_detect(as.character(cop$Stage), "_M_")] <- "Male"
cop$Stage2[str_detect(as.character(cop$Stage), "_C_")] <- "Cop."
cop$Stage2[str_detect(as.character(cop$Stage), "_J_")] <- "Juv."
cop$Stage2[str_detect(as.character(cop$Stage), "_A_")] <- "Adult"
cop$Stage2[str_detect(as.character(cop$Stage), "_N_")] <- "Nauplii"
cop$Stage2[is.na(cop$Stage2)] <- "Total"

unique(cop$Stage2)




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

unique(cop$Stage3)



#Ordering stages 
cop$Stage3 <- factor(cop$Stage3, levels = c("Adult", "Nauplii", "Juv.", "Cop.", "Total") )
                     
unique(cop$Stage3)

#Constructing date of sample as Day from yar beginning


cop$DOY <- as.numeric(strftime(cop$Date, format = "%j"))

cop$Log_N <- log(cop$N + 1)









cop_Oit <- cop %>% filter(Depth == "0_10", Sp == "Oithona", Stage3 != "Total")


means_Oit <- cop_Oit %>% group_by(Stage3, Year) %>% summarise(N_mean = log(mean(N)+1))



Mod_Oit <- gam(Log_N ~ s(Year, DOY, by = Stage3), data = cop_Oit)


plot(Mod_Oit, pages = 1)



new_data_Oit <- expand.grid(Stage3 = unique(cop_Oit$Stage3), DOY = 1:365, Year = seq(min(cop_Oit$Year), max(cop_Oit$Year), 1))

new_data_Oit <- merge(new_data_Oit, means_Oit)


new_data_Oit$Predicted <- predict(Mod_Oit, newdata = new_data_Oit)

new_data_Oit$Anom <- ifelse(new_data_Oit$N_mean < new_data_Oit$Predicted, -1, 1)



ggplot(new_data_Oit, aes(x = Year, y = DOY)) + geom_tile(aes(fill = Anom)) + facet_wrap(~Stage3, nrow = 1) + scale_fill_gradient(low = "white", high = "red") + geom_point(data = cop_Oit, aes(x = Year, y = DOY), size = 0.1) + guides(fill = "none") + theme(axis.text.x = element_text(angle = 90)) + labs(y = "Day of Year") 





cop_Mic <- cop %>% filter( Sp == "Microsetella", Stage3 != "Total")

Mod_Mic <- gam(N ~ s(Year, DOY, by = Stage3), data = cop_Mic, family = "nb")

plot(Mod_Mic, pages = 1)

new_data_Mic <- expand.grid(Stage3 = unique(cop_Mic$Stage3), DOY = 1:365, Year = seq(min(cop_Mic$Year), max(cop_Mic$Year), 1))

new_data_Mic$Predicted <- predict(Mod_Mic, newdata = new_data_Mic)



#The day when the maximum of abundance was observed

days_of_max <- new_data_Mic %>% group_by(Year, Stage3) %>% summarise(Day_of_max = DOY[which(Predicted == max(Predicted))]  )



ggplot(new_data_Mic, aes(x = Year, y = DOY)) + geom_tile(aes(fill = Predicted)) + facet_wrap(~Stage3, nrow = 1) + scale_fill_gradient(low = "white", high = "red") + geom_point(data = cop_Mic, aes(x = Year, y = DOY), size = 0.1) + guides(fill = "none") + theme(axis.text.x = element_text(angle = 90)) + labs(y = "Day of Year") + stat_contour(aes(z = Predicted), bins = 3, color="yellow", size=0.25) 
