# Первичное преобразование данных. 

# Packages  ####
library(reshape2)
library(ggplot2)
library(dplyr)


# Подготовка данных ####
plankt_wide <- read.csv("data/Plankton_raw_tidy.csv", header = TRUE)
vars <- read.csv("data/Plankton_raw_tidy_variables.csv", header = T)


d <- (melt(plankt_wide, id.vars = c("Day", 	"Month", 	"Year", 	"Data"), variable.name = "Var_ID", value.name = "Abundance"))


plankt <- merge(d, vars)



# Первичный осмотр данных ####

ggplot(plankt[plankt$Species == "Microsetella norvegica", ], aes(x = Abundance, y = 1:nrow(plankt[plankt$Species == "Microsetella norvegica", ]))) + geom_point()

# Вопрос Это ошибка в набивке или реально так много.
plankt[plankt$Abundance > 10000 & !is.na(plankt$Abundance) & plankt$Species == "Microsetella norvegica",  ]

plankt$Date2 <- strptime(paste(plankt$Day,"/", plankt$Month, "/", plankt$Year, sep = ""), format=("%d/%m/%Y"))

Start_day <- strptime(paste(plankt$Year,"/01/01", sep = ""), format=("%Y/%d/%m"))


plankt$Days_from_year_start <-  as.numeric(round(difftime(plankt$Date2, as.Date(Start_day))))

hist(plankt$Days_from_year_start)


## Среденвзвешанные значения, объединение слоев

plankt_total <- plankt[plankt$Stage == "Total", ] %>% group_by(Species, Year, Month, Day) %>% summarise(Abundance_0_10 = sum(Abundance[Level == "0-10"]), Abundance_10_25 =  sum(Abundance[Level == "10-25"]), Abundance_25_bottom =  sum(Abundance[Level == "25-bottom"]) )

plankt_total$N_weig <- round((plankt_total$Abundance_0_10 * 10 + plankt_total$Abundance_10_25 * 15 + plankt_total$Abundance_25_bottom * 40)/65, 1)  


plankt_total$N_weig_25 <- round((plankt_total$Abundance_0_10 * 10 + plankt_total$Abundance_10_25 * 15)/25, 1)  





# 
# plankt_1 <- plankt[plankt$Level == "0-10", ]
# nrow(plankt_1)
# 
# plankt_2 <- plankt[plankt$Level == "10-25", ]
# nrow(plankt_2)
# 
# plankt_3 <- plankt[plankt$Level == "25-bottom", ]
# nrow(plankt_3)
# 
# 
# names(plankt_1)

# plankt_mean <- plankt_1
# 
# plankt_mean$Abund_mean <- (plankt_1$Abundance*10 + plankt_2$Abundance*15 + plankt_3$Abundance *40 )/65
# 
# qplot(plankt_mean$Abund_mean, plankt_mean$Abundance)


# names(plankt_mean)


## Среденвзвешанные значения, объединение слоев

plankt_1 <- plankt[plankt$Level == "0-10", ]
nrow(plankt_1)

plankt_2 <- plankt[plankt$Level == "10-25", ]
nrow(plankt_2)

plankt_3 <- plankt[plankt$Level == "25-bottom", ]
nrow(plankt_3)


names(plankt_1)

plankt_mean <- plankt_1

plankt_mean$Abund_mean <- (plankt_1$Abundance*10 + plankt_2$Abundance*15)/25

qplot(plankt_mean$Abund_mean, plankt_mean$Abundance)


names(plankt_mean)

# plankt_mean <- plankt_mean[, -6]



# names(plankt_mean)

# plankt_mean <- plankt_mean[, -8]

# names(plankt_mean)


## plankt_mean - dataframe  с усреденными обилиями по горизонтам 0-10 + 10-25





# Многолетние значения средних по летним месяцам  ####


Abundance <- read.csv("data/abundance.csv", header = TRUE) # Данные по обилию видов, усреденнные по всему столбу воды за март-сентябрь.


# ggplot(plankt_mean[plankt_mean$Stage == "Total" & plankt$Month %in% 6:9, ], aes(x = Year, y = (Abund_mean)))  + stat_summary(fun.data = "mean_cl_boot") + facet_wrap(~Species, scales = "free_y") + geom_smooth() 
# 
# colSums(is.na(plankt_mean[plankt_mean$Stage == "Total" & plankt$Month %in% 6:9, ]))
# 
              

# 
# # Находим пики в сезоны
# 
# names(plankt_mean)
# 
# 
# 
# plankt_mean$Date2 <- strptime(paste(plankt_mean$Day,"/", plankt_mean$Month, "/", plankt_mean$Year, sep = ""), format=("%d/%m/%Y"))
# 
# Start_day <- strptime(paste(plankt_mean$Year,"/01/01", sep = ""), format=("%Y/%d/%m"))
# 
# 
# plankt_mean$Days_from_year_start <-  as.numeric(round(difftime(plankt_mean$Date2, as.Date(Start_day))))
# 
# names(plankt_mean)
# 
# 

## Находим средние плотности для каждого вида в период с мая по октябрь


plankt_total$Date2 <- strptime(paste(plankt_total$Day,"/", plankt_total$Month, "/", plankt_total$Year, sep = ""), format=("%d/%m/%Y"))


plankt_total$Month <- months(plankt_total$Date2)

month_included <- if(.Platform$OS.type == "unix") {c("мая", "июня", "июля", "августа","сентября", "октября")}else {c("Май", "Июнь", "Июль", "Август","Сентябрь", "Октябрь")} 
  
  
  
  

plankt_total_summer <- plankt_total[plankt_total$Month %in% month_included, ]


table(plankt_total_summer$Month) 

Abundance_summer <-plankt_total_summer %>% group_by(Species, Year, Month, Day) %>% summarise(N = sum(N_weig, na.rm = TRUE)) %>% group_by (Species, Year)  %>% summarise(Mean_N = mean(N, na.rm = TRUE))

 

Abundance_summer <- round(dcast(Abundance_summer, formula = Year ~ Species ))
 

Abundance_summer_25 <-plankt_total_summer %>% group_by(Species, Year, Month, Day) %>% summarise(N = sum(N_weig_25, na.rm = TRUE)) %>% group_by (Species, Year)  %>% summarise(Mean_N = mean(N, na.rm = TRUE))



Abundance_summer_25 <- round(dcast(Abundance_summer_25, formula = Year ~ Species ))



qplot(x = Abundance_summer_25$`Calanus glacialis`, y = Abundance_summer$`Calanus glacialis`) + geom_abline()
qplot(x = Abundance_summer_25$`Pseudocalanus spp.`, y = Abundance_summer$`Pseudocalanus spp.`)+ geom_abline()
qplot(x = Abundance_summer_25$`Acartia spp.`, y = Abundance_summer$`Acartia spp.`) + geom_abline()
qplot(x = Abundance_summer_25$`Centropages hamatus`, y = Abundance_summer$`Centropages hamatus`) + geom_abline()
qplot(x = Abundance_summer_25$`Oithona similis`, y = Abundance_summer$`Oithona similis`) + geom_abline()
qplot(x = Abundance_summer_25$`Temora longicornis`, y = Abundance_summer$`Temora longicornis`) + geom_abline()
qplot(x = Abundance_summer_25$`Microsetella norvegica`, y = Abundance_summer$`Microsetella norvegica`) + geom_abline()

# write.table(Abundance_summer, "clipboard", sep = "\t", row.names = F)
# write.table(Abundance_summer_25, "clipboard", sep = "\t", row.names = F)



ggplot(plankt_mean[plankt_mean$Stage == "Total" & plankt$Month %in% 6:9, ], aes(x = Year, y = (Abund_mean)))  + stat_summary(fun.data = "mean_cl_boot") + facet_wrap(~Species, scales = "free_y") + geom_smooth() 

colSums(is.na(plankt_mean[plankt_mean$Stage == "Total" & plankt$Month %in% 6:9, ]))

              


# Находим пики в сезоны

names(plankt_mean)



plankt_mean$Date2 <- strptime(paste(plankt_mean$Day,"/", plankt_mean$Month, "/", plankt_mean$Year, sep = ""), format=("%d/%m/%Y"))

Start_day <- strptime(paste(plankt_mean$Year,"/01/01", sep = ""), format=("%Y/%d/%m"))


plankt_mean$Days_from_year_start <-  as.numeric(round(difftime(plankt_mean$Date2, as.Date(Start_day))))

names(plankt_mean)





##разделяем датасет на части по видам



#У калянуса объединяем данные по численности копеподитов 1, 2 и 3
calanus_all <- plankt_mean[plankt_mean$Species == "Calanus glacialis", ]

calanus <- calanus_all[calanus_all$Stage == "cop I", ]

calanus1 <- calanus_all[calanus_all$Stage == "cop I", ]
nrow(calanus1)
sum(calanus1$Abund_mean, na.rm = T)

calanus2 <- calanus_all[calanus_all$Stage == "cop II", ]
nrow(calanus2)
sum(calanus2$Abund_mean, na.rm = T)

calanus3 <- calanus_all[calanus_all$Stage == "cop III", ]
nrow(calanus3)
sum(calanus3$Abund_mean, na.rm = T)

calanus$Abundance <- calanus1$Abundance + calanus2$Abundance + calanus3$Abundance  

calanus$Abund_mean <- calanus1$Abund_mean + calanus2$Abund_mean + calanus3$Abund_mean  
nrow(calanus)

calanus$Abund_mean2 <- calanus$Abund_mean
calanus$Abund_mean2[is.na(calanus$Abund_mean)] <- 0
calanus <- calanus[!is.na(calanus$Year), ]






centrop <- plankt_mean[plankt_mean$Species == "Centropages hamatus", ]
centrop <- centrop[centrop$Stage == "cop.", ]
centrop$Abund_mean2 <- centrop$Abund_mean
centrop$Abund_mean2[is.na(centrop$Abund_mean)] <- 0 
centrop <- centrop[!is.na(centrop$Year), ]


temor <- plankt_mean[plankt_mean$Species == "Temora longicornis", ]
temor <- temor[temor$Stage == "cop.", ]
temor$Abund_mean2 <- temor$Abund_mean
temor$Abund_mean2[is.na(temor$Abund_mean)] <- 0 
temor <- temor[!is.na(temor$Year), ]


oit <- plankt_mean[plankt_mean$Species == "Oithona similis", ]
oit <- oit[oit$Stage == "cop.", ]
oit$Abund_mean2 <- oit$Abund_mean
oit$Abund_mean2[is.na(oit$Abund_mean)] <- 0 
oit <- oit[!is.na(oit$Year), ]



pseudocal <- plankt_mean[plankt_mean$Species == "Pseudocalanus spp.", ]
pseudocal <-pseudocal[pseudocal$Stage == "cop.", ]
pseudocal$Abund_mean2 <- pseudocal$Abund_mean
pseudocal$Abund_mean2[is.na(pseudocal$Abund_mean)] <- 0 
pseudocal <- pseudocal[!is.na(pseudocal$Year), ]


acart <- plankt_mean[plankt_mean$Species == "Acartia spp.", ]
acart <-acart[acart$Stage == "cop.", ]
acart$Abund_mean2 <- acart$Abund_mean
acart$Abund_mean2[is.na(acart$Abund_mean)] <- 0 
acart <- acart[!is.na(acart$Year), ]

microset <- plankt_mean[plankt_mean$Species == "Microsetella norvegica", ]
microset <-microset[microset$Stage == "cop.", ]
microset$Abund_mean2 <- microset$Abund_mean
microset$Abund_mean2[is.na(microset$Abund_mean)] <- 0 
microset <- microset[!is.na(microset$Year), ]


tricon <- plankt_mean[plankt_mean$Species == "Triconia spp.", ]
tricon <-tricon[tricon$Stage == "Total", ]
tricon$Abund_mean2 <- tricon$Abund_mean
tricon$Abund_mean2[is.na(tricon$Abund_mean)] <- 0 
tricon <- tricon[!is.na(tricon$Year), ]




## Строим накопленную сумму для каждого вида 
library (plyr)


Total_cum_calanus <- with(data = calanus, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_calanus <- ldply (Total_cum_calanus, data.frame)
df_count_Total_calanus$Date2 <- calanus$Date2[order(calanus$Date2)]
names(df_count_Total_calanus) <- c("Year", "N_accum", "Date2")
df_count_Total_calanus$Time <- as.numeric(df_count_Total_calanus$Date2)
df_count_Total_calanus$Time2 <- as.POSIXct(df_count_Total_calanus$Time, origin = "1970-01-01", tz = "UTC")           

qplot(df_count_Total_calanus$Year,df_count_Total_calanus$Time )



Total_cum_centrop <- with(data = centrop, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_centrop <- ldply (Total_cum_centrop, data.frame)
df_count_Total_centrop$Date2 <- centrop$Date2 [order(centrop$Date2)]
names(df_count_Total_centrop) <- c("Year", "N_accum", "Date2")
df_count_Total_centrop$Time <- as.numeric(df_count_Total_centrop$Date2)
df_count_Total_centrop$Time2 <- as.POSIXct(df_count_Total_centrop$Time, origin = "1970-01-01", tz = "UTC")           

qplot(df_count_Total_centrop$Year,df_count_Total_centrop$Time)



Total_cum_temor <- with(data = temor, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_temor <- ldply (Total_cum_temor, data.frame)
df_count_Total_temor$Date2 <- temor$Date2 [order(temor$Date2)]
names(df_count_Total_temor) <- c("Year", "N_accum", "Date2")
df_count_Total_temor$Time <- as.numeric(df_count_Total_temor$Date2)
df_count_Total_temor$Time2 <- as.POSIXct(df_count_Total_temor$Time, origin = "1970-01-01", tz = "UTC")  

qplot(df_count_Total_temor$Year,df_count_Total_temor$Time )



Total_cum_oit <- with(data = oit, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_oit <- ldply (Total_cum_oit, data.frame)
df_count_Total_oit$Date2 <- oit$Date2[order(oit$Date2)]
names(df_count_Total_oit) <- c("Year", "N_accum", "Date2")
df_count_Total_oit$Time <- as.numeric(df_count_Total_oit$Date2)
df_count_Total_oit$Time2 <- as.POSIXct(df_count_Total_oit$Time, origin = "1970-01-01", tz = "UTC")      

qplot(df_count_Total_oit$Year,df_count_Total_oit$Time )


Total_cum_pseudocal <- with(data = pseudocal, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_pseudocal <- ldply (Total_cum_pseudocal, data.frame)
df_count_Total_pseudocal$Date2 <- pseudocal$Date2[order(pseudocal$Date2)]
names(df_count_Total_pseudocal) <- c("Year", "N_accum", "Date2")
df_count_Total_pseudocal$Time <- as.numeric(df_count_Total_pseudocal$Date2)
df_count_Total_pseudocal$Time2 <- as.POSIXct(df_count_Total_pseudocal$Time, origin = "1970-01-01", tz = "UTC")      

qplot(df_count_Total_pseudocal$Year,df_count_Total_pseudocal$Time )



Total_cum_acart <- with(data = acart, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_acart <- ldply (Total_cum_acart, data.frame)
df_count_Total_acart$Date2 <- acart$Date2[order(acart$Date2)]
names(df_count_Total_acart) <- c("Year", "N_accum", "Date2")
df_count_Total_acart$Time <- as.numeric(df_count_Total_acart$Date2)
df_count_Total_acart$Time2 <- as.POSIXct(df_count_Total_acart$Time, origin = "1970-01-01", tz = "UTC")      

qplot(df_count_Total_acart$Year,df_count_Total_acart$Time )



Total_cum_microset <- with(data = microset, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_microset <- ldply (Total_cum_microset, data.frame)
df_count_Total_microset$Date2 <- microset$Date2[order(microset$Date2)]
names(df_count_Total_microset) <- c("Year", "N_accum", "Date2")
df_count_Total_microset$Time <- as.numeric(df_count_Total_microset$Date2)
df_count_Total_microset$Time2 <- as.POSIXct(df_count_Total_microset$Time, origin = "1970-01-01", tz = "UTC")      

qplot(df_count_Total_microset$Year,df_count_Total_microset$Time )




Total_cum_tricon <- with(data = tricon, tapply(Abund_mean2, INDEX=list(Year), FUN=function(x) as.numeric(cumsum(x))))
df_count_Total_tricon <- ldply (Total_cum_tricon, data.frame)
df_count_Total_tricon$Date2 <- tricon$Date2[order(tricon$Date2)]
names(df_count_Total_tricon) <- c("Year", "N_accum", "Date2")
df_count_Total_tricon$Time <- as.numeric(df_count_Total_tricon$Date2)
df_count_Total_tricon$Time2 <- as.POSIXct(df_count_Total_tricon$Time, origin = "1970-01-01", tz = "UTC")      

qplot(df_count_Total_tricon$Year,df_count_Total_tricon$Time )



#########################


## Подбираем модели для описания накопленной суммы для Calanus

# Величины для описания фенологии видов:
# Юлианский день, когда накопленная сумма превысила 50%



# Функция для вычисления параметров логистической кумуляты на выходе три парамтера логистической кривой

log_param <- function(df, species = NULL){
  df_param <- data.frame(Year = rep(NA, length(unique(df$Year))), Asym = NA, xmid = NA, scal = NA, RSE = NA, df =NA)
  i <- 1
  for(year in unique(df$Year)){
    fit <- tryCatch(expr = nls(N_accum ~ SSlogis(Time, Asym, xmid, scal), data = df[df$Year == year, ]), error = function(e)NA)
    
    df_param$Year[i] <-  year
    
    if(is.na(fit)) {df_param$Asym[i] <-  df_param$xmid[i] <- df_param$scal[i] <- NA}
    else{
      df_param$Asym[i] <-  coef(fit)[1]
      df_param$xmid[i] <- coef(fit)[2]
      df_param$scal[i] <- coef(fit)[3]
      df_param$RSE[i] <- summary(fit)$sigma
      df_param$df[i] <- summary(fit)$df
    }
    i <- i+1
  }
  
  df_param$perc_15 <- with(df_param, xmid - log(Asym/(0.15*Asym) - 1)*scal )
  df_param$perc_50 <- with(df_param, xmid - log(Asym/(0.5*Asym) - 1)*scal )
  df_param$perc_85 <- with(df_param, xmid - log(Asym/(0.85*Asym) - 1)*scal )
  df_param$Time_perc_15 <- as.POSIXct(df_param$perc_15, origin = "1970-01-01", tz = "UTC")
  df_param$Time_perc_50 <- as.POSIXct(df_param$perc_50, origin = "1970-01-01", tz = "UTC") 
  df_param$Time_perc_85 <- as.POSIXct(df_param$perc_85, origin = "1970-01-01", tz = "UTC") 
  
  df_param$Start_day <- as.POSIXct(paste(df_param$Year,"-01-01", sep = ""), tz = "UTC") 
  df_param$Days_perc_15 <- as.numeric(round(difftime( df_param$Time_perc_15, as.Date(df_param$Start_day))))
  df_param$Days_perc_50 <- as.numeric(round(difftime( df_param$Time_perc_50, as.Date(df_param$Start_day))))
  df_param$Days_perc_85 <- as.numeric(round(difftime( df_param$Time_perc_85, as.Date(df_param$Start_day))))
  
  df_param$Species = species
  df_param
    
}

########################

log_param_Calanus <- log_param(df = df_count_Total_calanus, species = "Calanus")
log_param_Centropages <- log_param(df = df_count_Total_centrop, species = "Centropages")
log_param_Temora <- log_param(df = df_count_Total_temor, species = "Temora")
log_param_Oithona <- log_param(df = df_count_Total_oit, species = "Oithona")
log_param_Pseudocalanus <- log_param(df = df_count_Total_pseudocal, species = "Pseudocalanus")
log_param_Acartia <- log_param(df = df_count_Total_acart, species = "Acartia")
log_param_Microsetella <- log_param(df = df_count_Total_microset, species = "Microsetella")
log_param_Triconia <- log_param(df = df_count_Total_tricon, species = "Triconia")



## Смотрим есть ли зависимость между асимптотой кумулятивной крвой (максимальное количество особей отмеченных в данном году) и датой начала сезона



ggplot(log_param_Calanus, aes(x = Days_perc_15, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Centropages, aes(x = Days_perc_15, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Temora, aes(x = Days_perc_15, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Oithona, aes(x = Days_perc_15, y = log(Asym) )) + geom_point()+ geom_smooth(method = "lm")
ggplot(log_param_Pseudocalanus, aes(x = Days_perc_15, y = log(Asym) )) + geom_point()+ geom_smooth(method = "lm")
ggplot(log_param_Acartia, aes(x = Days_perc_15, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Microsetella, aes(x = Days_perc_15, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")



# Функция для определения характеристик кумуляты

cum_param <- function(df, species = NULL){
  df_param <- data.frame(Year = rep(NA, length(unique(df$Year))), Mean = NA)
  i <- 1
  for(year in unique(df$Year)){
    df_param$Mean[i] <- mean(df$N_accum[df$Year == year])
    # df_param$Mean[i] <- max(df$N_accum[df$Year == year])/2
    df_param$Year[i] <-  year
    days <- which( (df$N_accum[df$Year == year] - df_param$Mean[i])^2 == min((df$N_accum[df$Year == year] - df_param$Mean[i])^2) )[1]
    
    df_param$xmid[i] <- (as.numeric(df[df$Year == year,]$Date2[days]))
    i <- i+1
  }
  
  df_param$Time_Mean <- as.POSIXct(df_param$xmid, origin = "1970-01-01", tz = "UTC")
# 
#   df_param$Start_day <- as.POSIXct(paste(df_param$Year,"-01-01", sep = ""), tz = "UTC")
# 
#   df_param$Days_perc_50 <- as.numeric(round(difftime( df_param$Time_perc_50, as.Date(df_param$Start_day))))

  df_param$Species = species
  df_param
  
}


cum_param(df = df_count_Total_calanus, species = "Calanus")


##Даты пиков численности видов

peaks <- function(df, species = NULL){
  df_param <- data.frame(Year = rep(NA, length(unique(df$Year))))
  i <- 1
  for(year in unique(df$Year)){
    df_param$Year[i] <-  year
    df_param$Peak_Days_from_year_start[i] <- mean(df$Days_from_year_start[df$Year == year & df$Abund_mean2 == max(df$Abund_mean2[df$Year == year])])
    i <- i + 1
  }
  df_param
  
}

log_param_Calanus <- merge(log_param_Calanus, peaks(df = calanus), by="Year")
log_param_Centropages <- merge(log_param_Centropages, peaks(df = centrop), by="Year")
log_param_Temora <- merge(log_param_Temora, peaks(df = temor), by="Year")
log_param_Oithona <- merge(log_param_Oithona, peaks(df = oit), by="Year")
log_param_Pseudocalanus<- merge(log_param_Pseudocalanus, peaks(df = pseudocal), by="Year")
log_param_Acartia <- merge(log_param_Acartia, peaks(df = acart), by="Year")
log_param_Microsetella <- merge(log_param_Microsetella, peaks(df = microset), by="Year")
log_param_Triconia <- merge(log_param_Triconia, peaks(df = tricon), by="Year")



log_param_all_species <- rbind(log_param_Calanus, log_param_Centropages, log_param_Temora, log_param_Oithona, log_param_Pseudocalanus, log_param_Acartia, log_param_Microsetella, log_param_Triconia)




# write.table(log_param_Calanus, "clipboard", sep = "\t", row.names = FALSE)

# write.table(log_param_Calanus, "clipboard", sep = "\t", row.names = FALSE)


# Функция для рисования кумулят






plot_cum <- function(df =df_count_Total_calanus,  year = 1961){
  library(ggplot2)
  fit <- tryCatch(expr = nls(N_accum ~ SSlogis(Time, Asym, xmid, scal), data = df[df$Year == year, ]), error = function(e)NA)
  
  Asym <- coef(fit)[1]
  xmid <- coef(fit)[2]
  scal <- coef(fit)[3]
  
  
  perc_15 <- xmid - log(Asym/(0.15*Asym) - 1)*scal 
  perc_50 <- xmid - log(Asym/(0.5*Asym) - 1)*scal 
  perc_85 <- xmid - log(Asym/(0.85*Asym) - 1)*scal
  
  
  Time_perc_15 <- as.POSIXct(perc_15, origin = "1970-01-01", tz = "UTC")
  Time_perc_50 <- as.POSIXct(perc_50, origin = "1970-01-01", tz = "UTC") 
  Time_perc_85 <- as.POSIXct(perc_85, origin = "1970-01-01", tz = "UTC") 
  
  Start_day <- as.POSIXct(paste(year,"-01-01", sep = ""), tz = "UTC") 
  Days_perc_15 <- as.numeric(round(difftime(Time_perc_15, as.Date(Start_day))))
  Days_perc_50 <- as.numeric(round(difftime(Time_perc_50, as.Date(Start_day))))
  Days_perc_85 <- as.numeric(round(difftime( Time_perc_85, as.Date(Start_day))))
  
    
  if(is.na(fit[1])){
    Pl <- ggplot(df[df$Year == year, ], aes(x = Time2, y = N_accum))  + scale_x_datetime(date_breaks = "4 week", date_labels = "%d-%m")+ geom_point(size = 2, shape = 21, fill = "gray")
    
  }
  else{
    df_predict <- data.frame(Date2 = seq(as.Date(paste(year,"-01-01", sep = "")), as.Date(paste(year,"-12-20", sep = "")), by="days"))
    df_predict$Time2 <- as.POSIXct(df_predict$Date2)
    df_predict$Time <- as.numeric(df_predict$Time2) 
    df_predict$predict <- predict(fit, newdata = df_predict)
    
    
    Pl <- ggplot(df[df$Year == year, ], aes(x = Time2, y = N_accum))  + geom_line(data = df_predict, aes(y = predict), size = 2) + scale_x_datetime(date_breaks = "6 week", date_labels = "%d-%m")+ geom_point(size = 2, shape = 21, fill = "gray")
    
    Pl <- Pl + 
      geom_segment(aes(x= Time_perc_50, xend = Time_perc_50, yend = 0, y = predict(fit, newdata = data.frame(Time = perc_50))[1]), arrow = arrow(type = "closed", angle = 10), color = "gray") + 
      geom_segment(aes(x= Time_perc_15, xend = Time_perc_15, yend = 0, y = predict(fit, newdata = data.frame(Time = perc_15))[1]), arrow = arrow(type = "closed", angle = 10), color = "gray90")+ 
      geom_segment(aes(x= Time_perc_85, xend = Time_perc_85, yend = 0, y = predict(fit, newdata = data.frame(Time = perc_85))[1]), arrow = arrow(type = "closed", angle = 10), color = "gray10")
  }
  
  Pl
}






# cum <- df_count_Total_calanus$N_accum[df_count_Total_calanus$Year == year]
# 
# plot_cum(year = year, df = df_count_Total_calanus) + geom_hline(yintercept = mean(cum)) + geom_hline(yintercept = max(cum)/2, color = "blue") + geom_line(aes())


## Выбираем логистические регрессии с максимальной, среденей и минимальной Residual Standard Error

log_param_all_species %>% filter(round(RSE) == round(max(RSE, na.rm = T)))


(Pl_max_RSE <- plot_cum(year = 1989, df = df_count_Total_oit) + geom_line(color = "gray") + theme_bw() + ggtitle("Max RSE. Oithona, 1989") + labs(x = "Date", y = "Abundance" ) + theme(panel.grid = element_blank()))  


log_param_all_species %>% filter(round(RSE) == round(min(RSE, na.rm = T)))


Pl_min_RSE <- plot_cum(year = 1999, df = df_count_Total_calanus) + geom_line(color = "gray") + theme_bw()+ ggtitle("Min RSE. Calanus, 1999") + labs(x = "Date", y = "Abundance" )  + theme(panel.grid = element_blank())
  


log_param_all_species %>% filter(round(RSE) == round(median(RSE, na.rm = T)))

Pl_median_RSE <- plot_cum(year = 1977, df = df_count_Total_temor) + geom_line(color = "gray") + theme_bw()+ ggtitle("Medium RSE. Temora, 1977")  + labs(x = "Date", y = "Abundance" )+ theme(panel.grid = element_blank())
  
library(cowplot)

Pl_logistic <- plot_grid(Pl_max_RSE, Pl_median_RSE, Pl_min_RSE, ncol = 1, align = "v")








######### Анализ качества регрессионных логистических моделей.++++++++++++++++++++
## Для оценки goodness of feet логистических моделей сравнивали соответствие Days_perc_50 и Peak_Days_from_year_start



log_param_all_species$Species <- factor(log_param_all_species$Species) 

log_param_all_species$Peak_Dif <- with(log_param_all_species, Days_perc_50 - Peak_Days_from_year_start)



# РАспределение RSE от логистической кривой, описывающей кумуляту у видов
ggplot(log_param_all_species, aes(x = Species, y = RSE)) + geom_boxplot() + geom_hline(yintercept = 0)

ggplot(log_param_all_species, aes(x = as.numeric(Year), y = RSE)) + geom_point() +facet_wrap(~Species) + geom_smooth()


log_param_all_species %>% filter(RSE == max(RSE, na.rm = T))




# Распределение разниц в днях между Days_perc_50 и Peak_Days_from_year_start 
Plot_diff <- ggplot(log_param_all_species, aes(x = Species, y = Peak_Dif)) + geom_boxplot() + geom_hline(yintercept = 0) + theme_bw() + labs(y = "Difference Middle and Peak") + theme(axis.text.x = element_text(angle = 90))




ggplot(log_param_all_species, aes(x = Peak_Dif)) + geom_histogram() + geom_vline(xintercept = 0) + facet_wrap(~Species)




# Комбинированная картинка с примерами логистических аппроксимаций и разницей между датой точки перегиба логистической кривой и датой наблюдаемого пика

plot_grid(Pl_logistic, Plot_diff, labels = c("A", "B"))



ggplot(log_param_all_species, aes(x = Days_perc_50, y = Peak_Days_from_year_start, color = Species)) + geom_point() + geom_smooth(method = "lm") + geom_abline() + facet_wrap(~Species)

# detach(reshape2)

library(dplyr)
log_param_all_species %>% group_by(Species) %>% summarise(Median_Peak_Dif = median(Peak_Dif, na.rm = T)) 

log_param_all_species %>% group_by(Species) %>%   summarise(cor(Peak_Days_from_year_start, Days_perc_50, use = "pairwise.complete.obs", method = "spearman"))


df_count_Total_acart %>% group_by(Year) %>% summarize(n())

ggplot(log_param_all_species, aes(x = as.numeric(Year), y = Peak_Dif)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth()


ggplot(log_param_all_species, aes(x = as.numeric(Year), y = Peak_Dif)) + geom_point() + geom_hline(yintercept = 0) + geom_smooth() + facet_wrap(~Species)




ggplot(log_param_Calanus, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Centropages, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Temora, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Oithona, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point()+ geom_smooth(method = "lm")
ggplot(log_param_Pseudocalanus, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point()+ geom_smooth(method = "lm")
ggplot(log_param_Acartia, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")
ggplot(log_param_Microsetella, aes(x = Peak_Days_from_year_start, y = log(Asym) )) + geom_point() + geom_smooth(method = "lm")














## Функция для поиска дат макисмально близких к опорным ключевым событиям в фенологии (начало появления, конец пребывания) для тех случаев, когда кумулята не выходит на плато.#######

date_search <- function(df = df_count_Total_oit, year = 2014, Begin_thresh = 0.15, End_thresh = 0.85){
  df_year <- df[df$Year == year, ]
  Max <- max(df_year$N_accum)
  Begin <- Max*Begin_thresh
  End <- Max*End_thresh
  Meadle <- (Begin + End)/2
  
  Dev_Begin <- (df_year$N_accum - Begin)^2
  Dev_End <- (df_year$N_accum - End)^2
  Dev_Meadle <- (df_year$N_accum - Meadle)^2

  Date_Start <- (as.POSIXct(paste(year,"-01-01", sep = ""), origin = "1970-01-01", tz = "UTC"))
  
  Date_Begin <- as.POSIXct(df_year[which(Dev_Begin == min(Dev_Begin)), ]$Date2, origin = "1970-01-01", tz = "UTC") 
  Date_End <- as.POSIXct(df_year[which(Dev_End == min(Dev_End)), ]$Date2, origin = "1970-01-01", tz = "UTC") 
  Date_Meadle <- as.POSIXct(df_year[which(Dev_Meadle == min(Dev_Meadle)), ]$Date2, origin = "1970-01-01", tz = "UTC") 
  
  Day_Begin <- as.numeric(difftime(Date_Begin, Date_Start))
  Day_Begin <- Day_Begin[1] # Берем первую из ближайших дат 
  Day_End <- as.numeric(difftime(Date_End, Date_Start))
  Day_End <- Day_End[length(Day_End)] # Берем поледнюю из ближайших дат
  Day_Meadle <- as.numeric(difftime(Date_Meadle, Date_Start))
  Day_Meadle <- mean(Day_Meadle) #Берем среднюю из ближайшх дат
  
  result <- data.frame(Param = c("Days_perc_15",	"Days_perc_50", 	"Days_perc_85"), Value = c(Day_Begin, Day_Meadle, Day_End))
  
  result
  
}


date_search(df = df_count_Total_calanus, year = 1990)




#############################


## Рисунок с динамикой численности видов

plot_dynam <- function(df_a, df_par, begin, end, Ncol) {
  library(ggplot2)
  df_a <- df_a[order(df_a$Date2),]
  ggplot(df_a[df_a$Year %in% begin:end,], aes(x = Days_from_year_start, y = Abund_mean2)) + geom_line() +  facet_wrap(~Year, ncol=Ncol, scales = "free_y") + geom_vline(data = df_par[df_par$Year%in% begin:end, ], aes(xintercept = Days_perc_50), color = "blue") + geom_vline(data = df_par[df_par$Year%in% begin:end, ], aes(xintercept = Peak_Days_from_year_start), color = "red") + geom_smooth(se = FALSE) + labs(x = "Year days", y = "Abundance") + theme_bw()
}


# Динамика Oithona ##############

(Pl_0it_1 <- plot_dynam(oit, log_param_Oithona, 1961, 1966, 3))

(Pl_0it_2 <- plot_dynam(oit, log_param_Oithona, 1967, 1972, 3))

(Pl_0it_3 <- plot_dynam(oit, log_param_Oithona, 1973, 1978, 3))

(Pl_0it_4 <- plot_dynam(oit, log_param_Oithona, 1979, 1984, 3))

(Pl_0it_5 <- plot_dynam(oit, log_param_Oithona, 1985, 1990, 3))

(Pl_0it_6 <- plot_dynam(oit, log_param_Oithona, 1991, 1996, 3))

(Pl_0it_7 <- plot_dynam(oit, log_param_Oithona, 1997, 2002, 3))

(Pl_0it_8 <- plot_dynam(oit, log_param_Oithona, 2003, 2008, 3))

(Pl_0it_9 <- plot_dynam(oit, log_param_Oithona, 2009, 2014, 3))

(Pl_0it_10 <- plot_dynam(oit, log_param_Oithona, 2015, 2018, 3))





# Динамика Calanus ##############

(Pl_cal_1 <- plot_dynam(calanus, log_param_Calanus, 1961, 1966, 3))

(Pl_cal_2 <- plot_dynam(calanus, log_param_Calanus, 1967, 1972, 3))

(Pl_cal_3 <- plot_dynam(calanus, log_param_Calanus, 1973, 1978, 3))

(Pl_cal_4 <- plot_dynam(calanus, log_param_Calanus, 1979, 1984, 3))

(Pl_cal_5 <- plot_dynam(calanus, log_param_Calanus, 1985, 1990, 3))

(Pl_cal_6 <- plot_dynam(calanus, log_param_Calanus, 1991, 1996, 3))

(Pl_cal_7 <- plot_dynam(calanus, log_param_Calanus, 1997, 2002, 3))

(Pl_cal_8 <- plot_dynam(calanus, log_param_Calanus, 2003, 2008, 3))

(Pl_cal_9 <- plot_dynam(calanus, log_param_Calanus, 2009, 2014, 3))

(Pl_cal_10 <- plot_dynam(calanus, log_param_Calanus, 2015, 2018, 3))




# Динамика Centropages ##############

(Pl_centrop_1 <- plot_dynam(centrop, log_param_Centropages, 1961, 1966, 3))

(Pl_centrop_2 <- plot_dynam(centrop, log_param_Centropages, 1967, 1972, 3))

(Pl_centrop_3 <- plot_dynam(centrop, log_param_Centropages, 1973, 1978, 3))

(Pl_centrop_4 <- plot_dynam(centrop, log_param_Centropages, 1979, 1984, 3))

(Pl_centrop_5 <- plot_dynam(centrop, log_param_Centropages, 1985, 1990, 3))

(Pl_centrop_6 <- plot_dynam(centrop, log_param_Centropages, 1991, 1996, 3))

(Pl_centrop_7 <- plot_dynam(centrop, log_param_Centropages, 1997, 2002, 3))

(Pl_centrop_8 <- plot_dynam(centrop, log_param_Centropages, 2003, 2008, 3))

(Pl_centrop_9 <- plot_dynam(centrop, log_param_Centropages, 2009, 2014, 3))

(Pl_centrop_10 <- plot_dynam(centrop, log_param_Centropages, 2015, 2018, 3))




# Динамика Temora ##############


(Pl_temor_1 <- plot_dynam(temor, log_param_Temora, 1961, 1966, 3))

(Pl_temor_2 <- plot_dynam(temor, log_param_Temora, 1967, 1972, 3))

(Pl_temor_3 <- plot_dynam(temor, log_param_Temora, 1973, 1978, 3))

(Pl_temor_4 <- plot_dynam(temor, log_param_Temora, 1979, 1984, 3))

(Pl_temor_5 <- plot_dynam(temor, log_param_Temora, 1985, 1990, 3))

(Pl_temor_6 <- plot_dynam(temor, log_param_Temora, 1991, 1996, 3))

(Pl_temor_7 <- plot_dynam(temor, log_param_Temora, 1997, 2002, 3))

(Pl_temor_8 <- plot_dynam(temor, log_param_Temora, 2003, 2008, 3))

(Pl_temor_9 <- plot_dynam(temor, log_param_Temora, 2009, 2014, 3))

(Pl_temor_10 <- plot_dynam(temor, log_param_Temora, 2015, 2018, 3))




# Динамика Pseudocalanus ##############


(Pl_pseudocal_1 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1961, 1966, 3))

(Pl_pseudocal_2 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1967, 1972, 3))

(Pl_pseudocal_3 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1973, 1978, 3))

(Pl_pseudocal_4 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1979, 1984, 3))

(Pl_pseudocal_5 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1985, 1990, 3))

(Pl_pseudocal_6 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1991, 1996, 3))

(Pl_pseudocal_7 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 1997, 2002, 3))

(Pl_pseudocal_8 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 2003, 2008, 3))

(Pl_pseudocal_9 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 2009, 2014, 3))

(Pl_pseudocal_10 <- plot_dynam(pseudocal, log_param_Pseudocalanus, 2015, 2018, 3))



# Динамика Acartia ##############


(Pl_acart_1 <- plot_dynam(acart, log_param_Acartia, 1961, 1966, 3))

(Pl_acart_2 <- plot_dynam(acart, log_param_Acartia, 1967, 1972, 3))

(Pl_acart_3 <- plot_dynam(acart, log_param_Acartia, 1973, 1978, 3))

(Pl_acart_4 <- plot_dynam(acart, log_param_Acartia, 1979, 1984, 3))

(Pl_acart_5 <- plot_dynam(acart, log_param_Acartia, 1985, 1990, 3))

(Pl_acart_6 <- plot_dynam(acart, log_param_Acartia, 1991, 1996, 3))

(Pl_acart_7 <- plot_dynam(acart, log_param_Acartia, 1997, 2002, 3))

(Pl_acart_8 <- plot_dynam(acart, log_param_Acartia, 2003, 2008, 3))

(Pl_acart_9 <- plot_dynam(acart, log_param_Acartia, 2009, 2014, 3))

(Pl_acart_10 <- plot_dynam(acart, log_param_Acartia, 2015, 2018, 3))





# Динамика Mirosetella ##############


(Pl_microset_1 <- plot_dynam(microset, log_param_Microsetella, 1961, 1966, 3))

(Pl_microset_2 <- plot_dynam(microset, log_param_Microsetella, 1967, 1972, 3))

(Pl_microset_3 <- plot_dynam(microset, log_param_Microsetella, 1973, 1978, 3))

(Pl_microset_4 <- plot_dynam(microset, log_param_Microsetella, 1979, 1984, 3))

(Pl_microset_5 <- plot_dynam(microset, log_param_Microsetella, 1985, 1990, 3))

(Pl_microset_6 <- plot_dynam(microset, log_param_Microsetella, 1991, 1996, 3))

(Pl_microset_7 <- plot_dynam(microset, log_param_Microsetella, 1997, 2002, 3))

(Pl_microset_8 <- plot_dynam(microset, log_param_Microsetella, 2003, 2008, 3))

(Pl_microset_9 <- plot_dynam(microset, log_param_Microsetella, 2009, 2014, 3))

(Pl_microset_10 <- plot_dynam(microset, log_param_Microsetella, 2015, 2018, 3))





# Динамика Tricona ##############


(Pl_tricon_1 <- plot_dynam(tricon, log_param_Triconia, 1961, 1966, 3))

(Pl_tricon_2 <- plot_dynam(tricon, log_param_Triconia, 1967, 1972, 3))

(Pl_tricon_3 <- plot_dynam(tricon, log_param_Triconia, 1973, 1978, 3))

(Pl_tricon_4 <- plot_dynam(tricon, log_param_Triconia, 1979, 1984, 3))

(Pl_tricon_5 <- plot_dynam(tricon, log_param_Triconia, 1985, 1990, 3))

(Pl_tricon_6 <- plot_dynam(tricon, log_param_Triconia, 1991, 1996, 3))

(Pl_tricon_7 <- plot_dynam(tricon, log_param_Triconia, 1997, 2002, 3))

(Pl_tricon_8 <- plot_dynam(tricon, log_param_Triconia, 2003, 2008, 3))

(Pl_tricon_9 <- plot_dynam(tricon, log_param_Triconia, 2009, 2014, 3))

(Pl_tricon_10 <- plot_dynam(tricon, log_param_Triconia, 2015, 2018, 3))




