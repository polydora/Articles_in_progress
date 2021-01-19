# Этот код написан для борьбы с пропусками значений в данных

library(Rssa)
library(ggplot2)

env <- read.csv("data/env.csv", header = T)



gap <- function(x, L = 5, comp = 1:3) {
  s <- ssa(x, L = L)
  # Fill in gaps using the trend and 2 periodicty components
  g <- gapfill(s, groups = list(comp))
  g
  
}


ss <- rep(NA, 1000)

# Если берем 1:3 сингулярные триплеты
for(i in 1:1000){
  a <- env$Su_end
  a[sample(1:58, 4)] <- NA
  X <- data.frame(init = env$Su_end, miss = a, reconstr = gap(a, L = 5, comp = 1:3))
  ss[i] <- sum((X$init - X$reconstr)^2, na.rm = T)
    
}

ss1_3 <- ss
ggplot(as.data.frame(ss1_3), aes(x = ss)) + geom_histogram()


# Если берем 1:2 сингулярные триплеты
for(i in 1:1000){
  a <- env$Su_end
  a[sample(1:58, 4)] <- NA
  X <- data.frame(init = env$Su_end, miss = a, reconstr = gap(a, L = 5, comp = 1:2))
  ss[i] <- sum((X$init - X$reconstr)^2, na.rm = T)
  
}

ss1_2 <- ss
ggplot(as.data.frame(ss1_2), aes(x = ss)) + geom_histogram()




# Если берем 1 сингулярный триплет
for(i in 1:1000){
  a <- env$Su_end
  a[sample(1:58, 4)] <- NA
  X <- data.frame(init = env$Su_end, miss = a, reconstr = gap(a, L = 5, comp = 1))
  ss[i] <- sum((X$init - X$reconstr)^2, na.rm = T)
  
}

ss1_1 <- ss
ggplot(as.data.frame(ss1_1), aes(x = ss)) + geom_histogram()


mean(ss1_3)
mean(ss1_2)
mean(ss1_1)


# НАименьшая сумма квадратов отклонений наблюдается при 1 сингулярном триплете

#####################################
# Проводим реконструкцийю для всех переменных с пропусками

obs <- env[,-1]

reconst <- as.data.frame(matrix(rep(NA, ncol(obs)*nrow(obs)), ncol =  ncol(obs)))

for(j in 1:ncol(obs)) reconst[, j] <- gap(obs[, j], L = 5, comp = 1)

obs$Type <- "Observed"

reconst$Type <- "Reconstructed"

ncol(obs)
ncol(reconst)


names(reconst) <- names(obs)

obs_reconst <- obs

obs_reconst[is.na(obs)] <- reconst[is.na(obs)]


write.csv(obs_reconst, "data/env_gap_filled.csv")



##################################################
# Изучаем поведение функци gapfill() на симулированных данных

env <- read.csv("data/env_gap_filled_short.csv", header = TRUE)  #Данные по средовым показателям


column_name <- c("Year", "TPD", "SpSD", "SuSD", "SuFD", "ICD", "SuSDPY",  "SuDur","SuDurPY", "SuFDPY",     "SpT", "SuT", "SpTPY", "SuTPY", "NAO", "NAOPY", "AOI", "AOIPY"  )


names(env) <- column_name 

library(dplyr)

env_obs <- env %>% select(TPD, SpSD, SuSD, SuFD, ICD)






nrow(env_obs)*ncol(env_obs)

N_na <- sum(env_obs %% 1 !=0)


N_na/(nrow(env_obs)*ncol(env_obs))

df = env_obs  

write.table(df, "clipboard", sep = "\t")

df <- as.data.frame(sapply(df, as.numeric))

str(df)


gap <- function(x, L = 5, comp = 1:3) {
  library(Rssa)
  s <- ssa(x, L = L)
  # Fill in gaps using the trend and 2 periodicty components
  g <- gapfill(s, groups = list(comp))
  g
  
}



Na_add <- function(df, N_na){
  library(reshape2)
  position = sample(nrow(df)*ncol(df), N_na)
  df_melt <- melt(df)
  df_melt$value[position] <- NA
  df_wide <- dcast(df_melt, formula = 1:nrow(df) ~ variable,value.var ="value")
  df_wide <- df_wide[ ,-1]
  output <- list(df = df_wide, position = position)
  
}

N_simulation = 10000

result <- data.frame(Var = NA, Year = NA, Delta = NA)

for(i in 1:N_simulation){
  output <- Na_add(df, N_na)
  
  df2 <- output$df
  position <- output$position
  
  reconst <- as.data.frame(matrix(rep(NA, ncol(df2)*nrow(df2)), ncol =  ncol(df2)))
  
  for(j in 1:ncol(df2)) {
    if(sum(is.na(df2[, j])) != 0) 
      reconst[, j] <- gap(df2[, j], L = 5, comp = 1)
    else
      reconst[, j] <- df2[, j]
  }
  
  diff <- data.frame(Var = melt(df2)[position, 1], Year = rep(env$Year, nrow(df2)*ncol(df2))[position],  Delta = melt(df)[position,2] - melt(reconst)[position, 2])

  result <- rbind(result, diff) 
  
  print(i)
  
}



result2 <- result %>% group_by(Var) %>% mutate(Scaled_value = scale(Delta)) %>% filter(!is.na(Var))

# доля оутлауров предсказаний
sum(abs(result2$Scaled_value) > 3) / nrow(result2)  

ggplot(result2 , aes(x = Var, y = Delta)) + geom_boxplot()  + geom_hline(yintercept = 0, color = "blue") + theme_bw()+ theme(axis.text.x = element_text(angle = 90)) + labs(x = "Environmental phenological event", y ="Deviaton reconstructed from observed values")

result2$Var <- factor(result2$Var)

result2 %>% filter(Var == "SuFD") %>% summarise(Med_Delta = median(Delta))

ggplot(result2 , aes(x = Year, y = Delta)) + geom_point(size = 0.1)  + facet_wrap(~Var) + geom_hline(yintercept = 0, color = "blue") + geom_smooth(method = "lm") + theme_bw()+ theme(axis.text.x = element_text(angle = 90)) + labs(x = "Year", y ="Deviaton reconstructed from observed values")




#####################################
# Лечим датасет с фенологией Calanus

gap <- function(x, L = 5, comp = 1:3) {
  s <- ssa(x, L = L)
  # Fill in gaps using the trend and 2 periodicty components
  g <- gapfill(s, groups = list(comp))
  g
  
}



cal <- read.csv("data/Phenological_tables/Calanus_phenology.csv", header = T)


# В местах, соответстующих острым пикам замещаем датой физическоо пика
cal$Days_perc_15[cal$Days_perc_15 == 0 & !is.na(cal$Days_perc_15)] <-  cal$Peak_Days_from_year_start[cal$Days_perc_15 == 0 & !is.na(cal$Days_perc_15)] 

cal$Days_perc_50[cal$Days_perc_50 == 0& !is.na(cal$Days_perc_50)] <-  cal$Peak_Days_from_year_start[cal$Days_perc_50 == 0& !is.na(cal$Days_perc_50)] 

cal$Days_perc_85[cal$Days_perc_85 == 0& !is.na(cal$Days_perc_85)] <-  cal$Peak_Days_from_year_start[cal$Days_perc_85 == 0& !is.na(cal$Days_perc_85)] 




obs <- cal[,-c(1,6)]


reconst <- as.data.frame(matrix(rep(NA, ncol(obs)*nrow(obs)), ncol =  ncol(obs)))

for(j in 1:ncol(obs)) reconst[, j] <- gap(obs[, j], L = 5, comp = 1)

obs$Type <- "Observed"

reconst$Type <- "Reconstructed"

ncol(obs)
ncol(reconst)

names(reconst) <-  names(obs)

obs_reconst <- obs

obs_reconst[is.na(obs)] <- reconst[is.na(obs)]


write.table(obs_reconst, "clipboard", sep = "\t", row.names = FALSE)






#####################################
# Лечим датасет с фенологией Pseudocalanus

pcal <- read.csv("data/Phenological_tables/Pseudocalanus_phenology.csv", header = T)


# В местах, соответстующих острым пикам замещаем датой физическоо пика
pcal$Days_perc_15[pcal$Days_perc_15 == 0 & !is.na(pcal$Days_perc_15)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_15 == 0 & !is.na(pcal$Days_perc_15)] 

pcal$Days_perc_50[pcal$Days_perc_50 == 0& !is.na(pcal$Days_perc_50)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_50 == 0& !is.na(pcal$Days_perc_50)] 

pcal$Days_perc_85[pcal$Days_perc_85 == 0& !is.na(pcal$Days_perc_85)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_85 == 0& !is.na(pcal$Days_perc_85)] 






obs <- pcal[,-c(1,6)]

reconst <- as.data.frame(matrix(rep(NA, ncol(obs)*nrow(obs)), ncol =  ncol(obs)))

for(j in 1:ncol(obs)) reconst[, j] <- gap(obs[, j], L = 5, comp = 1)

obs$Type <- "Observed"

reconst$Type <- "Reconstructed"

ncol(obs)
ncol(reconst)

names(reconst) <- names(obs)

obs_reconst <- obs

obs_reconst[is.na(obs)] <- reconst[is.na(obs)]


write.table(obs_reconst, "clipboard", sep = "\t", row.names = FALSE)
 

#####################################
# Лечим датасет с фенологией Oithona

pcal <- read.csv("data/Phenological_tables/Oithona_phenology.csv", header = T)


# В местах, соответстующих острым пикам замещаем датой физическоо пика
pcal$Days_perc_15[pcal$Days_perc_15 == 0 & !is.na(pcal$Days_perc_15)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_15 == 0 & !is.na(pcal$Days_perc_15)] 

pcal$Days_perc_50[pcal$Days_perc_50 == 0& !is.na(pcal$Days_perc_50)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_50 == 0& !is.na(pcal$Days_perc_50)] 

pcal$Days_perc_85[pcal$Days_perc_85 == 0& !is.na(pcal$Days_perc_85)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_85 == 0& !is.na(pcal$Days_perc_85)] 






obs <- pcal[,-c(1,6)]

reconst <- as.data.frame(matrix(rep(NA, ncol(obs)*nrow(obs)), ncol =  ncol(obs)))

for(j in 1:ncol(obs)) reconst[, j] <- gap(obs[, j], L = 5, comp = 1)

obs$Type <- "Observed"

reconst$Type <- "Reconstructed"

ncol(obs)
ncol(reconst)

names(reconst) <- names(obs)

obs_reconst <- obs

obs_reconst[is.na(obs)] <- reconst[is.na(obs)]


write.table(obs_reconst, "clipboard", sep = "\t", row.names = FALSE)





#####################################
# Лечим датасет с фенологией Microsetella

pcal <- read.csv("data/Phenological_tables/Microsetella_phenology.csv", header = T)


# В местах, соответстующих острым пикам замещаем датой физическоо пика
pcal$Days_perc_15[pcal$Days_perc_15 == 0 & !is.na(pcal$Days_perc_15)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_15 == 0 & !is.na(pcal$Days_perc_15)] 

pcal$Days_perc_50[pcal$Days_perc_50 == 0& !is.na(pcal$Days_perc_50)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_50 == 0& !is.na(pcal$Days_perc_50)] 

pcal$Days_perc_85[pcal$Days_perc_85 == 0& !is.na(pcal$Days_perc_85)] <-  pcal$Peak_Days_from_year_start[pcal$Days_perc_85 == 0& !is.na(pcal$Days_perc_85)] 






obs <- pcal[,-c(1,6)]

reconst <- as.data.frame(matrix(rep(NA, ncol(obs)*nrow(obs)), ncol =  ncol(obs)))

for(j in 1:ncol(obs)) reconst[, j] <- gap(obs[, j], L = 5, comp = 1)

obs$Type <- "Observed"

reconst$Type <- "Reconstructed"

ncol(obs)
ncol(reconst)

names(reconst) <- names(obs)

obs_reconst <- obs

obs_reconst[is.na(obs)] <- reconst[is.na(obs)]


write.table(obs_reconst, "clipboard", sep = "\t", row.names = FALSE)







citation("Rssa")
