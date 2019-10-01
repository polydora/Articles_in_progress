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
