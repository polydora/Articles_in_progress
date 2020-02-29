library(reshape2)
library(dplyr)


il_long <- read.table("data/Ilistaya_inlet_data_1987_2019.csv", sep = ",", header = TRUE)

il_long$Taxa <- gsub("_", " ", il_long$Taxa)


levels(il_long$Type)

il_long$Station <- factor(il_long$Station, levels = c("St1", "St2", "St3",  "St4",  "St5",  "St6",  "St7",  "St8",  "St9", "St10", "St11", "St12", "St13", "St14", "St15", "St16", "St17", "St18", "St19", "St20" ))


il_long_NB <- dcast(il_long, Year + Station + Taxa ~ Type, value.var = "Value")

il_long_NB$B <- round(il_long_NB$B, 2)

il_long_NB$N <- round(il_long_NB$N, 0)


# средний вес особи
P <- il_long_NB %>% group_by(Taxa) %>% summarize(P = sum(B, na.rm = T)/sum(N, na.rm = T))


# Лечим отсутсвие биомассы

for(i in 1:nrow (il_long_NB)) {
      if(is.na(il_long_NB$B[i]) & !is.na(il_long_NB$N[i])) il_long_NB$B[i] <- P$P[P$Taxa == il_long_NB$Taxa[i]] *  il_long_NB$N[i]
    
}

il_long_NB[is.na(il_long_NB$B), ]


# Лечим отсутствие численности

problem <- (il_long_NB[is.na(il_long_NB$N), ]) 



write.table(il_long_NB, file = "data/Ilistaya_inlet_1987_2019_N_B.csv", sep = ",")
