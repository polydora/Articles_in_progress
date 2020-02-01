library(ggplot2)
library(dplyr)

hot <- read.table("Data/Hotred_2019.csv", sep = ",", header = T)

hot_alive <- hot[hot$Status == "alive", ]
hot_dead <- hot[hot$Status == "dead", ]


hot_N_alive <- hot_alive %>% group_by(Square) %>% summarise(N_T_alive = round(mean(N_T) * 182, 0), N_E_alive = round(mean(N_E) * 182, 0))

hot_N_dead <- hot_dead [hot_dead$Sample != "C", ] %>% group_by(Square) %>% summarise(N_T_dead = sum(N_T), N_E_dead = sum(N_E))


hot_N <- merge(hot_N_alive, hot_N_dead)

hot_N$Prop_T_alive <- with(hot_N, N_T_alive / (N_T_alive + N_E_alive) )

hot_N$Prop_dead <- with(hot_N, (N_T_dead + N_E_dead)/ (N_T_dead + N_E_dead + N_T_alive + N_E_alive) )


hot_N$Prop_T_dead <- with(hot_N, (N_T_dead)/ (N_T_dead + N_E_dead) )



ggplot(hot_N, aes(x =  (N_T_dead + N_E_dead), y = Prop_T_alive)) + geom_point()  + geom_smooth(method = "lm")

ggplot(hot_N, aes(x = Prop_T_alive, y = Prop_T_dead)) + geom_point()  + geom_smooth(method = "lm") + geom_abline()

Prop_eaten <- hot_N %>% group_by(Square) %>% summarise(P_eaten = sum(N_E_dead + N_T_dead)/sum(N_E_dead + N_T_dead + N_T_alive + N_E_alive)*100)

mean(Prop_eaten$P_eaten)


