
library(ggplot2)




#Вычисляем количество M.trossulus, M.edulis, как выборку из биномиального распределения
simulate_pop <- function(P_MT=0.5, P_T_MT=1, P_E_ME=1, Ntotal = 1000, a = 0, b = 0){
  N_MT <- rbinom(1, Ntotal, P_MT) #Извлекаем MT
  N_ME <- Ntotal - N_MT #Сколько осталось MЕ 
  
  N_T_MT <- N_MT*(P_T_MT) #Сколько из M.trossulus имеют T морфотип
  N_E_MT <- N_MT - N_T_MT #Сколько среди MT особей с E морфотипом
  
  N_E_ME <- N_ME*(P_E_ME) #Сколько среди ME особей Е морфотипа
  N_T_ME <- N_ME - N_E_ME
  
  pop <- round(c(N_T_MT, N_E_MT, N_E_ME, N_T_ME))
  names(pop) <- c("N_T_MT", "N_E_MT", "N_E_ME", "N_T_ME")
  pop
}



simulate_pop(P_MT = 0.1, P_T_MT = 273/(273+91), P_E_ME = 680/(680+59))


pop_simulated <- data.frame(P_MT = seq(0, 1, 0.01), N_T_MT = NA, N_E_MT = NA, N_E_ME = NA, N_T_ME = NA)

P_T_MT_glob = (204 + 264 + 273)/(204 + 264 + 273 + 60 + 53 + 91)

P_E_ME_glob = (215 + 134 + 680)/(215 + 134 + 680 + 21 + 222 + 59)



a = 0.5 #то как эдулисы гнобят троссулус
b = 0.2  #то как  троссулус гнобят эдулисов 
for(i in 1:nrow(pop_simulated))  pop_simulated[i, 2:5] <- simulate_pop(P_MT = pop_simulated$P_MT[i], P_T_MT = P_T_MT_glob, P_E_ME = P_E_ME_glob)


pop_simulated$P_T_MT <- with(pop_simulated, N_T_MT/(N_T_MT + N_E_MT))
pop_simulated$P_E_MT <- 1 - pop_simulated$P_T_MT
pop_simulated$P_E_ME <- with(pop_simulated, N_E_ME/(N_T_ME + N_E_ME))
pop_simulated$P_T_ME <- 1 - pop_simulated$P_E_ME
pop_simulated$P_MT_comp <- with(pop_simulated, (N_T_MT + N_E_MT - a*(N_T_ME + N_E_ME))/(N_T_MT + N_E_MT + N_T_ME + N_E_ME - a*(N_T_ME + N_E_ME)))

pop_simulated$P_ME_comp <- with(pop_simulated, (N_T_ME + N_E_ME - b*(N_T_MT + N_E_MT))/(N_T_MT + N_E_MT + N_T_ME + N_E_ME - b*(N_T_MT + N_E_MT)))


pop_simulated$P_MT_T <- with(pop_simulated, P_T_MT*P_MT/(P_T_MT*P_MT + P_T_ME*(1-P_MT)))

pop_simulated$P_MT_T_comp <- with(pop_simulated, P_T_MT*P_MT_comp/(P_T_MT*P_MT_comp + P_T_ME*(1-P_MT_comp)))

pop_simulated$P_ME_E <- with(pop_simulated, P_E_ME*(1-P_MT)/(P_E_ME*(1-P_MT) + P_E_MT*P_MT))


pop_simulated$P_ME_E_comp <- with(pop_simulated, P_E_ME*P_ME_comp/(P_E_ME*P_ME_comp + P_E_MT*P_MT_comp))


ggplot(pop_simulated, aes(x = P_MT)) + geom_line(aes(y = P_MT_T), color = "red", linetype = 2) + geom_line(aes(y = P_ME_E), color = "blue", linetype = 2) + geom_point(aes(x = P_MT, y = P_MT_T_comp), color = "red") + geom_point(aes(x = P_MT, y = P_ME_E_comp), color = "blue") + ylim(0,1) + xlim(0,1)  + geom_smooth(aes(x = P_MT, y = P_MT_T_comp),  method = "glm", method.args = list(family = "binomial"), color = "red")  + geom_smooth(aes(x = P_MT, y = P_ME_E_comp),  method = "glm", method.args = list(family = "binomial"), color = "blue") + labs(x = "Proportion of MT without competition", y = "Correct identification")






