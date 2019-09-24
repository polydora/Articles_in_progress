P_T = 	0.7469880

P_MT_T =   0.9112903
P_T_MT = 0.8692308
P_ME_E = 0.5952381
P_E_ME = 0.6944444
P_MT_E = 0.4047619
P_T_ME = 0.3055556
P_ME_T = 0.0887097
P_E_MT = 0.1307692

P_MT_of_T = 0.6807229
P_MT_of_E = 0.1024096

P_MT	= 0.7831325




# P_MT
P_MT_T*P_T + P_MT_E*(1-P_T)


# P_MT
P_MT_of_T+P_MT_of_E


# P_MT_of_E
(1-P_T)*P_MT_E

# P_ME_of_T
P_T*P_ME_T

# P_MT
P_T*(1 - P_MT_E - P_ME_T) + P_MT_E

P_ME_of_T + P_MЕ_of_E



# P_MT
P_T*(1 + P_MT_T/ P_T_MT - P_MT_T + P_ME_E/P_E_ME - P_ME_E) - P_ME_E/P_E_ME + P_ME_E

P_T*(1 + P_MT_T/ P_T_MT - P_MT_T + P_ME_E/P_E_ME - P_ME_E) - (P_ME_E*(1-P_T)+P_ME_T*P_T)/(1-P_T) + P_ME_E

P_T*(1 + P_MT_T/ P_T_MT - P_MT_T + P_ME_E/P_E_ME - P_ME_E) - P_ME_T*P_T/(1-P_T)


P_T*1 + P_T*P_MT_T/ P_T_MT - P_T*P_MT_T + P_T*P_ME_E/P_E_ME - P_T*P_ME_E - P_ME_T*P_T/(1-P_T)


P_T + (P_T*P_MT_T - P_T*P_MT_E + P_MT_E) - P_T*P_MT_T + (P_T*P_ME_E - P_T^2*P_ME_E + P_T^2*P_ME_T)/(1-P_T) - P_T*P_ME_E - P_ME_T*P_T/(1-P_T)

(P_T*P_MT_T - P_T*P_MT_E + P_MT_E)  +  P_T   - P_T*P_MT_T + (P_T*P_ME_E - P_T^2*P_ME_E + P_T^2*P_ME_T)/(1-P_T) - P_T*P_ME_E - P_ME_T*P_T/(1-P_T)


# Итоговая формула для расчета P_MT!
  
P_T*(P_MT_T - P_MT_E) + P_MT_E  







# P_T_MT OK!
P_MT_T*P_T/(P_MT_T*P_T + P_MT_E*(1-P_T))

# P_E_ME
P_ME_E*(1-P_T)/(P_ME_E*(1-P_T) + P_ME_T*P_T)

# P_MT_T OK!
P_T_MT * P_MT/(P_T_MT * P_MT + P_T_ME*(1-P_ME))

# P_ME_E
P_E_ME*(1-P_MT)/(P_E_ME*(1-P_MT) + P_E_MT*P_MT)




# P_T*P_MT_T/P_T_MT

P_MT_T*P_T + P_MT_E - P_MT_E*P_T

P_T*P_MT_T - P_T*P_MT_E + P_MT_E 




# P_T*P_ME_E/P_E_ME

(P_T*P_ME_E - P_T^2*P_ME_E + P_T^2*P_ME_T)/(1-P_T)

 
# По Стрелкову
# p_Tm, или по нашему P_T
P_T_MT*P_MT + P_T_ME*(1-P_MT)


# AcE или по нашму P_ME_E 
(1 - P_MT) * (1 - P_T_ME)/(1 - P_T)


calc_1 <- function(x) (x - P_T_ME)/(P_T_MT-P_T_ME)  


calc_2 <- function(x) P_MT_T*x + P_MT_E*(1-x)


calculators <- data.frame(P_T <- seq(0, 1, 0.1), PPS = calc_1(P_T), VMK = calc_2(P_T))


ggplot(calculators, aes(x = P_T)) + geom_line(aes(y = PPS), color = "red") + geom_line(aes(y = VMK), color = "blue") + geom_hline(yintercept = c(0, 1), linetype = 2)
