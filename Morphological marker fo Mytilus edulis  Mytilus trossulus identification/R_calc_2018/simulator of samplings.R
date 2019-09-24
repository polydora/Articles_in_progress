# Смешиваем в определенной пропорции морфотипы
library(ggplot2)

N = 100000
populations <- matrix(rep(NA, N*10), ncol = 10)



P_T_seq <- seq(0, 1, length.out = 10)


mixing <- function(P_MT_T, P_ME_E, P_T, N = 100000) {
  morph <- c(rep("T",round(N*P_T, 0)), rep("E", N-round(N*P_T, 0)))  
  spec<-rep(NA, N)
  spec[morph == "T"] <-  c(rep("MT", round(P_MT_T*sum(morph == "T"),0)), rep("ME",round((1-P_MT_T)*sum(morph == "T"),0)))
  spec[morph != "T"] <-  c(rep("ME",round(P_ME_E*sum(morph != "T"),0)), rep("MT", round((1-P_ME_E)*sum(morph != "T"),0)))
  spec_morph <- data.frame(Morph = morph, Spec = spec)
  spec_morph$Sp = paste(spec_morph$Morph,".", spec_morph$Spec, sep = "")
  spec_morph$Sp <- spec_morph$Sp[sample(1:N)]
  spec_morph$Sp
}


for(i in 1:10){
  populations[,i] <- mixing(P_T = P_T_seq[i],P_ME_E = 0.781, P_MT_T = 0.846)
}


P_MT_seq_abram <- apply(X = populations, MARGIN = 2,FUN = function(x) {y <- x; mean(y=="E.MT" | y == "T.MT" )})



qplot(x = P_T_seq, y = P_MT_seq) + ylim(0,1) + geom_smooth(method = "lm")

table(populations)


# Смешиваем в определенной пропорции виды

N = 100000
populations2 <- matrix(rep(NA, N*10), ncol = 10)

P_MT_seq2 <- seq(0, 1, length.out = 10)


mixing_sp <- function(P_T_MT, P_E_ME, P_MT, N = 100000) {
  spec <- c(rep("MT",round(N*P_MT, 0)), rep("ME", N-round(N*P_MT, 0)))  
  morph <- rep(NA, N)
  morph[spec == "MT"] <-  c(rep("T", round(P_T_MT*sum(spec == "MT"),0)), rep("E",round((1-P_T_MT)*sum(spec == "MT"),0)))
  morph[spec != "MT"] <-  c(rep("E",round(P_E_ME*sum(spec != "MT"),0)), rep("T", round((1-P_E_ME)*sum(spec != "MT"),0)))
  spec_morph <- data.frame(Morph = morph, Spec = spec)
  spec_morph$Sp = paste(spec_morph$Morph,".", spec_morph$Spec, sep = "")
  spec_morph$Sp <- spec_morph$Sp[sample(1:N)]
  spec_morph$Sp
}


for(i in 1:10){
  populations2[,i] <- mixing_sp(P_MT = P_MT_seq2[i],P_E_ME =0.926, P_T_MT = 0.611)
}


P_T_seq2_abram <- apply(X = populations2, MARGIN = 2,FUN = function(x) {y <- x; mean(y=="T.MT" | y == "T.ME" )})






ggplot() + ylim(0,1)+ xlim(0,1) + geom_line(aes(x = P_T_seq2_abram, y = P_MT_seq2), linetype = 2)  + geom_line(aes(x = P_T_seq, y = P_MT_seq_abram)) + geom_line(aes(x = P_T_seq2_zmys, y = P_MT_seq2), color = "blue", linetype = 2) + geom_line(aes(x = P_T_seq, y = P_MT_seq_zmys), color = "blue") 



 



populations_2d_1 <- array(rep(NA,  2*N*10*10),dim = c(2*N, 10, 10))


for(j in 1:10) for(i in 1:10){
  populations_2d_1[,i,j] <- c(mixing_sp(P_MT = P_MT_seq2[i],P_E_ME =0.3661972	 , P_T_MT = 0.8333333 ),  mixing(P_T = P_T_seq[j],P_ME_E = 0.7103825, P_MT_T = 0.5408163))
}


populations_2d_1