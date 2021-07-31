
## Эффект подковы в симулированном примере







# Построим искусственный градиент

abundance <- function(k){
  d <- -(1:100)^2 + k*(1:100) + 500 
  d[d<0] <- 0
  d
}


dat <- data.frame(Site = 1:100, Sp1 = abundance(5), Sp2 = abundance(50), Sp3 = abundance(100), Sp4 = abundance(150)/10, Sp5 = abundance(200)/10, Sp6 = abundance(250)/10 )

dat2 <- dat[, -1] + rnorm(100, 0, 200)
dat2[dat2 < 0] <- 0
rownames(dat2) <-make.unique(as.character(dat$Site))

env_data <- data.frame(Env_param1 = 1:100, Env_param2 = rnorm(100) )


library(ggplot2)

Pl_grad1 <- ggplot(dat2, aes(x = 1:100))+ geom_point(aes(y=Sp1), color = "black", size = 1)  + geom_point(aes(y=Sp2), color = "green", size = 1) +  geom_point(aes(y=Sp3), color = "red", size = 1)  + geom_point(aes(y=Sp4), color = "blue", size = 1) + geom_point(aes(y=Sp5), color = "darkblue", size = 1) + geom_point(aes(y=Sp6), color = "pink", size = 1) + geom_smooth(aes(y=Sp1), color = "black", se = F)  + geom_smooth(aes(y=Sp2), color = "green", se= F) +  geom_smooth(aes(y=Sp3), color = "red", se = F)  + geom_smooth(aes(y=Sp4), color = "blue", se = F) + geom_smooth(aes(y=Sp5), color = "darkblue", se = F) + geom_smooth(aes(y=Sp6), color = "pink", se = F) + labs(x = "Environmental gradient 1 (directional)", y = "Species abundance")




Pl_grad2 <- ggplot(dat2, aes(x = env_data$Env_param2))+ geom_point(aes(y=Sp1), color = "black", size = 1)  + geom_point(aes(y=Sp2), color = "green", size = 1) +  geom_point(aes(y=Sp3), color = "red", size = 1)  + geom_point(aes(y=Sp4), color = "blue", size = 1) + geom_point(aes(y=Sp5), color = "darkblue", size = 1) + geom_point(aes(y=Sp6), color = "pink", size = 1) + geom_smooth(aes(y=Sp1), color = "black", se = F)  + geom_smooth(aes(y=Sp2), color = "green", se= F) +  geom_smooth(aes(y=Sp3), color = "red", se = F)  + geom_smooth(aes(y=Sp4), color = "blue", se = F) + geom_smooth(aes(y=Sp5), color = "darkblue", se = F) + geom_smooth(aes(y=Sp6), color = "pink", se = F) + labs(x = "Environmental gradient 2 (random)", y = "Species abundance")

library(cowplot)

plot_grid(Pl_grad1, Pl_grad2)



L <- dat2
Ltrans <- L^(1/2)

R <- env_data
Rstand<- as.data.frame(scale(R))



#Random distribution of functional traits

Q_types <- c(rep("BF", 5), rep("BS", 5), rep("FH", 5), rep("F",3), rep("LD", 3), rep("LH", 6), rep("MV", 4), rep("R", 4), rep("SO", 3))


Q <- matrix(rep(0, 38*6), ncol = 38, byrow = T)

Q <- as.data.frame(Q)

names(Q) <- names(ChukchiQ)
row.names(Q) <- names(dat2)


QQ <- Q %>% t() %>% as.data.frame() %>% mutate(Q_types = Q_types)  

qq1 <- NULL
for(i in unique(Q_types)){
  qq <- QQ[QQ$Q_types == i, -7] 
  for(j in 1:6) qq[ , j] <- sample(c(rep(0,nrow(qq)-3),1, 2, 3))
  qq1 <- rbind(qq1, qq)
  (qq1)
}

Q <- as.data.frame(t(qq1))

Qfuzzy<-prep.fuzzy(Q, c(5,5,5,3,3,6,4,4,3)) ##makes all modalities add to 1 within 

row.names(Qfuzzy) <- colnames(Ltrans)


LQ<-as.data.frame(functcomp(as.matrix(Qfuzzy),
                            as.matrix(Ltrans)),
                  bin.num=NULL)



# View(LQ)


#arch-effect

RDA_L <- rda(Ltrans ~ ., data = Rstand)

plot(RDA_L, display = c("sites", "cn"), scaling = "symmetric")



RDA_LQ <- rda(LQ ~ ., data = Rstand)

RDA_LQ$CCA$tot.chi/RDA_LQ$tot.chi
RsquareAdj(RDA_LQ)

summary(RDA_LQ)

RDA_LQ_plot<-plot(RDA_LQ, scaling = "symmetric")
LQ_Sites<-as.data.frame(RDA_LQ_plot$sites)


###### Trait Convergence ########
##FDis comapred between shelves

SpeciesTraitsdist<-vegdist(Qfuzzy, method="gower",
                                 na.rm = TRUE) ##created a gower distance matrix of species based

FDis<-fdisp(SpeciesTraitsdist, 
                          as.matrix(dat2))

FDisRDAaxes<-cbind(FDis$FDis, LQ_Sites)
names(FDisRDAaxes)[1]<-"FDis"

CmodFDis<-lm(FDis~poly(RDA1,2) + poly(RDA2,2), data=FDisRDAaxes)
summary(CmodFDis)

library(visreg)
visreg(CmodFDis, "RDA1") 
abline(v = 0, col="blue", lty=2)

visreg(CmodFDis, "RDA2")
abline(v = 0, col="blue", lty=2)

