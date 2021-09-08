##Beaufort
BeaufortQ<-read.csv(file.choose(), row.names = 1)
head(BeaufortQ)


library(ade4)
BeaufortQfuzzy<-prep.fuzzy(BeaufortQ, c(5,5,5,3,3,6,4,4,3)) ##makes all modalities add to 1 within a trait
BeaufortL<-read.csv(file.choose(), row.names = 1) ##taxa by station file
##transformation
library(reshape2)
o<-melt(BeaufortL)

library(ggplot2)
ggplot()+
  geom_point(data=o, aes(x=variable, y=value))

BeaufortLtrans<-(BeaufortL)^(1/2) ##transform biomass matrix
o2<-melt(BeaufortLtrans)
ggplot()+
  geom_point(data=o2, aes(x=variable, y=value))

##Used square root transformation
##Make LQ matrix (trait x station matrix)

rownames(BeaufortQfuzzy)<-colnames(BeaufortLtrans)

BeaufortQfuzzy[is.na(BeaufortQfuzzy)] <- 0
library(FD)
BeaufortLQ<-as.data.frame(functcomp(as.matrix(BeaufortQfuzzy),
                                    as.matrix(BeaufortLtrans)))

##Which traits to choose for environmental analyses
##Use FCA correlations values
##need to have ChukchiLQ be a dudi.object

station.dataBeaufort<-read.csv(file.choose(), row.names = 1)

Beaufort_Latitude <-  station.dataBeaufort$Latitude [station.dataBeaufort$Sea == "Beaufort"]

station.dataBeaufort$Latitude <- Beaufort_Latitude 

BeaufortR<-read.csv(file.choose(), row.names = 1)

BeaufortRstand<-(as.data.frame(scale(BeaufortR)))

cor(station.dataBeaufort)

RDA_model0B <- rda( BeaufortLQ ~
                      1+Condition(as.matrix(station.dataBeaufort[,1])),
                    data = BeaufortRstand[,1:7])

RDA_model1B <- rda( BeaufortLQ ~
                      .+Condition(as.matrix(station.dataBeaufort[,1])),
                    data = BeaufortRstand[,1:7])
plot(RDA_model1B, display=c("cn", "species"))

BeaufortRDAFull<-plot(RDA_model1B)

BeaufortRDAFullMods<-as.data.frame(BeaufortRDAFull$species) ##modalities
BeaufortRDAFullStations<-as.data.frame(BeaufortRDAFull$sites)
BeaufortRDAFullEnv<-as.data.frame(cbind(as.data.frame(BeaufortRDAFull$biplot),c("Temp", "Salinity", "Chl.a", "Gravel","Sand", "Mud")))

names(BeaufortRDAFullEnv)[3]<-"Env"
##Forward selection occurs by including environmental variables with p-value < 0.05
##implies that factors with strong relationships to species
##or traits will be among those selected.
Beaufort_RDAmodel<-ordistep(RDA_model0B, scope=formula(RDA_model1B),
                            Pin=.05, permutations = 999)

BeaufortRDAplot<-plot(Beaufort_RDAmodel, display=c("cn", "species"))

##Depth and Gravel chosen
vif.cca(Beaufort_RDAmodel)##low vif numbers
plot(Beaufort_RDAmodel, display=c("cn", "species"))

anova(Beaufort_RDAmodel) ##significant

anova(Beaufort_RDAmodel , by = "axis")##RDA 1 and 2 are significant

anova(Beaufort_RDAmodel , by = "margin") ## mud, depth, sal, sand are significant

RsquareAdj(Beaufort_RDAmodel) ##adj. r squared = 0.19

summary(Beaufort_RDAmodel)

BeaufortSitesFull<-BeaufortRDAplot$sites


BeaufortSites_partialplot<-plot(Beaufort_RDAmodel)

##
##To see if relationships exist between modalities and environmental
##variables, used a 4th corner test
fourthB<-fourthcorner(BeaufortRstand[,c(1,5)],
                      BeaufortLtrans,
                      BeaufortQfuzzy, nrepet = 99,
                      modeltype = 6, p.adjust.method.G = "fdr",
                      p.adjust.method.D = "fdr")
plot(fourthB, alpha=.05)
fourthB

##when adjust for fdr, no traits are signficant
dudiLB <- dudi.coa (BeaufortLtrans, scannf = FALSE) ##biomass
dudiRB <- dudi.pca (BeaufortRstand[,c(1,5)], scannf = FALSE, row.w = dudiLB$lw)##env
dudiQB <- dudi.pca (BeaufortQfuzzy, scannf = FALSE, row.w = dudiLB$cw)##traits
vlt.rlqB <- rlq (dudiR = dudiRB, dudiL = dudiLB, dudiQ = dudiQB, scannf = FALSE)
plot (vlt.rlqB)

summary(vlt.rlqB)

randtest(vlt.rlqB, modeltype=6)

vlt.rlqB$cw

##only model 2 is significant
##use false discovery rate for p.adjust method

four2B<-fourthcorner.rlq(vlt.rlqB, modeltype=2,
                         p.adjust.method.G = "fdr",
                         p.adjust.method.D = "fdr",
                         typetest = "Q.axes")
plot(four2B, alpha=.05)

four2boutput<-as.data.frame(cbind(four2B$tabD2$obs,
                                  four2B$tabD2$names,
                                  four2B$tabD2$adj.pvalue))

names(four2boutput)<-c("Pearson","Relation", "adj.pvalue")

View(four2boutput)

##make 2 data frames:
four2Subsetaxis1 <- four2boutput[grep("AxcR1", four2boutput$Relation), ]
View(four2Subsetaxis1)

names(four2Subsetaxis1)<-c("PearsonAxis1", "RelationAxis1", "adj.pvalueAxis1")

four2Subsetaxis2<-four2boutput[grep("AxcR2", four2boutput$Relation), ]

head(four2Subsetaxis2)

names(four2Subsetaxis2)<-c("PearsonAxis2", "RelationAxis2", "adj.pvalueAxis2")

modB<-c("BF1", "BF2", "BF3", "BF4", "BF5",
        "BS1", "BS2", "BS3", "BS4", "BS5",
        "FH1", "FH2", "FH3", "FH4", "FH5",
        "F1", "F2", "F3",
        "LD1", "LD2", "LD3",
        "LH1", "LH2", "LH3", "LH4", "LH5", "LH6",
        "MV1", "MV2", "MV3", "MV4",
        "RS1", "RS2", "RS3", "RS4",
        "SO1", "SO2", "SO3")

TraitsB<-c(rep("Body form", 5),rep("Body size", 5), rep("Feeding habit", 5),
           rep("Fragility",3), rep("Larval development", 3), rep("Living habit", 6),
           rep("Movement",4), rep("Reproductive strategy", 4), rep("Sociability",3))


four2statDoutput<-as.data.frame(cbind(four2Subsetaxis1[,c(1,3)],four2Subsetaxis2[,c(1,3)], modB,
                                      BeaufortRDAFullMods, TraitsB))
View(four2statDoutput)

##Use full model to represent significant and non-significant environmental variables and
##modalities
##signficant modalities (p.adj < 0.05 from rlq.4thcorner model 2)
BeaufortModsSig<-as.data.frame(subset(four2statDoutput, adj.pvalueAxis1<0.05 | adj.pvalueAxis2<0.05))


# Possible visualisation

ggplot(four2statDoutput, aes(x = TraitsB, y = RDA1)) + geom_boxplot() + geom_hline(yintercept = 0)

ggplot(four2statDoutput, aes(x = TraitsB, y = RDA2)) + geom_boxplot() + geom_hline(yintercept = 0)


View(BeaufortModsSig)

BeaufortModsNotSig<-as.data.frame(subset(four2statDoutput, adj.pvalueAxis1>0.05 & adj.pvalueAxis2>0.05))

View(BeaufortModsNotSig)
##Stations from full model

BeaufortRDAFullStations

#Environmental variables
BeaufortRDAFullEnv

##Put all information on full RDA using ggplot
##show relationships of environmental variables that were signficant in model 2

RLQB<-ggplot()+
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2)+
  
  geom_point(data=BeaufortRDAFullStations, aes(x=RDA1, y=RDA2),
             size=4, alpha=.2)+
  geom_segment(data=BeaufortRDAFullEnv[c(1,5),], aes(x=0, xend=RDA1, y=0, yend=RDA2),
               alpha=1, size=.4, color="blue",
               arrow = arrow(length = unit(.2,"cm")))+
  geom_label(data=BeaufortRDAFullEnv[c(1,5),], aes(x=RDA1, y=RDA2, label=Env),
             color="blue")+
  geom_label(data=BeaufortRDAFullEnv[c(2:4,6,7),], aes(x=RDA1, y=RDA2, label=Env),
             color="grey50")+
  geom_label(data=BeaufortModsNotSig,
             aes(x=RDA1, y=RDA2, label=modB),color="grey50")+
  geom_label(data=BeaufortModsSig,
             aes(x=RDA1, y=RDA2, label=modB), color="red")+
  facet_wrap(~TraitsB)+
  xlim(-.4, 1.1)+
  ylim(-.5,1)+
  xlab("RDA1 71%")+
  ylab("RDA2 29%")+
  labs(title = "Beaufort Sea CWM-RDA with results from RLQ-4th corner anlasis")+
  theme_bw()


RLQB
##compare cwm-RDA to FDis

##FDisp
View(BeaufortQfuzzy)
dim(BeaufortLtrans)

rownames(BeaufortQfuzzy)<-colnames(BeaufortLtrans)

BeaufortQfuzzy[is.na(BeaufortQfuzzy)] <- 0
BeaufortSpeciesTraitsdist<-gowdis(BeaufortQfuzzy) ##created a gower distance matrix of species based on traits

FDisBeaufort<-fdisp(BeaufortSpeciesTraitsdist,
                    as.matrix(BeaufortLtrans))

hist(FDisBeaufort$FDis)


##FDis and RLQ stations



BeaufortFDisRDAFull<-as.data.frame(cbind(FDisBeaufort$FDis,
                                         BeaufortRDAFullStations, ##station points for RLQ
                                         station.dataBeaufort$Latitude,
                                         station.dataBeaufort$Longitude))
names(BeaufortFDisRDAFull)[1:5]<-c("FDis", "Axis1","Axis2", "Lat", "Long")
##test for SAC
library(nlme)
BmodFDis1<-gls(FDis~1, correlation = corExp(form = ~ Lat + Long,
                                            nugget = FALSE),
               method="ML", data=BeaufortFDisRDAFull)

BmodFDis2<-gls(FDis~1, method="ML",data=BeaufortFDisRDAFull)
anova(BmodFDis1, BmodFDis2)
library(MuMIn)
AICc(BmodFDis1, BmodFDis2)
##No spatial autocorrelation
BmodFDis<-lm(FDis~+poly(Axis1,2),
             data=BeaufortFDisRDAFull)
summary(BmodFDis)
library(visreg)
visreg(BmodFDis)

BmodFDis3<-lm(FDis~1, data=BeaufortFDisRDAFull)
anova(BmodFDis, BmodFDis3)
AICc(BmodFDis, BmodFDis3)

###Chukchi
ChukchiQ<-read.csv(file.choose(), row.names = 1) ##read in taxa by trait file
head(ChukchiQ)
ChukchiQfuzzy<-prep.fuzzy(ChukchiQ, c(5,5,3,3,6,4,4,5,3)) ##makes all modalities add to 1 within a trait
cor2<-cor(ChukchiQfuzzy)
View(cor2)
cor2<- ChukchiQfuzzy[(ChukchiQfuzzy > 0.75).any(1)]
ChukchiL<-read.csv(file.choose(), row.names = 1) ##taxa by station file
ChukchiLtrans<-ChukchiL^(1/2) ##transform biomass matrix
##make sure rows of Q-matrix match column names of L-matrix
##make LQ matrix
rownames(ChukchiQfuzzy)<-colnames(ChukchiLtrans)
ChukchiQfuzzy[is.na(ChukchiQfuzzy)] <- 0
ChukchiLQ<-as.data.frame(functcomp(as.matrix(ChukchiQfuzzy),
                                   as.matrix(ChukchiLtrans)))

ChukchiR<-read.csv(file.choose(), row.names = 1) ##import environmental data
ChukchiRstand<-as.data.frame(scale(ChukchiR)) ##standardized environmental data centered on zero
station.data<-read.csv(file.choose(), row.names = 1)##read in ChukchiLatLong

head(ChukchiRstand)
RDA_model0 <- rda( ChukchiLQ ~ 1+ Condition(as.matrix(station.data)),
                   data = ChukchiRstand[,c(1:7)])
RDA_model1 <- rda( ChukchiLQ ~ .+Condition(as.matrix(station.data)),
                   data = ChukchiRstand[,c(1:7)])

plot(RDA_model1, display=c("cn", "species"))
ChukchiRDAFull<-plot(RDA_model1)
ChukchiRDAFullMods<-as.data.frame(ChukchiRDAFull$species) ##modalities
ChukchiRDAFullStations<-as.data.frame(ChukchiRDAFull$sites)

ChukchiRDAFullEnv<-as.data.frame(cbind(as.data.frame(ChukchiRDAFull$biplot),c("Gravel","Sand", "Mud",
                                                                              "Depth","Temp", "Salinity",
                                                                              "Chl.a")))
names(ChukchiRDAFullEnv)[3]<-"Env"
##Forward selection occurs by including environmental variables with p-value < 0.05
##implies that factors with strong relationships to species
##or traits will be among those selected.
Chukchi_RDAmodel<-ordistep(RDA_model0, scope=formula(RDA_model1),
                           direction="forward",
                           Pin=.05, permutations = 999, R2scope = TRUE)
plot(Chukchi_RDAmodel)
##model selects mud, salinity, depth, sand, temp (but mud and temp are correlated..)
vif.cca(Chukchi_RDAmodel)##temp is high...not sure if I should remove this or not
anova(Chukchi_RDAmodel) ##significant
anova(Chukchi_RDAmodel , by = "axis")##RDA 1 and 2 are significant
anova(Chukchi_RDAmodel , by = "margin") ## mud, depth, sal, gravel, temp
plot(Chukchi_RDAmodel, display=c("cn", "species"))
RsquareAdj(Chukchi_RDAmodel) ##adj. r squared = 0.22
summary(Chukchi_RDAmodel)

##RLQ-4th corner analysis on selected environmental variables:
dudiL <- dudi.coa (ChukchiLtrans, scannf = FALSE) ##biomass
dudiR <- dudi.pca (ChukchiRstand[,c(3,4,5,6)], scannf = FALSE, row.w = dudiL$lw)##env
dudiQ <- dudi.pca (ChukchiQfuzzy, scannf = FALSE, row.w = dudiL$cw)##traits
vlt.rlq <- rlq (dudiR = dudiR, dudiL = dudiL, dudiQ = dudiQ, scannf = FALSE)
plot(vlt.rlq)
summary(vlt.rlq)
randtest(vlt.rlq)##rlq test model 2 is significant
four2<-fourthcorner.rlq(vlt.rlq, modeltype=2,
                        p.adjust.method.G = "fdr",
                        p.adjust.method.D = "fdr",
                        typetest = "Q.axes")
plot(four2, alpha=.05)
four2$tabD2
##plot modalities on cwm-RDA
##modalities of cwm:
modsAbr<-c("BF1", "BF2", "BF3", "BF4","BF5",
           "FH1", "FH2", "FH3", "FH4", "FH5",
           "F1", "F2", "F3",
           "LD1", "LD2","LD3",
           "LH1", "LH2", "LH3","LH4", "LH5", "LH6",
           "MV1", "MV2", "MV3", "MV4",
           "R1", "R2", "R3","R4",
           "BS1", "BS2", "BS3", "BS4", "BS5",
           "SO1", "SO2", "SO3")
Traits<-c(rep("Body form", 5), rep("Feeding habit", 5),
          rep("Fragility",3), rep("Larval development",3),
          rep("Living habit", 6), rep("Movement",4),
          rep("Reproductive strategy", 4), rep("Body size", 5),
          rep("Sociability", 3))
##make dataframes for signficant and non-significat modalities
four2Coutput<-as.data.frame(cbind(four2$tabD2$obs,
                                  four2$tabD2$names,
                                  four2$tabD2$adj.pvalue))
names(four2Coutput)<-c("Pearson","Relation", "adj.pvalue")
View(four2Coutput)
##make 2 data frames:
four2CSubsetaxis1 <- four2Coutput[grep("AxcR1", four2Coutput$Relation), ]
View(four2CSubsetaxis1)
names(four2CSubsetaxis1)<-c("PearsonAxis1", "RelationAxis1", "adj.pvalueAxis1")
four2CSubsetaxis2<-four2Coutput[grep("AxcR2", four2Coutput$Relation), ]
head(four2CSubsetaxis2)
names(four2CSubsetaxis2)<-c("PearsonAxis2", "RelationAxis2", "adj.pvalueAxis2")

four2CstatDoutput<-as.data.frame(cbind(four2CSubsetaxis1[,c(1,3)],four2CSubsetaxis2[,c(1,3)], modsAbr,
                                       ChukchiRDAFullMods, Traits))
View(four2statDoutput)
##Use full model to represent significant and non-significant environmental variables and
##modalities
##signficant modalities (p.adj < 0.05 from rlq.4thcorner model 2)
ChukchiModsSig<-as.data.frame(subset(four2CstatDoutput, adj.pvalueAxis1<0.05 | adj.pvalueAxis2<0.05))
View(ChukchiModsSig)
ChukchiModsNotSig<-as.data.frame(subset(four2CstatDoutput, adj.pvalueAxis1>0.05 & adj.pvalueAxis2>0.05))
View(ChukchiModsNotSig)

##ChukchiEnv
ChukchiRDAFullEnv
##ChukchiMods
RLQC<-ggplot()+
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2)+
  geom_point(data=ChukchiRDAFullStations, aes(x=RDA1, y=RDA2),
             size=4, alpha=.2)+
  geom_segment(data=ChukchiRDAFullEnv[c(3:6),], aes(x=0, xend=RDA1, y=0, yend=RDA2),
               alpha=1, size=.4, color="blue",
               arrow = arrow(length = unit(.2,"cm")))+
  geom_label(data=ChukchiRDAFullEnv[c(3:6),], aes(x=RDA1, y=RDA2, label=Env),
             color="blue")+
  geom_label(data=ChukchiRDAFullEnv[c(1,2,7),], aes(x=RDA1, y=RDA2, label=Env),
             color="grey50")+
  geom_label(data=ChukchiModsNotSig,
             aes(x=RDA1, y=RDA2, label=modsAbr),color="grey50")+
  geom_label(data=ChukchiModsSig,
             aes(x=RDA1, y=RDA2, label=modsAbr), color="red")+
  facet_wrap(~Traits)+
  xlim(-.8, .8)+
  ylim(-.75,.5)+
  xlab("RDA1 58%")+
  ylab("RDA2 23%")+
  labs(title = "Chukchi Sea CWM-RDA with results from RLQ-4th corner anlasis")+
  theme_bw()
RLQC

##FDis ~cwmRDA points
ChukchiRDAFull<-plot(Chukchi_RDAmodel)
ChukchiSitesFull<-as.data.frame(ChukchiRDAFull$sites)

##FDisp
View(ChukchiQfuzzy)
dim(BeaufortLtrans)
rownames(ChukchiQfuzzy)<-colnames(ChukchiLtrans)
ChukchiQfuzzy[is.na(ChukchiQfuzzy)] <- 0
ChukchiSpeciesTraitsdist<-gowdis(ChukchiQfuzzy) ##created a gower distance matrix of species based on traits
FDisChukchi<-fdisp(ChukchiSpeciesTraitsdist,
                   as.matrix(ChukchiLtrans))
hist(FDisChukchi$FDis)

##test for the relationship of FDis ~ Axis 1 of cwm-rda

ChukchiFDisRDAFull<-as.data.frame(cbind(FDisChukchi$FDis,
                                        ChukchiSitesFull,
                                        station.data$Latitude,
                                        station.data$Longitude))
names(ChukchiFDisRDAFull)[1:5]<-c("FDis", "Axis1","Axis2", "Lat", "Long")
View(ChukchiFDisRDAFull)
##we know SAC
##test FDis amongst RDA axes
CmodFDisFull1<-gls(FDis~1,
                   correlation = corExp(form = ~ Lat + Long,
                                        nugget = FALSE),
                   data=ChukchiFDisRDAFull)
CmodFDisFull<-gls(FDis~1,
                  data=ChukchiFDisRDAFull)
anova(CmodFDisFull1, CmodFDisFull)
AICc(CmodFDisFull1, CmodFDisFull)
##test FDis
CmodFDisFull<-gls(FDis~poly(Axis1,2),
                  correlation = corExp(form = ~ Lat + Long,
                                       nugget = FALSE),
                  method="ML",data=ChukchiFDisRDAFull)

summary(CmodFDisFull)
visreg(CmodFDisFull)
summary(CmodFDisFull)