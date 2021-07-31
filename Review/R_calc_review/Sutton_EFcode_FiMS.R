##libraries needed
library(FD)
library(vegan)
library(nlme)
library(visreg)

######## CWM #########
##I evaluated the Chukchi and Beaufort Sea shelves separately. 
#First, I found the community-weighted-means (cwm) for each shelf. 
#For this, you need a fuzzy-coded trait by taxon matrix (Q) 
#and an abundance (in my case, biomass) by taxon matrix (L). 
#Before calculating the CWMs, I square root transformed the biomass 
#for each shelf. Additionally, the Q matrix was standardized using 
#the prep.fuzzy function in the ade4 package. 

##CWM (Chukchi)
ChukchiQ<-read.csv(file.choose(), row.names = 1) ##read in taxa by trait file
ChukchiQfuzzy<-prep.fuzzy(ChukchiQ, c(5,5,5,3,3,6,4,4,3)) ##makes all modalities add to 1 within a trait
ChukchiL<-read.csv(file.choose(), row.names = 1) ##taxa by station file
ChukchiLtrans<-ChukchiL^(1/2) ##transform biomass matrix
##make sure rows of Q-matrix match column names of L-matrix
rownames(ChukchiQfuzzy)<-colnames(ChukchiLtrans)
##CWM computation
ChukchiLQ<-as.data.frame(functcomp(as.matrix(ChukchiQfuzzy),
                                   as.matrix(ChukchiLtrans)),
                         bin.num=NULL)
View(ChukchiLQ) ##cwm matrix (Chukchi)

##CWM (Beaufort)
BeaufortQ<-read.csv(file.choose(), row.names=1) ##read in taxa by trait file
BeaufortQfuzzy<-prep.fuzzy(BeaufortQ, c(5,5,5,3,3,6,4,4,3))##makes all modalities add to 1 within a trait
BeaufortL<-read.csv(file.choose(), row.names = 1) ##taxa by station file
BeaufortLtrans<-BeaufortL^(1/2) ##transform biomass matrix
##make sure rows of Q-matrix match column names of L-matrix
rownames(BeaufortQfuzzy)<-colnames(BeaufortLtrans)
##CWM computation
BeaufortLQ<-as.data.frame(functcomp(as.matrix(BeaufortQfuzzy),
                                    as.matrix(BeaufortLtrans)),
                          bin.num=NULL)

View(BeaufortLQ) ##cwm matrix (Beaufort)


######## CWM-RDA (shelf-level) #########
##To perform a forward-slection CWM-RDA, we need the LQ-matrix and and environmental
## (R) matrix. 

##Shelf-level CWM-RDA (Chukchi; all traits included)
ChukchiR<-read.csv(file.choose(), row.names = 1) ##import environmental data
ChukchiRstand<-as.data.frame(scale(ChukchiR)) ##standardized environmental data centered on zero
ChukchiRDA0<-rda(ChukchiLQ ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1<-rda(ChukchiLQ~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstep<-ordiR2step(ChukchiRDA0, scope=formula(ChukchiRDA1),direction="forward",
                           Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstep$CCA$tot.chi/ChukchiRDAstep$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstep) ##find the adjusted r-squared
summary(ChukchiRDAstep) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
Chukplot<-plot(ChukchiRDAstep)
ChukchiSites<-as.data.frame(Chukplot$sites)##get station locations on RDA axes based on RDA
##ChukhiSites will be be used as proxies for environmental gradients

##Shelf-level CWM-RDA (Beaufort; all traits included)
BeaufortR<-read.csv(file.choose(), row.names = 1) ##import environmental data
BeaufortRstand<-as.data.frame(scale(BeaufortR)) ##standardized environmental data centered on zero
BeaufortRDA0<-rda(BeaufortLQ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1<-rda(BeaufortLQ~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstep<-ordiR2step(BeaufortRDA0, scope=formula(BeaufortRDA1),direction="forward",
                            Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model
BeaufortRDAstep$CCA$tot.chi/BeaufortRDAstep$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstep) ##find the adjusted r-squared
summary(BeaufortRDAstep) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
Beaufortplot<-plot(BeaufortRDAstep)
BeaufortSites<-as.data.frame(Beaufort$sites)##get station locations on RDA axes based on RDA
##BeaufortSites will be be used as proxies for environmental gradients

##plot CWM-RDA (Chukchi and Beaufort)
par(mfrow=c(1,1),mar=c(1,1,0,0))
plot(ChukchiRDAstep)
plot(BeaufortRDAstep)

######## CWM-RDA (trait -level) #########

##Trait-level CWM-RDA (Chukchi)

##Trait-level CWM-RDA  (Chukchi; Body form)
head(ChukchiLQ)
ChukchiRDA0BF<-rda(ChukchiLQ[,1:5] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1BF<-rda(ChukchiLQ[,1:5]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepBF<-ordiR2step(ChukchiRDA0BF, scope=formula(ChukchiRDA1BF),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepBF$CCA$tot.chi/ChukchiRDAstepBF$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepBF) ##find the adjusted r-squared
summary(ChukchiRDAstepBF) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepBF)

##Trait-level CWM-RDA  (Chukchi; Body size)
head(ChukchiLQ)
ChukchiRDA0BS<-rda(ChukchiLQ[,6:10] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1BS<-rda(ChukchiLQ[,6:10]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepBS<-ordiR2step(ChukchiRDA0BS, scope=formula(ChukchiRDA1BS),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepBS$CCA$tot.chi/ChukchiRDAstepBS$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepBS) ##find the adjusted r-squared
summary(ChukchiRDAstepBS) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepBS)

##Trait-level CWM-RDA  (Chukchi; Feeding habit)
View(ChukchiLQ)
ChukchiRDA0FH<-rda(ChukchiLQ[,11:15] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1FH<-rda(ChukchiLQ[,11:15]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepFH<-ordiR2step(ChukchiRDA0FH, scope=formula(ChukchiRDA1FH),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepFH$CCA$tot.chi/ChukchiRDAstepFH$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepFH) ##find the adjusted r-squared
summary(ChukchiRDAstepFH) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepFH)

##Trait-level CWM-RDA  (Chukchi; Fragility)
View(ChukchiLQ)
ChukchiRDA0FR<-rda(ChukchiLQ[,16:18] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1FR<-rda(ChukchiLQ[,16:18]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepFR<-ordiR2step(ChukchiRDA0FR, scope=formula(ChukchiRDA1FR),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepFR$CCA$tot.chi/ChukchiRDAstepFR$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepFR) ##find the adjusted r-squared
summary(ChukchiRDAstepFR) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepFR)

##Trait-level CWM-RDA  (Chukchi; Larval development)
View(ChukchiLQ)
ChukchiRDA0LD<-rda(ChukchiLQ[,19:21] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1LD<-rda(ChukchiLQ[,19:21]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepLD<-ordiR2step(ChukchiRDA0LD, scope=formula(ChukchiRDA1LD),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepLD$CCA$tot.chi/ChukchiRDAstepLD$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepLD) ##find the adjusted r-squared
summary(ChukchiRDAstepLD) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepLD)

##Trait-level CWM-RDA  (Chukchi; Living habit)
View(ChukchiLQ)
ChukchiRDA0LH<-rda(ChukchiLQ[,22:27] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1LH<-rda(ChukchiLQ[,22:27]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepLH<-ordiR2step(ChukchiRDA0LH, scope=formula(ChukchiRDA1LH),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepLH$CCA$tot.chi/ChukchiRDAstepLH$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepLH) ##find the adjusted r-squared
summary(ChukchiRDAstepLH) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepLH)

##Trait-level CWM-RDA  (Chukchi; Movement)
ChukchiRDA0MV<-rda(ChukchiLQ[,28:31] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1MV<-rda(ChukchiLQ[,28:31]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepMV<-ordiR2step(ChukchiRDA0MV, scope=formula(ChukchiRDA1MV),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepMV$CCA$tot.chi/ChukchiRDAstepMV$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepMV) ##find the adjusted r-squared
summary(ChukchiRDAstepMV) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepMV)

##Trait-level CWM-RDA  (Chukchi; Reproductive strategy)
ChukchiRDA0RS<-rda(ChukchiLQ[,32:35] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1RS<-rda(ChukchiLQ[,32:35]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepRS<-ordiR2step(ChukchiRDA0RS, scope=formula(ChukchiRDA1RS),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepRS$CCA$tot.chi/ChukchiRDAstepRS$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepRS) ##find the adjusted r-squared
summary(ChukchiRDAstepRS) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepRS)

##Trait-level CWM-RDA  (Chukchi; Sociability)
ChukchiRDA0SO<-rda(ChukchiLQ[,36:38] ~1, ChukchiRstand) # Model with intercept only
ChukchiRDA1SO<-rda(ChukchiLQ[,36:38]~., ChukchiRstand) # Model with all explanatory variables
ChukchiRDAstepSO<-ordiR2step(ChukchiRDA0SO, scope=formula(ChukchiRDA1SO),direction="forward",
                             Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

ChukchiRDAstepSO$CCA$tot.chi/ChukchiRDAstepSO$tot.chi ##total variance explained
RsquareAdj(ChukchiRDAstepSO) ##find the adjusted r-squared
summary(ChukchiRDAstepSO) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(ChukchiRDAstepSO)

##Trait-level CWM-RDA (Beaufort)
##Trait-level CWM-RDA  (Beaufort; Body form)
BeaufortRDA0BF<-rda(BeaufortLQ[,1:5] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1BF<-rda(BeaufortLQ[,1:5]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepBF<-ordiR2step(BeaufortRDA0BF, scope=formula(BeaufortRDA1BF),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepBF$CCA$tot.chi/BeaufortRDAstepBF$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepBF) ##find the adjusted r-squared
summary(BeaufortRDAstepBF) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepBF)

##Trait-level CWM-RDA  (Beaufort; Body size)
BeaufortRDA0BS<-rda(BeaufortLQ[,6:10] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1BS<-rda(BeaufortLQ[,6:10]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepBS<-ordiR2step(BeaufortRDA0BS, scope=formula(BeaufortRDA1BS),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepBS$CCA$tot.chi/BeaufortRDAstepBS$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepBS) ##find the adjusted r-squared
summary(BeaufortRDAstepBS) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepBS)

##Trait-level CWM-RDA  (Beaufort; Feeding habit)
head(BeaufortLQ)
BeaufortRDA0FH<-rda(BeaufortLQ[,11:15] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1FH<-rda(BeaufortLQ[,11:15]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepFH<-ordiR2step(BeaufortRDA0FH, scope=formula(BeaufortRDA1FH),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepFH$CCA$tot.chi/BeaufortRDAstepFH$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepFH) ##find the adjusted r-squared
summary(BeaufortRDAstepFH) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepFH)

##Trait-level CWM-RDA  (Beaufort; Fragility)
BeaufortRDA0FR<-rda(BeaufortLQ[,16:18] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1FR<-rda(BeaufortLQ[,16:18]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepFR<-ordiR2step(BeaufortRDA0FR, scope=formula(BeaufortRDA1FR),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepFR$CCA$tot.chi/BeaufortRDAstepFR$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepFR) ##find the adjusted r-squared
summary(BeaufortRDAstepFR) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepFR)

##Trait-level CWM-RDA  (Beaufort; Larval development)
BeaufortRDA0LD<-rda(BeaufortLQ[,19:21] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1LD<-rda(BeaufortLQ[,19:21]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepLD<-ordiR2step(BeaufortRDA0LD, scope=formula(BeaufortRDA1LD),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepLD$CCA$tot.chi/BeaufortRDAstepLD$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepLD) ##find the adjusted r-squared
summary(BeaufortRDAstepLD) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepLD)

##Trait-level CWM-RDA  (Beaufort; Living habit)
BeaufortRDA0LH<-rda(BeaufortLQ[,22:27] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1LH<-rda(BeaufortLQ[,22:27]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepLH<-ordiR2step(BeaufortRDA0LH, scope=formula(BeaufortRDA1LH),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepLH$CCA$tot.chi/BeaufortRDAstepLH$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepLH) ##find the adjusted r-squared
summary(BeaufortRDAstepLH) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepLH)

##Trait-level CWM-RDA  (Beaufort; Movement)
BeaufortRDA0MV<-rda(BeaufortLQ[,28:31] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1MV<-rda(BeaufortLQ[,28:31]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepMV<-ordiR2step(BeaufortRDA0MV, scope=formula(BeaufortRDA1MV),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepMV$CCA$tot.chi/BeaufortRDAstepMV$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepMV) ##find the adjusted r-squared
summary(BeaufortRDAstepMV) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepMV)

##Trait-level CWM-RDA  (Beaufort; Reproductive strategy)
BeaufortRDA0RS<-rda(BeaufortLQ[,32:35] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1RS<-rda(BeaufortLQ[,32:35]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepRS<-ordiR2step(BeaufortRDA0RS, scope=formula(BeaufortRDA1RS),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepRS$CCA$tot.chi/BeaufortRDAstepRS$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepRS) ##find the adjusted r-squared
summary(BeaufortRDAstepRS) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepRS)

##Trait-level CWM-RDA  (Beaufort; Sociability)
BeaufortRDA0SO<-rda(BeaufortLQ[,36:38] ~1, BeaufortRstand) # Model with intercept only
BeaufortRDA1SO<-rda(BeaufortLQ[,36:38]~., BeaufortRstand) # Model with all explanatory variables
BeaufortRDAstepSO<-ordiR2step(BeaufortRDA0SO, scope=formula(BeaufortRDA1SO),direction="forward",
                              Pin=.05, permutations = 999, R2scope = FALSE) ##forward-selection model

BeaufortRDAstepSO$CCA$tot.chi/BeaufortRDAstepSO$tot.chi ##total variance explained
RsquareAdj(BeaufortRDAstepSO) ##find the adjusted r-squared
summary(BeaufortRDAstepSO) ##find the Accumulated constrained eigenvalues for variance explained by RDA axes
plot(BeaufortRDAstepSO)


###### Trait Convergence ########
##FDis comapred between shelves
ArcticSp<-read.csv(file.choose(), row.names = 1)
colSums(ArcticSp)
ArcticSp2<-ArcticSp^(1/2)
ChukchiQfuzzy2<-ChukchiQfuzzy[c(1:57, 59:327),]##make sure all species are present!
rownames(ChukchiQfuzzy2)<-colnames(ArcticSp2)
ArcticSpeciesTraitsdist<-vegdist(ChukchiQfuzzy2, method="gower",
                                 na.rm = TRUE) ##created a gower distance matrix of species based on traits
ArcticFDisBothSeas<-fdisp(ArcticSpeciesTraitsdist, 
                          as.matrix(ArcticSp2))

##Import csv file with station, sea, latitude, and longitude
ArcticSea<-read.csv(file.choose(), row.names = 1)
head(ArcticSea)
##Add FDis to ArcticSea dataframe
ArcticSeaFDis<-cbind(ArcticSea, ArcticFDisBothSeas$FDis) ##import "Arctic_Sea_Lat_Long.csv"
names(ArcticSeaFDis)[4]<-"FDis" ##rename column 4 'FDis'

##First check for spatial-auto-correlation
m1<-gls(FDis~Sea, corExp(form = ~ Latitude + Longitude | Sea, 
                         nugget = TRUE),method="ML",
        data=ArcticSeaFDis)
summary(m1)
m2<-gls(FDis~Sea, method="ML",data=ArcticSeaFDis)
anova(m1, m2)
##No autocorrelation, so proceed with ordinary least squares
m3<-lm(FDis~Sea,data=ArcticSeaFDis)
m4<-lm(FDis~1, data=ArcticSeaFDis)
anova(m3, m4)
summary(m3)

######## Model selection for FDis ~ RDA axes #######
##Here, we used the position of stations along each shelf-RDA,
##indicated by ChukchiSites in the Chukchi Sea and BeaufortSites in the Beaufort Sea


##Model-selection (Chukchi)
#Chukchi
#create new data frame with ChukchiSites and FDis
ChukchiFDisRDAaxes<-cbind(ArcticSeaFDis[1:67,]$FDis,
                          ChukchiSites)
names(ChukchiFDisRDAaxes)[1]<-"FDis"

CmodFDis<-lm(FDis~poly(RDA1,2)+poly(RDA2,2),
             data=ChukchiFDisRDAaxes)
summary(CmodFDis)
CmodFDis2<-step(CmodFDis)
summary(CmodFDis2)
library(visreg)
visreg(CmodFDis)

#Beaufort
#create new data frame with BeaufortSites and FDis
BeaufortRDAaxes<-cbind(ArcticSeaFDis[68:113,]$FDis,
                       BeaufortSites)
names(BeaufortRDAaxes)[1]<-"FDis"

BmodFDis<-lm(FDis~poly(RDA1,2)+poly(RDA2,2), 
             data = BeaufortRDAaxes)
BmodFDis2<-step(BmodFDis)
summary(BmodFDis2)

BmodFDis3<-lm(FDis~poly(RDA1,2)+poly(RDA2,1),
              data=BeaufortRDAaxes)
summary(BmodFDis3)
anova(BmodFDis, BmodFDis3) ##no significant difference between BmodFDis and BmodFDis3.
##More simple, better fit model was BmodFDis3
visreg(BmodFDis3)
