#    Introduction to Introduction to MCMC, 
#    Linear Mixed Effects models and GLMM with R
#    Highland Statistics Ltd.
#    www.highstat.com

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  


######################################################################
#Cheetah exercise
#################################################################
#Set the working directory and import the data
setwd("/Users/Highstat/applicat/HighlandStatistics/Books/BGS/GLM/Data/Cheetahs")

ZooData <- read.table('extra_data/ZooData.txt', header=TRUE)
names(ZooData)

#See pdf for a description
###################################################################




###################################################################
#Load packages and library files
library(lattice)  #Needed for multi-panel graphs
library(lme4)
library(R2jags)
source(file="extra_data/HighstatLibV6.R")  
source("extra_data/MCMCSupportHighstat.R")
########################################################



########################################################
#Housekeeping
#Code factors as factors
ZooData$fRaised  	<- factor(ZooData$Raised)
ZooData$fFeeding 	<- factor(ZooData$Feeding)
ZooData$fOc 	    <- factor(ZooData$Oc)
ZooData$fOther 		<- factor(ZooData$Other)
ZooData$fEnrichment <- factor(ZooData$Enrichment)
ZooData$fGroup 		<- factor(ZooData$Group)
ZooData$fSex 		<- factor(ZooData$Sex)
ZooData$fZoo 		<- factor(ZooData$Zoo) 
########################################################


 


########################################################
#Data exploration
#Outliers
MyVar <- c("Scans", "Number", "Proportion", "Size", "Visual", "Visitors", "Enclosure", "Vehicle", "Diet", "Age")
Mydotplot(ZooData[,MyVar])

#Open to discussion...but we decided to do this:
ZooData$LSize <- log(ZooData$Size)



#Collinearity
MyVar <- c("LSize", "Visual", "Visitors", 
           "Enclosure", "Vehicle", "Diet", "Age")
pairs(ZooData[,MyVar], lower.panel = panel.cor)

#Use the 0/1 variables and not there factor equivalents
corvif(ZooData[ ,c("LSize",  "Visual", "Visitors", 
                   "Age", "Enclosure", "Vehicle", 
                   "Diet" , "Raised", 
                   "Feeding", "Oc",  "Other", 	
                   "Enrichment","Group", 	"Sex")])
#Drop Diet

corvif(ZooData[ ,c("LSize",  "Visual", "Visitors", 
                   "Age", "Enclosure", "Vehicle", 
                          "Raised", 
                   "Feeding", "Oc",  "Other", 	
                   "Enrichment","Group", 	"Sex")])
#Drop Visual
corvif(ZooData[ ,c("LSize",            "Visitors", 
                   "Age", "Enclosure", "Vehicle", 
                          "Raised", 
                   "Feeding", "Oc",  "Other", 	
                   "Enrichment","Group", 	"Sex")])
#Now all VIFs < 3
#See Zuur et al. (2010) for an explanation
#Conclusion: drop diet and visual from the analysis

#Enough observations per zoo?
table(ZooData$Zoo)
#Hmmm, one zoo has only 1 observation.
#Keep an eye on it

#Relationships
MyVar <- c("LSize", 
           "Visitors", "Enclosure", 
           "Vehicle", "Age")
Myxyplot(ZooData, MyVar, "Proportion", MyYlab = "Proportions")



par(mfrow = c(2,4), mar = c(3,3,2,2))
boxplot(Proportion ~ fRaised, data = ZooData)
boxplot(Proportion ~ fFeeding, data = ZooData)
boxplot(Proportion ~ fOc, data = ZooData)
boxplot(Proportion ~ fOther, data = ZooData)
boxplot(Proportion ~ fEnrichment, data = ZooData)
boxplot(Proportion ~ fGroup, data = ZooData)
boxplot(Proportion ~ fSex, data = ZooData)
#Add some titles!!!
########################################################


#Results data exploration
#One covariate was transformed
#Two covariates were dropped
#One zoo with only one observation...keep an eye in this one!
########################################################




########################################################
#Start Bayesian GLMM analysis
detach(package:mgcv)
detach(package:nlme)
library(lme4)

#To make the code more compact, rename ZooData as ZD
ZD <- ZooData


#Also for this anaysis it is helpful to 
#standardise all continuous covariates.
#
MyStd <- function(x) {(x-mean(x, na.rm = TRUE)) / sd(x, na.rm=TRUE)}


ZD$cLSize     <- MyStd(ZD$LSize)
ZD$cVisitors  <- MyStd(ZD$Visitors)
ZD$cAge       <- MyStd(ZD$Age)
ZD$cEnclosure <- MyStd(ZD$Enclosure)
ZD$cVehicle   <- MyStd(ZD$Vehicle)

#By design of the study we will use zoo as random intercept.
#We have Number out of Scans successes....hence this is 
#a binomial disitrbution. The model is:

#The model:
 # Success_ij    ~ Bin(pi_ij, N_ij)
 # E(Success_ij)  = pi_ij * N_ij

 # logit(pi_ij)   = fixed stuff + a_i
 # p_ij = exp(fixed stuff + a_i) / (1 + exp(fixed stuff + a_i))

 # a_i ~ N(0, sigma^2_zoo)
 # i is zoo index
 # j = 1, .., n_i

#Reference model:
ZD$Neg <- ZD$Scans - ZD$Number
M1 <- glmer(cbind(Number, Neg) ~ cLSize + cVisitors+ fFeeding+ 
            fOc + fOther + fEnrichment + fGroup + fSex + 
            cEnclosure + cVehicle+ cAge + (1 | fZoo), 
            family = binomial, data = ZD)
summary(M1)

####################################
#JAGS..what will we learn in this exercise
# 1. How to do a binomial GLMM
# 2. How to do a beta-binomial GLMM in JAGS
#    This can also be done in glmmADMB....

##Step 1: Fit the model.
#Use MCMC to estimate the parameters                     
#1. Bundle data
X <- model.matrix(~ cLSize + cVisitors+ fFeeding+ 
                    fOc + fOther + fEnrichment + fGroup + fSex + 
                    cEnclosure + cVehicle+ cAge , data = ZD)                      
K <- ncol(X)  #Number of columns
head(X)

#Random effects:
Zoo <- as.numeric(as.factor(ZD$fZoo))
Zoo
Nre <- length(unique(ZD$fZoo))


#The code below is copied an pasted from the owls
#Nest was changed into Plot
win.data <- list(Y      = ZD$Number, 
                 Trials = ZD$Scans,
                 X      = X,
                 N      = nrow(ZD),
                 K      = ncol(X),
                 Zoo    = Zoo,
                 Nre    = Nre)
win.data

###################################################
# 2. JAGS modelling code
sink("GLMMZooJAGS.txt")
cat("
model{
    #1A. Priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}

    #1B. Priors random effects and sigma_Zoo
    for (i in 1:Nre) { a[i] ~ dnorm(0, tau_Zoo)}
    tau_Zoo <- 1 / (sigma_Zoo * sigma_Zoo)
    num           ~ dnorm(0, 0.0016)             #<----half-Cauchy(25)
    denom         ~ dnorm(0, 1)                  #<----half-Cauchy(25)
    sigma_Zoo   <- abs(num / denom)              #<----half-Cauchy(25)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]         ~ dbin(Pi[i], Trials[i])  
      logit(Pi[i]) <- eta[i] 
      eta[i]      <- inprod(beta[], X[i,]) + a[Zoo[i]] 
  
      #Saving computer time
      ##3. Discrepancy measures 
      Exp[i] <- Pi[i] * Trials[i] 
      Var[i] <- Pi[i] * Trials[i] * (1 - Pi[i])
      E[i]   <- (Y[i]  - Exp[i]) / sqrt(Var[i])    
     
      YNew[i] ~  dbin(Pi[i], Trials[i])            #Simulated data with mean/variance taken from the fitted model
      ENew[i] <- (YNew[i] - Exp[i]) / sqrt(Var[i]) #Normalized residual for predicted data
      D[i]    <- pow(E[i], 2)                      
      DNew[i] <- pow(ENew[i], 2)   
     }          
     Fit         <- sum(D[1:N])                     #Sum of squared residuals  
     FitNew      <- sum(DNew[1:N])                  #Sum of squared predicted residuals
}
",fill = TRUE)
sink()
#####################################

#3. Initial values & parameters to save
inits  <- function () {
  list(
    beta  = rnorm(ncol(X), 0, 0.01),
    a     = rnorm(Nre, 0, 0.01),
    num   = rnorm(1, 0, 25),   #<-- half-Cauchy(25)
    denom = rnorm(1, 0, 1)  )  }

#This now becomes an issue for this data size.
#Let's take the minimal output
params <- c("beta", "E", "a", 
            "sigma_Zoo", "Pi", 
            "Fit", "FitNew")


#Don't forget to change the file name!
#4. Start JAGS
J1 <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "GLMMZooJAGS.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

J2  <- update(J1, n.iter = 10000)  
out <- J2$BUGSoutput

print(J2, digits = 3)  #Takes a few minutes
#Write down DIC at this point


#5. Assess mixing
MyBUGSChains(out, c(uNames("beta", K), "sigma_Zoo"))
MyBUGSACF(out, c(uNames("beta", K), "sigma_Zoo"))


#6. Present output
K <- ncol(X)
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma_Zoo"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma_Zoo"))
#Some parameters are not significant!


######################################################
# Assess goodness of fit: Bayesion p-value:
# We have a large number of FitNew and Fit values. 
# Count how often one is bigger than the other:

mean(out$sims.list$FitNew >  out$sims.list$Fit)
#Yes...we expected that based on the frequentist results!

E   <- out$mean$E
sum(E^2) / (nrow(ZD) - 12-1)


#Plot the sum of squares for each iteration. 
Min <- min(out$sims.list$Fit, out$sims.list$FitNew)
Max <- max(out$sims.list$Fit, out$sims.list$FitNew)

xyplot(out$sims.list$FitNew ~ out$sims.list$Fit, 
       aspect = "iso", 
       col = 1, 
       pch = 16, 
       cex = 0.7,
       xlim = c(Min, Max), 
       ylim = c(Min, Max)) 

#################################################################
#Same story...why do we have overdispersion?

#Investigate the cause of overdispersion
#############################################################
#Why is there overdispersion?
 #A. Missing covariate(s), missing interactions  --> Add them
 #B. Dependency                  --> Add random slope (brave)
 #C. Zero inflation              --> Zero inflated Binomial GLMM (MCMC)
 #D. Outliers                    --> Remove?
 #E. Non-linear effects          --> Binomial GAMM 
 #F. More variation              --> Beta-binomial
 #   Poisson <--> NB  / binomial <--> beta-binomial 
#############################################################
#What is the story here?

#Apply a model validation
ZD$E <- E
vars <- c("cLSize", "cVisitors",  "cEnclosure", "cVehicle", "cAge")
Myxyplot(ZD, vars,"E")

Pi <- out$mean$Pi
par(mar = c(5,5,2,2))
plot(x = Pi,
     y = E, 
     xlab = "Fitted values", 
     ylab = "Pearson residuals", 
     cex.lab = 1.5)
abline(h = 0, lty = 2)

#Plot fitted versus observed values
plot(x = Pi, y = ZD$Proportion)


#Can't pinpoint any cause for overdispersion.
#Now what?
#
# 1.Quick and dirty solution for overdispersion: 
#   Add the observation level random intercept
# 2. Beta binomial   (this is what NB is for Poisson)
 
# Both can be done in a Bayesian context. 



########################################################
#Beta-binomial GLMM in JAGS

X <- model.matrix(~ cLSize + cVisitors+ fFeeding+ 
                    fOc + fOther + fEnrichment + fGroup + fSex + 
                    cEnclosure + cVehicle+ cAge , data = ZD)                      
K <- ncol(X)  #Number of columns
head(X)

#Random effects:
Zoo <- as.numeric(as.factor(ZD$fZoo))
Zoo
Nre <- length(unique(ZD$fZoo))


#The code below is copied an pasted from the owls
#Nest was changed into Plot
win.data <- list(Y      = ZD$Number, 
                 Trials = ZD$Scans,
                 X      = X,
                 N      = nrow(ZD),
                 K      = ncol(X),
                 Zoo    = Zoo,
                 Nre    = Nre)
win.data

###################################################
# 2. JAGS modelling code
sink("GLMMZooJAGS.txt")
cat("
model{
    #1A. Priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}

    #1B. Priors random effects and sigma_Zoo
    for (i in 1:Nre) { a[i] ~ dnorm(0, tau_Zoo)}
    tau_Zoo <- 1 / (sigma_Zoo * sigma_Zoo)
    num           ~ dnorm(0, 0.0016)             #<----half-Cauchy(25)
    denom         ~ dnorm(0, 1)                  #<----half-Cauchy(25)
    sigma_Zoo   <- abs(num / denom)              #<----half-Cauchy(25)

    #1C. Priors for theta
    numtheta   ~ dnorm(0, 0.0016) 
    denomtheta ~ dnorm(0, 1)
    theta <- abs(numtheta / denomtheta) 
    

    #2. Likelihood
    for (i in 1:N) {
      Y[i]         ~ dbin(p[i], Trials[i])  
      
      p[i] ~ dbeta(shape1[i], shape2[i])
      shape1[i] <- theta * Pi[i]        #a
      shape2[i] <- theta * (1 - Pi[i])  #b

      logit(Pi[i]) <- eta[i] 
      eta[i]      <- inprod(beta[], X[i,]) + a[Zoo[i]] 
  
      #3. Discrepancy measures 
      Exp[i] <- p[i] * Trials[i] 
      Var[i] <- p[i] * Trials[i] * (1 - p[i]) * (1 + (Trials[i] / (theta + 1)))
      E[i]   <- (Y[i]  - Exp[i]) / sqrt(Var[i])    
     
      YNew[i] ~  dbin(p[i], Trials[i])            #Simulated data with mean/variance taken from the fitted model
      ENew[i] <- (YNew[i] - Exp[i]) / sqrt(Var[i]) #Normalized residual for predicted data
      D[i]    <- pow(E[i], 2)                      
      DNew[i] <- pow(ENew[i], 2)   
     }          
     Fit         <- sum(D[1:N])                     #Sum of squared residuals  
     FitNew      <- sum(DNew[1:N])                  #Sum of squared predicted residuals
}
",fill = TRUE)
sink()
#####################################

#3. Initial values & parameters to save
inits  <- function () {
  list(
    beta  = rnorm(ncol(X), 0, 0.01),
    a     = rnorm(Nre, 0, 0.01),
    num   = rnorm(1, 0, 25),   #<-- half-Cauchy(25)
    denom = rnorm(1, 0, 1),
    numtheta   = rnorm(1, 0, 25), 
    denomtheta = rnorm(1, 0, 1)
   )}

#This now becomes an issue for this data size.
#Let's take the minimal output
params <- c("beta", "E", "a", 
            "sigma_Zoo",  
            "Fit", "FitNew",
            "theta")


#Don't forget to change the file name!
#4. Start JAGS
J1 <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "GLMMZooJAGS.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

J2  <- update(J1, n.iter = 5000)  
out <- J2$BUGSoutput

print(J2, digits = 3)  #Takes a few minutes
#Note...the DIC may not be that of a beta-binomial
#It is better to calculate the DIC yourself, or the AIC,
#using the likelihood of the beta-binomial.
#The reason for this is that we mixing two things here.
#So..you need to find an expression for the log likelihood
#of a beta-binomial and program that in JAGS. And then
#use the log-likelihood to get the AIC or BIC



#5. Assess mixing
MyBUGSChains(out, c(uNames("beta", K), "sigma_Zoo", "theta"))
MyBUGSACF(out, c(uNames("beta", K), "sigma_Zoo", "theta"))


#6. Present output
K <- ncol(X)
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma_Zoo", "theta"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma_Zoo", "theta"))
#Some parameters are not significant!




######################################################
# Assess goodness of fit: Bayesion p-value:
# We have a large number of FitNew and Fit values. 
# Count how often one is bigger than the other:

mean(out$sims.list$FitNew >  out$sims.list$Fit)
#That is good!!


#Model validation and presenting results follow previous exercises.
#You need to use the expression for the mean value of a beta-binomial.
#See the E(p_i) expression on page 230. Just calculate it in JAGS.
#####################################################################



