library(readxl)
library(dplyr)
library(ggplot2)



# Data reading ############
# All Station position
stations <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Station parameters", na = "NA")

# Stations with biological samples
stat_full <- stations %>% filter(Exclude == 0)

stat_full <- as.data.frame(stat_full)




# Data on benthic communities ######
bent <- read_excel("Data/Obskaya_bay_2020.xlsx", sheet = "Benthos")


bent_B <- bent %>% filter(Type == "Biomass") %>% select(-Type)

bent_N <- bent %>% filter(Type == "Abundance") %>% select(-Type)


Spec_abund <- bent_N %>% select(-Station) %>% colSums(.) %>% t() %>% as.vector() 

spec_total_abund <- data.frame(Sp = bent_N %>% select(-Station) %>% colnames(), Spec_abund)

N_porog <- 0#200 

spec_selected <- spec_total_abund %>% filter(Spec_abund > N_porog)

bent_short <- bent_N %>% select(Station, spec_selected$Sp)


bent_short <-
  bent_short %>% mutate(Oligochaeta = Oligochaeta_gen._sp. + Oligochaeta_gen._spp.) %>% 
  select(-c(Oligochaeta_gen._sp., Oligochaeta_gen._spp., Oligochaeta_gen._sp._cocons,Senecella_siberica) )










### Данные для модели ######

n <- nrow(bent_short) # Число станций 
ns <- ncol(bent_short)-1 #Число видов


xycoords <- stat_full %>% select(Station, Long, Lat) %>% filter(Station %in% bent_short$Station) %>% select(-Station) %>% as.matrix(.)

colnames(xycoords) = c("x-coordinate","y-coordinate")
rownames(xycoords) = 1:n




Ydata <- bent_short %>% select(-Station) %>% round(., 0) %>% as.matrix(.)
colnames(Ydata) <- names(bent_short)[-1]
rownames(Ydata) = 1:n

Ydata <- log(Ydata + 1)



Xdata <- stat_full %>% select(Bottom_Salinity_Aug_20, Bottom_Turbidity_Aug_20,Transparency_aug_20, Depth_aug_20, Station ) %>% filter(Station %in% bent_N$Station) %>% select(-Station) 


colnames(Xdata) <- c("Sal", "Turb", "Transp", "Depth")




library(Hmsc)

# Model building ####################

studyDesign = data.frame(sample = as.factor(1:n))
rL.spatial = HmscRandomLevel(sData = xycoords)
rL.spatial = setPriors(rL.spatial,nfMin=1,nfMax=1) 


XFormula = ~ poly(Sal, degree = 2, raw = TRUE) + poly(Depth, degree = 2, raw = TRUE)

#We limit the model to one latent variables for visualization 



m.spatial = Hmsc(Y=Ydata, XData=Xdata, XFormula= XFormula,
                 studyDesign=studyDesign, ranLevels=list("sample"=rL.spatial),distr="normal")


# Bayesian set up

nChains = 2
test.run = FALSE
if (test.run){
  # with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 1
} else {
  # with this option, the vignette evaluates slow but it reproduces the results of
  # the .pdf version
  thin = 10
  samples = 1000 #1000
  transient = 1000 #1000
  verbose = 1
}



# Bayesian samplings

m.spatial = sampleMcmc(m.spatial, thin = thin, samples = samples, transient = transient,
                       nChains = nChains, verbose = verbose, initPar = "fixed effects", updater=list(GammaEta=FALSE))


# Model evaluation

mpost = convertToCodaObject(m.spatial, spNamesNumbers =
                              c(T,F), covNamesNumbers = c(T,F))

ess.beta = effectiveSize(mpost$Beta)

psrf.beta = gelman.diag(mpost$Beta,multivariate = FALSE)$psrf

ess.alpha = effectiveSize(mpost$Alpha[[1]])
psrf.alpha = gelman.diag(mpost$Alpha[[1]],multivariate =
                           FALSE)$psrf



#Explanatory power
preds.spatial = computePredictedValues(m.spatial)

# qplot(log(bent_short$Portlandia_aestuariorum + 1), preds.spatial[,12,1])


MF.spatial = evaluateModelFit(hM=m.spatial, predY=preds.spatial)
MF.spatial

# Model matrix

m.spatial$X



#Predictive power
partition = createPartition(m.spatial, nfolds = 2, column = "sample")
cvpreds.spatial = computePredictedValues(m.spatial, partition=partition, updater=list(GammaEta=FALSE))

qplot(x = Ydata[, 1], y = cvpreds.spatial[,1,1] )


mpost.spatial = convertToCodaObject(m.spatial)
plot(mpost.spatial$Alpha[[1]])

summary(mpost.spatial$Alpha[[1]])

# Оценки параметров модели
round(summary(mpost.spatial$Beta, quantiles = c(0.025, 0.5, 0.975))
      [[2]],2)


Gradient = constructGradient(m.spatial, focalVariable = "Sal")

predY = predict(m.spatial, Gradient = Gradient, expected = TRUE)

# predY_no_gradient = predict(m.spatial)
# str(predY_no_gradient)

predY_no_gradient[[1]]


q = c(0.25,0.5,0.75)

plotGradient(m.spatial, Gradient, pred = predY, measure = "Y", showData = TRUE, q = q, index = 13)

