library(readxl)
library(vegan)
library(dplyr)
library(ggvegan)



# Read data

bent <- read_excel("Data/Obskaya_bay.xlsx", sheet = "Benthos", col_types = "guess")

planct <- read_excel("Data/Obskaya_bay.xlsx", sheet = "Plancton", col_types = "guess", na = "NA")

phyt_planct <- read_excel("Data/Obskaya_bay.xlsx", sheet = "Phytoplancton abundance", col_types = "guess")


phyt_planct_present <- read_excel("Data/Obskaya_bay.xlsx", sheet = "Phytoplancton species presence", col_types = "guess")

phyt_planct_present[is.na(phyt_planct_present)] <- 0


env <-  read_excel("Data/Obskaya_bay.xlsx", sheet = "Environment", col_types = "guess")


# Possible Predictors

phyt_planct_station <-  phyt_planct %>% filter(Type == "Abundance") %>%  group_by(Station) %>% summarise_all(mean) %>% select(-c(Horizont,  Type))


predictors <- merge(env %>% select(Station, Depth), phyt_planct_station)



#Benthic community 

bent_N <- bent %>% filter(Type == "Abundance") %>% select(-Type) 
row.names(bent_N) <- bent_N$Station

totals <- bent_N %>% select(-Station) %>%  rowwise() %>% mutate(Total_N = sum())



predictors2 <- merge(predictors, bent_N)[ ,1:10 ]

bent_N <- merge(predictors, bent_N) %>% select(-c(2:10))


Total_N <- bent_N %>% select(-Station) %>% rowSums()

bent_N2 <- bent_N %>% filter(Total_N>0)

predictors3 <- predictors2 %>% filter(Total_N>0)





bent_cca <- cca(bent_N2[, -1] ~ . - Depth, data = predictors3[, -1])


vif.cca(bent_cca)


anova(bent_cca, permutations = 9999)

anova(bent_cca, by = "axis", permutations = 9999)

anova(bent_cca, by = "terms", permutations = 9999)


plot(bent_cca, display = c("species", "cn"))

ordistep(bent_cca)

bent_cca_opt <- cca(formula = bent_N2[, -1] ~ Bacillariophyta + Chrysophyta + Cryptophyta + Euglenophyta,
                    data = predictors3[, -1])

plot(bent_cca_opt, display = c("species", "cn") )



bent_mds <- metaMDS(log(bent_N2[ , -1]+1))

env_fit <- envfit(bent_mds ~ ~ Bacillariophyta + Chrysophyta + Cryptophyta + Euglenophyta,
                  data = predictors3[, -1])

ordisurf(bent_mds ~ Chrysophyta, data = predictors3[,-1])

ordisurf(bent_mds ~ Depth, data = predictors3[,-1])

ordisurf(bent_mds ~ Bacillariophyta, data = predictors3[,-1])



plot(bent_mds, type = "t")

plot(env_fit)



# Plancton

planct_ca <- cca(phyt_planct_station[, -1])

plot(planct_ca)
