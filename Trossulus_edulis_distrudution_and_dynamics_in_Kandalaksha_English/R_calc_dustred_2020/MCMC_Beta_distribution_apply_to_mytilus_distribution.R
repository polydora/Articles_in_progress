library(ggplot2)
library(lme4)
library(dplyr)




##### Data reading
myt <- read.table("data/Distred_samples_fetch_corrected_2021.csv", header = T, sep = ",")

sal <- read.table("data/Distred_samples_salinity_2021.csv", header = T, sep = ",")

myt <- merge(myt, sal, all = T)

river <- read.table("data/Rivers_2021.csv", sep = ",", header = T)

ports <- data.frame(Shore = c("Kand", "Karel", "Kand", "Karel", "Karel"), Port = c("Kandalaksha", "Vitino", "Umba", "Chupa", "Sredny"), Status = c("Active", "Active", "Abandoned", "Abandoned", "Abandoned" ), Lat = c(67.137283, 67.076570,  66.677970, 66.269964, 66.294178), Lon = c(32.407995, 32.333630, 34.357655, 33.069534, 33.640656))


sites_fetch_df <- read.table("data/Distred_samples_fetch_values_2021.csv", sep = ",", header = T)





######## Формируем датасет со всми предикторами

# Функция для вычисления от заданной точки до ближайшего объекта


nearest_dist <- function(XY, objects = river, x.name = "Lon", y.name = "Lat"){
  
  XY1 <-as.numeric(XY[,1])
  XY2 <- as.numeric(XY[,2])
  dist <- (acos(sin(XY1*pi/180)*sin(objects[ ,y.name]*pi/180) + cos(XY1*pi/180)*cos(objects[ ,y.name]*pi/180)*cos(XY2*pi/180 - objects[ ,x.name]*pi/180)) * 6371)
  
  MD <- data.frame(Min_dist = min(dist))
  cbind(objects[which(dist == min(dist)), ], MD)
  
}


df_river <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = river)

df_river[1,] <- NA

for(i in 1:nrow(myt)) {
  df_river[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = river)
  df_river$Site[i] <- as.character(myt$Site)[i]
}

names(df_river) <- c( "Shore_river", "River", "Drainage_Area", "River_size", "Lat_river", "Lon_river", "Min_dist_river", "Site" )


# Находим расстояния для ближайшего порта.

df_port <- nearest_dist(XY = myt[1, c("Lat", "Lon")], objects = ports)

df_port[1,] <- NA

for(i in 1:nrow(myt)) {
  df_port[i,] <- nearest_dist(XY = myt[i, c("Lat", "Lon")], objects = ports)
  df_port$Site[i] <- as.character(myt$Site)[i]
}

names(df_port) <- c("Shore_port",  "Port","Port_Status", "Lat_port", "Lon_port", "Min_dist_port", "Site")






# Сводим все датафреймы в один

nrow(myt)
nrow(df_river)

d <- cbind(myt, df_river %>% select(-Site))

dd <- cbind(d, df_port %>% select(-Site))


ddd <- merge(dd, sites_fetch_df, by = "Site")

myt_full <- ddd

# Условная граница кута Кандалакшского залива
Shore_boundary = c(67.162360, 32.332371)

# Переводим в радианы
Shore_boundary <- Shore_boundary*pi/180


myt_full$Dist_cut <- with(myt_full, acos(sin(Shore_boundary[1])*sin(Lat*pi/180) + cos(Shore_boundary[1])*cos(Lat*pi/180)*cos(Shore_boundary[2] - Lon*pi/180))) * 6371


myt_full <- myt_full %>% mutate(Total_N = N_T + N_E) %>% mutate(Prop_T = N_T/(N_T +N_E)) %>% mutate(Fi_T = 2*asin(sqrt(Prop_T))*180/pi) 

myt_site <- myt_full %>% select(-Prop_T, -Fi_T) %>% group_by(Shore, Site, Position) %>% select(Lat,      Lon, N_T, N_E, Min_dist_river, River, River_size, Min_dist_port, Port, Port_Status, Average, Dist_cut, Total_N) %>% summarise(Lat = mean(Lat), Lon = mean(Lon), N_T = sum(N_T), N_E = sum(N_E), Min_dist_river = mean(Min_dist_river), River = unique(River), River_size = unique(River_size), Min_dist_port = mean(Min_dist_port), Port = unique(Port), Port_Status = unique(Port_Status), Average = min(Average),   Dist_cut = mean(Average), Total_N = sum(Total_N)) %>% mutate(Prop_T = N_T/(N_T+N_E)) %>% mutate(Fi_T = 2*asin(sqrt(Prop_T))*180/pi)

str(myt_site)

myt_site <- myt_site %>% as.data.frame()

# Убираем сайты с NA

myt_site <- myt_site %>% filter(complete.cases(.))


# Убираем сайты с неполной схемой вятия проб (нет пары фукус-грунт, или какой-нибудь другой сбой в схеме взятия проб)

sites_excluded <- c("chupa_fg", "umba_pioner", "umba_06", "umba_fg", "umba_sovhoz", "umba_kamni", "umba_bridge", "umba_pikut", "padan", "porya", "Vor5", "Ovech", "oenij", "Korg", "Mat", "Mal", "salnij", "Lubch", "kanal",  "Vor4", "Vor2", "Kurt", "Ryazh4", "Ryazh5", "Youzh")

nrow(myt_site)

myt_site <- myt_site %>% filter(! Site %in% sites_excluded) 

nrow(myt_site)

myt_full <- myt_full %>% filter(! Site %in% sites_excluded) 

nrow(myt_full)

myt_full$Position <- factor(myt_full$Position)

myt_full$Position <- relevel(myt_full$Position, ref = "Bottom")

myt_full$Port_Status <- factor(myt_full$Port_Status)

myt_full$Port_Status <- relevel(myt_full$Port_Status, ref = "Abandoned")


myt_full$River_Size <- factor(myt_full$River_size)

myt_full$River_Size <- relevel(myt_full$River_size, ref = "Small")


myt_full$Site <- factor(myt_full$Site)


str(myt_full$Position)

str(myt_full$Port_Status)

str(myt_full$River_Size)


# myt_full$Lat2 <- myt_full$Lat + rep(seq(0.00000, 0.00005, by = 0.00001), nrow(myt_full)/6) 

# myt_full$Lat - myt_full$Lat2

### Общая характеристика материала #########

library(reshape2)
myt_full %>% group_by(Site, Position) %>% summarise(N_samples = n()) %>% dcast(Site ~ Position ) 


# Добавляю к широте и долготе минимальное значение, чтобы обеспечить смещение ближайших проб на небольшое расстояние друг относительно друга. Это нужно для анализа пространственных автокорреляций.

myt_full$Lat2 <- myt_full$Lat + rep(seq(0.00000, 0.00000005, by = 0.00000001), nrow(myt_full)/6)

myt_full$Lon2 <- myt_full$Lon + rep(seq(0.00000, 0.00000005, by = 0.00000001), nrow(myt_full)/6)


### Строю модель по шаблогу  Ex11B_CeetahsV3_MCMC.R из лекций Zuur см. папку "D:\text\leteures\Zuur MCMC technik"


library(R2jags)
source(file="extra_data/HighstatLibV6.R")  
source("extra_data/MCMCSupportHighstat.R")




MyVar <- c("Salinity", "Min_dist_river",  "Average_Fetch", "Min_dist_port")

pairs(myt_full[,MyVar], lower.panel = panel.cor)

#Use the 0/1 variables and not there factor equivalents
corvif(myt_full[ ,c("Salinity", "Min_dist_river",  "Average_Fetch", "Min_dist_port", "River_Size", "Position", "Port_Status")])

#Now all VIFs < 3
#See Zuur et al. (2010) for an explanation

MyVar <- c("Salinity", "Min_dist_river",  "Average_Fetch", "Min_dist_port")
Myxyplot(myt_full, MyVar, "Prop_T", MyYlab = "Proportions")


par(mfrow = c(2,2), mar = c(3,3,2,2))
boxplot(Prop_T ~ Position, data = myt_full)
boxplot(Prop_T ~ Port_Status, data = myt_full)


####################################
#JAGS..what will we learn in this exercise
# 1. How to do a binomial GLMM
# 2. How to do a beta-binomial GLMM in JAGS
#    This can also be done in glmmADMB....

##Step 1: Fit the model.
#Use MCMC to estimate the parameters                     
#1. Bundle data

X <- model.matrix(~ Position +  Salinity + Min_dist_river + River_Size + Average_Fetch + Min_dist_port + Port_Status, data = myt_full)                      

K <- ncol(X)  #Number of columns
head(X)


Site <- as.numeric(as.factor(myt_full$Site))
Site
Nre <- length(unique(myt_full$Site)) #Количество уровней случайного фактора

win.data <- list(Y      = myt_full$N_T, 
                 Trials = myt_full$Total_N,
                 X      = X,
                 N      = nrow(myt_full),
                 K      = ncol(X),
                 Site   = Site,
                 Nre    = Nre)
win.data

###################################################
# 2. JAGS modelling code
sink("MCMC_myt_full_JAGS.txt")

cat("
model{
    #1A. Priors beta and sigma
    for (i in 1:K) { beta[i] ~ dnorm(0, 0.0001)}

    #1B. Priors random effects and sigma
    for (i in 1:Nre) { a[i] ~ dnorm(0, tau_myt)}
    tau_myt <- 1 / (sigma_myt * sigma_myt)
    num           ~ dnorm(0, 0.0016)             #<----half-Cauchy(25)
    denom         ~ dnorm(0, 1)                  #<----half-Cauchy(25)
    sigma_myt   <- abs(num / denom)              #<----half-Cauchy(25)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]         ~ dbin(Pi[i], Trials[i])  
      logit(Pi[i]) <- eta[i] 
      eta[i]      <- inprod(beta[], X[i,]) + a[Site[i]] 
  
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
            "sigma_myt", "Pi", 
            "Fit", "FitNew")


#Don't forget to change the file name!
#4. Start JAGS
J1 <- jags(data       = win.data,
           inits      = inits,
           parameters = params,
           model      = "MCMC_myt_full_JAGS.txt",
           n.thin     = 10,
           n.chains   = 5,
           n.burnin   = 2500,
           n.iter     = 7500)

J2 <- J1

J2  <- update(J1, n.iter = 10000)  

save(J2, file = "JAGS_results_myt_full.RData")


out <- J2$BUGSoutput

print(J2, digits = 3)  #Takes a few minutes
#Write down DIC at this point



#5. Assess mixing
MyBUGSChains(out, c(uNames("beta", K), "sigma_myt"))
MyBUGSACF(out, c(uNames("beta", K), "sigma_myt"))


#6. Present output
K <- ncol(X)
OUT1 <- MyBUGSOutput(out, c(uNames("beta", K), "sigma_myt"))
print(OUT1, digits =5)
MyBUGSHist(out, c(uNames("beta", K), "sigma_myt"))
#Some parameters are not significant!


str(out)
colnames(X)

intercept <- out$sims.matrix[,"beta[1]"]
Position_Algae <- out$sims.matrix[,"beta[2]"]
Salinity <- out$sims.matrix[,"beta[3]"]
Min_dist_river <- out$sims.matrix[,"beta[4]"]
River_Size_Small <- out$sims.matrix[,"beta[5]"]
Average_Fetch <- out$sims.matrix[,"beta[6]"]
Min_dist_port <- out$sims.matrix[,"beta[7]"]
Port_StatusActive <- out$sims.matrix[,"beta[8]"]
Sigma <- out$sims.matrix[,"sigma_myt"]


betas <- data.frame(intercept = out$sims.matrix[,"beta[1]"],
                    Position_Algae = out$sims.matrix[,"beta[2]"],
                    Salinity = out$sims.matrix[,"beta[3]"],
                    Min_dist_river = out$sims.matrix[,"beta[4]"],
                    River_Size_Small = out$sims.matrix[,"beta[5]"],
                    Average_Fetch = out$sims.matrix[,"beta[6]"],
                    Min_dist_port = out$sims.matrix[,"beta[7]"],
                    Port_Status_Active = out$sims.matrix[,"beta[8]"]
)


theme_set(theme_bw() + theme(panel.grid = element_blank()))
# Pl_templ 
Pl_int <- ggplot(betas, aes(x = intercept)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(intercept, probs = 0.025), xend = quantile(intercept, probs = 0.975), y = 0, yend = 0), size = 4, color = "red") +
  geom_vline(xintercept = 0, linetype = 2)


Pl_pos_alg <- ggplot(betas, aes(x = Position_Algae)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(Position_Algae, probs = 0.025), xend = quantile(Position_Algae, probs = 0.975), y = 0, yend = 0), size = 4, color = "red") 

Pl_sal <- ggplot(betas, aes(x = Salinity)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(Salinity, probs = 0.025), xend = quantile(Salinity, probs = 0.975), y = 0, yend = 0), size = 4, color = "red")+
  geom_vline(xintercept = 0, linetype = 2)

Pl_dist_riv <- ggplot(betas, aes(x = Min_dist_river)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(Min_dist_river, probs = 0.025), xend = quantile(Min_dist_river, probs = 0.975), y = 0, yend = 0), size = 4, color = "red")+
  geom_vline(xintercept = 0, linetype = 2)

Pl_riv_siz <- ggplot(betas, aes(x = River_Size_Small)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(River_Size_Small, probs = 0.025), xend = quantile(River_Size_Small, probs = 0.975), y = 0, yend = 0), size = 4, color = "red")+
  geom_vline(xintercept = 0, linetype = 2)

Pl_fetch <- ggplot(betas, aes(x = Average_Fetch)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(Average_Fetch, probs = 0.025), xend = quantile(Average_Fetch, probs = 0.975), y = 0, yend = 0), size = 4, color = "red")+
  geom_vline(xintercept = 0, linetype = 2)


Pl_dist_port  <- ggplot(betas, aes(x = Min_dist_port)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(Min_dist_port , probs = 0.025), xend = quantile(Min_dist_port , probs = 0.975), y = 0, yend = 0), size = 4, color = "red")+
  geom_vline(xintercept = 0, linetype = 2)


Pl_port_stat  <- ggplot(betas, aes(x = Port_Status_Active)) + 
  geom_density(color = "blue", size = 1) +
  geom_hline(yintercept = 0) +
  geom_segment(aes(x = quantile(Port_StatusActive, probs = 0.025), xend = quantile(Port_StatusActive, probs = 0.975), y = 0, yend = 0), size = 4, color = "red")+
  geom_vline(xintercept = 0, linetype = 2)

library(patchwork)

(Pl_int + Pl_pos_alg + Pl_sal + Pl_dist_riv) / (Pl_riv_siz + Pl_fetch + Pl_dist_port + Pl_port_stat )
