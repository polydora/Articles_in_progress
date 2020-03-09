
#Functions =================

#Detrending function ================
detrending <- function(x, AddIntercept = FALSE, AddMin=TRUE){
  years <- seq(1:length(x))
  Mod <- lm(x~years)
  detr_x <- residuals(Mod)
  if(AddIntercept) detr_x <- detr_x +coef(Mod)[1]
  if(AddMin) detr_x <- detr_x + abs(min(detr_x)) + min(x)
  return(detr_x)
}

# Функция для вычисления PRCF (Partial Rate Correlation Function по Berrymann) ===============

PRCF <- function (x, AddMean=FALSE, AddMin =FALSE, PACF=FALSE,...){
  Lt <- x
  if (AddMean) Lt <- Lt + mean(Lt)
  if (AddMin) Lt <- Lt + abs(min(Lt)) + 0.00001
  
  Lt <- log(Lt)
  n <- length(Lt)
  R <- Lt[2:n]-Lt[1:(n-1)]
  prcf <- as.vector(pacf(Lt, plot=FALSE)$acf)
  if (!PACF) prcf[1] <- cor(R,Lt[1:(n-1)], use="pairwise.complete.obs")
  return(prcf)
  
}


# Function for calculating confidence intervals for PRCF by bootstrapping of time series

boot_PRCF <- function (data, n=999, dl=3, sim="fixed", confid=0.95, ...)
{
  library(boot)
  longiv = length(data)
  set.seed(1234)
  data<-as.ts(data)
  longiv <- length(data)
  boot_res <- tsboot(data, PRCF, n, sim=sim, l=round(longiv/dl))
  result <- data.frame(lag=1:length(as.numeric(boot_res$t0)), PRCF=as.numeric(boot_res$t0))
  
  for (i in 1:nrow(result)) result$se[i] <- sd(boot_res$t[,i])
  for(i in 1:length(as.numeric(boot_res$t0)))
  {
    result$low.ci[i] <- boot.ci(boot_res, type="basic", conf=confid, index=i)$basic[4]
    result$upper.ci[i] <- boot.ci(boot_res, type="basic",conf=confid, index=i)$basic[5]
  }   
  return(result)
}




# Function for diagnostic graph plotting ==========


Plot_dynamics <- function(x, detrend = F, ...){
  library(ggplot2)
  library(gridExtra)
  ts_initial <- ts <- x
  
  if(detrend) {
    ts <- detrending(x, AddMin = T)
    
  }
  
  Pl_dynam <- ggplot(data.frame(ts = ts, ts_initial = ts_initial), aes(x=1:length(ts), y = ts)) + geom_line(color = "blue", size = 1) + geom_line(aes(x=1:length(ts), y = ts_initial), color = "black", size = 1)+ theme_bw() + xlab("Time")
  
  prcf <- boot_PRCF((ts), n = 999, ...)
  prcf <- prcf[prcf$lag <= 10,]
  Pl_PRCF <- ggplot(prcf, aes(x=factor(lag), y=PRCF)) + geom_bar(fill="gray", color = "black", stat="identity")  + geom_errorbar(aes(x=(lag), ymin=(PRCF - 2*se), ymax=(PRCF + 2*se)), width = 0.2, color = "blue") + geom_hline(yintercept = 0, size = 1) + labs(x="Lags")+ theme_bw() + geom_hline(yintercept = c(-2/sqrt(length(ts)), 2/sqrt(length(ts))), linetype = 2) 
  
  R <- log(ts[1:(length(ts)-1)] +1) - log(ts[2:(length(ts))]+1)  
  
  phase <- data.frame(R = R, Lt_1 = log(ts[2:(length(ts))]+1))
  
  Pl_phase <- ggplot(phase, aes(x=Lt_1, y = R)) + geom_path(aes(color = 1:nrow(phase)), size = 1) + scale_color_continuous(low = "gray", high = "blue") + geom_smooth(method = "lm", color = "black") + theme_bw() + guides(color = FALSE)
  
  Pl_Rdist <- ggplot(phase, aes(x= R)) + geom_histogram(fill = "blue", color = "black") + theme_bw()
  
  grid.arrange(Pl_dynam, Pl_PRCF, Pl_phase, Pl_Rdist, ncol=2) 
}
