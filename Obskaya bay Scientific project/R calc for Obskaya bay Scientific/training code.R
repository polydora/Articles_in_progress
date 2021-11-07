library(Hmsc)
library(MASS)
set.seed(6)




# Создание симулированных данных

n = 100
ns = 5
beta1 = c(-2,-1,0,1,2)
alpha = rep(0,ns)
beta = cbind(alpha,beta1)
x = cbind(rep(1,n),rnorm(n))

Lf = x%*%t(beta) #Фиксированные эффекты



xycoords = matrix(runif(2*n),ncol=2)
plot(xycoords)

colnames(xycoords) = c("x-coordinate","y-coordinate")
rownames(xycoords) = 1:n

sigma.spatial = c(2)
alpha.spatial = c(0.35)

Sigma = sigma.spatial^2*exp(-as.matrix(dist(xycoords))/alpha.spatial)

eta1 = mvrnorm(mu=rep(0,n), Sigma=Sigma)

lambda1 = c(1,2,-2,-1,0)
Lr = eta1%*%t(lambda1)

L = Lf + Lr #Фиксированные эффекты плюс случайное варьирование

# Матрица откликов (обилия видов)
y = as.matrix(L + matrix(rnorm(n*ns),ncol=ns))
yprob = 1*((L +matrix(rnorm(n*ns),ncol=ns))>0)
XData = data.frame(x1=x[,2])



rbPal = colorRampPalette(c('cyan','red'))
par(mfrow=c(2,3))
Col = rbPal(10)[as.numeric(cut(x[,2],breaks = 10))]
plot(xycoords[,2],xycoords[,1],pch = 20,col = Col,main=paste('x'), asp=1)
for(s in 1:ns){
  Col = rbPal(10)[as.numeric(cut(y[,s],breaks = 10))]
  plot(xycoords[,2],xycoords[,1],pch = 20,col = Col,main=paste('Species',s), asp=1)
}




# Анализ

studyDesign = data.frame(sample = as.factor(1:n))
rL.spatial = HmscRandomLevel(sData = xycoords)
rL.spatial = setPriors(rL.spatial,nfMin=1,nfMax=1) 
#We limit the model to one latent variables for visualization 

m.spatial = Hmsc(Y=yprob, XData=XData, XFormula=~x1,
                 studyDesign=studyDesign, ranLevels=list("sample"=rL.spatial),distr="probit")





nChains = 2
test.run = FALSE
if (test.run){
  # with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 0
} else {
  # with this option, the vignette evaluates slow but it reproduces the results of
  # the .pdf version
  thin = 10
  samples = 1000
  transient = 1000
  verbose = 0
}



m.spatial = sampleMcmc(m.spatial, thin = thin, samples = samples, transient = transient,
                       nChains = nChains, verbose = verbose,updater=list(GammaEta=FALSE))

#Explanatory power
preds.spatial = computePredictedValues(m.spatial)
MF.spatial = evaluateModelFit(hM=m.spatial, predY=preds.spatial)
MF.spatial

#Predictive power
partition = createPartition(m.spatial, nfolds = 2, column = "sample")
cvpreds.spatial = computePredictedValues(m.spatial, partition=partition,updater=list(GammaEta=FALSE))


cvMF.spatial = evaluateModelFit(hM=m.spatial, predY=cvpreds.spatial)
cvMF.spatial


mpost.spatial = convertToCodaObject(m.spatial)
plot(mpost.spatial$Alpha[[1]])

summary(mpost.spatial$Alpha[[1]])



