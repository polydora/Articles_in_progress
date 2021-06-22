library(Hmsc)
example(Hmsc)
set.seed(1)

# Симулированныый пример, где одномерная величина завяан на единственный предиктор
n = 50
x = rnorm(n)
alpha = 0
beta = 1
sigma = 1
L = alpha + beta*x
y = L + rnorm(n, sd = sigma)
plot(x, y, las=1)



df = data.frame(x,y)
m.lm = lm(y ~ x, data=df)
summary(m.lm)

Y = as.matrix(y)
XData = data.frame(x = x)
m = Hmsc(Y = Y, XData = XData, XFormula = ~x)

str(m)

# Задаем парамтеры выборок из постериорного распределения
nChains = 2
test.run = F
if (test.run){
  #with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 5
} else {
  #with this option, the vignette evaluates slow but it reproduces the results of the
  #.pdf version
  thin = 5
  samples = 1000
  transient = 500*thin
  verbose = 500*thin
}


# Код для формиования выборок из постериорного распределиня
m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)


# Для просмотра парамеров надо конвертировать объект m в объект класса Coda (что за хрень не понятно)

mpost = convertToCodaObject(m)
summary(mpost$Beta)



# Оценка R2 b Оценка предиктивной значимости модлеи

preds = computePredictedValues(m)

evaluateModelFit(hM=m, predY=preds)

# HMSC applies the Bayesian framework (and thus e.g. assumes prior distributions and yields credible intervals)


# Диагностика
plot(mpost$Beta)

effectiveSize(mpost$Beta)

gelman.diag(mpost$Beta,multivariate=FALSE)$psrf


# Стандартная диагностика линейных моделей
nres.lm = rstandard(m.lm)
preds.lm = fitted.values(m.lm)
par(mfrow=c(1,2))
hist(nres.lm, las = 1)
plot(preds.lm,nres.lm, las = 1)
abline(a=0,b=0)


# ДИагностика баессовской оценки
# Для 50 предсказанных значений вычислется среднее значение 
preds.mean = apply(preds, FUN=mean, MARGIN=1) 

# Остатки находим как разность между наблюдемым значением и предсказанным
nres = scale(y-preds.mean)


par(mfrow=c(1,2))
hist(nres, las = 1)
plot(preds.mean,nres, las = 1)
abline(a=0,b=0)


# Теперь аналогичное упражнение, но для пробит связывающей функции
y = 1*(L+ rnorm(n, sd = 1)>0)
plot(x,y, las = 1)

Y=as.matrix(y)
m = Hmsc(Y=Y, XData=XData, XFormula=~x, distr="probit")
verbose = 100
m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)

mpost = convertToCodaObject(m)
summary(mpost$Beta)

effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta, multivariate=FALSE)$psrf
preds = computePredictedValues(m)
evaluateModelFit(hM=m, predY=preds)




# Теперь аналогичное упражнение, но для пуассоновского распределения и log связывающей функции
y = rpois(n, lambda = exp(L))
plot(x,y,las=1)

Y=as.matrix(y)
m = Hmsc(Y=Y, XData=XData, XFormula=~x, distr="poisson")
m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)



mpost = convertToCodaObject(m)
summary(mpost$Beta)


effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta, multivariate=FALSE)$psrf


# To get an unbiased sample from the posterior and results that can be fully trusted, one should increase the thinning and thus run the model longer.


# Теперь аналогичное упражнение, но для log-пуассоновского распределения\

y = rpois(n, lambda = exp(L+rnorm(n, sd=2)))
plot(x,y, las = 1)

Y=as.matrix(y)
m = Hmsc(Y=Y, XData=XData, XFormula=~x, distr="lognormal poisson")
m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)

mpost = convertToCodaObject(m)
summary(mpost$Beta)

effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta, multivariate=FALSE)$psrf

preds = computePredictedValues(m, expected = FALSE)
evaluateModelFit(hM=m, predY=preds)




# В случае Zero-inflated Пуассона в этом пакете применяют так называемые Hurdle models

### Смешаные модели

# We first consider a hierarchical study design in which 100 sampling units belonging to a discrete set of np=10 plots. We assume that the plots have an additive effect to the response variable, i.e. we consider a random intercept model.

n = 100 #общее количество проб 
x = rnorm(n) #Это предиктор
alpha = 0
beta = 1
sigma = 1
L = alpha + beta*x #Фиксированная часть модели (Линейный предиктор)
np = 10 #Число плотов
sigma.plot = 1 #"Дрожание стрелки"  от плота к плоту
sample.id = 1:n # Уникальные номера проб
plot.id = sample(1:np, n, replace = TRUE) #Номера плотов

table(plot.id) # несбаллансированное распределение проб по плотам

ap = rnorm(np, sd = sigma.plot) 
a = ap[plot.id] #Случайные значения интерцепта для каждого плота

y = L + a + rnorm(n, sd = sigma) #Линейный предиктор с добалением случайной части в интерсепте
plot.id = as.factor(plot.id)
plot(x,y,col = plot.id, las = 1)

library(ggplot2)
ggplot(data.frame(x = x, y = y, plot.id = plot.id), aes(x, y, color = plot.id)) + geom_point() + geom_smooth(method = "lm", se = F)


XData = data.frame(x = x)
Y = as.matrix(y)


# Опсание модели для HMSC
studyDesign = data.frame(sample = as.factor(sample.id), plot = as.factor(plot.id)) #Это описание того, к какому плоту относится та или иная проба.

rL = HmscRandomLevel(units = studyDesign$plot) #Описание случайных факторов


m = Hmsc(Y=Y, 
         XData=XData, 
         XFormula=~x,
         studyDesign=studyDesign, 
         ranLevels=list("plot"=rL))

m = sampleMcmc(m, # Модель (см.выше)
               thin = thin, #he number of MCMC steps between each recording of samples from the posterior
               samples = samples, # Сколько будет случайных выборок на одну цепочку
               transient = transient, # Сколько случ.выборок будет выкинуто для разогрева
               nChains = nChains, # Количество марковских цепочек
               verbose = verbose) # the interval between MCMC steps printed to the console


preds = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=preds)

MF$R2 #R2 measures explanatory power rather than predictive power.

#Cross validation

#Все пробы Разделяют на два датасета
partition = createPartition(m, nfolds = 2, column = "sample")
partition
preds = computePredictedValues(m, partition = partition)

MF = evaluateModelFit(hM = m, predY = preds)
MF$R2



# Все плоты разделяют на два датаста

partition = createPartition(m, nfolds = 2, column = "plot") 
t(cbind(plot.id, partition)[1:15,])

preds = computePredictedValues(m, partition=partition)
MF = evaluateModelFit(hM = m, predY = preds)
MF$R2

# The predictive power of the model is lower when assigning entire plots rather than individual sampling units into different cross-validation folds. This is because the prediction task is now more difficult: when making a prediction for a particular sampling unit, the model was fitted without training data from any other sampling units in the same plot. Thus the model has no possibility to estimate the actual random effect for the focal plot, and thus its predictive power is based solely on the fixed effects.



# В модель вводится пространственная структура

sigma.spatial = 2
alpha.spatial = 0.5
sample.id = rep(NA,n)
for (i in 1:n){
  sample.id[i] = paste0("location_",as.character(i))
}
sample.id = as.factor(sample.id)

xycoords = matrix(runif(2*n), ncol=2)
rownames(xycoords) = sample.id
colnames(xycoords) = c("x-coordinate","y-coordinate")

a = MASS::mvrnorm(mu=rep(0,n),
                  Sigma = sigma.spatial^2*exp(-as.matrix(dist(xycoords))/alpha.spatial))

y = L + a + rnorm(n, sd = sigma)

Y=as.matrix(y)

colfunc = colorRampPalette(c("cyan", "red"))
ncols = 100
cols = colfunc(100)
par(mfrow=c(1,2))
for (i in 1:2){
  if (i==1) value = x
  if (i==2) value = y
  value = value-min(value)
  value = 1+(ncols-1)*value/max(value)
  plot(xycoords[,1],xycoords[,2],col=cols[value],pch=16,main=c("x","y")[i], asp=1) }



studyDesign = data.frame(sample = sample.id)
rL = HmscRandomLevel(sData = xycoords)
m = Hmsc(Y=Y, XData=XData, XFormula=~x,
         studyDesign=studyDesign, ranLevels=list("sample"=rL))


m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
               nChains = nChains, verbose = verbose)
preds = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=preds)
MF$R2

partition = createPartition(m, nfolds = 2, column = "sample")
preds = computePredictedValues(m, partition=partition)
MF = evaluateModelFit(hM=m, predY=preds)
MF$R2


mpost = convertToCodaObject(m)
plot(mpost$Alpha[[1]])

summary(mpost$Alpha[[1]])
