
library(Hmsc)


load("m.spatial.RData") # Загрузка модели, где предиктором является соленость из модели без строительства конструкций

load("m.spatial_constructed.RData") # Загрузка модели, где предиктором является соленость из модели после строительства конструкций

load("m.spatial_initial.RData") # Загрузка модели, где предиктором является изначально измеренная соленость 









Preds.spatial_Init = computePredictedValues(m.spatial_initial)

Predicted_Init <- apply(Preds.spatial_Init, c(1,2), mean)

Predicted_Init <- round(exp(Predicted_Init)-1, 0)

Predicted_Init <- as.data.frame(Predicted_Init)
Predicted_Init$Type <- "Init"

str(Predicted_Init)



# Новые значения для предсказания для сценария, когда есть конструкции 
X_new <- as.matrix(data.frame(intercept = 1, Sal = Xdata_Constructed$Sal, Sal2 = Xdata_Constructed$Sal^2))

Predicted_Constr <- predict(m.spatial_initial, X = X_new)


Predicted_Constr_mean <- Reduce("+", Predicted_Constr) / length(Predicted_Constr)

Predicted_Constr_mean <- round(exp(Predicted_Constr_mean) -1, 0)
Predicted_Constr_mean <- as.data.frame(Predicted_Constr_mean)

Predicted_Constr_mean$Type <- "Constructed"




# Новые значения для предсказания для сценария, когда нет конструкций 
X_new <- as.matrix(data.frame(intercept = 1, Sal = Xdata_No$Sal, Sal2 = Xdata_No$Sal^2))

Predicted_No <- predict(m.spatial_initial, X = X_new)


Predicted_No_mean <- Reduce("+", Predicted_No) / length(Predicted_No)

Predicted_No_mean <- round(exp(Predicted_No_mean) -1, 0)

Predicted_No_mean <- as.data.frame(Predicted_No_mean)

Predicted_No_mean$Type <- "No"





All_Predicted <- rbind(Predicted_No_mean, Predicted_Constr_mean, Predicted_Init)


All_Predicted[All_Predicted<0] <- 0




library(vegan)
library(dplyr)
library(ggplot2)



H_init <- diversity(Predicted_Init[,-ncol(Predicted_Init)])

H_No <- diversity(Predicted_No_mean[,-ncol(Predicted_No_mean)])

H_Constructed <- diversity(Predicted_Constr_mean[,-ncol(Predicted_Constr_mean)])

qplot(H_init, H_No, geom = "text", aes(label = 1:97)) + geom_abline()

qplot(H_init, H_Constructed, geom = "text", aes(label = 1:97)) + geom_abline()


qplot(Xdata$Sal,  H_init, geom = "text", aes(label = 1:97)) 

qplot(Xdata_No$Sal,  H_No, geom = "text", aes(label = 1:97)) 

qplot(Xdata_Constructed$Sal,  H_Constructed, geom = "text", aes(label = 1:97)) 


diversity <- data.frame(H = c(H_No, H_Constructed, H_init), Type = All_Predicted$Type, Station = stat_full)

ggplot(diversity, aes(x = Type, y = H)) + geom_boxplot()




mds_all_predicted <- All_Predicted %>%   select(-Type)  %>%  metaMDS(., autotransform = F)

scores(mds_all_predicted) %>% as.data.frame(.) %>% 
  ggplot(., aes(NMDS1, NMDS2)) + geom_point(aes(color = All_Predicted$Type)) + facet_wrap(~All_Predicted$Type)






# Разница между изначальными данными и ожидаемыми при сценарии создания конструкций
Constructed_Init <- (Predicted_Constr_mean %>% select(-Type) - Predicted_Init %>% select(-Type)) 




Constructed_Init %>% filter(V1 >= quantile(V1, probs = c(0.75)))

Constructed_Init %>% filter(V1 <= quantile(V1, probs = c(0.25)))

sum(Constructed_Init)



No_Init <- (Predicted_No_mean %>% select(-Type) - Predicted_Init %>% select(-Type)) %>% summarise_all(.funs = mean) %>% t(.) %>% as.data.frame(.)

No_Init %>% filter(V1 >= quantile(V1, probs = c(0.75)))

No_Init %>% filter(V1 <= quantile(V1, probs = c(0.25)))


sum(No_Init)


All_Predicted %>% group_by(Type) %>% summarise_all(.funs = mean) %>% t(.) 





