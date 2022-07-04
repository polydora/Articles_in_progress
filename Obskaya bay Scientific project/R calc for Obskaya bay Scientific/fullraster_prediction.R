# Работа с растарми и прогнозами на всю акваторию

# library(raster)
# library(dplyr)
# 
# path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/No_construction_building"
# 
# files <- list.files(path)
# 
# no_construction_df <- NULL
# 
# name <- NULL
# 
# name <- files[1]
# 
# i <- 0
# 
# for(name in files){
#   i <- i + 1
#   name = paste(path, "/", name, sep = "")
#   Sal_layer <- brick(name, var="salt")
#   Uocn_layer <- brick(name, var="uocn")
#   Vocn_layer <- brick(name, var="vocn")
#   
#   sal_df <- as.data.frame(rasterToPoints(Sal_layer))
#   
#   sal_df <- sal_df %>% mutate(Sal = case_when(!is.na(X20) ~ X20,
#                                               is.na(X20) & !is.na(X15) ~ X15,
#                                               is.na(X20) & is.na(X15) & !is.na(X10) ~ X10,
#                                               is.na(X20) & is.na(X15) & is.na(X10) & !is.na(X5)~ X5,
#                                               is.na(X20) & is.na(X15) & is.na(X10) & is.na(X5) &  !is.na(X1) ~ X1))
#   
#   
#   uucn_df <- as.data.frame(rasterToPoints(Uocn_layer))
#   
#   uucn_df <- uucn_df %>% mutate(Uucn = case_when(!is.na(X20) ~ X20,
#                                                  is.na(X20) & !is.na(X15) ~ X15,
#                                                  is.na(X20) & is.na(X15) & !is.na(X10) ~ X10,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & !is.na(X5)~ X5,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & is.na(X5) &  !is.na(X1) ~ X1))
#   
#   
#   
#   vucn_df <- as.data.frame(rasterToPoints(Vocn_layer))
#   
#   vucn_df <- vucn_df %>% mutate(Vucn = case_when(!is.na(X20) ~ X20,
#                                                  is.na(X20) & !is.na(X15) ~ X15,
#                                                  is.na(X20) & is.na(X15) & !is.na(X10) ~ X10,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & !is.na(X5)~ X5,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & is.na(X5) &  !is.na(X1) ~ X1))
#   
#   sal_df <- sal_df %>% select(x, y, Sal)
#   uucn_df <- uucn_df %>% select(x, y, Uucn)
#   vucn_df <- vucn_df %>% select(x, y, Vucn)
#   
#   df <- merge(sal_df, uucn_df)
#   df<- merge(df, vucn_df)
#   
#   if(i == 1)  no_construction_df  <- df else no_construction_df  <-no_construction_df + df
#   
#   print(i)
#   
# }
# 
# 
# no_construction_df <- no_construction_df/365
# 
# 
# write.csv(no_construction_df, file = "no_construction_raster.csv", row.names = F)
# 
# 
# 
# ###################################3
# 
# 
# path = "D:/Data_LMBE/Obskaya Bay additional data/nc_files_from_model/After_construction_building"
# 
# files <- list.files(path)
# 
# construction_df <- NULL
# 
# name <- NULL
# 
# name <- files[1]
# 
# i <- 0
# 
# for(name in files){
#   i <- i + 1
#   name = paste(path, "/", name, sep = "")
#   Sal_layer <- brick(name, var="salt")
#   Uocn_layer <- brick(name, var="uocn")
#   Vocn_layer <- brick(name, var="vocn")
#   
#   sal_df <- as.data.frame(rasterToPoints(Sal_layer))
#   
#   sal_df <- sal_df %>% mutate(Sal = case_when(!is.na(X20) ~ X20,
#                                               is.na(X20) & !is.na(X15) ~ X15,
#                                               is.na(X20) & is.na(X15) & !is.na(X10) ~ X10,
#                                               is.na(X20) & is.na(X15) & is.na(X10) & !is.na(X5)~ X5,
#                                               is.na(X20) & is.na(X15) & is.na(X10) & is.na(X5) &  !is.na(X1) ~ X1))
#   
#   
#   uucn_df <- as.data.frame(rasterToPoints(Uocn_layer))
#   
#   uucn_df <- uucn_df %>% mutate(Uucn = case_when(!is.na(X20) ~ X20,
#                                                  is.na(X20) & !is.na(X15) ~ X15,
#                                                  is.na(X20) & is.na(X15) & !is.na(X10) ~ X10,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & !is.na(X5)~ X5,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & is.na(X5) &  !is.na(X1) ~ X1))
#   
#   
#   
#   vucn_df <- as.data.frame(rasterToPoints(Vocn_layer))
#   
#   vucn_df <- vucn_df %>% mutate(Vucn = case_when(!is.na(X20) ~ X20,
#                                                  is.na(X20) & !is.na(X15) ~ X15,
#                                                  is.na(X20) & is.na(X15) & !is.na(X10) ~ X10,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & !is.na(X5)~ X5,
#                                                  is.na(X20) & is.na(X15) & is.na(X10) & is.na(X5) &  !is.na(X1) ~ X1))
#   
#   sal_df <- sal_df %>% select(x, y, Sal)
#   uucn_df <- uucn_df %>% select(x, y, Uucn)
#   vucn_df <- vucn_df %>% select(x, y, Vucn)
#   
#   df <- merge(sal_df, uucn_df)
#   df<- merge(df, vucn_df)
#   
#   if(i == 1)  construction_df  <- df else construction_df  <- construction_df + df
#   
#   print(i)
#   
# }
# 
# 
# construction_df <- construction_df/365
# 
# 
# write.csv(construction_df, file = "construction_present_raster.csv", row.names = F)
# 

############################################






Native_df <- read.csv("Data/no_construction_raster.csv")

library(Hmsc)

load( "m.spatial_No_new.RData")


X_new <- as.matrix(data.frame(intercept = 1, Sal = Native_df$Sal, Sal2 = Native_df$Sal^2, Cur_Zon = Native_df$Uucn, Cur_Mer = Native_df$Vucn ))



# Предсказанное сообщество  до строительства #####################

betas <- getPostEstimate(m.spatial, parName = "Beta")

betas <- betas$mean


predicted_community <- round((exp(X_new %*% betas) - 1), 2)


                    
predicted_community[predicted_community < 0] <- 0

predicted_community_Native <- predicted_community 

predicted_B <- rowSums(predicted_community_Native * mean_weight) 


predicted_B_df_Native <- data.frame(x = Native_df$x, y = Native_df$y, B_total = predicted_B, Sal = Native_df$Sal, Cur_Zon = Native_df$Uucn, Cur_Mer = Native_df$Vucn )


mod_raster <- gam(log(B_total) ~ s(x, y), data = predicted_B_df_Native)


predicted_B_df_Native$Smooth_B_total <- predict(mod_raster)




Pl_B_total_Native <- 
ggplot(predicted_B_df_Native, aes(x, y, color = exp(Smooth_B_total) ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")


Pl_Sal_Native <- 
ggplot(predicted_B_df_Native, aes(x, y, color = Sal) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red") + geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20")


Pl_Cur_Zon_Native <- 
ggplot(predicted_B_df_Native, aes(x, y, color = abs(Cur_Zon) ) )+ geom_point() + scale_color_gradient(low = "yellow", high = "red") + geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20")



Pl_Cur_Mer_Native <- 
ggplot(predicted_B_df_Native, aes(x, y, color = abs(Cur_Mer) ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red") + geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20")



#####################



########## Предсказываем сообщество при условии создания сооружений #####################


Constructed_df <- read.csv("Data/construction_present_raster.csv")




X_new <- as.matrix(data.frame(intercept = 1, Sal = Constructed_df$Sal, Sal2 = Constructed_df$Sal^2, Cur_Zon = Constructed_df$Uucn, Cur_Mer = Constructed_df$Vucn ))



# Предсказанное сообщество  после строительства #####################

predicted_community <- round((exp(X_new %*% betas) - 1), 2)

predicted_community[predicted_community < 0] <- 0

predicted_community_Constructed <- predicted_community

predicted_B <- rowSums(predicted_community * mean_weight) 



predicted_B_df_Constructed <- data.frame(x = Constructed_df$x, y = Constructed_df$y, B_total = predicted_B, Sal = Constructed_df$Sal, Cur_Zon = Constructed_df$Uucn, Cur_Mer = Constructed_df$Vucn )


mod_raster <- gam(log(B_total) ~ s(x, y), data = predicted_B_df_Constructed)


predicted_B_df_Constructed$Smooth_B_total <- predict(mod_raster)








Pl_B_total_Construted <- 
ggplot(predicted_B_df_Constructed, aes(x, y, color = exp(Smooth_B_total) ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")

Pl_Sal_Construted <- 
  ggplot(predicted_B_df_Constructed, aes(x, y, color = Sal) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")+ geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20")


Pl_Cur_Zon_Construted <-
ggplot(predicted_B_df_Constructed, aes(x, y, color = abs(Cur_Zon) ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")+ geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20")

Pl_Cur_Mer_Construted <-
ggplot(predicted_B_df_Constructed, aes(x, y, color = abs(Cur_Mer) ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")+ geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20")



library(patchwork)
(Pl_B_total_Native / Pl_B_total_Construted)

library(ggpubr)
ggarrange(Pl_B_total_Native, Pl_B_total_Construted, ncol=2, common.legend = TRUE, legend="bottom")





df1 <- predicted_B_df_Constructed[,c(1, 2, 3)]

df1$x <- round(df1$x, 4)
df1$y <- round(df1$y, 4)
nrow(df1)


df2 <- predicted_B_df_Native[,c(1, 2, 3)]

df2$x <- round(df2$x, 4)
df2$y <- round(df2$y, 4)

nrow(df2)

dif <- base::merge(df1, df2, by = c("x", "y"))

nrow(dif)

dif$Dif <- log(dif$B_total.x / dif$B_total.y)

dif$Decrease <- as.numeric(dif$Dif < 0) 


dif %>% filter(Decrease == 1) %>% 
  ggplot(., aes(x, y)) +  
  stat_density2d(aes(fill = ..level.. ), geom = "polygon",  contour_var = "density") +
  scale_fill_gradient(low = "yellow", high = "red") + 
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 22, fill = "blue") + 
  geom_text(aes(x = Sabetta_x - 0.7, y = Sabetta_y, label = "Sabetta")) + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "blue") +
  geom_text(aes(x = Terminal_x + 0.7, y = Terminal_y, label = "Utrenny")) +
  geom_polygon(data = chanel_df, aes(x = X, y = Y), fill = "black")

+
  guides(fill = "none")




mod_Dif_raster <- gam(Decrease ~ s(x, y), data = dif, family = "binomial")

dif$Smooth_Decrease <- predict(mod_Dif_raster, type = "response")

mean(dif$B_total.x)

mean(dif$B_total.y)

Dif_quant <- quantile(dif$Smooth_Decrease , probs = c(0.25, 0.75))


dif <- dif %>% mutate(Dif_class = case_when(Smooth_Decrease >= Dif_quant[2] ~ "High probability of Decrease", 
                                            Smooth_Decrease  < Dif_quant[2] & Smooth_Decrease  > Dif_quant[1] ~ "Stable",
                                            Smooth_Decrease  <= Dif_quant[1] ~ "High probability of Increase"))


ggplot(dif, aes(x, y, fill = Dif_class ) ) + 
  geom_raster() + 
  scale_fill_manual(values = c("red", "yellow", "green"))+
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") +
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 22, fill = "blue") + 
  geom_text(aes(x = Sabetta_x - 0.7, y = Sabetta_y, label = "Sabetta")) + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "blue") +
  geom_text(aes(x = Terminal_x + 0.7, y = Terminal_y, label = "Utrenny")) +
  geom_polygon(data = chanel_df, aes(x = X, y = Y), fill = "black")

  

hist(dif$Smooth_Decrease)

Pl_Dif <- 
  ggplot(dif, aes(x, y, fill = Smooth_Decrease ) ) + geom_raster() + scale_fill_gradient(low = "yellow", high = "red")+
geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  geom_point(aes(x = Sabetta_x, y = Sabetta_y), size = 3, shape = 22, fill = "blue") + 
  geom_text(aes(x = Sabetta_x - 0.7, y = Sabetta_y, label = "Sabetta")) + 
  geom_point(aes(x = Terminal_x, y = Terminal_y), size = 3, shape = 22, fill = "blue") +
  geom_text(aes(x = Terminal_x + 0.7, y = Terminal_y, label = "Utrenny")) +
  geom_polygon(data = chanel_df, aes(x = X, y = Y), fill = "black")



sp_number <- which(colnames(predicted_community_Constructed) %in% c("Saduria_entomon"))

Sp_Constructed <- predicted_community_Constructed[, sp_number]

Sp_Native <- predicted_community_Native[, sp_number]




ggplot(predicted_B_df_Native, aes(x, y, color = Sp_Native ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")+ guides(fill = "none")

ggplot(predicted_B_df_Constructed, aes(x, y, color = Sp_Constructed ) ) + geom_point() + scale_color_gradient(low = "yellow", high = "red")+ guides(fill = "none")

