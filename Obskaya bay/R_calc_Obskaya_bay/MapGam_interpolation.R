
library(MapGAM)



my_data <- expand.grid(Lat = seq(from = Ob_y[1], to =Ob_y[2], length.out = 100), Long =  seq(from = Ob_x[1], to =Ob_x[2] , length.out = 100))


fit_depth <- gam(formula = Depth  ~ lo(Long, Lat), data = stat_depth, rgrid = my_data, family = gaussian)

summary(fit_depth)

plot(fit_depth)


df <- stat_depth %>% select(Depth, Long, Lat)

depth_pred <- mypredict.gam(object = fit_depth , newdata = my_data, reference = "mean")

my_data$pred_depth <- depth_pred$pred



ggplot(data = my_data, aes(x = Long, y = Lat, fill = (pred_depth + mean(stat_depth$Depth) )))+ geom_tile() + scale_fill_gradient(low = "white", high = "blue") + 
  geom_polygon(data = Ob_df, aes(x=long, y=lat, group=group), fill = "gray90", colour = "gray20") + 
  coord_map(xlim = Ob_x, ylim = Ob_y) + 
  theme_bw() +  
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) + 
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) + 
  theme(axis.ticks = element_blank()) 







data(MAdata)							
data(MAmap)

gamgrid <- predgrid(MAdata, map=MAmap)

fit <- gam(Case~lo(Xcoord,Ycoord,span=0.2)+Smoking,data=MAdata,family=binomial())

pred1 = mypredict.gam(fit)

colormap(list(fit=pred1$pred,grid=data.frame(X=MAdata$X,Y=MAdata$Y)),map=MAmap)

pred2 = mypredict.gam(fit,gamgrid)

colormap(list(fit=pred2$pred,grid=data.frame(X=gamgrid$X,Y=gamgrid$Y)),map=MAmap)

pred3 = mypredict.gam(fit,gamgrid,se.fit=TRUE)

colormap(list(fit=pred3$pred,conf.low = pred3$conf.low, conf.high = pred3$conf.high, 
              grid=data.frame(X=gamgrid$X,Y=gamgrid$Y)),map=MAmap,contours = "interval")


