setwd("/home/birch/Документы/Tridonta_Borialis/tridonta")
source("data/coordinata.R")
library(ggplot2)
tb_brait_means<-
  merge(coordinates_of_stations, tb_brait_means)
samples_abundance<-
  merge(samples, abundance)

Pl_old<-
  ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  #geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = tb_brait_means,aes(x = E, y = N, size = N_mean))+
  guides(color = "none", size = "none")+
  theme_bw() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())+
  ggtitle("1987-2023")


Pl_2023<-
  ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  #geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = samples_abundance %>% filter(N_alive > 0), aes(size = N_alive*40))+
  guides(color = "none", size = "none") +
  theme_bw() + 
  theme(axis.text = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank())+
  ggtitle("2023")

plot_grid(Pl_old, Pl_2023)

