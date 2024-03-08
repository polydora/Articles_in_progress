#install.packages("ggplot2")
library(ggplot2)
#install.packages("readxl")
library(readxl)
library(dplyr)
# install.packages("cowplot")
library(cowplot)
# install.packages("patchwork")
library(patchwork)

setwd("/home/birch/Документы/Tridonta_Borialis/tridonta")
size <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "Size")
pile <- read_excel("data/Координаты береговой линии Илистой губы.xlsx", sheet = "piles")
shore <- read_excel("data/Координаты береговой линии Илистой губы.xlsx", sheet = "Shoreline")
samples <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "sites")
abundance <- read_excel("data/Tridonta_Ilistaya_inlet_2023.xlsx", sheet = "Abundance")
arxiv <- read_excel("data/Ilistaya_inlet_1987_2019_N_B.xlsx", sheet = "Ilistaya_inlet_1987_2019_N_B")
#coordinates_of_stations <- read_excel("data/Coordinates_of_stations.xlsx", sheet = "Coordinates_of_stations")
source("data/coordinata.R")

#размерная структура этого года
ggplot(data = size, aes(x = L)) +
  geom_histogram(binwidth = 2)
#hist(size$L)
size
#размерная структура живых и мёртвых этого года
ggplot(data = size, aes(x = L, fill = Status)) +
  geom_histogram(binwidth = 2) +
  facet_wrap(~Status, ncol = 1)+
  theme_bw()+
  labs(x = "размер,мм", y = "количество")+
  theme(legend.position =" none ")+
  scale_y_continuous(breaks=c(2,4,6,8,10))


#кол-во мёртвых в этом году на  карте
ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = abundance,aes(x = E, y = N, size=N_dead))+
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank()) # number of dead

#кол-во живых в этом году на  карте
ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = abundance,aes(x = E, y = N, size=N_alive))+
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank())


Plot_alive<- 
abundance %>% 
  filter(N_alive > 0) %>% 
  ggplot(aes(x = E, y = N))+
  geom_point(aes( size = N_alive))+
  geom_path(data = shore, aes(color = depth))+
  #geom_point(data = pile,aes(x = E2, y = N2))+
  guides(color = "none", size = "none")+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  ggtitle("живые")

Plot_dead<-
abundance %>% 
  filter(N_dead > 0) %>% 
  ggplot(aes(x = E, y = N))+
  geom_point(aes(size = N_dead))+
  geom_path(data = shore, aes(color = depth))+
  #geom_point(data = pile,aes(x = E2, y = N2))+
  guides(color = "none", size = "none")+
  theme_minimal()+
  theme(axis.text = element_blank(), axis.title = element_blank())+
  ggtitle('мёртвые')
  
#кол-во мёртвых и живых в этом году на  карте
plot_grid(Plot_dead, Plot_alive)
Plot_alive+Plot_dead

#определяем среднее значение для станций
average_N <- function(station){
  table_of_staion <- arxiv %>% 
    filter(Taxa == "Tridonta borealis" & Station == station)
  return(mean(table_of_staion$N))
}

average_B <- function(station){
  table_of_staion <- arxiv %>% 
    filter(Taxa == "Tridonta borealis" & Station == station)
  return(mean(table_of_staion$B))
}

avg_B=c()
for (st in coordinates_of_stations$Station) {
  avg_B = append(avg_B,average_B(st))
}
coordinates_of_stations["avg_B"] = avg_B


ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  geom_point(data = pile,aes(x = E2, y = N2))+ 
  geom_point(data = coordinates_of_stations, aes(x = E, y = N, size = avg_B))



avg_N=c()
for (st in coordinates_of_stations$Station) {
  avg_N = append(avg_N,average_N(st))
}
coordinates_of_stations["avg_N"] = avg_N


ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  geom_point(data = pile,aes(x = E2, y = N2))+ 
  geom_point(data = coordinates_of_stations, aes(x = E, y = N, size = avg_N))

