library(ggplot2)
library(readxl)
library(dplyr)
library(cowplot)
library(patchwork)

coordinates_of_stations <- read_excel("data/Coordinates_of_stations.xlsx", sheet = "Coordinates_of_stations")


#N <- c(67.0944666666667, 67.09475, 67.0948333333333)
#E <- c(32.67855, 32.6783333333333, 32.6782)
#A <- matrix(c(729, 2556, 1,
#              716,	2063, 1,
#              557,  1886, 1), 3, 3, byrow=TRUE)



N <- c(67.0944666666667, 67.0952, 67.0953333333333)
E <- c(32.67855, 32.6815, 32.67765)

A <- matrix(c(729, 2556, 1,
              2609,	1466, 1,
              269,  1038, 1), 3, 3, byrow=TRUE)

abc <- solve(A, N)
def <- solve(A, E)

a <- abc[1]
b <- abc[2]
c <- abc[3]
d <- def[1]
e <- def[2]
f <- def[3]

pixels_to_N <- function(x,y) {
  N <- a*x + b*y + c
  E <- d*x + e*y + f
  return(N)
}

pixels_to_N(729, 2556)

pixels_to_E <- function(x,y) {
  N <- a*x + b*y + c
  E <- d*x + e*y + f
  return(E)
}




pixels_to_E(729, 2556)

coordinates_of_stations$N = pixels_to_N(coordinates_of_stations$N_paint,
                                        coordinates_of_stations$E_paint)


coordinates_of_stations$E = pixels_to_E(coordinates_of_stations$N_paint,
                                        coordinates_of_stations$E_paint)


ggplot(shore, aes(x = E, y = N))+
  geom_path(aes(color = depth))+
  geom_point(data = pile,aes(x = E2, y = N2))+
  geom_point(data = coordinates_of_stations,aes(x = E, y = N),color = "green")+
  geom_point(data = samples, aes(), size = N_alive)


