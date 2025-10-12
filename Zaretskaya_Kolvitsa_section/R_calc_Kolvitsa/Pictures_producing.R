library(dplyr)
library(ggplot2)


occur <- read.table("Data/occurences_depth.csv", sep = ";", header = T)


occur_reduced <-
  occur %>%
  filter((decimalLongitude > -100 & decimalLongitude < 68))

occur_reduced %>%
  group_by(Species) %>%
  summarise(Median_depth = median(depth))


ggplot(occur, aes(x = Species, y = (depth) )) +
  geom_boxplot()
