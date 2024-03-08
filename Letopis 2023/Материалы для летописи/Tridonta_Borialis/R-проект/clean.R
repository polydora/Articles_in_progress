#install.packages("ggplot2")
library(ggplot2)
#install.packages("readxl")
library(readxl)
# install.packages("patchwork")
library(patchwork)
library(purrr)


setwd("/home/birch/Документы/Tridonta_Borialis/tridonta")
all_sizes <- read_excel("data/Tridonta borealis data base 1987-2023.xls", sheet = "T.borealis")


#размерная структура за все годы
years <- unique(all_sizes$Year)
year_plot <- function(year) {
  message(year)
  sizes_of_year <- all_sizes %>%
    filter(Year == year & Status == "alive")
  plot <- ggplot(data = sizes_of_year, aes(x = L)) +
    geom_histogram(binwidth = 2, bins = 20) +
    xlim(0,70) + ylim(0,20)+
    theme(axis.text = element_blank(), axis.title = element_blank())
}
plots <- years %>% map(year_plot)
#Первые 9
plot_grid(plotlist = head(plots, n=9), ncol = 3)
#Все
plot_grid(plotlist = plots, ncol = 6)
