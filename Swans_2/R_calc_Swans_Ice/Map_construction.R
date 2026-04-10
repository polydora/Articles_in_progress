library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)
library(DHARMa)
library(gratia)
library(reshape2)
library(tidyr)
library(lubridate)



Fin_df <- read.csv("Data/Finnish_Gulf_East.csv") # Map poligones

# Basic map layer####################

theme_set(theme_bw())



Pl_Finnish_Gulf <-
  ggplot(Fin_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") +
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) +
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) +
  theme(axis.ticks = element_blank())

dat <- read_excel("Data/all.data.xlsx")

dat <-
  dat %>%
  mutate(Ice_Break_DOY = yday(Date))


year = 2025

dat %>%
  filter(Year == year)  ->
  df

Pl_Finnish_Gulf +
  geom_point(data = df, aes(x = Lon, y = Lat, group = 1, size = nests, color = Ice_Break_DOY)) +
  scale_size_continuous(
    breaks = c(seq(1,30,3)),
    limits = c(1, 30),
    range = c(4, 8),  # диапазон размеров точек (мин, макс)
    name = "Количество гнезд"
  ) +
  scale_color_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = mean(dat$Ice_Break_DOY)) +
  ggtitle(paste(year) ) +
  guides(size = "none") ->
  Pl_nest



dat %>%
  filter(Year <= year) %>%
  group_by(Year) %>%
  summarise(Mean_Ice_Break_DOY = mean(Ice_Break_DOY)) %>%
  ungroup() ->
  doy_df

doy_df %>%
  ggplot(aes(x = as.numeric(Year), y = Mean_Ice_Break_DOY)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), 2)) +
  xlim(1984, 2025) +
  ylim(0, 150) +
  labs(y = "Ice break-up day", x = "Year" ) ->
  Pl_doy



x_range <- layer_scales(Pl_nest)$x$range$range
y_range <- layer_scales(Pl_nest)$y$range$range

x_min <- x_range[1]
x_max <- x_range[2]
y_min <- y_range[1]
y_max <- y_range[2]

# Рассчитываем размер и положение вставки
inset_width <- (x_max - x_min) * 0.4   # % ширины карты
inset_height <- (y_max - y_min) * 0.35  # % высоты карты

# Вставляем график в правый верхний угол
Pl_nest +
  annotation_custom(
    grob = ggplotGrob(Pl_doy),
    xmin = x_max - inset_width,
    xmax = x_max,
    ymin = y_max - inset_height,
    ymax = y_max
  )







plot_nests <- function(Year){
  dat %>%
    filter(Year == year)  ->
    df


  Pl_Finnish_Gulf +
    geom_point(data = df, aes(x = Lon, y = Lat, group = 1, size = nests, color = Ice_Break_DOY)) +
    scale_size_continuous(
      breaks = c(seq(1,30,3)),
      limits = c(1, 30),
      range = c(4, 8),  # диапазон размеров точек (мин, макс)
      name = "Количество гнезд"
    ) +
    scale_color_gradient2(low = "red", mid = "yellow", high = "blue", midpoint = mean(dat$Ice_Break_DOY)) +
    ggtitle(paste(year) ) +
    guides(size = "none") ->
    Pl_nest




  dat %>%
    filter(Year <= year) %>%
    group_by(Year) %>%
    summarise(Mean_Ice_Break_DOY = mean(Ice_Break_DOY)) %>%
    ungroup() ->
    doy_df

  doy_df %>%
    ggplot(aes(x = as.numeric(Year), y = Mean_Ice_Break_DOY)) +
    geom_line() +
    scale_x_continuous(breaks = seq(min(dat$Year), max(dat$Year), 2)) +
    xlim(1984, 2025) +
    ylim(0, 150) +
    labs(y = "Ice break-up day", x = "Year" ) ->
    Pl_doy



  x_range <- layer_scales(Pl_nest)$x$range$range
  y_range <- layer_scales(Pl_nest)$y$range$range

  x_min <- x_range[1]
  x_max <- x_range[2]
  y_min <- y_range[1]
  y_max <- y_range[2]

  # Рассчитываем размер и положение вставки
  inset_width <- (x_max - x_min) * 0.4   # % ширины карты
  inset_height <- (y_max - y_min) * 0.35  # % высоты карты

  # Вставляем график в правый верхний угол
  Pl_nest +
    annotation_custom(
      grob = ggplotGrob(Pl_doy),
      xmin = x_max - inset_width,
      xmax = x_max,
      ymin = y_max - inset_height,
      ymax = y_max
    )

    }



plot_nests(2003)


### Картинки для каждого года####
for(year in unique(dat$Year) ) {
  ggsave(filename = paste0(year,".jpg"), plot = plot_nests(as.numeric(year)))
  print(year)
}


### Анимация ###############
library(magick)

# Список файлов (предполагается, что они отсортированы по порядку)
file_list <- list.files(path = "Figures",
                        pattern = "\\.jpg$",
                        full.names = TRUE)

# Загружаем все изображения
images <- image_read(file_list)

# Создаем анимированный GIF
animation <- image_animate(images, fps = 1, loop = 0)  # fps - кадров в секунду

# Сохраняем
image_write(animation, "animation.gif")


