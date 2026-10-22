###### Packages
library(readxl)
library(dplyr)
library(reshape2)
library(mgcv)
library(ggplot2)
library(cowplot)
library(gratia)
library(sp)
library(raster)
library(png)
library(magick)
library(patchwork)




############# GAMM1 construction ##################

### Reading data

dat <- read_excel("Data/swans.xlsx")

dat <-
  dat %>% 
  filter(Depth < 20)

dat2 <- 
  dat %>% 
  dplyr::select(Ice, Depth,  Dist,  Year, Date, Site, C.olor, C.cygnus, C.bewickii) %>% 
  melt(id.vars = c("Ice", "Depth",  "Dist",  "Year", "Date", "Site" ), variable.name = "Species", value.name = "N")

dat2$Species <- factor(dat2$Species)
dat2$Ice <- factor(dat2$Ice, labels = c("Ice_cover", "No_ice"))
dat2$Site <- factor(dat2$Site)



### GAMM1 fitting

Mod <- gam(N ~ s(Year, by = Species, k = 5) +
             s(Dist, Depth,  by = interaction(Species, Ice), k = 10) +
             Species + 
             Ice +
             s(Site, bs = "re", k = length(unique(dat2$Site))),
           data = dat2,
           family = nb(),
           method = "REML")

### GAMM1 visualization

theme_set(theme_bw() + theme(legend.position = "botom"))


##### Ice free period 


### First species plot


sm <- smooth_estimates(Mod) 


# color and theme set

color_high = "red"
color_low = "yellow"

theme_set(theme(axis.text = element_text(size = 20), plot.title = element_text(size = 20, hjust = 0), plot.subtitle = element_text(size = 20, face = "italic"), axis.title = element_text(size = 20)))


# Data for a specific combination
points_data <- dat2[dat2$Species == "C.olor" & dat2$Ice == "No_ice", c("Dist", "Depth")]

# Create a raster mask based on the distance to the nearest point
smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)C.olor.No_ice")

# Convert to SpatialPoints
coordinates(points_data) <- ~Dist+Depth

# For each grid point, compute the distance to the nearest data point
distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

# Keep points that fall within the threshold distance
threshold <- quantile(distances[distances > 0], 0.15) * 3  # e.g., twice the median distance

smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot

Pl_sp1 <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high =  color_high) +
  coord_fixed() +
  ggtitle(label = "B", subtitle = "C.olor") +
  labs(fill = "Partial effect", x = NULL, y = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.86, 0.21),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"),
    legend.key.size = unit(0.5, "cm"),  
    legend.key.width = unit(0.5, "cm"), 
    legend.text = element_text(size = 9), 
    legend.title = element_text(size = 10),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")
  )

ggsave(plot = Pl_sp1, filename = "Figures/Pl_sp1_ice_free.png", dpi = 600)


img <- image_read("Figures/Pl_sp1_ice_free.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_sp1_ice_free.png")


### Second species plot

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)C.cygnus.No_ice")

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 3  

smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_sp2 <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high =  color_high) +
  coord_fixed() +
  ggtitle(label = " ", subtitle = "C.cygnus") +
  labs(fill = "Partial effect", x = NULL, y = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.86, 0.21),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"),
    legend.key.size = unit(0.5, "cm"),  # Legend key size
    legend.key.width = unit(0.5, "cm"), # Legend key width
    legend.text = element_text(size = 9), # Text size
    legend.title = element_text(size = 10) # Title size
  )

ggsave(plot = Pl_sp2, filename = "Figures/Pl_sp2_ice_free.png", dpi = 600)

img <- image_read("Figures/Pl_sp2_ice_free.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_sp2_ice_free.png")





### Thirg species plot
smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)C.bewickii.No_ice")

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 3

smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_sp3 <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high =  color_high) +
  coord_fixed() +
  ggtitle(label = " ", subtitle = "C.bewickii") +
  labs(fill = "Partial effect", x = "Distance", y = NULL) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.86, 0.21),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"),
    legend.key.size = unit(0.5, "cm"),  # Legend key size
    legend.key.width = unit(0.5, "cm"), # Legend key width
    legend.text = element_text(size = 9), # Text size
    legend.title = element_text(size = 10) # Title size
  )

ggsave(plot = Pl_sp3, filename = "Figures/Pl_sp3_ice_free.png", dpi = 600)


img <- image_read("Figures/Pl_sp3_ice_free.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_sp3_ice_free.png")



### Combine all figures 

combined <- image_read(c(
  "Figures/Pl_sp1_ice_free.png",
  "Figures/Pl_sp2_ice_free.png",
  "Figures/Pl_sp3_ice_free.png"
)) %>%
  image_append(stack = TRUE)  # stack = TRUE for vertical concatenation

# Save
image_write(combined, "Figures/combined_vertical_ice_free.png")




##### Ice melting period 

### First species plot

points_data <- dat2[dat2$Species == "C.olor" & dat2$Ice == "Ice_cover", c("Dist", "Depth")]

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)C.olor.Ice_cover")

coordinates(points_data) <- ~Dist+Depth

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 1.6  

smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_sp1 <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high = color_high) +
  coord_fixed() +
  ggtitle(label = "A", subtitle ="C.olor") +
  labs(fill = "Partial effect", x = NULL, y = "Depth") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.88, 0.18),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"),
    legend.key.size = unit(0.5, "cm"),  # Legend key size
    legend.key.width = unit(0.5, "cm"), # Legend key width
    legend.text = element_text(size = 9), # Text size
    legend.title = element_text(size = 10) # Title size
  )

ggsave(plot = Pl_sp1, filename = "Figures/Pl_sp1_ice_melting.png", dpi = 600)


img <- image_read("Figures/Pl_sp1_ice_melting.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_sp1_ice_melting.png")



### Second species plot

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)C.cygnus.Ice_cover")

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 1.6

smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_sp2 <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high = color_high) +
  coord_fixed() +
  ggtitle(label = " ", subtitle ="C.cygnus") +
  labs(fill = "Partial effect", x = NULL, y = "Depth") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.88, 0.18),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"),
    legend.key.size = unit(0.5, "cm"),  
    legend.key.width = unit(0.5, "cm"), 
    legend.text = element_text(size = 9), 
    legend.title = element_text(size = 10) 
)

ggsave(plot = Pl_sp2, filename = "Figures/Pl_sp2_ice_melting.png", dpi = 600)


img <- image_read("Figures/Pl_sp2_ice_melting.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_sp2_ice_melting.png")



### Third species plot

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):interaction(Species, Ice)C.bewickii.Ice_cover")

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 1.6

smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_sp3 <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high = color_high) +
  coord_fixed() +
  ggtitle(label = " ", subtitle ="C.bewickii") +
  labs(fill = "Partial effect", x = "Distance", y = "Depth") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.88, 0.18),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"),
    legend.key.size = unit(0.5, "cm"),  # Legend key size
    legend.key.width = unit(0.5, "cm"), # Legend key width
    legend.text = element_text(size = 9), # Text size
    legend.title = element_text(size = 10) # Title size
  )

ggsave(plot = Pl_sp3, filename = "Figures/Pl_sp3_ice_melting.png", dpi = 600)


img <- image_read("Figures/Pl_sp3_ice_melting.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_sp3_ice_melting.png")




#### Combine all figures

combined_ice_melting <- image_read(c(
  "Figures/Pl_sp1_ice_melting.png",
  "Figures/Pl_sp2_ice_melting.png",
  "Figures/Pl_sp3_ice_melting.png"
)) %>%
  image_append(stack = TRUE)  

# Save
image_write(combined_ice_melting, "Figures/combined_ice_melting.png")


### Combine two periods

combined_two_periods <- image_read(c(
  "Figures/combined_ice_melting.png",
  "Figures/combined_vertical_ice_free.png")) %>%
  # image_trim() %>%  
  image_border("white", "0x10") %>%  
  image_append(stack = FALSE) 

# Save
image_write(combined_two_periods, "Figures/combined_two_periods.png")


############## Pie-charts construction #########################

dat_ice <- read_excel("Data/swans.xlsx", sheet = "Ice_period")

dat_ice_long <- 
  melt(dat_ice, id.vars = c("ID", "Ice", "Feeding_place", "Depth", "Dist"       , "Year", "Date", "Site" ), variable.name = "Species", value.name = "Abund")

mean_data <- dat_ice_long %>%
  group_by(Feeding_place, Species) %>%
  summarise(Mean_abund = mean(Abund, na.rm = TRUE), .groups = "drop") %>%
  group_by(Species) %>%
  mutate(Proportion = Mean_abund / sum(Mean_abund))  # proportion for the pie chart


feeding_place_colors <- c(
  "Coastal shallow-water polynyas" = "#66c2a5",
  "Remote deep-water areas near the fast ice" = "#fc8d62"
)

# Function to create a pie chart for a single species
plot_species_pie <- function(species_name) {
  species_data <- mean_data %>%
    filter(Species == species_name)
  
  ggplot(species_data, aes(x = "", y = Proportion, fill = Feeding_place)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = c("#66c2a5", "#fc8d62")) +
    labs(title = paste(species_name),
         fill = "Feeding place",
         x = NULL, y = NULL) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11, face = "bold")
    ) 
}

# Create charts for each species
plot_Sp1 <- plot_species_pie("C.olor")
plot_Sp2 <- plot_species_pie("C.cygnus")
plot_Sp3 <- plot_species_pie("C.bewickii")


combined_plots <- plot_Sp1 + plot_Sp2 + plot_Sp3 +
  plot_layout(guides = "collect") &  
  theme(legend.position = "bottom") +
  theme(
    plot.margin = margin(0, 0, 0, 0, unit = "cm")
  )

ggsave(filename = "Figures/Pie_charts.png", plot = combined_plots, dpi = 600) 

img <- image_read("Figures/Pie_charts.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pie_charts.png")


############### GAMM2 construction ##################

dat_cygnets <- read_excel("Data/swans.xlsx", sheet = "C.olor_Chicks")

dat_cygnets <-
  dat_cygnets %>% 
  filter(Depth < 20)

dat_cygnets2 <- 
  dat_cygnets %>% 
  dplyr::select(Depth,  Dist,  Year, Site, Cygnets_Young, Cygnets_Old) %>% 
  melt(id.vars = c("Depth",  "Dist",  "Year", "Site" ), variable.name = "Cygnets_Age", value.name = "N")

dat_cygnets2$Cygnets_Age <- factor(dat_cygnets2$Cygnets_Age)
dat_cygnets2$Site <- factor(dat_cygnets2$Site)

#### GAMM2 fitting

Mod_2 <- gam(N ~ s(Year, by = Cygnets_Age, k = 10) +
               s(Dist, Depth,  by = Cygnets_Age, k = 10) +
               Cygnets_Age +
               s(Site, bs = "re", k = length(unique(dat_cygnets2$Site))),
             data = dat_cygnets2,
             family = nb(),
             method = "REML")


###### GAMM2 visualization

sm <- smooth_estimates(Mod_2) 

points_data <- dat_cygnets2[dat_cygnets2$Cygnets_Age == "Cygnets_Young", c("Dist", "Depth")]

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):Cygnets_AgeCygnets_Young")

coordinates(points_data) <- ~Dist+Depth

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 3  
smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_Cygnets_Young <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high = color_high) +
  coord_fixed() +
  ggtitle("Cygnets up to two weeks old") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.85, 0.26),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"))  +
  labs(fill = "Partial effect", x = "", y = "Depth")

ggsave(plot = Pl_Cygnets_Young, filename = "Figures/Pl_Cygnets_Young.png")

img <- image_read("Figures/Pl_Cygnets_Young.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_Cygnets_Young.png")



# 
points_data <- dat_cygnets2[dat_cygnets2$Cygnets_Age == "Cygnets_Old", c("Dist", "Depth")]

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth):Cygnets_AgeCygnets_Old")

coordinates(points_data) <- ~Dist+Depth

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 3  
smooth_data_filtered <- smooth_data[distances <= threshold, ]

# Plot
Pl_Cygnets_Old <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high = color_high) +
  coord_fixed() +
  ggtitle("Cygnets older two manth") +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.85, 0.26),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black")) +
  labs(fill = "Partial effect", x = "Distance", y = "Depth")

ggsave(plot = Pl_Cygnets_Old, filename = "Figures/Pl_Cygnets_Old.png")

img <- image_read("Figures/Pl_Cygnets_Old.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_Cygnets_Old.png")



############# GAMM3 construction #######################


dat_molting <- read_excel("Data/swans.xlsx", sheet = "C.olor_Chicks")


dat_molting <-
  dat_molting %>% 
  filter(Depth < 20)

dat_molting2 <- 
  dat_molting %>% 
  dplyr::select(Depth,  Dist,  Year, Site, Molting_Birds) %>% 
  melt(id.vars = c("Depth",  "Dist",  "Year", "Site" ), variable.name = "Stage", value.name = "N")

dat_molting2$Site <- factor(dat_molting2$Site)


### GAMM3 fitting

Mod_3 <- gam(N ~ s(Year, k = 10) +
               s(Dist, Depth, k = 10) +
               s(Site, bs = "re", k = length(unique(dat2$Site))),
             data = dat_molting2,
             family = nb(),
             method = "REML")


### GAMM3 visualization

sm <- smooth_estimates(Mod_3) 

points_data <- dat_molting2[, c("Dist", "Depth")]

smooth_data <- sm %>% 
  filter(.smooth == "s(Dist,Depth)")

coordinates(points_data) <- ~Dist+Depth

distances <- apply(smooth_data[, c("Dist", "Depth")], 1, function(point) {
  min(sqrt((points_data$Dist - point["Dist"])^2 + (points_data$Depth - point["Depth"])^2))
})

threshold <- quantile(distances[distances > 0], 0.15) * 3  
smooth_data_filtered <- smooth_data[distances <= threshold, ]

Pl_Molting_Birds <-
  ggplot(smooth_data_filtered, aes(Dist, Depth)) +
  geom_tile(aes(fill = .estimate)) +
  geom_point(data = as.data.frame(points_data), size = 1, alpha = 0.5) +
  geom_contour(aes(z = .estimate), color = "black", bins = 10) +
  scale_fill_gradient2(low = color_low, mid = "white", high = color_high) +
  coord_fixed() +
  ggtitle("Molting C.olor") +
  labs(fill = "Partial effect", x = "Distance", y = "Depth") +   
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90", color = NA),
    legend.position = c(0.87, 0.25),
    legend.background = element_rect(fill = alpha("white", 0.5), color = "black"))


ggsave(plot = Pl_Molting_Birds, filename = "Figures/Pl_Molting_Birds.png")


img <- image_read("Figures/Pl_Molting_Birds.png")
img <- image_trim(img)
img <- image_border(img, "white", "5x5")  

image_write(img, "Figures/Pl_Molting_Birds.png")



combined_cygnets_molting <- image_read(c(
  "Figures/Pl_Cygnets_Young.png",
  "Figures/Pl_Cygnets_Old.png",
  "Figures/Pl_Molting_Birds.png")) %>%
  image_border("white", "0x0") %>%  
  image_append(stack = TRUE) 

# Save
image_write(combined_cygnets_molting, "Figures/combined_cygnets_molting.png")


