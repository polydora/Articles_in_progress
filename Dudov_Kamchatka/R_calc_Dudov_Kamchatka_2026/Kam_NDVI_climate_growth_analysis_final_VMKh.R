# Primary spruce forests of Kamchatka are relatively stable in the face of global change
# Resampling-based analysis of (i) NDVI-climate, (ii) NDVI-tree growth and (iii) climate-tree growth relationships
# Authors: Vadim M. Khaitov, Sergey V. Dudov
# Lomonosov Moscow State University, Faculty of Biology, Moscow, Russia
# Contact email: serg.dudov@gmail.com
# R code written under R version 4.5.2
# © 2026 by Sergey V. Dudov is licensed under CC BY-NC-SA 4.0

# data sources: Landsat Collection 2, Tier 1, Level-2 surface reflectance (30 m), U.S. Geological Survey, accessed through Google Earth Engine (Gorelick et al., 2017, https://doi.org/10.1016/j.rse.2017.06.031)
# data sources: Annual maximum NDVI and the greening / browning / no-trend categories for 2000-2023, derived from the Landsat record with the LandsatTS R package v1.2.3 (Berner et al., 2023)
# data sources: Climate data from CRU TS 4.07, 1901-2023 (Harris et al., 2020, https://doi.org/10.1038/s41597-020-0453-3)
# data sources: Tree-ring width measurements (RWI and BAI), collected by the authors and published for the first time with this study

# 1. Load packages ------------

library(readxl) # Reading Excel input files
library(openxlsx) # Writing Excel output files
library(tidyverse) # Data manipulation and visualization
library(ggplot2) # Grammar-of-graphics plotting
library(reshape2) # Reshaping data between wide and long formats
library(ggridges) # Ridgeline density plots
library(foreach) # Loop construct for parallel execution
library(doParallel) # Parallel backend for foreach
library(doRNG) # Reproducible random number streams in parallel loops
library(vegan) # Community ecology and multivariate analysis

# 2. Parameters   -----------

# setwd('/media/sergey/Data/Science/Kamtshatka/spruce/Article1_2025/RScripts/Vadim') # Set working directory

fig.dir <- "/figures_final/" # Output directory for figures and result tables

# 3. Load and prepare data   -----------

# 3.1. Load climate data
# Annual climate variables; the analysis is restricted to the period covered by the NDVI time series (2000-2023)

clim <- read_excel("Clim_upd_3.xlsx")
clim$Year <- as.numeric(clim$Year)
clim <- clim %>% filter(Year > 1999 & Year < 2024)

# 3.2. Load dendrochronological data
# Sheet 1 - basal area increment (BAI), sheet 2 - ring width index (RWI); data are in long format by Species, Plot and Year

trees_BAI <- read_excel("RWI_BAI_ed-2.xlsx", sheet = 1)
trees_RWI <- read_excel("RWI_BAI_ed-2.xlsx", sheet = 2)
trees_BAI <- trees_BAI %>% mutate(Plot = as.numeric(Plot)) |> rename(BAI_Value = Value) |> mutate(BAI_Value = as.numeric(BAI_Value)) # Convert plot identifiers to numeric and rename the value column

trees_RWI <- trees_RWI %>% mutate(Plot = as.numeric(Plot)) |> rename(RWI_Value = Value) # Convert plot identifiers to numeric and rename the value column

# 3.3. Load NDVI time series for the random points distributed within the spruce forest cover of the study area
# Only the annual maximum NDVI (ndvi.max) is used as the vegetation index in this study

ndvi <-
  read.csv("picea.gsdt.ndvi_random_points.csv", sep = ";") %>%
  select(sample.id, year, ndvi.max) %>%
  rename(Year = year) %>%
  filter(Year > 1999 & Year < 2024) %>%
  rename(Plot = sample.id)

# 3.4. Load NDVI trend categories of the random points (greening / browning / no trend, 2000-2023)

ndvi_trend <-
  read.csv("trend_ndvi_2000_2023.csv") %>%
  select(sample.id, trend.cat)

# 3.5. Load NDVI time series extracted for the sample plots with tree-ring data

ndvi.local.path <- "picea.gsdt.ndvi_sample_plots.csv" # Path to the NDVI time series of the sample plots

ndvi_local <-
  read.csv(ndvi.local.path, sep = ";") %>%
  select(sample.id, year, ndvi.max) %>%
  rename(Year = year) %>%
  filter(Year > 1999 & Year < 2024) %>%
  mutate(Plot = gsub(pattern = "S_", replacement = "", x = sample.id)) %>%
  select(-sample.id)

ndvi_local <- ndvi_local %>% mutate(Plot = as.numeric(Plot)) %>% mutate(Year = as.integer(Year)) # Convert the join keys to the types used in the tree-ring data (numeric Plot, integer Year)

# 4. NDVI response to climate -----------
# Monte Carlo analysis of the relationship between spruce forest NDVI and each climate variable across the study area.
# Question addressed: does forest NDVI respond to interannual climate variation at the landscape scale?
# For every iteration a single-predictor linear model is fitted on a random subset of years and points;
# the resulting coefficient distributions are summarised by their median and 95% quantile interval.

# 4.1. Resampling parameters

library(DHARMa)

set.seed(12345)

resample_n <- 100 # Number of Monte Carlo iterations per climate variable


year_num <- round(length(unique(ndvi$Year)) * 0.75) # Each model is fitted on 75% of the available years
site_num <- round(length(unique(ndvi$Plot)) * 0.25) # Each model is fitted on 25% of the available points
years_full <- unique(clim$Year) # Pool of years available for resampling
sites_full <- unique(ndvi$Plot) # Pool of points available for resampling

clim_metric <- clim |> select(-Year) |> colnames() # Names of the climate variables to be tested

# 4.2. Resampling over all points, irrespective of the NDVI trend category

n_cores <- detectCores() - 2 # Number of parallel processes; two cores are left to the operating system
cl <- makeCluster(n_cores)
registerDoParallel(cl)

results_list <- foreach(k = seq_along(clim_metric),
                        .combine = rbind,
                        .packages = c("dplyr", "tidyr")) %dorng% {  # %dorng% ensures reproducible random streams
                          param_name <- clim_metric[k]

                          # Inner loop: iterations run sequentially within a climate variable, while the variables are processed in parallel
                          param_results <- list()
                          i <- 1
                          while (i <= resample_n) {
                            years <- sample(years_full, size = year_num) # Draw a random subset of years
                            Site <- sample(sites_full, size = site_num) # Draw a random subset of points

                            x <- ndvi %>%
                              filter(Plot %in% Site & Year %in% years)

                            y <- clim %>%
                              select(Year, all_of(param_name)) %>%
                              filter(Year %in% years)

                            df <- merge(x, y, by = "Year", all.x = TRUE) %>%
                              filter(complete.cases(.))

                            if (nrow(df) >= 2) {
                              colnames(df) <- c("Year", "Plot", "ndvi.max", "clim_param")
                              mod <- lm(scale(ndvi.max) ~ scale(clim_param), data = df) # Both variables are standardized, so the slope is a standardized coefficient
                              
res <- DHARMa::simulateResiduals(mod)
res_agr <- DHARMa::recalculateResiduals(res, group = df$Year)
DW_test <- DHARMa::testTemporalAutocorrelation(res_agr, time = unique(df$Year))
                              

                              param_results[[length(param_results) + 1]] <- data.frame(
                                N = i,
                                clim_param = param_name,
                                coef = coef(mod)[2],
                                R2 = summary(mod)$adj.r.squared,
                                DW = DW_test$statistic,
                                DW_p.val <- DW_test$p.value,
                                stringsAsFactors = FALSE
                              )
                              i <- i + 1
                            }
                          }
                          do.call(rbind, param_results)
                        }

# Stop the cluster
stopCluster(cl)

# Bind all iterations into a single data frame and label the subset
lm_results_long <- results_list
lm_results_long$trend.cat <- 'all data'





# 4.3. Resampling restricted to the greening points

cl <- makeCluster(n_cores)
registerDoParallel(cl)

results_list_gr <- foreach(k = seq_along(clim_metric),
                        .combine = rbind,
                        .packages = c("dplyr", "tidyr")) %dorng% {  # %dorng% ensures reproducible random streams
                          param_name <- clim_metric[k]

                          # Inner loop: iterations run sequentially within a climate variable, while the variables are processed in parallel
                          param_results <- list()
                          i <- 1
                          while (i <= resample_n) {
                            years <- sample(years_full, size = year_num) # Draw a random subset of years
                            Site <- sites_full[sites_full %in% ndvi_trend[ndvi_trend$trend.cat == 'greening',]$sample.id] # Restrict the pool to the greening points
                            Site <- Site |> sample(size = round(length(Site) * 0.25)) # Draw 25% of them

                            x <- ndvi %>%
                              filter(Plot %in% Site & Year %in% years)

                            y <- clim %>%
                              select(Year, all_of(param_name)) %>%
                              filter(Year %in% years)

                            df <- merge(x, y, by = "Year", all.x = TRUE) %>%
                              filter(complete.cases(.))

                            if (nrow(df) >= 2) {
                              colnames(df) <- c("Year", "Plot", "ndvi.max", "clim_param")
                              mod <- lm(scale(ndvi.max) ~ scale(clim_param), data = df)

                              param_results[[length(param_results) + 1]] <- data.frame(
                                N = i,
                                clim_param = param_name,
                                coef = coef(mod)[2],
                                R2 = summary(mod)$adj.r.squared,
                                stringsAsFactors = FALSE
                              )
                              i <- i + 1
                            }
                          }
                          do.call(rbind, param_results)
                        }

# Stop the cluster
stopCluster(cl)

# Label the subset
results_list_gr$trend.cat <- 'greening'

# 4.4. Resampling restricted to the browning points

cl <- makeCluster(n_cores)
registerDoParallel(cl)

results_list_br <- foreach(k = seq_along(clim_metric),
                           .combine = rbind,
                           .packages = c("dplyr", "tidyr")) %dorng% {  # %dorng% ensures reproducible random streams
                             param_name <- clim_metric[k]

                             param_results <- list()
                             i <- 1
                             while (i <= resample_n) {
                               years <- sample(years_full, size = year_num) # Draw a random subset of years
                               Site <- sites_full[sites_full %in% ndvi_trend[ndvi_trend$trend.cat == 'browning',]$sample.id] # Restrict the pool to the browning points
                               Site <- Site |> sample(size = round(length(Site) * 0.25)) # Draw 25% of them

                               x <- ndvi %>%
                                 filter(Plot %in% Site & Year %in% years)

                               y <- clim %>%
                                 select(Year, all_of(param_name)) %>%
                                 filter(Year %in% years)

                               df <- merge(x, y, by = "Year", all.x = TRUE) %>%
                                 filter(complete.cases(.))

                               if (nrow(df) >= 2) {
                                 colnames(df) <- c("Year", "Plot", "ndvi.max", "clim_param")
                                 mod <- lm(scale(ndvi.max) ~ scale(clim_param), data = df)

                                 param_results[[length(param_results) + 1]] <- data.frame(
                                   N = i,
                                   clim_param = param_name,
                                   coef = coef(mod)[2],
                                   R2 = summary(mod)$adj.r.squared,
                                   stringsAsFactors = FALSE
                                 )
                                 i <- i + 1
                               }
                             }
                             do.call(rbind, param_results)
                           }

# Stop the cluster
stopCluster(cl)

# Label the subset
results_list_br$trend.cat <- 'browning'

# 4.5. Resampling restricted to the points without a significant NDVI trend

cl <- makeCluster(n_cores)
registerDoParallel(cl)

results_list_nt <- foreach(k = seq_along(clim_metric),
                           .combine = rbind,
                           .packages = c("dplyr", "tidyr")) %dorng% {  # %dorng% ensures reproducible random streams
                             param_name <- clim_metric[k]

                             param_results <- list()
                             i <- 1
                             while (i <= resample_n) {
                               years <- sample(years_full, size = year_num) # Draw a random subset of years
                               Site <- sites_full[sites_full %in% ndvi_trend[ndvi_trend$trend.cat == 'no_trend',]$sample.id] # Restrict the pool to the points without a trend
                               Site <- Site |> sample(size = round(length(Site) * 0.25)) # Draw 25% of them

                               x <- ndvi %>%
                                 filter(Plot %in% Site & Year %in% years)

                               y <- clim %>%
                                 select(Year, all_of(param_name)) %>%
                                 filter(Year %in% years)

                               df <- merge(x, y, by = "Year", all.x = TRUE) %>%
                                 filter(complete.cases(.))

                               if (nrow(df) >= 2) {
                                 colnames(df) <- c("Year", "Plot", "ndvi.max", "clim_param")
                                 mod <- lm(scale(ndvi.max) ~ scale(clim_param), data = df)

                                 param_results[[length(param_results) + 1]] <- data.frame(
                                   N = i,
                                   clim_param = param_name,
                                   coef = coef(mod)[2],
                                   R2 = summary(mod)$adj.r.squared,
                                   stringsAsFactors = FALSE
                                 )
                                 i <- i + 1
                               }
                             }
                             do.call(rbind, param_results)
                           }

# Stop the cluster
stopCluster(cl)

# Label the subset
results_list_nt$trend.cat <- 'no trend'

# 4.6. Merge the four subsets and convert the results to long format
# One column holds the values and another one the name of the metric (standardized coefficient or adjusted R-squared)

lm_results_long_all <- rbind(lm_results_long, results_list_gr, results_list_br, results_list_nt)

lm_results_long_combined <- lm_results_long_all %>%
  pivot_longer(cols = c(coef, R2), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(case_when(
    metric == "coef" ~ "Standardized coefficient",
    metric == "R2"   ~ "Adjusted R-squared"
  ), levels = c("Standardized coefficient", "Adjusted R-squared")))

# 4.7. Define the plotting order and the colour scheme of the NDVI trend categories

category_colors <- c(
  "browning" = "#CD8500",
  "no trend" = "gray60",
  "greening" = "#698B69",
  "all data" = "#697ebf"
)

# Set the factor levels explicitly so that the categories keep the same order in all panels
lm_results_long_combined$trend.cat <- factor(lm_results_long_combined$trend.cat,
                                             levels = c("all data", "greening", "browning", "no trend"))

unique(lm_results_long_combined$clim_param) # Check which climate variables are present

# Order the climate variables and shorten their labels; gy - growing year, py - previous year
lm_results_long_combined$clim_param <- factor(lm_results_long_combined$clim_param,
                                             levels = c("Mean_Tavg_gy","Mean_Tavg_py","Tsummer","Tsummer_py","SWI_gy","SWI_py","Prec_summer_gy","Prec_summer_py","Prec_winter_gy","Prec_winter_py","CMIsummer","CMIsummer_py"),
                                             labels = c("Tavg_gy","Tavg_py","Tsummer","Tsummer_py","SWI_gy","SWI_py","Prec_summer_gy","Prec_summer_py","Prec_winter_gy","Prec_winter_py","CMIsummer","CMIsummer_py"))


# 4.8. Summarise the Monte Carlo distributions and assess significance
# Medians and 95% quantile intervals are computed for every combination of climate variable, metric and trend category

quants_combined <- lm_results_long_combined %>%
  group_by(clim_param, metric, trend.cat) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    q2.5   = quantile(value, 0.025, na.rm = TRUE),
    q97.5  = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(significant = case_when(
    # Coefficients: the interval must not include zero
    metric == "Standardized coefficient" & !(q2.5 < 0 & q97.5 > 0) ~ TRUE,
    # Adjusted R-squared: the lower bound must exceed zero (the model performs better than the null model)
    metric == "Adjusted R-squared" & q2.5 > 0 ~ TRUE,
    TRUE ~ FALSE
  ))

# 4.9. Export the summary table

write.xlsx(quants_combined, paste0(fig.dir, 'NDVI_clim_quants.xlsx'))

# 4.10. Standard error of the mean adjusted R-squared, used as an annotation in the figure

sem_data <- lm_results_long_combined %>%
  filter(metric == "Adjusted R-squared") %>%
  group_by(clim_param, trend.cat) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value, na.rm = TRUE),
    n        = n(),
    sem      = sd_val / sqrt(n),
    label    = paste0('R² = ',round(mean_val, 3), " ± ", round(sem, 3)),
    .groups  = "drop"
  )

# 4.11. Plotting the graph: median standardized coefficients with 95% quantile intervals, faceted by NDVI trend category
# Solid lines denote significant relationships, dashed lines non-significant ones; mean adjusted R-squared ± SEM is printed on the right of each panel

ggplot() +
  geom_errorbarh(
    data = quants_combined %>% filter(metric == "Standardized coefficient"),
    aes(y = clim_param, x = median, xmin = q2.5, xmax = q97.5,
        color = trend.cat, linetype = significant),
    height = 0.3, size = 0.8, position = position_dodge(0.2)
  ) +
  geom_point(
    data = quants_combined %>% filter(metric == "Standardized coefficient"),
    aes(y = clim_param, x = median, color = trend.cat),
    size = 3, position = position_dodge(0.2)
  ) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "gray40") + # Reference line at zero
  facet_grid(metric ~ trend.cat) +#, scales = "free_x") +  # Rows - metrics, columns - trend categories
  scale_linetype_manual(
    values = c("TRUE" = "solid", "FALSE" = "dashed"),
    labels = c("Significant", "Non-significant")
  ) +
  scale_color_manual(values = category_colors) +
  #coord_cartesian(clip = "off") +   # Allow drawing outside the panel area
  labs(x = "Value", y = "Climate variable",
       color = "Trend category", linetype = "Significance") +
  # Adjusted R-squared: text labels placed at the right edge of each panel (x = Inf)
  geom_text(
    data = sem_data,
    aes(y = clim_param, x = Inf, label = label, color = trend.cat),
    size = 2.8, hjust = 1.5, vjust = -1.2,
    position = position_dodge(0.6), show.legend = FALSE, fontface = "italic"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold"),
    plot.margin = margin(r = 30, t = 5, b = 5, l = 5)  # Extra right margin to accommodate the R-squared labels
  )

ggsave(paste0(fig.dir,'Figure8_climate_ndvi_errorbars.png'), plot = last_plot(), scale = 1,
       width = 25, height = 16, units = c("cm"), dpi = 600)


# 5. NDVI and tree growth -----------
# Resampling-based linear regression between plot-level NDVI and radial tree growth.
# Lead and lag terms of up to 2 years test the timing of the signal.

# 5.1. Join the dendrochronological and the plot-level NDVI data

trees <- trees_BAI %>%
  full_join(trees_RWI, by = c("Species", "Plot", "Year")) %>%
  full_join(ndvi_local, by = c("Plot", "Year")) |> drop_na()

trees <- trees |> select(Species,Year,Plot,BAI_Value,RWI_Value,ndvi.max) |> drop_na() # Keep the variables used in the models

#head(trees)

# 5.2. Create the lagged and leading NDVI predictors within each species-plot time series

trees_lag_lead <- trees %>%
  arrange(Species, Plot, Year) %>%
  group_by(Species, Plot) %>%
  mutate(
    ndvi.max_lag1  = lag(ndvi.max, n = 1),
    ndvi.max_lag2  = lag(ndvi.max, n = 2),
    ndvi.max_lead1 = lead(ndvi.max, n = 1),
    ndvi.max_lead2 = lead(ndvi.max, n = 2)
  ) %>%
  ungroup()

# 5.3. Bootstrap parameters
set.seed(12345)
resample_n <- 1000 # Number of bootstrap iterations per combination
year_num <- round(length(unique(trees_lag_lead$Year)) * 0.7) # Each model is fitted on 70% of the years
site_num <- round(length(unique(trees_lag_lead$Plot)) * 0.7) # Each model is fitted on 70% of the plots

years_full <- unique(trees_lag_lead$Year)
sites_full <- unique(trees_lag_lead$Plot)
species_list <- unique(trees_lag_lead$Species)

# 5.4. Bootstrap function; the lag argument selects the temporal offset of the NDVI predictor
# Negative values correspond to leading NDVI (future years), positive values to lagged NDVI (previous years)

tree_NDVI_resp_leadlag <- function(response, species, lag = 0, n_iter = resample_n) {
  # Select the name of the predictor variable
  pred_var <- switch(as.character(lag),
                     "-2" = "ndvi.max_lead2",
                     "-1" = "ndvi.max_lead1",
                     "0"  = "ndvi.max",
                     "1"  = "ndvi.max_lag1",
                     "2"  = "ndvi.max_lag2")

  res <- data.frame(N = integer(), coef = numeric(), R2 = numeric())
  i <- 1
  while (i <= n_iter) {
    years <- sample(years_full, size = year_num) # Draw a random subset of years
    sites <- sample(sites_full, size = site_num) # Draw a random subset of plots

    df <- trees_lag_lead %>%
      filter(Species == species,
             Year %in% years,
             Plot %in% sites) %>%
      select(Year, Plot, all_of(response), all_of(pred_var)) %>%
      filter(complete.cases(.))

    if (response == "BAI_Value") {
      df <- df %>% filter(BAI_Value > 0)
    }

    if (nrow(df) >= 5) { # Only iterations with a sufficient number of observations are retained
      if (response == "BAI_Value") {
        mod <- lm(log(BAI_Value) ~ scale(get(pred_var)), data = df)
      } else {
        mod <- lm(scale(RWI_Value) ~ scale(get(pred_var)), data = df)
      }
      res <- rbind(res, data.frame(N = i,
                                   coef = coef(mod)[2],
                                   R2 = summary(mod)$adj.r.squared))
      i <- i + 1
    }
  }
  res$Lag <- lag
  return(res)
}

# 5.5. Run the bootstrap for all combinations: response (2) x species (4) x lag (-2, -1, 0, 1, 2)

all_results_lag2 <- list()
counter <- 1

for (resp in c("BAI_Value", "RWI_Value")) {
  for (sp in species_list) {
    for (lg in c(-2, -1, 0, 1, 2)) {
      cat("Processing:", resp, sp, "lag =", lg, "\n")
      tmp <- tree_NDVI_resp_leadlag(resp, sp, lag = lg)
      tmp$Response <- ifelse(resp == "BAI_Value", "BAI", "RWI")
      tmp$Species <- sp
      all_results_lag2[[counter]] <- tmp
      counter <- counter + 1
    }
  }
}

# 5.6. Merge the results and convert them to long format

results_long_lag2 <- bind_rows(all_results_lag2) %>%
  pivot_longer(cols = c(coef, R2),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         coef = "Standardized coefficient",
                         R2 = "Adjusted R-squared"))

# 5.7. Define the plotting order of the factors

results_long_lag2$metric <- factor(results_long_lag2$metric,
                                   levels = c("Standardized coefficient", "Adjusted R-squared"))

# Order of the tree species
results_long_lag2$Species <- factor(results_long_lag2$Species,
                                    levels = c("Picea_jezoensis", "Larix_cajanderi", "Betula_platyphylla", "Betula_ermanii"))

# Convert the temporal offset into a readable factor
results_long_lag2$Lag <- factor(results_long_lag2$Lag, levels = c(-2, -1, 0, 1, 2),
                                labels = c("Lead 2 years", "Lead 1 year",
                                           "Current year", "Lag 1 year", "Lag 2 years"))

# 5.8. Summarise the bootstrap distributions by response, species, lag and metric

quants_lag2 <- results_long_lag2 %>%
  group_by(Response, Species, Lag, metric) %>%
  summarise(
    median = median(value, na.rm = TRUE),
    q2.5   = quantile(value, 0.025, na.rm = TRUE),
    q97.5  = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

quants_lag2 <- quants_lag2 |>
  mutate(significant = case_when(
    # Coefficients: the interval must not include zero
    metric == "Standardized coefficient" & !(q2.5 < 0 & q97.5 > 0) ~ TRUE,
    # Adjusted R-squared: the lower bound must exceed zero (the model performs better than the null model)
    metric == "Adjusted R-squared" & q2.5 > 0 ~ TRUE,
    TRUE ~ FALSE
  ))

write.xlsx(quants_lag2, paste0(fig.dir, 'NDVI_dendro_quants.xlsx'))

# 5.9. Standard error of the mean adjusted R-squared for annotation in the figure

sem_data_r2 <- results_long_lag2 %>%
  filter(metric == "Adjusted R-squared") %>%
  group_by(Response, Species, Lag) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value, na.rm = TRUE),
    n        = n(),
    sem      = sd_val / sqrt(n),
    label    = paste0("R² = ", round(mean_val, 3), " ± ", round(sem, 3)),
    .groups  = "drop"
  )

# 5.10. Prepare the factors of the figure
# Species codes and the italicised species names used in the legend and the axis labels

species_order <- c("Picea_jezoensis", "Larix_cajanderi", "Betula_platyphylla", "Betula_ermanii")
species_names <- c("Picea jezoensis", "Larix cajanderi", "Betula platyphylla", "Betula ermanii")
quants_lag2$Species <- factor(quants_lag2$Species, levels = species_order)

sem_data_r2$Species <- factor(sem_data_r2$Species, levels = species_order)

# Order of the responses and of the temporal offsets
quants_lag2$Response <- factor(quants_lag2$Response, levels = c("BAI", "RWI"))
quants_lag2$Lag <- factor(quants_lag2$Lag,
                          labels = c("Lead 2 years", "Lead 1 year",
                                     "Current year", "Lag 1 year", "Lag 2 years"))
sem_data_r2$Response <- factor(sem_data_r2$Response, levels = c("BAI", "RWI"))
sem_data_r2$Lag <- factor(sem_data_r2$Lag,
                          labels = c("Lead 2 years", "Lead 1 year",
                                     "Current year", "Lag 1 year", "Lag 2 years"))

unique(quants_lag2$Species) # Check the factor levels
unique(sem_data_r2$Species) # Check the factor levels

# Colour scheme of the tree species
species_colors <- c("Picea_jezoensis" = "#385723",
                    "Larix_cajanderi" = "#1f4e79",
                    "Betula_platyphylla" = "#626262",
                    "Betula_ermanii" = "#ac73b7")

# 5.11. Plotting the graph: NDVI-growth relationships by species, response variable and temporal offset

ggplot() +
  # Coefficient layer: horizontal error bars (95% quantile interval) and points (medians)
  geom_errorbarh(
    data = quants_lag2 %>% filter(metric == "Standardized coefficient"),
    aes(y = Species, x = median, xmin = q2.5, xmax = q97.5,
        color = Species, linetype = significant),
    height = 0.2, size = 0.9, position = position_dodge(0.6)
  ) +
  geom_point(
    data = quants_lag2 %>% filter(metric == "Standardized coefficient"),
    aes(y = Species, x = median, color = Species),
    size = 3, position = position_dodge(0.6)
  ) +
  # Adjusted R-squared layer: text labels (mean ± SEM) at the right edge of each panel

  # Reference line at zero
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  # Facets: rows - response variable (BAI / RWI), columns - temporal offset
  facet_grid(Response ~ Lag, scales = "free_x") +
  scale_linetype_manual(
    values = c("TRUE" = "solid", "FALSE" = "dashed"),
    labels = c("Significant", "Non-significant")
  ) +
  scale_color_manual(values = species_colors,
                     labels = species_names) +
  scale_y_discrete(limits = rev(species_order),
                   labels = rev(species_names)) +
  labs(x = "Value", y = "Species",
       color = "Species", linetype = "Significance") +
  geom_text(
    data = sem_data_r2,
    aes(y = Species, x = Inf, label = label, color = Species),
    hjust = 1.2, vjust = -1, size = 3.5, fontface = "italic",
    position = position_dodge(0.6), show.legend = FALSE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.box.just = "left",
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(face = "italic", size = 10),
    legend.text = element_text(face = "italic")
  )

ggsave(paste0(fig.dir,'Figure9_tree_NDVI_lag_errorbar.png'), plot = last_plot(), #scale = 1.2,
       width = 28, height = 20, units = c("cm"), dpi = 600)

# 6. Tree growth response to climate -----------
# Resampling-based linear regression between radial tree growth (BAI and RWI) and each climate variable, fitted per species.

# 6.1. Join the dendrochronological and the climate data

trees_clim_clean <- trees_BAI %>%
  full_join(trees_RWI, by = c("Species", "Plot", "Year")) %>%
  left_join(clim, by = c("Year"))

clim_vars <- c("Tsummer", "CMIsummer", "SWI_gy", "Mean_Tavg_gy",
               "Prec_summer_gy", "Prec_winter_gy", "Tsummer_py",
               "CMIsummer_py", "SWI_py", "Mean_Tavg_py",
               "Prec_summer_py", "Prec_winter_py")

# Pools of years and plots available for resampling
years_full <- unique(trees_clim_clean$Year)
sites_full <- unique(trees_clim_clean$Plot)

# 6.2. Bootstrap parameters
set.seed(12345)
resample_n <- 1000 # Number of bootstrap iterations per combination
year_frac <- 0.7 # Proportion of years drawn in each iteration
site_frac <- 0.5 # Proportion of plots drawn in each iteration
year_num <- round(length(years_full) * year_frac)
site_num <- round(length(sites_full) * site_frac)

# 6.3. Set up the parallel backend

n_cores <- detectCores() - 1  # One core is left to the operating system
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# 6.4. Parallel bootstrap
# All combinations of response variable, species and climate variable are generated first and then processed in parallel

responses <- c("BAI_Value", "RWI_Value")
species_list <- unique(trees_clim_clean$Species)
combinations <- expand.grid(Response = responses,
                            Species = species_list,
                            Clim_var = clim_vars,
                            stringsAsFactors = FALSE)

# Bootstrap function applied to a single combination
bootstrap_one <- function(response, species, clim_var,
                          trees_data, years_full, sites_full,
                          year_num, site_num, resample_n) {

  library(dplyr) # Required on every parallel worker

  res <- data.frame(N = integer(), coef = numeric(), R2 = numeric())
  i <- 1
  while (i <= resample_n) {
    years <- sample(years_full, size = year_num) # Draw a random subset of years
    sites <- sample(sites_full, size = site_num) # Draw a random subset of plots

    df <- trees_data %>%
      filter(Species == species,
             Year %in% years,
             Plot %in% sites) %>%
      select(Year, Plot, all_of(response), all_of(clim_var)) %>%
      filter(complete.cases(.))

    if (response == "BAI_Value") {
      df <- df %>% filter(BAI_Value > 0)
    }

    if (nrow(df) >= 5) { # Only iterations with a sufficient number of observations are retained
      if (response == "BAI_Value") {
        mod <- lm(log(BAI_Value) ~ scale(get(clim_var)), data = df)
      } else {
        mod <- lm(RWI_Value ~ scale(get(clim_var)), data = df)
      }
      res <- rbind(res, data.frame(N = i,
                                   coef = coef(mod)[2],
                                   R2 = summary(mod)$adj.r.squared))
      i <- i + 1
    }
  }
  # Add the identifiers of the combination
  res$Response <- ifelse(response == "BAI_Value", "BAI", "RWI")
  res$Species <- species
  res$Clim_param <- clim_var
  return(res)
}

# Parallel loop over all combinations
results_list <- foreach(idx = 1:nrow(combinations),
                        .packages = c("dplyr"),
                        .combine = rbind,
                        .options.RNG = 12345) %dorng% {
                          comb <- combinations[idx, ]
                          bootstrap_one(response = comb$Response,
                                        species = comb$Species,
                                        clim_var = comb$Clim_var,
                                        trees_data = trees_clim_clean,
                                        years_full = years_full,
                                        sites_full = sites_full,
                                        year_num = year_num,
                                        site_num = site_num,
                                        resample_n = resample_n)
                        }

stopCluster(cl) # Stop the cluster

# 6.5. Convert the results to long format

results_long <- results_list %>%
  pivot_longer(cols = c(coef, R2),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         coef = "Standardized coefficient",
                         R2 = "Adjusted R-squared"))

# 6.6. Define the plotting order of the factors

# Order of the climate variables
clim_order <- c("Mean_Tavg_gy", "Mean_Tavg_py", "Tsummer", "Tsummer_py",
                "SWI_gy", "SWI_py", "Prec_summer_gy", "Prec_summer_py",
                "Prec_winter_gy", "Prec_winter_py", "CMIsummer", "CMIsummer_py")
clim_order <- intersect(clim_order, unique(results_long$Clim_param))
results_long$Clim_param <- factor(results_long$Clim_param, levels = clim_order)

# Order of the tree species
results_long$Species <- factor(results_long$Species, levels = c("Picea_jezoensis", "Larix_cajanderi",
                                                                "Betula_platyphylla", "Betula_ermanii"))

# Order of the metrics (coefficient first, adjusted R-squared second)
results_long$metric <- factor(results_long$metric,
                              levels = c("Standardized coefficient", "Adjusted R-squared"))

# 6.7. Summarise the bootstrap distributions and assess significance

quants <- results_long %>%
  group_by(Response, Clim_param, Species, metric) %>%
  summarise(median = median(value, na.rm = TRUE),
            q2.5   = quantile(value, 0.025, na.rm = TRUE),
            q97.5  = quantile(value, 0.975, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(significant = case_when(
    metric == "Standardized coefficient" & !(q2.5 < 0 & q97.5 > 0) ~ TRUE, # The interval must not include zero
    metric == "Adjusted R-squared" & q2.5 > 0 ~ TRUE, # The lower bound must exceed zero
    TRUE ~ FALSE
  ))


# Order of the metrics (coefficient on top, adjusted R-squared below when faceting by metric)
quants$metric <- factor(quants$metric, levels = c("Standardized coefficient", "Adjusted R-squared"))

# Order of the tree species
quants$Species <- factor(quants$Species, levels = species_order)

write.xlsx(quants, paste0(fig.dir, 'clim_dendro_quants.xlsx'))

# 6.8. Standard error of the mean adjusted R-squared for annotation in the figures

sem_data_r2_clim <- results_long %>%
  filter(metric == "Adjusted R-squared") %>%
  group_by(Response, Clim_param, Species) %>%
  summarise(
    mean_val = mean(value, na.rm = TRUE),
    sd_val   = sd(value, na.rm = TRUE),
    n        = n(),
    sem      = sd_val / sqrt(n),
    label    = paste0("R² = ", round(mean_val, 3), " ± ", round(sem, 3)),
    .groups = "drop"
  )

# Reapply the factor levels to make sure both tables share the same ordering
sem_data_r2_clim$Species <- factor(sem_data_r2_clim$Species, levels = species_order)
sem_data_r2_clim$Clim_param <- factor(sem_data_r2_clim$Clim_param, levels = clim_order)

# 6.9. Plotting function, one figure per response variable

plot_clim_response <- function(resp_name) {
  coef_data <- quants %>%
    filter(Response == resp_name, metric == "Standardized coefficient") %>%
    mutate(Species = factor(Species, levels = species_order, labels = species_names))

  r2_data <- sem_data_r2_clim %>%
    filter(Response == resp_name) %>%
    mutate(Species = factor(Species, levels = species_order, labels = species_names))

  species_colors_new <- c("Picea jezoensis" = "#385723",
                          "Larix cajanderi" = "#1f4e79",
                          "Betula platyphylla" = "#626262",
                          "Betula ermanii" = "#ac73b7")

  ggplot() +
    geom_errorbarh(
      data = coef_data,
      aes(y = Clim_param, x = median, xmin = q2.5, xmax = q97.5,
          color = Species, linetype = significant),
      height = 0.2, size = 0.8
    ) +
    geom_point(
      data = coef_data,
      aes(y = Clim_param, x = median, color = Species),
      size = 2.5
    ) +
    geom_text(
      data = r2_data,
      aes(y = Clim_param, x = Inf, label = label, color = Species),
      fontface = "italic",   # Adjusted R-squared labels in italics
      hjust = 2, vjust = -0.3, size = 3, show.legend = FALSE
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") + # Reference line at zero
    scale_linetype_manual(
      values = c("TRUE" = "solid", "FALSE" = "dashed"),
      labels = c("Significant", "Non-significant")
    ) +
    scale_color_manual(values = species_colors_new, guide = "none") +
    labs(
      x = "Value",
      y = "Climate variable",
      title = paste("Response:", resp_name),
      linetype = "Significance"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      strip.text = element_text(face = "italic", size = 9),  # Facet labels in italics
      strip.background = element_rect(fill = "grey90", color = NA),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 8)
    )
}

# 6.10. Plotting the graphs: growth-climate relationships for BAI and RWI

plot_BAI <- plot_clim_response("BAI")
print(plot_BAI)

plot_RWI <- plot_clim_response("RWI")
print(plot_RWI)

ggsave(paste0(fig.dir, 'Figure5_BAI_clim_errorbar.png'), plot = plot_BAI,
       width = 18, height = 20, units = "cm", dpi = 600)
ggsave(paste0(fig.dir, 'Figure6_RWI_clim_errorbar.png'), plot = plot_RWI,
       width = 18, height = 20, units = "cm", dpi = 600)
