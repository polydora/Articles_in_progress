
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

resample_n <- 1000 # Number of Monte Carlo iterations per climate variable


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
                              
                              
                              
                              ########## Получаем симулированные остатки с помощью DHARMa ##################                               
                              res <- DHARMa::simulateResiduals(mod)
                              res_agr <- DHARMa::recalculateResiduals(res, group = df$Year)
                              
                              #Тест Дарбин-Уотсона на автокорреляцию с лагом 1
                              DW_test <- DHARMa::testTemporalAutocorrelation(res_agr, time = unique(df$Year))
                              
                              
                              param_results[[length(param_results) + 1]] <- data.frame(
                                N = i,
                                clim_param = param_name,
                                coef = coef(mod)[2],
                                R2 = summary(mod)$adj.r.squared,
                                DW = DW_test$statistic,  #Сама статистикка DW-test
                                DW_p.val <- DW_test$p.value, # p-value от нее
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



lm_results_long |> 
  ggplot(aes(x = DW)) +
  geom_histogram()+
  geom_vline(xintercept = c(1.5, 2.5)) +
  facet_wrap(~clim_param)

# Надо, конечно, смотреть на большем количестве циклов, но в ряде случаев сигнал автокорреляции первого порядка, все-таки, есть.


# Здесь я посмотрел на частоту отвержения нулевой гипотезы об отсутствии автокорреляции. 

lm_results_long |> 
  group_by(clim_param) |> 
  summarise(H0_rejecting_freq = mean(DW_p.val....DW_test.p.value < 0.05))

# Ну... где-то повляются такие сочетания данных, когда H_0 отвергаем довольно часто. В некоторых случаях почти до 20%. Это плохо, конечно. Самые ненадежные показатели в этом смысле - SWI_py, Prec_summer_py,Tsummer_py, Prec_winter_gy. 



