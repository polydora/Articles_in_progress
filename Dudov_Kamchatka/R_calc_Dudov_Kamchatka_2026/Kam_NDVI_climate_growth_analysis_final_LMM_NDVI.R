
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

library(astrochron)


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


x <- ndvi
y <- clim

df <- merge(x, y, by = "Year", all.x = TRUE) %>%
  filter(complete.cases(.))

ndvi_trend |> 
  rename(Plot = sample.id) ->
  ndvi_trend

head(ndvi_trend)

df <- merge(df, ndvi_trend)



# Смотрим на коллинеарность предикторов

df |> 
  filter(Plot == "S_1") |> 
  select(-Year, -Plot, -trend.cat) ->
  df_foo

mod_foo <- lm(ndvi.max ~ ., data = df_foo)

library(car)

vif(mod_foo)

mod_foo_2 <- update(mod_foo, . ~ . - SWI_gy)
vif(mod_foo_2)

mod_foo_3 <- update(mod_foo_2, . ~ . - SWI_py)
vif(mod_foo_3)

mod_foo_4 <- update(mod_foo_3, . ~ . - Prec_summer_py)
vif(mod_foo_4)

mod_foo_5 <- update(mod_foo_4, . ~ . - Prec_summer_gy)
vif(mod_foo_5)

mod_foo_6 <- update(mod_foo_5, . ~ . - Tsummer_py)
vif(mod_foo_6)


predictors <- names(coef(mod_foo_6))[-1]

df_selected <- 
  df |> 
  select(Year, Plot, ndvi.max, trend.cat,  all_of(predictors))

df_selected$trend.cat <- factor(df_selected$trend.cat)
df_selected$Plot <- factor(df_selected$Plot)

names(df_selected)

library(lme4)

Mod_lmer <- lmer(log(ndvi.max) ~ Tsummer + CMIsummer + Mean_Tavg_gy + Prec_winter_gy + CMIsummer_py + Mean_Tavg_py + Prec_winter_py + (1|Plot), data = df_selected)


summary(Mod_lmer)

simulateResiduals(Mod_lmer, plot = T)



library(mgcv)

Mod <- bam(log(ndvi.max) ~ s(Tsummer) + s(CMIsummer) + s(Mean_Tavg_gy) + s(Prec_winter_gy) + s(CMIsummer_py) + s(Mean_Tavg_py) + s(Prec_winter_py)  + s(Plot, bs = "re", k = length(unique(df_selected$Plot))), data = df_selected)


simulateResiduals(Mod, plot = T)




Mod_by_trend <- bam(log(ndvi.max) ~ s(Tsummer, by = trend.cat) + s(CMIsummer, by = trend.cat) + s(Mean_Tavg_gy, by = trend.cat) + s(Prec_winter_gy, by = trend.cat) + s(CMIsummer_py, by = trend.cat) + s(Mean_Tavg_py, by = trend.cat) + s(Prec_winter_py, by = trend.cat) + trend.cat + s(Plot, bs = "re"), data = df_selected)


Mod_by_trend <- bam(log(ndvi.max) ~ s(Tsummer, by = trend.cat) + s(CMIsummer, by = trend.cat) + s(Mean_Tavg_gy, by = trend.cat) + s(Prec_winter_gy, by = trend.cat) + s(CMIsummer_py, by = trend.cat) + s(Mean_Tavg_py, by = trend.cat) + s(Prec_winter_py, by = trend.cat) + trend.cat + s(Plot, bs = "re"), data = df_selected)

simulateResiduals(Mod_by_trend, plot = T)

AIC(Mod, Mod_by_trend)


res <- DHARMa::simulateResiduals(Mod_by_trend)

res_agr <- DHARMa::recalculateResiduals(res, group = df_selected$Year)

DHARMa::testTemporalAutocorrelation(res_agr, time = unique(df$Year))


Mod_by_trend <- bam(ndvi.max ~ s(Tsummer, by = trend.cat) + s(CMIsummer, by = trend.cat) + s(Mean_Tavg_gy, by = trend.cat) + s(Prec_winter_gy, by = trend.cat) + s(CMIsummer_py, by = trend.cat) + s(Mean_Tavg_py, by = trend.cat) + s(Prec_winter_py, by = trend.cat) + trend.cat + s(Plot, bs = "re"), data = df_selected)

simulateResiduals(Mod_by_trend_2, plot = T)


library(gratia)


appraise(Mod_by_trend)

summary(Mod_by_trend)
draw(Mod_by_trend)

res <- DHARMa::simulateResiduals(Mod_by_trend_2)

res_agr <- DHARMa::recalculateResiduals(res, group = df_selected$Year)

DHARMa::testTemporalAutocorrelation(res_agr, time = unique(df$Year))



AIC(Mod_lmer, Mod, Mod_by_trend, Mod_by_trend_2)


df_selected$ndvi.max_adj <- df_selected$ndvi.max/max(df_selected$ndvi.max)

Mod_by_trend_beta <- bam(ndvi.max_adj ~ s(Tsummer, by = trend.cat) + s(CMIsummer, by = trend.cat) + s(Mean_Tavg_gy, by = trend.cat) + s(Prec_winter_gy, by = trend.cat) + s(CMIsummer_py, by = trend.cat) + s(Mean_Tavg_py, by = trend.cat) + s(Prec_winter_py, by = trend.cat) + trend.cat + s(Plot, bs = "re"), data = df_selected, family = "bcg")


summary(Mod_by_trend_beta)


