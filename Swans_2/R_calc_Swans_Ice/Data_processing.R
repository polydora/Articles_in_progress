library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)
library(DHARMa)
library(gratia)
library(reshape2)
library(tidyr)
library(lubridate)


swan_ice_end <- read_excel("Data/swan_data_2.xlsx", sheet = "Ice_end_date")

swan_ice_end %>%
  melt(id.vars = c("site_id", "Area", "Coord" )) %>%
  separate_wider_delim(Coord, delim = ",", names = c("Lat", "Lon")) %>%
  mutate(across(c(Lat, Lon), as.numeric)) %>%
  mutate(value_updated = update(value, year = as.numeric(as.character(variable)))) %>%
  select(-value, - variable) %>%
  rename(Date = value_updated) %>%
  mutate(Year = year(Date))->
  ice_end_df



swan_ice_free <- read_excel("Data/swan_data_2.xlsx", sheet = "No_ice_lonivity")

swan_ice_free %>%
  melt(id.vars = c("site_id", "Area", "Coord" )) %>%
  separate_wider_delim(Coord, delim = ",", names = c("Lat", "Lon")) %>%
  mutate(across(c(Lat, Lon), as.numeric)) %>%
  rename(Year = variable, Longivity = value) ->
  ice_free_df



swan_nest <- read_excel("Data/swan_data_2.xlsx", sheet = "nests")

swan_nest %>%
  separate_wider_delim(Coord, delim = ",", names = c("Lat", "Lon")) %>%
  mutate(across(c(Lat, Lon), as.numeric))  ->
  swan_nest


merge(ice_free_df, ice_end_df) %>%
  merge(swan_nest) ->
  all_data


# library(writexl)
#
# write_xlsx(x = all_data, path = "Data/all.data.xlsx")


all_data <-
  all_data %>%
  mutate(DOY_ice = yday(Date))

all_data %>%
  ggplot(aes(x = Longivity)) +
  geom_histogram() +
  geom_vline(xintercept = 365, color = "blue") +
  labs(x = "Продолжительность безледного периода (дни)")


all_data %>%
  ggplot(aes(x = DOY_ice)) +
  geom_histogram() +
  labs(x = "День года, когда сошел лед")



all_data %>%
  ggplot(aes(x = DOY_ice, y = nests)) +
  # geom_point() +
  geom_smooth(se = F, method = "gam")

all_data %>%
  ggplot(aes(x = Longivity, y = nests)) +
  # geom_point() +
  geom_smooth(se = F, method = "gam")


all_data %>%
  ggplot(aes(x = DOY_ice, y = Longivity)) +
  geom_point()



