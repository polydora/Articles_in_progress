# Загрузка пакетов ##############
library(ggplot2)
library(readxl)
library(lubridate)
library(mgcv) 
library(gratia)
library(dplyr)
library(DHARMa)
library(readODS)
library(reshape2)
library(tidyr)

som <- read_excel("Data/all_nests_Kem_2024_2025.xlsx", na = "NA")


som %>% 
  select(Nest_ID, Date, Region, Place, N_eggs, N_chicks, N_sheets,  Ed, Cr, Cover, fluff_collection, Destroy, A1, D1, A2, D2, A3, D3, Suspended, Pipping, Pipping_External, Hatching, Chicks) %>% 
  melt(., id.vars = c("Nest_ID", "Date", "Region", "Place", "N_eggs", "N_chicks", "N_sheets",  "Ed", "Cr", "Cover", "fluff_collection", "Destroy"), variable.name = "Indicator", value.name = "Value") %>% 
  arrange(Date, Nest_ID)  -> 
  som_long


som_long <-
  som_long %>% 
  mutate(Indicator2 = case_when(Indicator %in% c("A1", "A2", "A3") ~ "A",
                                Indicator %in% c("D1", "D2", "D3") ~ "D",
                                !(Indicator %in% c("A1", "A2", "A3") & Indicator %in% c("D1", "D2", "D3"))  ~ Indicator)
         ) %>% 
  select(-Indicator) %>% 
  rename(Indicator = Indicator2)  %>% 
  filter_out(is.na(Value)) %>% 
  filter_out(Value == 0)




# Пересчет индикаторов водного теста в дни насиживания

som_long %>% 
  filter(Indicator == "A") %>%
  arrange(Value) %>% 
  pull(Value) %>% 
  unique()

som_long %>% 
  filter(Indicator == "D") %>%
  arrange(Value) %>% 
  pull(Value) %>% 
  unique()

str(som_long)

unique(som_long$Indicator)

som_long_2 <-
som_long %>%   
  mutate(Incubation_time = case_when(
    Indicator == "Suspended" ~ 11, # взвешено
    Indicator == "Pipping" ~ 24, # наклев
    Indicator == "Pipping_External" ~ 25, # проклев
    Indicator == "Chicks"  ~ 26, # птенцы
    Indicator == "A" & Value == 0 ~ 0,
    Indicator == "A" & Value ==10 ~ 0.5,
    Indicator == "A" & Value ==15 ~ 1,
    Indicator == "A" & Value ==20 ~ 2,
    Indicator == "A" & Value ==25 ~ 3,
    Indicator == "A" & Value ==30 ~ 3.5,
    Indicator == "A" & Value ==35 ~ 4,
    Indicator == "A" & Value ==40 ~ 5,
    Indicator == "A" & Value ==45 ~ 5.5,
    Indicator == "A" & Value ==50 ~ 6,
    Indicator == "A" & Value ==55 ~ 6,
    Indicator == "A" & Value ==60 ~ 6.5,
    Indicator == "A" & Value ==65 ~ 7,
    Indicator == "A" & Value ==70 ~ 7.5,
    Indicator == "A" & Value ==75 ~ 8,
    Indicator == "A" & Value ==80 ~ 8.5,
    Indicator == "A" & Value ==85 ~ 9.5,
    Indicator == "A" & Value ==90 ~ 10,
    Indicator == "D" & (Value  <= 10) ~ 11,
    Indicator == "D" & (Value > 10 & Value  <= 14) ~ 12,
    Indicator == "D" & (Value >= 15 & Value  <= 17) ~ 13,
    Indicator == "D" & (Value >= 18 & Value  <= 20) ~ 14,
    Indicator == "D" & (Value >= 21 & Value  <= 22) ~ 15,
    Indicator == "D" & (Value >= 23 & Value  <= 25) ~ 16,
    Indicator == "D" & (Value >= 26 & Value  <= 27) ~ 17,
    Indicator == "D" & (Value >= 28 & Value  <= 29) ~ 18,
    Indicator == "D" & (Value >= 30 & Value  <= 32) ~ 19,
    Indicator == "D" & (Value >= 33 & Value  <= 34) ~ 20,
    Indicator == "D" & (Value >= 35 & Value  <= 36) ~ 21,
    Indicator == "D" & (Value >= 37 & Value  <= 40) ~ 22,
    Indicator == "D" & (Value >= 41 & Value  <= 44) ~ 23
  )
  )  %>% 
filter(Incubation_time <= 40) 

table(som_long_2$Indicator)

som_long_2 %>% 
  group_by(Nest_ID, Date, Region, Place) %>% 
  summarise(N_eggs = mean(as.numeric(N_eggs)),
            N_chicks = mean(as.numeric(N_chicks)),
            Cr = mean(as.numeric(Cr)),
            Cover = unique(Cover),
            Fluff_collection = unique(fluff_collection),
            N_sheets = mean(as.numeric(N_sheets)),
            Ed = mean(as.numeric(Ed)),
            Incubation_time = round(mean(Incubation_time), 0), 
            Eggs_tested = n(),
            A = sum(Indicator == "A"),
            D = sum(Indicator == "D"),
            Suspended  = sum(Indicator == "Suspended"),
            Pipping = sum(Indicator == "Pipping"),
            Pipping_External = sum(Indicator == "Pipping_External"),
            Chicks = sum(Indicator == "Chicks")
            ) ->
nest_param


library(writexl)

write_xlsx(x = nest_param, path = "Data/all_nests_Kem.xlsx", )

