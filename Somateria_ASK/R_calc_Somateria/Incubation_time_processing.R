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

files <- list.files(path = "Data/Initial_data/")

som <- NULL

for(name in files){
  df <- read_excel(paste("Data/Initial_data/", name, sep = ""))
  som <- rbind(som, df)
}

# names(som)

som %>% 
  select(ID, Date, Region, Place, N_eggs, Ed, A1, D1, A2, D2, A3, D3, incubation_period) %>% 
  melt(., id.vars = c("ID", "Date", "Region", "Place", "N_eggs", "Ed", "incubation_period"), variable.name = "Indicator", value.name = "Value") %>% 
  arrange(Date, ID)  -> 
  som_long


table(som_long$Value)

# Коррекция кривых значений

som_long <-
  som_long %>% 
  mutate(Value = recode(Value,
                                "52 на боку" = "52",
                                "повреждено" = NA_character_,
                                "надбито" = NA_character_
  )) %>% 
  mutate(N_eggs = recode(N_eggs, 
                         "?" = NA_character_,
                         "3+1 мертвое в 30 см от гнезда" = NA_character_,
                         "4 (1 мертвое)" = NA_character_,
                         "5 (первое треснуто)" = NA_character_,
                         "4+1 разбитое с мертвым птенцом" = NA_character_,
                         "6+4 яйца чернети" = NA_character_,
                         "8 (4+4 разного цвета)" = NA_character_
         )) %>% 
  filter(complete.cases(.))

unique(som_long$N_eggs)

unique(som_long$Ed)



som_long <-
  som_long %>% 
  mutate(Value = recode(Value,
                        "взв" = "-1",
                        "накл" = "-2",
                        "прокл" = "-3",
                        "пт" = "-4"
  )) 


  

som_long$Value <- as.numeric(som_long$Value)

table(som_long$Indicator)

som_long$Indicator <- gsub("[123]", "", som_long$Indicator)

table(som_long$Indicator)


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

som_long_2 <-
som_long %>%   
  mutate(Incubation_time = case_when(
    Value == -1 ~ 11, # взвешено
    Value == -2 ~ 24, # наклев
    Value == -3 ~ 25, # проклев
    Value == -4 ~ 26, # птенцы
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
  group_by(ID, Date, Region, Place) %>% 
  summarise(N_eggs = mean(as.numeric(N_eggs)),
            Ed = mean(as.numeric(Ed)),
            incubation_period = mean(incubation_period),
            SD_incub_time = sd(Incubation_time),
            Incubation_time = mean(Incubation_time), 
            Eggs_tested = n(),
            A = sum(Indicator == "A"),
            D = sum(Indicator == "D")
            ) ->
  nest_param



##### Переводим в широкий формат ###############
temp_df <-
  som_long_2 %>%
  group_by(ID, Date, Region, Place) %>%
  mutate(
    # Нумеруем A и D значения внутри групп
    A_seq = cumsum(Indicator == "A"),
    D_seq = cumsum(Indicator == "D")
  ) %>%
  ungroup() %>%
  # Создаем колонки для широкого формата
  mutate(
    col_name = case_when(
      Indicator == "A" ~ paste0("A", A_seq),
      Indicator == "D" ~ paste0("D", D_seq),
      TRUE ~ NA_character_
    )
  ) %>%
  # Удаляем строки без A или D
  filter(!is.na(col_name))

# Этап 2: Преобразуем в широкий формат
wide_df <- temp_df %>%
  select(ID, Date, Region, Place, col_name, Value) %>%
  pivot_wider(
    id_cols = c(ID, Date, Region, Place),
    names_from = col_name,
    values_from = Value,
    values_fill = NA_real_
  )

# Этап 3: Считаем КОЛИЧЕСТВО специальных значений (вместо флагов)
flags_df <- temp_df %>%
  group_by(ID, Date, Region, Place) %>%
  summarise(
    Suspended = sum(Value == -1, na.rm = TRUE),  # Количество вместо "Yes"
    Pipping = sum(Value == -2, na.rm = TRUE),    # Количество вместо "Yes"
    Hatching = sum(Value == -3, na.rm = TRUE),   # Количество вместо "Yes"
    Chicks = sum(Value == -4, na.rm = TRUE),     # Количество вместо "Yes"
    .groups = 'drop'
  )

# Этап 4: Объединяем все с исходными агрегациями
nest_param <- som_long_2 %>%
  group_by(ID, Date, Region, Place) %>%
  summarise(
    N_eggs = mean(as.numeric(N_eggs), na.rm = TRUE),
    Ed = mean(as.numeric(Ed), na.rm = TRUE),
    incubation_period = mean(incubation_period, na.rm = TRUE),
    SD_incub_time = sd(Incubation_time, na.rm = TRUE),
    Inc_time_mean = round(mean(Incubation_time, na.rm = TRUE), 0),
    Min_Inc_Time = min(Incubation_time),
    Max_Inc_Time = max(Incubation_time),
    Diff = (max(Incubation_time) - min(Incubation_time))/Inc_time_mean,
    N_fresh = sum(Indicator == "A" & (Value <=30 & Value >= 0)),
    Eggs_tested = n(),
    Number_A = sum(Indicator == "A", na.rm = TRUE),
    Number_D = sum(Indicator == "D", na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Объединяем с широким форматом
  left_join(wide_df, by = c("ID", "Date", "Region", "Place")) %>%
  # Объединяем с КОЛИЧЕСТВАМИ специальных значений
  left_join(flags_df, by = c("ID", "Date", "Region", "Place")) %>%
  # Убеждаемся, что есть все колонки A1-A3, D1-D3
  mutate(
    across(A1:D3, ~ifelse(. %in% c(-1, -2, -3, -4), NA_real_, .))
  ) %>%
  # Упорядочиваем колонки
  select(
    ID, Date, Region, Place,
    N_eggs, Ed, SD_incub_time, Min_Inc_Time, Max_Inc_Time, Diff, Inc_time_mean,
    Eggs_tested, Number_A, Number_D,
    A1, A2, A3, D1, D2, D3,
    Suspended, Pipping, Hatching, Chicks,
    everything()
  )


nest_param <- 
  nest_param %>% 
  mutate(Exclude = ifelse(Diff >1 & !is.na(Diff), "Exclude", ""))

nest_param$Exclude

library(writexl)

write_xlsx(x = nest_param, path = "Data/all_nests.xlsx")


######### Ищем гнезда со смешанными кладками #############




nest_param %>% 
  ggplot(aes(x = Diff, y = SD_incub_time)) +
  geom_point()

nest_param %>% 
  ggplot(aes(x = Incubation_time_mean, y = Diff)) +
  geom_point() +
  geom_smooth()


nest_param %>%
  ggplot(aes(x = Inc_time_mean, y = N_fresh)) +
  geom_point() +
  geom_smooth()


nest_param %>%
  ggplot(aes(x = Max_Inc_Time, y = SD_incub_time)) +
  geom_point() +
  geom_smooth()


nest_param %>%
  ggplot(aes(x = Diff, y = SD_incub_time)) +
  geom_point() 

nest_param %>%
  ggplot(aes(x = N_eggs, y = Diff)) +
  geom_point(position = position_jitter(width = 0.1)) 

nest_param %>%
  ggplot(aes(x = N_eggs)) +
  geom_histogram(binwidth = 1) + 
  facet_wrap(~Exclude, scales = "free_y") +
  geom_point(data = nest_param %>% filter(Diff > 1.5), aes(x = N_eggs, y = 0), color ="yellow", position = position_jitter(height = 10))
