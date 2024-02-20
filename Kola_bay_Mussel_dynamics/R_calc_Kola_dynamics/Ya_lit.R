library(readxl)
library(dplyr)
library(reshape2)

Ya <- read_excel("Data/Ya_lit_initial_data.xlsx")

str(Ya)

unique(Ya$Frame_area)

# делаем матрицу с количеством изученных мидий
Ya_count <- dcast(Year + Sample + Part_studied + Frame_area + Include ~ Age, data = Ya)



# делаем матрицу численностей на квадратный метр
Ya_count[ , 6 : ncol(Ya_count)] <- Ya_count[ , 6 : ncol(Ya_count)] / Ya_count$Part_studied / Ya_count$Frame_area

# убираем мидий из 2002 года
Ya_count <- Ya_count %>% 
  filter(Include == 1)

Ya_count[Ya_count$Year == 2002 & Ya_count$Sample %in% c(2, 4), 6 : 8] <- NA

Ya_count[Ya_count$Year == 2002 & Ya_count$Sample %in% c(3, 5), 10 : 12] <- NA


Ya_count <- 
Ya_count %>% 
  group_by(Year, Sample) %>% 
  summarise_at(vars(- c(Part_studied,  Frame_area, Include)),.funs = sum)



# возвращаемся к "длинному" построчному формату
Ya_N <- melt(Ya_count, id.vars = c("Year", "Sample"), variable.name = "Age", value.name = "N") 

str(Ya_N)



# добавляем столбик с генерацией
Ya_N <- 
  Ya_N %>% 
  mutate(Generation = Year - as.numeric(Age))

# считаем среднюю численность каждогенерации для каждого года

Ya_gen <- 
  Ya_N %>% 
  group_by(Year, Generation, Age) %>% 
  summarise(mean_N = mean(N, na.rm = TRUE))
  # write.table("clipboard", sep = "\t")

  
# round(dcast(Generation ~ Year, data = Ya_gen))
  


Ya_gen %>% 
  filter(mean_N > 0) %>%
  filter(Generation >= 1995 & Generation <= 2014) %>% 
  mutate(Age = as.numeric(Age)) -> df


# 
# df %>% 
#   filter(Generation == 2014)
# 


df_sample_size <- 
  df %>%  
  group_by(Generation) %>% 
  summarise(N = n())


  df %>% 
  merge(., df_sample_size) %>% 
  group_by(Generation) %>% 
  filter(Age>1) %>%
  filter(N > 3) %>% 
  group_modify( ~ tidy(lm(log(mean_N + 1) ~ Age, data = .))) -> dd
  
  
  %>% 
  filter(term == "(Intercept)") %>%
  select(Generation, estimate) %>% 
    ggplot(aes(x = Generation, y = exp(estimate) )) +
    geom_line() 
  
  
  

  df %>%
    filter(Age>3) %>% 
    group_by(Generation) %>% 
    group_modify( ~ tidy(lm(log(mean_N) ~ Age, data = .))) %>% 
    # filter(term == "(Intercept)") %>% 
    select(Generation, estimate, term) %>% 
    dcast(., Generation ~ term, value.var = "estimate" ) %>% 
    rename(Intercept = '(Intercept)') %>% 
    ggplot(aes(y = Intercept, x = Age)) +
    geom_point()
  

                
fit_no_1 <- 
  df %>%
  filter(Age>1) %>% 
  group_by(Generation) %>% 
  group_modify( ~ tidy(lm(log(mean_N) ~ Age, data = .))) %>% 
  filter(term == "(Intercept)") %>% 
  select(Generation, estimate)

fit_no_1.2 <- 
  df %>%
  filter(Age>2) %>% 
  group_by(Generation) %>% 
  group_modify( ~ tidy(lm((logmean_N) ~ Age, data = .))) %>% 
  filter(term == "(Intercept)") %>% 
  select(Generation, estimate)


fit_no_1.2.3 <- 
  df %>%
  filter(Age>3) %>% 
  group_by(Generation) %>% 
  group_modify( ~ tidy(lm(log(mean_N) ~ Age, data = .))) %>% 
  filter(term == "(Intercept)") %>% 
  select(Generation, estimate)


dif <-
  merge(fit_all_ages, fit_no_1, by = "Generation") %>% 
  merge(., fit_no_1.2, by = "Generation") %>% 
  merge(., fit_no_1.2, by = "Generation")

