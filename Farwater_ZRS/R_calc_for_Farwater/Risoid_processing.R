# Обработка данных по ризоидам ламинарии

library(readxl)

library(dplyr)

lam <- read_excel("Data/Laminaria risoids 2001.xlsx", sheet = "Species_abundance")

riz_param <- read_excel("Data/Laminaria risoids 2001.xlsx", sheet = "Rizoid_parameters")

riz_area <-
riz_param %>% 
  select(L, B, H) %>% 
  mutate(Area = (L/100)*(B/100)) %>% 
  pull(Area)



spec <- 
lam %>% 
  filter(Value == "N") %>% 
  select(Species)


lam <- 
merge(sp_df, lam)

lam2 <-
lam %>% 
  select(-c(Species, Accepted_name, Value))

lam_sqm <- lam2/riz_area

lam_sqm <-
lam %>% 
  select(Species, Accepted_name,Value) %>% 
  cbind(., lam_sqm)

library(reshape2)
library(matrixStats)

# Mean_N <-
lam_sqm %>% 
  select(-Species) %>% 
  filter(Value == "N") %>% 
  pivot_longer(cols = starts_with("Riz"), 
               names_to = "Sample",
               values_to = "N") %>%
  as.data.frame() %>% 
  group_by(Accepted_name) %>% 
  dplyr::summarise(Mean = mean(N), SE = sd(N)/length(N)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "N") ->
  Mean_N


lam_sqm %>% 
  select(-Species) %>% 
  filter(Value == "B") %>% 
  pivot_longer(cols = starts_with("Riz"), 
               names_to = "Sample",
               values_to = "B") %>%
  as.data.frame() %>% 
  group_by(Accepted_name) %>% 
  dplyr::summarise(Mean = mean(B, na.rm = T), SE = sd(B, na.rm = T)/length(B)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "B") ->
  Mean_B


  


rbind(Mean_N, Mean_B) %>% 
  pivot_wider(names_from = Value, values_from = c("Mean", "SE", "Order")) %>% 
  filter(Order_N <= 10 | Order_B <= 10) %>% 
  select(Accepted_name, Mean_N, SE_N, Mean_B, SE_B ) %>% 
  write.table("clipboard", row.names = F, sep = "\t", dec = ",")
