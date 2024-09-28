
library(readxl)

myt <- read_excel("Data/Mussel beds 96-12 species abundance proofed 19.05.13.xls", sheet = "All_data_transposed")

myt[,-c(1, 2)] <- myt[,-c(1, 2)] * 182

myt %>% 
  filter(Value == "N") %>% 
  pivot_longer(cols = starts_with(c("Vor", "Korg", "Mat")), 
               names_to = "Sample",
               values_to = "N") %>%
  as.data.frame() %>% 
  group_by(Species) %>% 
  dplyr::summarise(Mean = mean(N), SE = sd(N)/length(N)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "N") ->
  Mean_N


myt %>% 
  filter(Value == "B") %>% 
  pivot_longer(cols = starts_with(c("Vor", "Korg", "Mat")), 
               names_to = "Sample",
               values_to = "B") %>%
  as.data.frame() %>% 
  group_by(Species) %>% 
  dplyr::summarise(Mean = mean(B), SE = sd(B)/length(B)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "B") ->
  Mean_B


rbind(Mean_N, Mean_B) %>%
  as.data.frame() %>% 
  pivot_wider(names_from = Value, values_from = c("Mean", "SE", "Order")) %>% 
  filter(Order_N <= 10 | Order_B <= 10) %>% 
  dplyr::select(Species, Mean_N, SE_N, Mean_B, SE_B ) %>% 
  write.table("clipboard", row.names = F, sep = "\t", dec = ",")
