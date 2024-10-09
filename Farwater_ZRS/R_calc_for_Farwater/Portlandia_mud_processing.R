
zin_portland <- 
  zin %>% 
  dplyr::select(valid_name, Type, ZIN_2005_Bank7, ZIN_2006_Bank7, ZIN_2007_Bank7) %>%
  melt(id.vars = c("valid_name", "Type"))



zin_portland %>% 
  filter(Type == "N") %>% 
  group_by(valid_name) %>% 
  dplyr::summarise(Mean = mean(value), SE = sd(value)/length(value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "N") ->
  Mean_N

zin_portland %>% 
  filter(Type == "B") %>% 
  group_by(valid_name) %>% 
  dplyr::summarise(Mean = mean(value), SE = sd(value)/length(value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "B") ->
  Mean_B

zin_portland %>% 
  filter(Type == "B") %>% 
  group_by(variable) %>% 
  summarise(Sum_B = sum(value)) %>% 
  summarise(Mean = mean(Sum_B), SE = sd(Sum_B)/sqrt(length(Sum_B)) )




library(tidyr)

rbind(Mean_N, Mean_B) %>%
  as.data.frame() %>% 
  pivot_wider(names_from = Value, values_from = c("Mean", "SE", "Order")) %>% 
  filter(Order_N <= 20 | Order_B <= 20) %>% 
  dplyr::select(valid_name, Mean_N, SE_N, Mean_B, SE_B ) %>% 
  write.table("clipboard", row.names = F, sep = "\t", dec = ",")
