il <- read.csv("Data/Ilistaya_inlet_data_1987_2019.csv")



  
  il %>% 
  filter(Type == "N") %>% 
  group_by(Taxa) %>% 
  dplyr::summarise(Mean = mean(Value), SE = sd(Value)/length(Value)) %>% 
  arrange(desc(Mean)) %>% 
  mutate(Order = 1:nrow(.), Value = "N") ->
  Mean_N
  
  il %>% 
    filter(Type == "B") %>% 
    group_by(Taxa) %>% 
    dplyr::summarise(Mean = mean(Value), SE = sd(Value)/length(Value)) %>% 
    arrange(desc(Mean)) %>% 
    mutate(Order = 1:nrow(.), Value = "B") ->
    Mean_B
  
  il %>% 
    filter(Type == "B") %>% 
    group_by(Year, Station) %>% 
    summarise(Sum_B = sum(Value)) %>%
    ungroup() %>% 
    summarise(Mean = mean(Sum_B), SE = sd(Sum_B)/sqrt(length(Sum_B)))
  
  
  rbind(Mean_N, Mean_B) %>%
    as.data.frame() %>% 
    pivot_wider(names_from = Value, values_from = c("Mean", "SE", "Order")) %>% 
    filter(Order_N <= 20 | Order_B <= 20) %>% 
    dplyr::select(Taxa, Mean_N, SE_N, Mean_B, SE_B ) %>% 
    write.table("clipboard", row.names = F, sep = "\t", dec = ",")
  