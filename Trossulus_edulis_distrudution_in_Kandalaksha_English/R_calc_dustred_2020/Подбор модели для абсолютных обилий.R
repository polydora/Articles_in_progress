
myt_full_reduced %>% 
  mutate(Log_D_T = log(N_T * SQM_factor + 1), Log_D_E = log(N_E*SQM_factor +1)) %>% 
  select(Site, Salinity, Position, Log_Min_dist_river, Log_Fetch, Log_Min_dist_port, River_Size, Port_Status, Log_D_T, Log_D_E) %>% 
  rename(T_morphotype = Log_D_T, E_morphotype = Log_D_E) %>% 
  melt(id.vars = c("Site","Position",  "Salinity", "Log_Min_dist_river", "Log_Fetch", "Log_Min_dist_port", "River_Size", "Port_Status"), variable.name = "Morphotype", value.name = "Abundance") -> myt_full_reduced_long
  




mod_all_abundances <- 
  gam(Abundance ~ s(Salinity, bs = "cr", by = Morphotype) + s(Log_Min_dist_river, bs = "cr", by = Morphotype) + s(Log_Fetch, bs = "cr", by = Morphotype) + s(Log_Min_dist_port, bs = "cr", by = Morphotype) +  River_Size*Morphotype + Port_Status*Morphotype + Position*Morphotype + s(Site, bs = "re"), method = "REML", family = "gaussian", data = myt_full_reduced_long )

summary(mod_all_abundances)

draw(mod_all_abundances)  

appraise(mod_all_abundances)

library(DHARMa)
simulateResiduals(mod_all_abundances, plot = T)

simulateResiduals(Mod_gam_Abundance, plot = T)

# summary(Mod_gam_Abundance)

# draw(Mod_gam_Abundance, grouped_by = F)



