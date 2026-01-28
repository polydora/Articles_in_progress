library(vegan)
library(ggplot2)
library(dplyr)

dat <- read.table("Data/cn_sp.csv", sep = ",", header = TRUE)

# dat %>% 
#   filter(!complete.cases(.))

pred <- read.table("Data/factors.csv", sep = ";", header = TRUE, dec = ",")

names(pred)

pred %>% 
  select(-c(Oxygen, Turbidity,  ChlA)) ->
  pred_reduced


pred_reduced %>% 
  filter(complete.cases(.)) ->
  pred_reduced


dat %>% 
  filter(Station %in% pred_reduced$Station ) ->
  dat_reduced

dat_reduced %>% 
  select(-1) %>% 
  rowSums() -> 
  rSums


dat_reduced %>% 
  filter(rSums >0) ->
  dat_reduced

nrow(dat_reduced)
nrow(pred_reduced)

pred_reduced %>% 
  filter(Station %in% dat_reduced$Station) ->
  pred_reduced

nrow(dat_reduced)
nrow(pred_reduced)

Z <- 
  pred_reduced %>% 
  select(Latitude)

Y <- 
  pred_reduced %>% 
  select(-Station, -Latitude, -Longitude)

X <- decostand(dat_reduced[, -1], method = "pa")

# X <- dat_reduced[, -1]


# mod_cca <- cca(X = X,  Y = Y, Z = Z )


mod_cca <- capscale(X ~ Latitude + Longitude + Temperature + pH + Conductivity + Veg_cover + Dist_to_city + Dist_to_coast, data = pred_reduced[-1], add = TRUE)




vif.cca(mod_cca)

anova(mod_cca, permutations = 9999)

anova(mod_cca,by = "axis", permutations = 9999)

anova(mod_cca,by = "margin", permutations = 9999)

anova(mod_cca,by = "terms", permutations = 9999)


plot(mod_cca, )


scores(mod_cca)$species %>% 
  as.data.frame() %>% 
  arrange(CAP1)


scores(mod_cca)$sites

scores(mod_cca)$biplot     

str(scores(mod_cca))


mod_0 <- capscale(X ~ 1, data = pred_reduced[-1], add = TRUE)

mod <- ordistep(mod_0, scope = formula(mod_cca))

anova(mod)

anova(mod,by = "axis", permutations = 9999)

anova(mod,by = "margin", permutations = 9999)

anova(mod,by = "terms", permutations = 9999)


plot(mod)

pred_reduced$Waterbody

library(reshape2)

melt(dat_reduced, id.vars = "Station") %>% 
  group_by(Station) %>% 
  summarise(N_sp = sum(value != 0)) ->
  df_sp_rich

pred_reduced %>% 
  select(Station, Waterbody) %>% 
  merge(., df_sp_rich) ->
  df_sp_rich_waterbody

df_sp_rich_waterbody %>% 
  ggplot(aes(x = factor(Waterbody), y = N_sp)) +
  geom_violin(fill = "cyan") +
  geom_boxplot(width = 0.1, fill = "gray" ) +
  theme_bw()

 
diversity(dat_reduced[, -1], index = c("shannon"))
