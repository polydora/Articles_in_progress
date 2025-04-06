library(readxl)
library(dplyr)


ind1 <-
  read_excel("Data/picea_phenological_data.xlsx", sheet = "picea.gsdt.evi2") %>% select(-n.obs)

ind2 <-
  read_excel("Data/picea_phenological_data.xlsx", sheet = "picea.gsdt.kndvi")%>% select(-n.obs)
ind3 <-
  read_excel("Data/picea_phenological_data.xlsx", sheet = "picea.gsdt.ndvi")%>% select(-n.obs)

ind4 <-
  read_excel("Data/picea_phenological_data.xlsx", sheet = "picea.gsdt.nirv") %>% select(-n.obs)


ind5 <-
  read_excel("Data/picea_phenological_data.xlsx", sheet = "picea.gsdt.evi") %>% select(-n.obs)



df1 <- merge(ind1, ind2, by = c("sample.id", "year", "latitude", "longitude"))

df2 <- merge(df1, ind3, by = c("sample.id", "year", "latitude", "longitude"))

df3 <- merge(df2, ind4, by = c("sample.id", "year", "latitude", "longitude"))

df4 <- merge(df3, ind5, by = c("sample.id", "year", "latitude", "longitude"))



df4 <-
  df4 %>%
  filter(year < 2024)

df4 <-
  df4 %>%
  rename(Year = year)


clim <- read_excel("Data/Kamch_clim_upd.xlsx")

library(reshape2)

clim %>% 
  select(-Tmin, -Tmax) %>% 
  melt(id.vars = c("Year", "Month")) %>% 
  dcast(Year ~ Month + variable, value.var = "value") -> clim_month



clim_mean <-
  clim %>%
  group_by(Year) %>%
  summarise(Mean_Tavg = mean(Tavg), Mean_Tmin = mean(Tmin), Mean_Tmax = mean(Tmax), Mean_Prec = mean(Prec), Mean_PET = mean(PET), Mean_CMI = mean(CMI), Mean_spei = mean(spei) )


# df4 %>%
#   select(sample.id, Year) %>%
#   merge(., clim_month) -> clim_month 

df5 <-
merge(df4, clim_mean)

names(df5)

bio <-
  df5 %>%
  select(c(5:39))


bio2 <-
  bio %>%
  select(evi2.gs.med,evi2.max.doy, kndvi.gs.med,kndvi.max.doy, ndvi.gs.med, ndvi.max.doy, nirv.gs.med, ndvi.max.doy, nirv.gs.med, nirv.max.doy )


env <-
  df5 %>%
  select(c(1:4), c(40:ncol(df5)))

# library(parallel)
library(vegan)

# bio_env_res <- bioenv(com = bio2, env = env[, -c(1:4)])

mod <- cca(bio2 ~ Mean_Tavg + Mean_Prec + Mean_PET, data = env)

vif.cca(mod)




plot(mod, display = c("species", "cn"), type = "t", scaling = "symmetric")

anova.cca(mod)
anova(mod, by = "axis")

anova(mod, by = "mar")

library(ggplot2)

ggplot(df5, aes(x = Mean_PET, y = evi2.max.upr)) +
  geom_point() +
  geom_smooth(method = "lm")
  
ggplot(df5, aes(x = Mean_Prec, y = evi2.max.upr)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df5, aes(x = Mean_Prec, y = evi.gs.avg)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df5, aes(x = Mean_Prec, y = ndvi.max.doy)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df5, aes(x = Mean_Prec, y = nirv.max.lwr)) +
  geom_point() +
  geom_smooth(method = "lm")


names(bio2)


ggplot(df5, aes(x = Mean_Prec, y = nirv.gs.med)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df5, aes(x = Mean_Prec, y = nirv.max.doy)) +
  geom_point() +
  geom_smooth(method = "lm")

ggplot(df5, aes(x = Mean_Tavg, y = nirv.max.doy)) +
  geom_point() +
  geom_smooth(method = "lm")


plot(mod, display = c("species", "cn"), type = "t", scaling = "symmetric")

spec_inertia <- inertcomp(mod, display = c("species"), 
                          proportional = FALSE )
spec_inertia <- as.data.frame(spec_inertia)

ggplot(spec_inertia , aes(x = CCA, y = CA)) + 
  geom_text(aes(label = rownames(spec_inertia))) + geom_abline(slope = 1)

