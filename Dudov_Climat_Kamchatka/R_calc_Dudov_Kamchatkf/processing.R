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

names(ind1)

df1 <- merge(ind1, ind2, by = c("sample.id", "year", "latitude", "longitude"))

df2 <- merge(df1, ind3, by = c("sample.id", "year", "latitude", "longitude"))

df3 <- merge(df2, ind4, by = c("sample.id", "year", "latitude", "longitude"))





library(vegan)

mod <- cca(df3[, -c(1:4)])

plot(mod, display = "species", type = "t")

plot(mod, display = "sites")


