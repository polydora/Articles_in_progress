library(vegan)
library(readxl)
library(dplyr)



myt <- read_excel("Data/summary table_Magadan_itog.xlsx", sheet = "data")






dat<-
myt %>% 
  group_by(Site_code, Year) %>% 
  dplyr::select(Site_code, N, DN_FC, BTN1, BTN2.1, BTN2.2, BTN2.SAM) %>% 
  mutate(Helth = N - BTN1 - BTN2.1 - BTN2.2 - BTN2.SAM) %>% 
  summarise_all(.funs = sum) 




# %>% 
#   dplyr::select(- BTN2.SAM)


dat_rel <- dat[, -c(1:3)]/dat$N 

dat_rel$Site <- dat$Site_code
dat_rel$Year <- dat$Year


ggplot(dat_rel, aes(BTN1, (BTN2.2 + BTN2.1) )) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)






ggplot(dat_rel, aes(BTN1, BTN2.1)) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)

ggplot(dat_rel, aes(BTN2.2, BTN2.1)) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)

ggplot(dat_rel, aes(BTN1, (BTN2.1 + BTN2.2) )) +
  geom_text(aes(label = Site)) + 
  geom_abline() +
  facet_wrap(~Year)


mod_cca <- cca(dat_rel) 

plot(mod_cca, display = "sp")
plot(mod_cca, display = "site")


summary(mod_cca)

scores(mod_cca)

dat_rel$PC1 <- scores(mod_cca)$sites[,1]
dat_rel$PC2 <- scores(mod_cca)$sites[,2]

dat_rel$Site <- dat$Site_code
dat_rel$Year <- dat$Year

library(ggplot2)
ggplot(dat_rel, aes(x = PC2, y = BTN1)) +
  geom_point()

ggplot(dat_rel, aes(x = PC2, y = BTN2.1)) +
  geom_point()

ggplot(dat_rel, aes(x = PC2, y = BTN2.2)) +
  geom_point()



ggplot(dat_rel, aes(x = PC1, y = BTN1)) +
  geom_point()


ggplot(dat_rel, aes(x = PC1, y = BTN2.1)) +
  geom_point()

ggplot(dat_rel, aes(x = PC1, y = BTN2.2)) +
  geom_point()


ggplot(dat_rel, aes(PC1, PC2)) +
  geom_text(aes(label = Site)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)




dat<-
  myt %>% 
  # filter(Year == 2023) %>% 
  dplyr::select(Site_code, N, DN_FC, BTN1, BTN2.1, BTN2.2, BTN2.SAM) %>% 
  mutate(Helth = N - BTN1 - BTN2.1 - BTN2.2 - BTN2.SAM) 


dat_rel <- dat[, -c(1:2)]/dat$N 

mod_cca <- cca(dat_rel) 

plot(mod_cca, display = "sp")
plot(mod_cca, display = "site")


dat_rel$PC1 <- scores(mod_cca)$sites[,1]
dat_rel$PC2 <- scores(mod_cca)$sites[,2]

dat_rel$Site <- dat$Site_code
dat_rel$Year <- myt$Year


ggplot(dat_rel, aes(PC1, PC2)) +
  geom_text(aes(label = Site)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)


ggplot(dat_rel, aes(Site, PC2)) +
  geom_boxplot()+
  geom_hline(yintercept = 0)
  
Pl_BTN2.2 <- 
ggplot(dat_rel, aes(Site, BTN2.2, fill = factor(Year) )) +
  geom_boxplot()+
  geom_hline(yintercept = 0)

Pl_BTN2.1 <- 
ggplot(dat_rel, aes(Site, BTN2.1, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)

Pl_BTN1 <- 
ggplot(dat_rel, aes(Site, BTN1, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)


library(patchwork)

Pl_BTN2.2/Pl_BTN1

ggplot(dat_rel, aes(Site, BTN2.2, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)


ggplot(dat_rel, aes(Site, BTN1, fill = factor(Year))) +
  geom_boxplot()+
  geom_hline(yintercept = 0)
