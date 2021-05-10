library(dplyr)
library(readxl)
library(ggplot2)


myt1 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Vor2, vor4, vor5 1996-2008", na = "NA")
myt2 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Vor2, vor4, vor5 2009-2016", na = "NA")
myt3 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Vor2, Vor4, Vor5 2017-2020", na = "NA")


myt4 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Korg, Mat 1997-2008", na = "NA")
myt5 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Korg, Mat 2009-2011", na = "NA")
myt6 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Korg, Mat 2012-2016", na = "NA")
myt7 <- read_excel("data/Mussel beds data base 96-20.xls", sheet = "Korg, Mat 2017-2020", na = "NA")



myt <- rbind(myt1, myt2, myt3, myt4, myt5, myt6, myt7)

table(myt$bank, myt$year, myt$status)






myt_reduced <- myt %>% filter(!(bank %in% c("vor2", "vor4", "vor5") & year %in% c(2013) & season %in% c("July", "June")) & !(bank %in% c("mat", "korg") & year %in% c(2008, 2009, 2010, 2011, 2012, 2013) & season %in% c("June", "March", "January", "July"))) %>% filter(status == "dead")


table(myt_reduced$year, myt_reduced$bank)


table(myt_reduced$season, myt_reduced$year, myt_reduced$bank)


