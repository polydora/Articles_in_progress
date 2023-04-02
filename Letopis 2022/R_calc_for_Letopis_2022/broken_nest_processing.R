library(readxl)
library(reshape2)
library(dplyr)



nest <- read_excel("Data/Учет_разорения_2015_2022.xlsx", na = "NA")

nest <-
  nest %>%
  filter(! Region %in% "Кемь-лудский арх.")

isl <- read_excel("Data/Kandalaksha_Bay_Islands_2021.xlsx", na = "NA")

isl <- isl %>%  select(-description, -gid)

islands_area <- read_excel("Data/Kandalaksha_islands_area.xlsx")


nest <-
  nest %>%
  dplyr::mutate(Outcome = case_when(
    Status == "Broken" ~ 1,
    Status == "Alive" ~ 0,
    is.na(Status) ~ NA
  )) %>%
  filter(!is.na(Outcome))






nest2 <- merge(nest, isl, all.x = T)

nest3 <- merge(nest2, islands_area, all.x = T)

#  Точка отсчета расстояния до Кандалкши (контора Заповедника)
Shore_boundary = c(67.133129, 32.417849)

# Переводим в радианы
Shore_boundary <- Shore_boundary*pi/180


nest3$Dist_Kand <- with(nest3, acos(sin(Shore_boundary[1])*sin(Lat*pi/180) + cos(Shore_boundary[1])*cos(Lat*pi/180)*cos(Shore_boundary[2] - Long*pi/180))) * 6371


nest3$Region <- factor(nest3$Region)
nest3$Name <- factor(nest3$Name)

nest3$Year <- year(nest3$Date)




library(mgcv)


Mod <- gam(Outcome ~ s(Year, by = Region, k = 5) + Region + s(Dist_Kand) + s(Area_Ga) + s(Name, bs = "re"), method = "REML", data = nest3, family = "binomial")

gam.check(Mod)

library(gratia)
appraise(Mod)


summary(Mod)

draw(Mod)



Mod2 <- gam(Outcome ~ s(Year, by = Region, k = 5) + Region + s(Name, bs = "re"), method = "REML", data = nest3, family = "binomial")

appraise(Mod2)
gam.check(Mod2)

summary(Mod2)

draw(Mod2)



My_data <- expand.grid(Year = seq(min(nest3$Year), max(nest3$Year), 0.1), Region = unique(nest3$Region))

predicted <- predict(Mod2, newdata = My_data, exclude = "s(Name)", newdata.guaranteed=TRUE, se.fit = TRUE, type = "response")

My_data$Fit <- predicted$fit
My_data$SE <- predicted$se.fit

prop_broken <-
  nest3 %>%
  group_by(Region, Name, Year) %>%
  summarise(Prop_Broken = mean(Outcome))

Total_prop_broken <- mean(nest3$Outcome)

ggplot() +
geom_boxplot(data = prop_broken, aes(x = Year, y = Prop_Broken *100, group = Year), outlier.size = 1) +
  facet_wrap(~Region) +
  geom_line(data = My_data, aes(x = Year, y = Fit*100), size = 1, color = "blue") +
  geom_ribbon(data = My_data, aes(x = Year, ymin = (Fit - 1.96*SE)*100, ymax = (Fit + 1.96*SE)*100 ), alpha = 0.2) +
  guides(color = "none") +
  geom_abline(intercept = Total_prop_broken *100, slope = 0, color = "red", linetype = 2, size = 1) +
  labs(x = "Годы", y = "Доля разоренных гнезд (%)")


result_table <-
  nest3 %>% group_by(Region, Name, Year) %>%
  summarise(Prop_Broken = round(mean(Outcome) * 100, 1))

dcast(result_table, formula = Region + Name  ~ Year, value.var = "Prop_Broken")


