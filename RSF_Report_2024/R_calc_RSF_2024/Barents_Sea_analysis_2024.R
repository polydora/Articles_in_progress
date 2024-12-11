library(readxl)
library(dplyr)
library(tidyr)
library(mgcv)
library(gratia)
library(magrittr)
library(DHARMa)
library(ggplot2)

# Читаем данные
myt <- read_excel("Data/ALLO_BarentsSea_inter.xlsx", na = "NA")
str(myt)



myt$an_of_gen <- factor(myt$an_of_gen)

myt$an_of_hybr <- factor(myt$an_of_hybr)

myt$pop <- factor(myt$pop)

myt$period <- factor(myt$period)

myt$region <- factor(myt$region)

myt <-
myt %>%
  mutate(Gen_Year = year - age,
         Sp = case_when(str > 0.5 ~ 1,
                        str <= 0.5 ~ 0))
myt_2 <-
  myt %>%
  filter(an_of_gen == "yes") %>%
  filter(! site %in% c("Pala", "Kislaya", "Noviy most ruchey", "BS_+2", "R_+0.5")) %>%
  filter(horizons == "littoral")

nrow(myt_2)

initial_data <-
  myt_2 %>%
  group_by(region, period, Gen_Year, pop) %>%
  summarise(Prop_T = mean(Sp), N = n(), Pop_str = mean(str)) %>%
  filter(N > 2)

myt_3 <- merge(myt_2, initial_data, by = c("region", "period", "Gen_Year", "pop"))

############################################
# Динамика гибридной зоны на уровне генераций в Кольском заливе и, отдельно, в губе Тюва — обязательный, делает ВМ

# Mod_dynam_period_Sp <- gam(Sp ~ s(Gen_Year, by = interaction(period, region), bs = "cr", k = 4) + region + period  + s(pop, bs = "re"), family = binomial(link = "logit"), method = "REML", data = myt_3)

# Модель показывающая связь str периодом для разных регионов

Mod_dynam_period_str <- gam(str ~ s(Gen_Year, by = interaction(period, region)) + region + period + s(pop, bs = "re"), family = betar(link = "cloglog"), method = "REML", data = myt_3)

# Проверка валидности модели

gam.check(Mod_dynam_period_str)

appraise(Mod_dynam_period_str)

res <- simulateResiduals(Mod_dynam_period_str)
plot(res)

# Итог
summary(Mod_dynam_period_str)

# Визуализация
# draw(Mod_dynam_period_Sp)

My_data <-
  myt_3 %>%
  group_by(region, period) %>%
  do(data.frame(Gen_Year = seq(from = min(.$Gen_Year, na.rm = T), to = max(.$Gen_Year, na.rm = T), length.out = 10)))


My_data$Predicted <- predict(Mod_dynam_period_str, newdata = My_data,  exclude = "s(pop)", newdata.guaranteed=TRUE, se.fit = F, type = "response")

ggplot(My_data, aes(x = Gen_Year, y = Predicted, color = period)) +
  geom_line(size = 2) +
  facet_wrap(~region) +
  geom_point(data = initial_data, aes(y = Pop_str)) +
  theme_bw() +
  geom_hline(yintercept = 0.5)


################################################################
# Анализ гибридизации в Кольском заливе (по свежим сборам)


  # myt %>%
  # filter(an_of_hybr != "no") %>%
  # ggplot(aes(x = str, y = 1:nrow(.))) +
  # geom_point() +
  # facet_wrap(~an_of_hybr)
  # +
  # geom_text(aes(label = pop))





myt_hybr <-
myt %>%
  filter(an_of_hybr != "no") %>%
  group_by(pop) %>%
  summarise(lat = mean(N), lon = mean(E), an_of_hybr = unique(an_of_hybr), Ptros = mean(str), P_hybr = mean(str > 0.1 & str < 0.9), md = 1 - (var(str))/(Ptros*(1-Ptros)), md_1 = 1-md)


# Первичка
myt_hybr %>%
  ggplot(aes(x = Ptros, y = md_1, color = an_of_hybr)) +
  geom_point(size = 3)

myt_hybr %>%
  ggplot(aes(x = Ptros, y = P_hybr, color = an_of_hybr)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm")


#
# myt_hybr %>%
#   ggplot(aes(x = md_1, y = P_hybr, color = an_of_hybr)) +
#   geom_point(size = 3) +
#   facet_wrap(~an_of_hybr) +
#   geom_text(aes(label = pop))


# Меняю последовательность уровней

myt_hybr$an_of_hybr <- factor(myt_hybr$an_of_hybr)

myt_hybr$an_of_hybr <- factor(myt_hybr$an_of_hybr, levels = c("top", "cline", "outer_part"))


# Модель для доли гибридов

Mod_hybr_P_hybr <- gam(P_hybr ~ s(Ptros, by = an_of_hybr, bs = "cr") + an_of_hybr,  family = "betar", data = myt_hybr)

# Проверка валидности
res <- simulateResiduals(Mod_hybr_P_hybr)
plot(res)

appraise(Mod_hybr_P_hybr)


# Итог
summary(Mod_hybr_P_hybr)

# Визуализация средствами gratia
draw(Mod_hybr, grouped_by = F, overall_uncertainty = F, parametric = T)

# Постхоки
library(itsadug)

wald_gam(Mod_hybr_P_hybr)

# Изображение первички в виде боксплотов
ggplot(myt_hybr, aes(x = an_of_hybr,  y =P_hybr)) +
  geom_boxplot()


# Визуализация вручную
My_data <-
  myt_hybr %>%
  group_by(an_of_hybr) %>%
  do(data.frame(Ptros = seq(min(.$Ptros), max(.$Ptros), length.out = 20)))



predicted <- predict(Mod_hybr_P_hybr, newdata = My_data, type = "response", se.fit = T)

My_data$Predicted <- predicted$fit
My_data$SE <- predicted$se.fit


medians <-
  myt_hybr %>%
  group_by(an_of_hybr) %>%
  summarise(Median = median(P_hybr))

ggplot(My_data, aes(x = Ptros, y = Predicted, color = an_of_hybr)) +
  geom_line() +
  geom_ribbon(aes(ymin = Predicted - 2*SE, ymax = Predicted + 2*SE, fill = an_of_hybr), alpha = 0.2) +
  geom_point(data = myt_hybr, aes(y = P_hybr), size = 2) +
  facet_wrap(~an_of_hybr) +
  geom_hline(data = medians, aes(yintercept = Median))











# myt_hybr$an_of_hybr <- factor(myt_hybr$an_of_hybr, levels = c("top", "outer_part", "cline"))
#
# Mod_hybr_md <- gam(md ~  an_of_hybr + s(Ptros, by = an_of_hybr, bs = "tp", k = 5 ),  family = "betar", data = myt_hybr)
#
#
# res <- simulateResiduals(Mod_hybr_md)
# plot(res)
#
# appraise(Mod_hybr_md)
#
# summary(Mod_hybr_md)
#
# draw(Mod_hybr_md, grouped_by = F, overall_uncertainty = F, parametric = T)
#
# library(itsadug)
#
# wald_gam(Mod_hybr_md)
#
# ggplot(myt_hybr, aes(x = an_of_hybr,  y = md)) +
#   geom_boxplot()
#
#
# plot_parametric(Mod_hybr_md, pred=list(an_of_hybr=c("cline", "top",  "outer_part")))
