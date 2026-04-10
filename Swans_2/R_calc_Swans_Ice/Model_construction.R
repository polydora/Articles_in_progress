library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)
library(DHARMa)
library(gratia)
library(reshape2)
library(tidyr)
library(lubridate)
library(spdep)
library(gstat)
library(sf)
library(spacetime)


Fin_df <- read.csv("Data/Finnish_Gulf_East.csv") # Map poligones

# Basic map layer####################

theme_set(theme_bw())



Pl_Finnish_Gulf <-
  ggplot(Fin_df, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill = "gray90", colour = "gray20") +
  theme(axis.title.x = element_blank(),  axis.title.y = element_blank(), plot.background = element_blank(), panel.border = element_blank(), panel.grid = element_blank()) +
  theme(axis.text.x =element_blank(), axis.text.y= element_blank()) +
  theme(axis.ticks = element_blank())

dat <- read_excel("Data/all.data.xlsx")

dat <-
  dat %>%
  mutate(Ice_Break_DOY = yday(Date), Year = as.numeric(Year))

dat2 <-
  dat %>%
  filter(complete.cases(.))

dat2 <-
  dat2 %>%
  mutate(Year_f = factor(Year))


dat2 <-
  dat2 %>%
  filter_out(Longivity > 500)



cor.test(dat2$Longivity, dat2$Ice_Break_DOY)

mod_spat <- bam(nests ~
                      # Нелинейные эффекты предикторов
                      te(Ice_Break_DOY, Longivity, bs = "tp") +
                      # Временной тренд
                      s(Year, bs = "cr", k = 5) +
                      # Пространственное сглаживание
                      s(Lon, Lat, bs = "tp") +
                      # Пространственно-временное взаимодействие
                      ti(Lon, Lat, Year) +
                      # Случайные эффекты участков
                      s(site_id, bs = "re"),
                    data = dat2,
                    method = "REML",
                    family = nb())



# Подготовка данных
dat2 <- dat2 %>% arrange(Year, site_id)

# Создание индикатора начала временного ряда
dat2 <- dat2 %>%
  group_by(site_id) %>%
  mutate(AR_start = c(TRUE, rep(FALSE, n() - 1))) %>%
  ungroup()

dat2 <-
  dat2 %>%
  mutate(Log_nests = log(nests + 1))


# Модель с AR(1) процессом
mod_spat_ar1 <- bam(nests ~
                      te(Ice_Break_DOY, Longivity, bs = "tp") +
                      s(Year, bs = "cr", k = 5) +
                      s(Lon, Lat, bs = "tp") +
                      ti(Lon, Lat, Year) +
                      s(site_id, bs = "re"),
                    data = dat2,
                    method = "REML",
                    family = negbin(theta = 0.631),
                    # rho = 0.5,  # начальное значение AR(1)
                    AR.start = dat2$AR_start)

# Проверка улучшения
dw_test_ar1 <- dwtest(residuals(mod_spat_ar1) ~ fitted(mod_spat_ar1))
print(dw_test_ar1)



AIC(mod_spat, mod_spat_ar1)

summary(mod_spat_ar1)

draw(mod_spat_ar1, select = 1)


# Симуляция остатков
simulationOutput <- simulateResiduals(fittedModel = mod_spat_ar1, n = 250, plot = FALSE)

# Графики DHARMa
plot(simulationOutput, quantreg = TRUE)

# Тест на передисперсию (для отрицательного биномиального распределения)
testDispersion(simulationOutput)
testZeroInflation(simulationOutput)
testOutliers(simulationOutput)



cat("\n========== 3. ПРОВЕРКА АВТОКОРРЕЛЯЦИИ ==========\n")

# 4.1 Временная автокорреляция
# Упорядочиваем данные по году

# Различные типы остатков
dat2$residuals_response <- residuals(mod_spat_ar1, type = "response")
dat2$residuals_deviance <- residuals(mod_spat_ar1, type = "deviance")
dat2$residuals_pearson <- residuals(mod_spat_ar1, type = "pearson")
dat2$fitted_values <- fitted(mod_spat_ar1)

# Удаление NA
dat2_clean <- dat2 %>% filter(!is.na(residuals_deviance))


dat2_clean %>%
  ggplot(aes(x = Year, y = residuals_deviance)) +
  geom_point() +
  geom_smooth()



dat2_acf <- dat2_clean %>% arrange(Year)

# ACF и PACF графики
par(mfrow = c(1,2))
acf(dat2_acf$residuals_deviance, lag.max = 40,
    main = "ACF of Deviance Residuals")
pacf(dat2_acf$residuals_deviance, lag.max = 40,
     main = "PACF of Deviance Residuals")
par(mfrow = c(1,1))


library(lmtest)
library(itsadug)

# Тест Дарбина-Уотсона
dw_test <- dwtest(residuals(mod_spat_ar1) ~ fitted(mod_spat_ar1))
print(dw_test)


library(car)
bg_test <- bgtest(residuals(mod_spat_ar1) ~ fitted(mod_spat_ar1), order = 3)
print(bg_test)


# Тест Дарбина-Уотсона
dw_test <- dwtest(residuals_deviance ~ fitted_values, data = dat2_clean)
cat("\nDurbin-Watson Test:\n")
print(dw_test)

# 4.2 Пространственная автокорреляция
if(all(c("Lon", "Lat") %in% colnames(dat2))) {
  # Агрегация остатков по site_id
  spatial_resid <- dat2_clean %>%
    group_by(site_id, Lon, Lat) %>%
    summarise(
      mean_residual = mean(residuals_deviance, na.rm = TRUE),
      n_obs = n(),
      .groups = 'drop'
    ) %>%
    na.omit()

  if(nrow(spatial_resid) > 3) {
    # Создание пространственных весов
    coords <- as.matrix(spatial_resid[, c("Lon", "Lat")])
    nb <- knn2nb(knearneigh(coords, k = min(5, nrow(coords)-1)))
    w <- nb2listw(nb, style = "W", zero.policy = TRUE)

    # Тест Морана
    moran_result <- moran.test(spatial_resid$mean_residual, w, zero.policy = TRUE)
    cat("\nMoran's I Test for Spatial Autocorrelation:\n")
    print(moran_result)

    # Локальный Морана
    local_moran <- localmoran(spatial_resid$mean_residual, w, zero.policy = TRUE)
    spatial_resid$local_moran <- local_moran[,1]
    spatial_resid$local_moran_pval <- local_moran[,5]

    # Визуализация
    p5 <- ggplot(spatial_resid, aes(x = Lon, y = Lat, color = local_moran)) +
      geom_point(size = 3) +
      scale_color_gradient2(low = "blue", mid = "white", high = "red") +
      labs(title = "Local Moran's I for Residuals",
           x = "Longitude", y = "Latitude") +
      theme_minimal()
    print(p5)
  }
}




####################3

# Самый простой и надежный метод
library(gstat)
library(ggplot2)

# Подготовка данных
st_data <- dat2_clean %>%
  as.data.frame() %>%
  dplyr::select(Lon, Lat, Year, residuals) %>%
  filter(!is.na(residuals), !is.na(Lon), !is.na(Lat)) %>%
  rename(x = Lon, y = Lat, time = Year, value = residuals)

# Создание списка вариограмм для каждого временного лага
time_lags <- unique(st_data$time)
max_lag <- min(3, length(time_lags)-1)
variogram_list <- list()

for(lag in 0:max_lag) {
  for(t in time_lags[1:(length(time_lags)-lag)]) {
    data_t1 <- st_data[st_data$time == t, ]
    data_t2 <- st_data[st_data$time == (t + lag), ]

    if(nrow(data_t1) > 5 & nrow(data_t2) > 5) {
      # Объединяем данные
      combined <- rbind(
        cbind(data_t1[, c("x", "y", "value")], time_lag = 0),
        cbind(data_t2[, c("x", "y", "value")], time_lag = lag)
      )

      # Создаем пространственный объект
      coordinates(combined) <- c("x", "y")
      proj4string(combined) <- CRS("+proj=longlat +datum=WGS84")
      combined_utm <- spTransform(combined, CRS("+proj=utm +zone=33 +datum=WGS84"))

      # Вариограмма
      var_temp <- variogram(value ~ 1, data = combined_utm)
      var_temp$time_lag <- lag
      variogram_list[[length(variogram_list) + 1]] <- var_temp
    }
  }
}


# Объединение результатов
variogram_st <- do.call(rbind, variogram_list)

# Визуализация
ggplot(variogram_st, aes(x = dist, y = gamma, color = factor(time_lag))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5) +
  scale_color_viridis_d(name = "Time lag\n(years)") +
  labs(title = "Space-Time Variogram of Residuals",
       x = "Distance (meters)",
       y = "Semivariance") +
  theme_minimal() +
  theme(legend.position = "right") +
  facet_wrap(~factor(time_lag))


draw(mod_spat_ar1, select = 1)

draw(mod_spat_ar1, select = 2)

draw(mod_spat_ar1, select = 3)

