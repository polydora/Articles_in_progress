# В этом скрипте анализируем щависимость доли грид, попавших на гонады, в общем количестве изученных грид. Анализируются срезы гонад мидий из Магадана.


library(readxl)
library(ggplot2)
library(dplyr)
library(mgcv)
library(gratia)


gon <- read_excel("Data/gonads_histology_weibel.xlsx")

gon$BTN_class_1_or_2 <- factor(gon$BTN_class_1_or_2)

names(gon)

gon_btn <- 
  gon %>% 
  filter_out(BTN_class_1_or_2 == "no")
  
gon_healthy <- 
  gon %>% 
  filter(BTN_class_1_or_2 == "no")

  

# mod <- gam(cbind(Gonad_grid,Tishue_grid) ~ s(Aneuploidy_rate, by = BTN_class_1_or_2, bs = "cs", k = 4 ) + BTN_class_1_or_2, data = gon_btn, family = "binomial")
# 
# summary(mod)
# 
# sm <- 
#   smooth_estimates(mod) |>
#   add_confint()
# 
# coef(mod)
# sm

# 
# mod <- glm(cbind(Gonad_grid,Tishue_grid) ~ BTN_class_1_or_2*Aneuploidy_rate, data = gon_btn, family = "binomial")
# 
# 
# 
# logit_back <- function(x) exp(x)/(1 + exp(x)) # обратная логит-трансформация
# 


# # Модельная матрица и коэффициенты
# X <- model.matrix(~ BTN_class_1_or_2 * Aneuploidy_rate, data =  pred_data)
# b <- coef(mod)
# 
# # Предсказанные значения и стандартные ошибки...
# # ...в масштабе функции связи (логит)
# 
# pred_data$fit_eta <- X %*% b
# pred_data$se_eta <- sqrt(diag(X %*% vcov(mod) %*% t(X)))
# 
# # ...в масштабе отклика (применяем функцию, обратную функции связи)
# 
# 
# pred_data$fit_pi <- logit_back(pred_data$fit_eta)
# 
# pred_data$lwr_pi <- logit_back(pred_data$fit_eta - 2 * pred_data$se_eta)
# pred_data$upr_pi <- logit_back(pred_data$fit_eta + 2 * pred_data$se_eta)
# 
# ggplot(pred_data, aes(x = Aneuploidy_rate, y = fit_pi, color = BTN_class_1_or_2)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lwr_pi, ymax = upr_pi), alpha = 0.2) +
#   geom_point(data = gon_btn, aes(y = Gonad_prop, color = BTN_class_1_or_2), size = 4) +
#   geom_text(data = gon_btn, aes(label = ID, y = Gonad_prop))
#     

library(boot)  # для бутстрепа

# Ваша модель
mod <- glm(cbind(Gonad_grid,Tishue_grid) ~ BTN_class_1_or_2*Aneuploidy_rate, data = gon_btn, family = "binomial")




# ============================================================
# МЕТОД 1: Бутстреп с использованием boot::boot
# ============================================================

# Функция для извлечения предсказанных значений из модели
predict_function <- function(data, indices, newdata_grid) {
  # Создаем бутстреп-выборку
  boot_data <- data[indices, ]
  
  # Подгоняем модель на бутстреп-выборке
  boot_mod <- tryCatch({
    glm(cbind(Gonad_grid, Tishue_grid) ~ BTN_class_1_or_2 * Aneuploidy_rate,
        data = boot_data,
        family = "binomial")
  }, error = function(e) return(NULL))
  
  # Если модель не сходится, возвращаем NA
  if (is.null(boot_mod)) return(rep(NA, nrow(newdata_grid)))
  
  # Предсказываем значения для новой сетки данных
  pred <- predict(boot_mod, newdata = newdata_grid, type = "response")
  return(pred)
}

# Подготовка данных для предсказаний
# Определяем уникальные значения BTN_class_1_or_2
btn_levels <- unique(gon_btn$BTN_class_1_or_2)

# Создаем сетку для Aneuploidy_rate
aneu_grid <- seq(min(gon_btn$Aneuploidy_rate, na.rm = TRUE), 
                 max(gon_btn$Aneuploidy_rate, na.rm = TRUE), 
                 length.out = 50)

# Создаем dataframe для всех комбинаций
newdata <- expand.grid(
  Aneuploidy_rate = aneu_grid,
  BTN_class_1_or_2 = btn_levels
)

# Количество бутстреп-итераций
R <- 1000  # Можно увеличить до 2000-5000 для более точных интервалов

# Запускаем бутстреп
library(boot)
set.seed(123)  # для воспроизводимости
boot_results <- boot(data = gon_btn, 
                     statistic = predict_function, 
                     R = R,
                     newdata_grid = newdata)

# Извлекаем бутстреп-предсказания
boot_predictions <- boot_results$t

# Рассчитываем доверительные интервалы (процентильный метод)
conf_level <- 0.95
alpha <- 1 - conf_level

lower_percentile <- apply(boot_predictions, 2, function(x) quantile(x, probs = alpha/2, na.rm = TRUE))
upper_percentile <- apply(boot_predictions, 2, function(x) quantile(x, probs = 1 - alpha/2, na.rm = TRUE))

# Также получаем средние предсказания из исходной модели
newdata$predicted <- predict(mod, newdata = newdata, type = "response")

# Добавляем доверительные интервалы
newdata$conf_low <- lower_percentile
newdata$conf_high <- upper_percentile

# ============================================================
# ВИЗУАЛИЗАЦИЯ
# ============================================================

# График 1: Основной график с бутстреп-интервалами


Pl_gonad_btn <- 
ggplot(newdata, aes(x = Aneuploidy_rate, y = predicted, color = BTN_class_1_or_2)) +
  geom_line(linewidth = 2, color = "red") +
  geom_ribbon(aes(ymin = conf_low, ymax = conf_high, fill = BTN_class_1_or_2), 
              alpha = 0.2, color = NA, fill = "red") +
  # Добавляем исходные точки данных
  geom_point(data = gon_btn, 
             aes(x = Aneuploidy_rate, 
                 y = Gonad_grid / (Gonad_grid + Tishue_grid),
                 color = BTN_class_1_or_2), 
             size = 4, color = "red") +
  labs(
    # title = "Binomial GLM with Bootstrap Confidence Intervals",
    # subtitle = paste("Based on", R, "bootstrap iterations (percentile method)"),
    x = "",
    y = "",
    color = NULL,
    fill = NULL
  ) +
  guides(color = "none", fill = "none") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size  = 15), 
    strip.text = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 1), 
                     labels = scales::percent_format(),
                     breaks = seq(0, 1, 0.2)) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  xlim(0,100) +
  facet_wrap(~BTN_class_1_or_2, ncol = 2)





set.seed(123)  # для воспроизводимости

gon_healthy <- 
  gon %>% 
  filter(BTN_class_1_or_2 == "no")



gon_healthy <-
  gon_healthy %>% 
  filter(ID_ENG %in% c("KAR_V30", "KAR_B21", "KAR_B4", "JEM_V24", "MAM_A24", "HOL_D8", "SV_V23", "SV_V17", "NUK_V23"))


gon %>% 
  ggplot(aes(x = Sex_2, y = Gonad_prop)) +
  geom_boxplot() + 
  facet_wrap(~BTN_class_1_or_2)

gon$Gonad_prop

names(gon)

library(betareg)
mod <- gam( Gonad_prop ~ Sex_2*BTN_class_1_or_2, data = gon %>% filter(Sex_2 %in% c("м", "ж")), family = "betar")
summary(mod)

library(car)
anova(mod)


# Функция для вычисления статистики (среднего)
mean_fun <- function(data, indices) {
  d <- data[indices]
  return(mean(d, na.rm = TRUE))
}

# Выполняем бутстреп
boot_results <- boot(data = gon_healthy$Gonad_prop, 
                     statistic = mean_fun, 
                     R = 1000)  # 1000 бутстреп-выборок

# Получаем доверительные интервалы
boot_ci <- boot.ci(boot_results, type = c("perc", "basic", "norm"))


# Извлекаем результаты
mean_value <- mean(gon_healthy$Gonad_prop, na.rm = TRUE)
percentile_ci <- boot_ci$percent[4:5]  # 95% перцентильный CI
basic_ci <- boot_ci$basic[4:5]         # 95% basic CI
normal_ci <- boot_ci$normal[2:3]       # 95% нормальный CI


Pl_gonad_btn_with_healthy <- 
Pl_gonad_btn +
  annotate(x = 0, y = mean_value, geom = "point", color = "black", size = 5) +
  annotate(x = 0, ymin = percentile_ci[1], ymax = percentile_ci[2], geom = "errorbar", width = 0.2) +
  annotate(x = seq(0, 100, 10), ymin = percentile_ci[1], ymax = percentile_ci[2],
           geom = "ribbon", alpha = 0.3, fill = "gray70") 



Pl_gonad_btn_with_healthy +
  labs(x = "Proportion of anneuploid cells", y = "Acinus size")
