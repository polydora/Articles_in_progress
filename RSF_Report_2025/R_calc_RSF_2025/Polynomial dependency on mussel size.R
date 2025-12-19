# Строим модель для связи вероятности встретить здоровую особь и особь больную bnt1 и btn2 в зависимости от размера

library(VGAM)

L_Rate_aneuploid$BTN_Type <- relevel(L_Rate_aneuploid$BTN_Type, ref = "Здоровые")

vgam_model <- vgam(BTN_Type  ~ s(value, df = 3),
                   family = multinomial(refLevel = 1),  # h как базовая
                   data = df)
summary(vgam_model)

plot(vgam_model)


# Создаем сетку значений для предсказания
value_seq <- seq(min(L_Rate_aneuploid$value), 
                 max(L_Rate_aneuploid$value), 
                 length.out = 200)
pred_data <- data.frame(value = value_seq)

# Предсказанные вероятности
pred_probs <- predict(vgam_model, newdata = pred_data, type = "response")

# Преобразуем в удобный формат для ggplot
plot_data <- data.frame(
  value = rep(value_seq, ncol(pred_probs)),
  probability = as.vector(pred_probs),
  category = rep(colnames(pred_probs), each = length(value_seq))
)

plot_data$category
# Сетка значений для предсказания
value_seq <- seq(min(L_Rate_aneuploid$value), 
                 max(L_Rate_aneuploid$value), 
                 length.out = 100)
pred_data <- data.frame(value = value_seq)

# Функция для bootstrap оценки доверительных интервалов
bootstrap_ci <- function(model, newdata, n_boot = 1000, alpha = 0.05) {
  n <- nrow(model@y)
  n_pred <- nrow(newdata)
  n_cat <- ncol(model@y)
  
  # Массивы для хранения bootstrap предсказаний
  boot_preds <- array(0, dim = c(n_boot, n_pred, n_cat))
  
  set.seed(123)  # для воспроизводимости
  for(i in 1:n_boot) {
    # Bootstrap выборка
    boot_indices <- sample(1:n, n, replace = TRUE)
    boot_data <- L_Rate_aneuploid[boot_indices, ]
    
    # Обучаем модель на bootstrap выборке
    boot_model <- try(vgam(BTN_Type ~ s(value, df = 3),
                           family = multinomial(refLevel = 1),
                           data = boot_data), silent = TRUE)
    
    if(!inherits(boot_model, "try-error")) {
      boot_pred <- predict(boot_model, newdata = newdata, type = "response")
      boot_preds[i, , ] <- boot_pred
    }
  }
  
  # Вычисляем квантили
  ci_lower <- apply(boot_preds, c(2, 3), quantile, probs = alpha/2, na.rm = TRUE)
  ci_upper <- apply(boot_preds, c(2, 3), quantile, probs = 1 - alpha/2, na.rm = TRUE)
  
  # Средние предсказания
  mean_pred <- predict(model, newdata = newdata, type = "response")
  
  return(list(mean = mean_pred, lower = ci_lower, upper = ci_upper))
}

# Получаем доверительные интервалы (занимает время!)
ci_result <- bootstrap_ci(vgam_model, pred_data, n_boot = 200)  # Можно увеличить n_boot

# Подготавливаем данные для ggplot
plot_data <- data.frame(
  value = rep(value_seq, ncol(ci_result$mean)),
  probability = as.vector(ci_result$mean),
  lower = as.vector(ci_result$lower),
  upper = as.vector(ci_result$upper),
  category = rep(colnames(ci_result$mean), each = length(value_seq))
)

# Визуализация с доверительными интервалами
ggplot(plot_data %>% filter(category == "Здоровые"), aes(x = value, y = probability, color = category, fill = category)) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  labs(title = "Вероятности категорий с 95% доверительными интервалами (bootstrap)",
       x = "Value", y = "Вероятность",
       color = "Категория", fill = "Категория") +
  theme_minimal() +
  theme(legend.position = "bottom") 
