# Функция для оценки модели с разными gamma
test_gamma <- function(gamma_val) {
  mod <- gam(cbind(N_cancer, N_helthy) ~ 
               s(Dist_Port, by = Lineage, bs = "cs", k = 5) + 
               s(log(fetch), by = Lineage, bs = "cs", k = 5) +  
               s(PC1, by = Lineage, bs = "cs", k = 5) + 
               s(PC2, by = Lineage, bs = "cs", k = 5) +
               s(OGP, by = Lineage, bs = "cs", k = 5) +
               s(Salinity, by = Lineage, bs = "cs", k = 5) +
               Lineage +  
               s(Year, Site, bs = "re"),  
             family = "binomial", 
             method = "REML", 
             optimizer = "outer",  # Важно для gamma
             gamma = gamma_val,
             data = cancer_21_23_predictors)
  
  # Собираем метрики
  data.frame(
    gamma = gamma_val,
    AIC = AIC(mod),
    BIC = BIC(mod),
    logLik = logLik(mod)[1],
    edf_total = sum(mod$edf),  # Общая сложность модели
    convergence = mod$converged
  )
}


# Тестируем gamma от 0.5 до 3 с шагом 0.25
gamma_values <- seq(0.5, 3, by = 0.25)
results <- do.call(rbind, lapply(gamma_values, test_gamma))

# Посмотрим результаты
print(results)


library(ggplot2)

# График AIC
ggplot(results, aes(x = gamma, y = AIC)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = results$gamma[which.min(results$AIC)], 
             linetype = "dashed", color = "red") +
  labs(title = "Зависимость AIC от gamma",
       x = "gamma", y = "AIC") +
  theme_minimal()

# График EDF (эффективных степеней свободы)
ggplot(results, aes(x = gamma, y = edf_total)) +
  geom_line() +
  geom_point() +
  labs(title = "Сложность модели vs gamma",
       x = "gamma", y = "Общее EDF") +
  theme_minimal()

# Совмещенный график
ggplot(results, aes(x = gamma)) +
  geom_line(aes(y = AIC, color = "AIC")) +
  geom_line(aes(y = edf_total * 50, color = "EDF (масштабировано)")) +  # Масштабируем для визуализации
  scale_y_continuous(
    name = "AIC",
    sec.axis = sec_axis(~./50, name = "EDF")
  ) +
  labs(title = "AIC и сложность модели при разных gamma") +
  theme_minimal()


# Находим gamma с минимальным AIC
best_gamma <- results$gamma[which.min(results$BIC)]
cat("Оптимальное gamma:", best_gamma, "\n")

# Или выбираем компромиссное значение (например, самое простое в пределах 2 AIC от минимума)
best_simple <- results %>%
  filter(BIC <= min(BIC) + 2) %>%
  arrange(edf_total) %>%
  slice(1)

cat("Самая простая модель в пределах 2 AIC от минимума:", 
    best_simple$gamma, "с EDF =", best_simple$edf_total)

Mod_final <- gam(cbind(N_cancer, N_helthy) ~ 
                   s(Dist_Port, by = Lineage, bs = "cs", k = 5) + 
                   s(log(fetch), by = Lineage, bs = "cs", k = 5) +  
                   s(PC1, by = Lineage, bs = "cs", k = 5) + 
                   s(PC2, by = Lineage, bs = "cs", k = 5) +
                   s(OGP, by = Lineage, bs = "cs", k = 5) +
                   s(Salinity, by = Lineage, bs = "cs", k = 5) +
                   Lineage +  
                   s(Year, Site, bs = "re"),  
                 family = "binomial", 
                 method = "REML", 
                 optimizer = "outer",
                 gamma = best_gamma,
                 data = cancer_21_23_predictors)

summary(Mod_final)
