library(cooccur)
library(readxl)
library(dplyr)

# 1. Загрузка данных с детальной диагностикой
syncl <- read_excel("Data/matrix_only syninclusions.xlsx")

# 3. Создаем матрицу с правильными именами

if (is.character(syncl[[1]]) | is.factor(syncl[[1]])) {
  species_names <- as.character(syncl[[1]])
  data_values <- syncl[, -1, drop = FALSE]
  
  # Преобразуем в матрицу
  mat <- as.matrix(data_values)
  
  # Присваиваем имена строк (видов)
  rownames(mat) <- species_names
  
  # Присваиваем имена столбцов (проб), если их нет
  if (is.null(colnames(mat))) {
    colnames(mat) <- paste("Sample", 1:ncol(mat))
  }
} else {
  # Если нет столбца с названиями видов
  mat <- as.matrix(syncl)
  rownames(mat) <- paste("Species", 1:nrow(mat))
}


# 5. Транспонирование (виды должны быть в столбцах для type="spp_site")
mat_t <- t(mat)
cat("\nРазмеры транспонированной матрицы:", dim(mat_t), "\n")


# 6. Проверка на NA и преобразование в числовой формат
if (any(is.na(mat_t))) {
  cat("Внимание: есть пропущенные значения. Заменяем на 0.\n")
  mat_t[is.na(mat_t)] <- 0
}

# Убедимся, что данные числовые
mat_t_numeric <- apply(mat_t, 2, as.numeric)
rownames(mat_t_numeric) <- rownames(mat_t)
colnames(mat_t_numeric) <- colnames(mat_t)

# 7. Удаляем виды, которые никогда не встречаются
species_sums <- colSums(mat_t_numeric)
cat("\nКоличество встреч по видам:\n")
print(table(species_sums))

# Оставляем только виды, которые встречаются хотя бы 1 раз
valid_species <- which(species_sums > 0)
mat_filtered <- mat_t_numeric[, valid_species, drop = FALSE]

cat("\nПосле фильтрации редких видов:\n")
cat("Было видов:", ncol(mat_t_numeric), "\n")
cat("Осталось видов:", ncol(mat_filtered), "\n")

# 8. Запуск анализа cooccur

str(mat_filtered)


cooccur.results <- cooccur(mat_filtered, 
                             type = "spp_site",
                             thresh = TRUE,
                             spp_names = TRUE,
                             true_rand_classifier = 0.001,
                             only_effects = FALSE)
  
  
  # Вывод результатов
  

# write.table(as.data.frame(cooccur.results$results), "clipboard", sep="\t")

  print(cooccur.results)
  
  summary(cooccur.results)
  
  # Визуализация
  plot(cooccur.results)
  
  # p_gt < 0.05: статистически значимая положительная ассоциация (виды встречаются вместе чаще, чем ожидалось случайно)
  # 
  # p_lt < 0.05: статистически значимая отрицательная ассоциация (виды встречаются вместе реже, чем ожидалось случайно)
  # 
  