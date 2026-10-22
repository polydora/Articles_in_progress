# ============================================================
# MPM с инфекцией: age x time-since-infection structured model
# ============================================================
# Состояние:
#   S[a]        — восприимчивые возраста a = 1..30   (A0..A29)
#   I[a, tau]   — инфицированные: возраст a, время с момента заражения tau
#                 tau = 1..Nyears (1 = заразился в текущем году)
#
# Динамика:
#   S(t+1) = A_S %*% S(t) - lambda * S(t)  +  пополнение
#   I(a+1, tau+1, t+1) = (1 - mu_I(tau)) * I(a, tau, t)
#   I(a, 1, t+1)       = lambda * S(a, t)   (новые заражения)
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

# ------------------------------------------------------------
# Параметры по умолчанию
# ------------------------------------------------------------
d <- 0.05   # стартовая смертность
l <- 0.85   # прирост смертности
s <- 0.92   # смертность ювенилов
p <- 2.5    # падение смертности ювенилов


# ------------------------------------------------------------
# lesley(): матрица Лесли (30 x 30)
# ------------------------------------------------------------
lesley <- function(d = 0.05, l = 0.85, s = 0.92, p = 2.5) {
  juv_dead <- rep(NA_real_, 30)
  juv_dead[1] <- d
  for (i in 2:30) {
    juv_dead[i] <- (juv_dead[i - 1] * exp(l)) /
      (1 + ((exp(l) - 1) * juv_dead[i - 1]))
  }
  
  dif_dead <- rep(NA_real_, 30)
  dif_dead[1] <- s
  for (i in 2:30) dif_dead[i] <- dif_dead[i - 1] / p
  
  total_dead <- juv_dead + dif_dead
  total_dead[total_dead > 1] <- 1
  p_survival <- 1 - total_dead
  
  A <- matrix(0, nrow = 30, ncol = 30)
  A[1, 4:30] <- 1
  for (i in 2:30) A[i, i - 1] <- p_survival[i - 1]
  A
}


# ------------------------------------------------------------
# biomassa(): биомасса по демографическому вектору
# ------------------------------------------------------------
biomassa <- function(x,
                     l_inf = 76.3, l0 = 0.35, k = 0.1375,
                     w0 = 0.000025, a = 0.000103, b = 2.964) {
  ages <- 0:(length(x) - 1)
  Bert <- l0 + l_inf * (1 - exp(ages * (-k)))
  (w0 + a * Bert^b) * x
}


# ------------------------------------------------------------
# random_add(): мультипликативный шум
# ------------------------------------------------------------
random_add <- function(x, SD_perc = 0) {
  if (SD_perc == 0) return(x)
  x + rnorm(length(x), 0, x * SD_perc / 100)
}


# ============================================================
# MPM_dynamics(): основная функция
# ============================================================
MPM_dynamics <- function(
    c = 30,                        # сила влияния взрослых на спат
    Nyears = 50,
    infection_probability = 0.01,  # lambda: сила инфекции
    r = 3,                         # скорость роста анеуплоидии
    x = rep(0, 30),                # стартовый демографический вектор
    plot = TRUE,
    m = 1,                         # степень влияния взрослых
    recruitment = 100000,
    Adult_age = 4,                 # индекс возраста, с которого считаем "взрослым"
    SD = 0,
    c_noise = 0,
    Relative_N = FALSE,
    year_drop = 10,
    # Порог гибели от анеуплоидии: доля анеуплоидных клеток, при которой
    # особь считается мёртвой
    aneuploid_threshold = 0.9,
    # Стартовая доля анеуплоидных клеток при заражении
    aneuploid_init = 0.00001,
    ...
) {
  
  # ---- 1. Демографическая матрица ---------------------------
  A <- lesley()
  
  # ---- 2. Матрица смертности инфицированных по tau ----------
  # Доля выживших через tau лет после заражения.
  # prop_aneuploid(tau) = aneuploid_init * exp(r)^(tau - 1)
  # Гибель, когда prop >= threshold.
  # tau_death — первый tau, при котором порог превышен.
  tau_death <- ceiling(log(aneuploid_threshold / aneuploid_init) / r) + 1
  
  # survival_I[tau] = 1, если особь ещё жива при данном tau, иначе 0.
  # (жёсткая смертность; можно заменить на плавную, если нужно)
  survival_I <- numeric(Nyears)
  survival_I[1:min(tau_death - 1, Nyears)] <- 1
  
  # ---- 3. Инициализация состояния ---------------------------
  # S: матрица Nyears x 30 (по годам)
  S <- matrix(0, nrow = Nyears, ncol = 30)
  # I: тензор Nyears x 30 x Nyears — [год, возраст, tau]
  I <- array(0, dim = c(Nyears, 30, Nyears))
  
  S[1, ] <- x
  
  # ---- 4. Вспомогательные векторы ---------------------------
  # Биомасса по возрастам (для пересчёта S -> B)
  # biomassa() возвращает вектор той же длины, что и вход
  
  # ---- 5. Основной цикл -------------------------------------
  for (t in 2:Nyears) {
    
    # --- 5.1 Демография восприимчивых ------------------------
    # S(t) = A %*% S(t-1), затем вычитаем заражённых
    S_t <- as.vector(A %*% S[t - 1, ])
    S_t <- round(S_t, 0)
    S_t[S_t < 1] <- 0
    S_t <- random_add(S_t, SD_perc = SD)
    S_t[S_t < 1] <- 0
    
    # --- 5.2 Новые заражения ---------------------------------
    # lambda * S(a, t) — сколько восприимчивых возраста a заразилось
    if (infection_probability > 0) {
      new_infected <- round(S_t * infection_probability, 0)
      # Нельзя заразить больше, чем есть
      new_infected <- pmin(new_infected, S_t)
      S_t <- S_t - new_infected
    } else {
      new_infected <- numeric(30)
    }
    
    # --- 5.3 Старение и гибель инфицированных ----------------
    # Сдвигаем I по возрасту и по tau: I(a, tau) -> I(a+1, tau+1)
    # Смертность: умножаем на survival_I[tau]
    if (t > 2) {
      # Берём состояние инфицированных на предыдущем шаге
      I_prev <- I[t - 1, , , drop = FALSE]
      I_prev <- I_prev[1, , , drop = FALSE]  # размерность 1 x 30 x Nyears
      I_prev <- I_prev[1, , ]                # 30 x Nyears
      
      # Сдвиг по возрасту: строка a -> строка a+1 (возраст 30 теряется)
      I_aged <- matrix(0, nrow = 30, ncol = Nyears)
      I_aged[2:30, ] <- I_prev[1:29, ]
      
      # Сдвиг по tau: столбец tau -> tau+1
      I_shifted <- matrix(0, nrow = 30, ncol = Nyears)
      I_shifted[, 2:Nyears] <- I_aged[, 1:(Nyears - 1)]
      
      # Применяем выживание
      I_shifted <- I_shifted * matrix(survival_I,
                                      nrow = 30, ncol = Nyears,
                                      byrow = TRUE)
      
      I[t, , ] <- I_shifted
    } else {
      I[t, , ] <- 0
    }
    
    # --- 5.4 Добавляем новых заражённых (tau = 1) ------------
    I[t, , 1] <- I[t, , 1] + new_infected
    
    # --- 5.5 Удаляем мёртвых из демографического вектора -----
    # Особи, достигшие tau_death, должны быть удалены из S_t.
    # (В этой формулировке они уже не входят в S_t — они были
    #  вычтены при заражении. Удаление из I происходит через
    #  survival_I = 0, поэтому отдельного шага не нужно.)
    
    # --- 5.6 Записываем S ------------------------------------
    S[t, ] <- S_t
    
    # --- 5.7 Биомасса и пополнение ---------------------------
    B_t <- biomassa(S_t)
    B_t[B_t < 0] <- 0
    biom_adult <- sum(B_t[(Adult_age + 1):30])
    
    c <- c + rnorm(1, 0, c_noise)
    
    if ((c * biom_adult^m) < recruitment) {
      S[t, 1] <- recruitment - (c * biom_adult^m)
    } else {
      S[t, 1] <- 0
    }
    
    # Пересчёт после пополнения
    S[t, ][S[t, ] < 1] <- 0
  }
  
  # ---- 6. Агрегаты ------------------------------------------
  # Суммарная численность восприимчивых по годам
  S_total <- rowSums(S)
  
  # Суммарная численность инфицированных по годам
  # I — тензор [t, a, tau]; суммируем по a и tau
  I_total <- apply(I, 1, sum)
  
  # Инфицированные возраста >= 5 (т.е. a >= 5)
  I_large_total <- apply(I[, 5:30, , drop = FALSE], 1, sum)
  
  # Восприимчивые возраста >= 5
  S_large_total <- rowSums(S[, 5:30, drop = FALSE])
  
  # ---- 7. Доли заражённых -----------------------------------
  Total_Infection_Prop <- ifelse(
    (S_total + I_total) > 0,
    I_total / (S_total + I_total),
    0
  )
  
  Large_Infection_Prop <- ifelse(
    (S_large_total + I_large_total) > 0,
    I_large_total / (S_large_total + I_large_total),
    0
  )
  
  infected_prop <- data.frame(
    Years = 1:Nyears,
    Total_Infection_Prop = Total_Infection_Prop,
    Large_Infection_Prop = Large_Infection_Prop
  )
  
  # ---- 8. Численность и биомасса ----------------------------
  # Общая численность = восприимчивые + инфицированные
  dynamics_totalN <- S_total + I_total
  
  # Биомасса: считаем по восприжимым + инфицированным
  # (для простоты — по общей численности)
  dynamics_totalB <- sapply(1:Nyears, function(t) sum(biomassa(S[t, ] + 
                                                                 apply(I[t, , , drop = FALSE], 1, sum))))
  
  dynamics_totalB_adult <- sapply(1:Nyears, function(t) {
    n_adult <- S[t, (Adult_age + 1):30] +
      apply(I[t, (Adult_age + 1):30, , drop = FALSE], 1, sum)
    sum(biomassa(c(rep(0, Adult_age), n_adult))[(Adult_age + 1):30])
  })
  
  dynamics_totalN_Juv <- rowSums(S[, 1:Adult_age, drop = FALSE]) +
    apply(I[, 1:Adult_age, , drop = FALSE], 1, sum)
  
  if (Relative_N) {
    dynamics_totalN <- dynamics_totalN / max(dynamics_totalN) * 100
    dynamics_totalB <- dynamics_totalB / max(dynamics_totalB) * 100
  }
  
  N_total <- data.frame(
    Years = seq((year_drop + 1), Nyears),
    Nt = dynamics_totalN[(year_drop + 1):Nyears]
  )
  
  B_total <- data.frame(
    Years = seq((year_drop + 1), Nyears),
    Bt = dynamics_totalB[(year_drop + 1):Nyears]
  )
  
  infected_prop <- infected_prop[-c(1:year_drop), ]
  
  # ---- 9. Графики -------------------------------------------
  pl_N <- NULL
  pl_B <- NULL
  
  if (plot) {
    
    # --- 9.1 Численность ---
    pl_N <- ggplot(N_total, aes(x = Years, y = Nt)) +
      geom_line(colour = "blue") +
      theme_bw() +
      labs(x = "Время", y = "Численность популяции") +
      theme(axis.title = element_text(size = 10),
            legend.text = element_text(size = 10))
    
    # --- 9.2 Инфекция ---
    if (infection_probability != 0) {
      pl_infected <- ggplot(infected_prop,
                            aes(x = Years, y = Large_Infection_Prop)) +
        geom_line(color = "red") +
        theme_bw() +
        labs(x = "Время", y = "частота зараженных \nкрупных") +
        theme(axis.title = element_text(size = 10),
              legend.text = element_text(size = 10))
      
      pl_infected_total <- ggplot(infected_prop,
                                  aes(x = Years, y = Total_Infection_Prop)) +
        geom_line(color = "red") +
        theme_bw() +
        labs(x = "Время", y = "частота зараженных \nобщая") +
        theme(axis.title = element_text(size = 10),
              legend.text = element_text(size = 10))
    } else {
      pl_infected <- NULL
      pl_infected_total <- NULL
    }
    
    # --- 9.3 Возрастная структура ---
    # Строим по восприимчивым
    S_df <- as.data.frame(S)
    names(S_df) <- paste0("A", 0:29)
    
    S_df |>
      mutate(Years = 1:Nyears) |>
      pivot_longer(cols = starts_with("A"),
                   values_to = "Abundance",
                   names_to = "Age") |>
      filter(Age %in% paste0("A", 1:8)) |>
      filter(Years > year_drop) |>
      ggplot(aes(x = Age, y = Abundance)) +
      geom_col() +
      facet_wrap(~Years, ncol = 5, dir = "v", scales = "free_y") +
      theme(strip.text = element_blank(),
            axis.text = element_blank()) ->
      pl_age_structure
    
    # --- 9.4 Связь доли заражённых с долей молоди ---
    N_total$N_juv <- dynamics_totalN_Juv[-c(1:year_drop)]
    N_total <- N_total |> mutate(Prop_Juv = N_juv / Nt)
    N_total <- N_total |>
      mutate(Large_infected_prop = infected_prop$Large_Infection_Prop)
    
    if (infection_probability != 0) {
      N_total |>
        ggplot(aes(x = Prop_Juv, y = Large_infected_prop)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(x = "Доля молоди", y = "Зараженность крупных") ->
        pl_infected_vs_juv
    } else {
      pl_infected_vs_juv <- NULL
    }
    
    pl_N <- plot_grid(
      pl_N, pl_infected, pl_infected_total,
      pl_age_structure, pl_infected_vs_juv,
      ncol = 1, align = "v", axis = "l"
    )
    
    # --- 9.5 Биомасса ---
    pl_B <- ggplot(B_total, aes(x = Years, y = Bt)) +
      geom_line(colour = "blue") +
      theme_bw() +
      labs(x = "Время", y = "Биомасса") +
      theme(axis.title = element_text(size = 20),
            legend.text = element_text(size = 20))
  }
  
  # ---- 10. Возврат ------------------------------------------
  list(
    S = S,                          # матрица восприимчивых Nyears x 30
    I = I,                          # тензор инфицированных Nyears x 30 x Nyears
    N_total = N_total,
    dynamics_totalN_Juv = dynamics_totalN_Juv,
    B_total = B_total,
    dynamics_totalB_adult = dynamics_totalB_adult,
    infected_prop = infected_prop,
    pl_N = pl_N,
    pl_B = pl_B
  )
}


# ============================================================
# Пример запуска
# ============================================================
res <- MPM_dynamics(Nyears = 60, infection_probability = 0.05)
res$pl_N


# ============================================================
# Shiny-приложение
# ============================================================
library(shiny)

ui <- fluidPage(
  titlePanel("MPM с инфекцией: динамика мидиевых поселений"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("c", "Степень отрицательного влияния взрослых на молодь",
                  min = -40, max = 40, step = 0.1, value = 30),
      sliderInput("Nyears", "Продолжительность наблюдений",
                  min = 20, max = 150, step = 1, value = 50),
      sliderInput("infection_probability", "Вероятность заражения",
                  min = 0, max = 0.5, step = 0.001, value = 0),
      sliderInput("r", "Скорость роста анеуплоидии",
                  min = 0.1, max = 5, step = 0.01, value = 1)
    ),
    
    mainPanel(
      plotOutput("my_graph", height = "100vh")
    )
  )
)

server <- function(input, output) {
  output$my_graph <- renderPlot({
    MPM_dynamics(
      c = input$c,
      Nyears = input$Nyears,
      infection_probability = input$infection_probability,
      r = input$r
    )$pl_N
  })
}

shinyApp(ui = ui, server = server)