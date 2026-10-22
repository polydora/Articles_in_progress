# ============================================================
# Модель динамики мидиевых поселений (оптимизированная версия)
# ============================================================

library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# Параметры по умолчанию
# ------------------------------------------------------------
d <- 0.05
l <- 0.85
s <- 0.92
p <- 2.5


# ------------------------------------------------------------
# lesley(): матрица Лесли
# ------------------------------------------------------------
lesley <- function(d = 0.05, l = 0.85, s = 0.92, p = 2.5) {
  juv_dead <- rep(NA, 30)
  juv_dead[1] <- d
  for (i in 2:30) {
    juv_dead[i] <- (juv_dead[i - 1] * exp(l)) /
      (1 + ((exp(l) - 1) * juv_dead[i - 1]))
  }
  
  dif_dead <- rep(NA, 30)
  dif_dead[1] <- s
  for (i in 2:30) dif_dead[i] <- dif_dead[i - 1] / p
  
  total_dead <- juv_dead + dif_dead
  total_dead[total_dead > 1] <- 1
  p_survival <- 1 - total_dead
  
  les <- matrix(0, nrow = 30, ncol = 30)
  les[1, 4:30] <- 1
  for (i in 2:30) les[i, (i - 1)] <- p_survival[(i - 1)]
  les
}

# ------------------------------------------------------------
# biomassa()
# ------------------------------------------------------------
biomassa <- function(x,
                     l_inf = 76.3, l0 = 0.35, k = 0.1375,
                     w0 = 0.000025, a = 0.000103, b = 2.964) {
  n <- length(x)
  ages <- 0:(n - 1)
  Bert <- l0 + l_inf * (1 - exp(ages * (-k)))
  Allo <- w0 + a * Bert^b
  Allo * x
}

# ------------------------------------------------------------
# random_add(): шум по каждой ячейке
# ------------------------------------------------------------
random_add <- function(x, SD_perc = 0) {
  if (SD_perc == 0) return(x)
  x + rnorm(length(x), 0, x * SD_perc / 100)
}


# ============================================================
# Naumov_dynamics(): основная функция
# ============================================================

Naumov_dynamics <- function(
    c = 30,
    Nyears = 50,
    infection_probability = 0.01,
    r = 3,
    x = rep(0, 30),
    plot = TRUE,
    m = 1,
    recruitment = 100000,
    Adult_age = 4,
    SD = 0,
    c_noise = 0,
    Relative_N = FALSE,
    year_drop = 10,
    ...
) {
  
  # ---- Инициализация ----------------------------------------
  
  Lesley_matrix <- lesley()
  
  # Матрицы численности и биомассы: Nyears x 30
  dynamics_N <- matrix(0, nrow = Nyears, ncol = 30)
  dynamics_B <- matrix(0, nrow = Nyears, ncol = 30)
  
  dynamics_N[1, ] <- x
  dynamics_B[1, ] <- biomassa(x)
  
  # Агрегированный пул инфицированных:
  # infected_by_age_year[a, y] = сколько особей возраста a заразились в год y
  # Возраст a = 1..30 (соответствует A0..A29), год y = 1..Nyears
  infected_by_age_year <- matrix(0L, nrow = 30, ncol = Nyears)
  
  # Вектор "эффективного возраста заражения" — храним долю анеуплоидных клеток
  # для каждой пары (возраст, год заражения). Упрощение: доля зависит только
  # от числа лет с момента заражения, поэтому храним вектор по годам:
  # prop_aneuploid[k] = доля через k лет после заражения
  # k = 1..Nyears
  prop_aneuploid <- numeric(Nyears)
  prop_aneuploid[1] <- 0.00001
  
  # Предзаранее считаем, на каком году после заражения доля превысит 0.9
  # prop(k) = 0.00001 * exp(r)^(k-1) >= 0.9
  # => (k-1) * r >= log(0.9 / 0.00001)
  # => k-1 >= log(0.9 / 0.00001) / r
  k_death <- ceiling(log(0.9 / 0.00001) / r) + 1  # год, когда особь погибает
  
  infected_prop <- data.frame(
    Years = 1:Nyears,
    Total_Infection_Prop = rep(NA_real_, Nyears),
    Large_Infection_Prop = NA_real_
  )
  
  # ---- Основной цикл ----------------------------------------
  
  for (i in 2:Nyears) {
    
    # --- Демография -----------------------------------------
    dem_vector_t_1 <- round(dynamics_N[(i - 1), ], 0)
    dem_vector_t   <- round(Lesley_matrix %*% dem_vector_t_1, 0)
    
    dynamics_N[i, ] <- dem_vector_t
    dynamics_N[i, ][dynamics_N[i, ] < 1] <- 0
    dynamics_N[i, ] <- random_add(dynamics_N[i, ], SD_perc = SD)
    dynamics_N[i, ][dynamics_N[i, ] < 1] <- 0
    
    N_i <- sum(dynamics_N[i, ])
    
    # --- Заражение ------------------------------------------
    if (infection_probability != 0 && N_i != 0) {
      
      # Сколько особей каждого возраста заражается в этом году
      # (мультиномиальное распределение = построчный sample по возрастам)
      n_infected_total <- round(N_i * infection_probability, 0)
      
      if (n_infected_total > 0) {
        infected_by_age <- as.vector(
          rmultinom(1, n_infected_total, prob = dynamics_N[i, ])
        )
        # Записываем в пул: возраст = индекс, год заражения = i
        infected_by_age_year[, i] <- infected_by_age_year[, i] + infected_by_age
      }
    }
    
    # --- Старение и гибель инфицированных -------------------
    # К году i в пуле находятся особи, заражённые в годы <= i.
    # Их возраст к году i = (возраст на момент заражения) + (i - год заражения).
    # Особь погибает, если прошло >= k_death лет с момента заражения.
    # Убираем её из пула: сдвигаем счётчики "вниз" по возрасту и стираем
    # тех, кто перешёл порог.
    
    # Считаем "живой" пул: только те, кто заражён не позднее, чем (i - k_death + 1)
    # Для подсчёта долей нам нужно:
    #   - total_alive_infected: всего живых инфицированных к году i
    #   - large_alive_infected: живых инфицированных возраста >= 5 к году i
    
    # Матрица возрастов инфицированных к году i:
    # age_at_year_i = age_at_infection + (i - year_of_infection)
    # Для каждой ячейки (a, y) в infected_by_age_year[, 1:i]:
    #   текущий возраст = a + (i - y)
    #   выжил, если (i - y) < k_death
    
    if (i > 1) {
      # Берём подматрицу заражений до года i включительно
      sub_inf <- infected_by_age_year[, 1:i, drop = FALSE]
      
      # Годы заражения: 1..i
      years_inf <- 1:i
      
      # Матрица текущих возрастов
      age_matrix <- outer(1:30, years_inf, FUN = function(a, y) a + (i - y))
      
      # Матрица "жив ли" — прошло ли меньше k_death лет
      alive_matrix <- outer(1:30, years_inf, FUN = function(a, y) (i - y) < k_death)
      
      alive_infected <- sub_inf * alive_matrix
      
      total_alive_infected <- sum(alive_infected)
      
      # Крупные: текущий возраст >= 5
      large_alive_matrix <- alive_matrix & (age_matrix >= 5)
      large_alive_infected <- sum(sub_inf * large_alive_matrix)
    } else {
      total_alive_infected <- 0
      large_alive_infected <- 0
    }
    
    # --- Удаление мёртвых из демографического вектора ------
    # Мёртвые к году i — те, у кого (i - y) >= k_death, но ещё не были удалены.
    # Проще: те, у кого (i - y) == k_death - 1 ... но так как мы уже удаляли
    # их в предыдущие годы, достаточно удалить тех, кто достигает порога
    # ровно в этом году: (i - y) == k_death - 1? 
    # Нет — надёжнее: удаляем всех, кто "должен умереть к году i", но ещё
    # числится в dynamics_N. Так как удаление идёт по одному разу на особь,
    # удалим тех, у кого (i - y) == k_death - 1 (т.е. в этом году исполняется
    # k_death лет с заражения).
    
    if (i > 1) {
      # Особи, у которых ровно в этом году наступает смерть:
      # (i - y) == k_death - 1  =>  y == i - k_death + 1
      y_death <- i - k_death + 1
      
      if (y_death >= 1 && y_death <= i) {
        # Возраст на момент заражения a; к году i им стало a + (i - y_death)
        # Но в демографическом векторе они числятся по возрасту на год i,
        # то есть по a + (i - y_death).
        dead_this_year <- infected_by_age_year[, y_death]
        
        # Текущие возрасты этих особей:
        current_ages <- (1:30) + (i - y_death)
        
        # Складываем число мёртвых по текущему возрасту
        for (a in 1:30) {
          n_dead <- dead_this_year[a]
          if (n_dead > 0) {
            cur_age <- current_ages[a]
            if (cur_age >= 1 && cur_age <= 30) {
              dynamics_N[i, cur_age] <- dynamics_N[i, cur_age] - n_dead
            }
          }
        }
        dynamics_N[i, ][dynamics_N[i, ] < 1] <- 0
      }
    }
    
    # --- Доли заражённых ------------------------------------
    
    infected_prop$Years[i] <- i
    
    if (infection_probability != 0 && N_i != 0) {
      denom_total <- N_i + total_alive_infected
      infected_prop$Total_Infection_Prop[i] <-
        if (denom_total > 0) total_alive_infected / denom_total else 0
      
      Number_old <- sum(dynamics_N[i, 5:30])
      denom_large <- Number_old + large_alive_infected
      infected_prop$Large_Infection_Prop[i] <-
        if (denom_large > 0) large_alive_infected / denom_large else 0
    } else {
      infected_prop$Total_Infection_Prop[i] <- 0
      infected_prop$Large_Infection_Prop[i] <- 0
    }
    
    # --- Биомасса и пополнение ------------------------------
    dynamics_B[i, ] <- biomassa(dynamics_N[i, ])
    dynamics_B[i, ][dynamics_B[i, ] < 0] <- 0
    
    biom_adult <- sum(dynamics_B[i, (Adult_age + 1):30])
    
    c <- c + rnorm(1, 0, c_noise)
    
    if ((c * biom_adult^m) < recruitment) {
      dynamics_N[i, 1] <- recruitment - (c * biom_adult^m)
    } else {
      dynamics_N[i, 1] <- 0
    }
  }
  
  # ---- Итоговые агрегаты ------------------------------------
  
  dynamics_totalN       <- rowSums(dynamics_N)
  dynamics_totalB       <- rowSums(dynamics_B)
  dynamics_totalB_adult <- rowSums(dynamics_B[, (Adult_age + 1):30])
  dynamics_totalN_Juv   <- rowSums(dynamics_N[, 1:Adult_age])
  
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
  
  # ---- Графики ----------------------------------------------
  
  if (plot) {
    
    library(ggplot2)
    library(cowplot)
    
    pl_N <-
      ggplot(N_total, aes(x = Years, y = Nt)) +
      geom_line(colour = "blue") +
      theme_bw() +
      labs(x = "Время", y = "Численность популяции") +
      theme(axis.title = element_text(size = 10),
            legend.text = element_text(size = 10))
    
    if (infection_probability != 0) {
      pl_infected <-
        ggplot(infected_prop, aes(x = Years, y = Large_Infection_Prop)) +
        geom_line(color = "red") +
        theme_bw() +
        labs(x = "Время", y = "частота зараженных \nкрупных") +
        theme(axis.title = element_text(size = 10),
              legend.text = element_text(size = 10))
      
      pl_infected_total <-
        ggplot(infected_prop, aes(x = Years, y = Total_Infection_Prop)) +
        geom_line(color = "red") +
        theme_bw() +
        labs(x = "Время", y = "частота зараженных \nобщая") +
        theme(axis.title = element_text(size = 10),
              legend.text = element_text(size = 10))
    } else {
      pl_infected <- NULL
      pl_infected_total <- NULL
    }
    
    # Возрастная структура
    dynamics_N_df <- as.data.frame(dynamics_N)
    names(dynamics_N_df) <- paste0("A", 0:29)
    
    dynamics_N_df |>
      mutate(Years = 1:Nyears) |>
      pivot_longer(cols = starts_with("A"),
                   values_to = "Abundance",
                   names_to = "Age") |>
      filter(Age %in% paste0("A", 1:8)) |>
      filter(Years > year_drop) |>
      ggplot(aes(x = Age, y = Abundance)) +
      geom_col() +
      facet_wrap(~Years, ncol = 5, dir = "v", scales = "free_y") +
      theme(strip.text = element_blank(), axis.text = element_blank()) ->
      pl_age_structure
    
    # Связь доли заражённых крупных с долей молоди
    N_total$N_juv <- rowSums(dynamics_N[, 1:3])[-c(1:year_drop)]
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
    
    pl_B <-
      ggplot(B_total, aes(x = Years, y = Bt)) +
      geom_line(colour = "blue") +
      theme_bw() +
      labs(x = "Время", y = "Биомасса") +
      theme(axis.title = element_text(size = 20),
            legend.text = element_text(size = 20))
  }
  
  # ---- Возврат результата -----------------------------------
  
  list(
    dynamics_N,
    dynamics_B,
    N_total,
    dynamics_totalN_Juv,
    B_total,
    dynamics_totalB_adult,
    infected_prop,
    pl_N,
    pl_B
  )
}

# ============================================================
# Пример запуска
# ============================================================

res <- Naumov_dynamics()
res[8]


# ============================================================
# Shiny-приложение
# ============================================================

library(shiny)

ui <- fluidPage(
  
  titlePanel("Динамика обилия моллюсков-фильтраторов"),
  
  sidebarLayout(
    
    # Все ползунки в одном sidebarPanel (верхний левый угол)
    sidebarPanel(
      
      sliderInput(
        inputId = "c",
        label = "Степень отрицательного влияния взрослых на молодь",
        min = -40, max = 40, step = 0.1, value = 30
      ),
      
      sliderInput(
        inputId = "Nyears",
        label = "Продолжительность наблюдений",
        min = 20, max = 80, step = 1, value = 50
      ),
      
      sliderInput(
        inputId = "infection_probability",
        label = "Вероятность заражения",
        min = 0, max = 0.5, step = 0.001, value = 0
      ),
      
      sliderInput(
        inputId = "r",
        label = "Скорость роста анеуплоидии",
        min = 0.1, max = 5, step = 0.01, value = 4
      )
    ),
    
    # Один график справа
    mainPanel(
      plotOutput("my_graph", height = "100vh")
    )
  )
)

server <- function(input, output) {
  
  output$my_graph <- renderPlot({
    
    Naumov_dynamics(
      c = input$c,
      input$Nyears,
      input$infection_probability,
      input$r
    )[[8]]
    
  })
}

shinyApp(ui = ui, server = server)