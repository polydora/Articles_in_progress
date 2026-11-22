#Тест на основе gls фрагмент цикла, который надо запустить!
# Без учета автокорреляций!

library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(doParallel)
library(doRNG)
library(ggdist)     # stat_halfeye, stat_interval
library(patchwork)

# 2. Parameters   -----------

setwd('/media/sergey/Data/Science/Kamtshatka/spruce/Article1_2025/RScripts/Vadim') # Set working directory

fig.dir <- "/media/sergey/Data/Science/Kamtshatka/spruce/Article1_2025/figures_final/" # Output directory for figures and result tables

# 3. Load and prepare data   -----------

# 3.1. Load climate data
# Annual climate variables; the analysis is restricted to the period covered by the NDVI time series (2000-2023)

clim <- read_excel("/media/sergey/Data/Science/Kamtshatka/spruce/clim/Clim_upd_3.xlsx")
clim$Year <- as.numeric(clim$Year)
clim <- clim %>% filter(Year > 1999 & Year < 2024)

# 3.3. Load NDVI time series for the random points distributed within the spruce forest cover of the study area
# Only the annual maximum NDVI (ndvi.max) is used as the vegetation index in this study

ndvi <-
  read.csv("/media/sergey/Data/Science/Kamtshatka/spruce/Article1_2025/RScripts/Vadim/random_points/picea.gsdt.ndvi.csv", sep = ",") %>%
  select(sample.id, year, ndvi.max) %>%
  rename(Year = year) %>%
  filter(Year > 1999 & Year < 2024) %>%
  rename(Plot = sample.id)



set.seed(12345)
resample_n <- 1000
min_n      <- 20      # минимум точек на год внутри итерации
min_years  <- 8       # минимум лет для подгонки модели

years_full <- intersect(unique(clim$Year), unique(ndvi$Year))
sites_full <- unique(ndvi$Plot)
year_num   <- round(length(years_full) * 0.9)
site_num   <- round(length(sites_full) * 0.25)
clim_metric <- setdiff(colnames(clim), "Year")

# Функция диагностики автокорреляции остатков модели
resid_diag <- function(m, years, max_lag = 5, n_ar = 1) {
  r <- residuals(m, type = "normalized")
  
  # ряд с NA на месте пропущенных лет
  ord  <- order(years)
  full <- rep(NA_real_, diff(range(years)) + 1)
  full[years[ord] - min(years) + 1] <- r[ord]
  
  a <- acf(full, lag.max = max_lag, na.action = na.pass, plot = FALSE)$acf[-1]
  N <- sum(!is.na(full))
  k <- seq_along(a)
  
  # Ljung–Box вручную
  Q    <- N * (N + 2) * sum(a^2 / (N - k))
  lb_p <- pchisq(Q, df = max_lag - n_ar, lower.tail = FALSE)
  
  kmax <- which.max(abs(a))
  phi  <- if (!is.null(m$modelStruct$corStruct))
    coef(m$modelStruct$corStruct, unconstrained = FALSE) else NA
  
  c(acf1     = a[1],
    acf_max  = a[kmax],
    lag_max  = kmax,
    any_sig  = as.numeric(any(abs(a) > 1.96 / sqrt(N))),
    lb_p     = lb_p,
    sw_p     = shapiro.test(r)$p.value,
    phi      = unname(phi))
}

one_iter1 <- function(param_name) {
  years <- sample(years_full, year_num)
  sites <- sample(sites_full, site_num)
  
  d_year <- ndvi |>
    filter(Plot %in% sites, Year %in% years) |>
    group_by(Plot) |>
    mutate(ndvi_anom = ndvi.max - mean(ndvi.max, na.rm = TRUE)) |>
    group_by(Year) |>
    summarise(ndvi_anom = mean(ndvi_anom, na.rm = TRUE),
              n = n(), .groups = "drop") |>
    filter(n >= min_n) |>
    inner_join(clim |> select(Year, clim_param = all_of(param_name)),
               by = "Year") |>
    filter(complete.cases(pick(everything()))) |>
    arrange(Year)
  
  if (nrow(d_year) < min_years) return(NULL)
  
  m <- tryCatch(
    gls(scale(ndvi_anom) ~ scale(clim_param),
        data = d_year,
        method = "ML"),
    error = function(e) NULL)
  if (is.null(m)) return(NULL)
  
  tt <- summary(m)$tTable
  rd <- resid_diag(m, d_year$Year, max_lag = 5, n_ar = 1)
  data.frame(
    clim_param = param_name,
    n_years    = nrow(d_year),
    coef       = tt["scale(clim_param)", "Value"],
    se         = tt["scale(clim_param)", "Std.Error"],
    p          = tt["scale(clim_param)", "p-value"],
    #phi        = coef(m$modelStruct$corStruct, unconstrained = FALSE),
    t(as.data.frame(rd))
  )
  
}

cl <- makeCluster(detectCores() - 2)
registerDoParallel(cl)

results_gls_test_lm <- foreach(
  k = seq_along(clim_metric), .combine = rbind,
  .packages = c("dplyr", "nlme"),
  .export = c("one_iter1", "resid_diag", "ndvi", "clim", "years_full", "sites_full",
              "year_num", "site_num", "min_n", "min_years")
) %dorng% {
  bind_rows(lapply(seq_len(resample_n), function(i) one_iter1(clim_metric[k])))
}
stopCluster(cl)

##### Пробуем визуализировать #######################

df <- results_gls_test_lm %>%
  #select(-phi.1) %>%
  mutate(
    sig      = p < 0.05,
    lb_bad   = lb_p < 0.05,
    acf_crit = 1.96 / sqrt(n_years)
  )

# сводка по параметрам
summ <- df %>%
  group_by(clim_param) %>%
  summarise(
    med     = median(coef),
    p_sig   = mean(p < 0.05),
    p_pos   = mean(coef > 0),
    lb_bad  = mean(lb_p < 0.05),
    any_sig = mean(any_sig),
    sw_bad  = mean(sw_p < 0.05),
    #phi_med = median(phi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(med)

# единый порядок параметров на всех графиках (по медиане эффекта)
lev <- summ$clim_param
df$clim_param   <- factor(df$clim_param, levels = lev)
summ$clim_param <- factor(summ$clim_param, levels = lev)

p_coef <- ggplot(df, aes(x = coef, y = clim_param)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  stat_halfeye(aes(fill = after_stat(x > 0)),
               .width = c(0.66, 0.95), point_interval = median_qi,
               slab_alpha = 0.6, normalize = "groups") +
  geom_text(data = summ,
            aes(x = Inf, y = clim_param,
                label = sprintf("%.0f%% p<0.05", 100 * p_sig)),
            hjust = 1.05, vjust = -0.6, size = 3, inherit.aes = FALSE) +
  scale_fill_manual(values = c(`TRUE` = "#d6604d", `FALSE` = "#4393c3"),
                    guide = "none") +
  labs(x = "Коэффициент GLS", y = NULL,
       title = "Распределение оценок эффекта по итерациям Монте-Карло") +
  theme_minimal(base_size = 12)
p_coef

p_ecdf <- ggplot(df, aes(x = p, colour = clim_param)) +
  stat_ecdf(linewidth = 0.8) +
  geom_vline(xintercept = 0.05, linetype = 2) +
  scale_x_log10() +
  labs(x = "p-значение (log)", y = "Доля итераций", colour = NULL,
       title = "Кумулятивное распределение p-значений") +
  theme_minimal(base_size = 12)
p_ecdf

p_share <- df %>%
  mutate(cat = case_when(p < 0.05 & coef > 0 ~ "значим, +",
                         p < 0.05 & coef < 0 ~ "значим, −",
                         TRUE                ~ "незначим")) %>%
  count(clim_param, cat) %>%
  group_by(clim_param) %>% mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = clim_param, fill = cat)) +
  geom_col() +
  scale_fill_manual(values = c("значим, +" = "#d6604d",
                               "значим, −" = "#4393c3",
                               "незначим"  = "grey80")) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = NULL,
       title = "Доля значимых итераций") +
  theme_minimal(base_size = 12)
p_share


p_phi <- ggplot(df, aes(x = phi, y = clim_param)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  stat_halfeye(.width = c(0.66, 0.95), fill = "#8073ac",
               slab_alpha = 0.6, normalize = "groups") +
  labs(x = expression(phi ~ "(AR1)"), y = NULL,
       title = "Оценки автокорреляции в моделях") +
  theme_minimal(base_size = 12)

crit <- 1.96 / sqrt(median(df$n_years))

p_acf <- df %>%
  select(clim_param, acf1, acf_max) %>%
  pivot_longer(-clim_param, names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, acf1 = "ACF, лаг 1",
                         acf_max = "ACF, максимальный |r|")) %>%
  ggplot(aes(x = value, y = clim_param)) +
  annotate("rect", xmin = -crit, xmax = crit, ymin = -Inf, ymax = Inf,
           fill = "grey90") +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_boxplot(fill = "#5aae61", alpha = 0.6) +
  stat_summary(fun = median, geom = "point", size = 2) +
  facet_wrap(~ metric) +
  labs(x = "Автокорреляция остатков", y = NULL,
       title = "ACF нормализованных остатков",
       subtitle = sprintf("Серая полоса: ±%.2f (N = %d)",
                          crit, median(df$n_years))) +
  theme_minimal(base_size = 12)
p_acf
crit <- 1.96 / sqrt(median(df$n_years))

p_acf <- df %>%
  select(clim_param, acf1, acf_max) %>%
  pivot_longer(-clim_param, names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric, acf1 = "ACF, лаг 1",
                         acf_max = "ACF, максимальный |r|")) %>%
  ggplot(aes(x = value, y = clim_param)) +
  annotate("rect", xmin = -crit, xmax = crit, ymin = -Inf, ymax = Inf,
           fill = "grey90") +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_violin(fill = "#5aae61", alpha = 0.6, scale = "width") +
  stat_summary(fun = median, geom = "point", size = 2) +
  facet_wrap(~ metric) +
  labs(x = "Автокорреляция остатков", y = NULL,
       title = "ACF нормализованных остатков",
       subtitle = sprintf("Серая полоса: ±%.2f (N = %d)",
                          crit, median(df$n_years))) +
  theme_minimal(base_size = 12)
p_acf
p_lag <- df %>%
  count(clim_param, lag_max) %>%
  group_by(clim_param) %>% mutate(share = n / sum(n)) %>%
  ggplot(aes(x = share, y = clim_param, fill = factor(lag_max))) +
  geom_col() +
  scale_fill_brewer(palette = "YlGnBu", name = "Лаг") +
  scale_x_continuous(labels = scales::percent) +
  labs(x = NULL, y = NULL, title = "Лаг с максимальной |ACF|") +
  theme_minimal(base_size = 12)
p_lag
p_lb <- ggplot(df, aes(x = lb_p)) +
  geom_histogram(aes(y = after_stat(density)), breaks = seq(0, 1, 0.05),
                 fill = "#4393c3", colour = "white") +
  geom_hline(yintercept = 1, colour = "red", linetype = 2) +
  facet_wrap(~ clim_param) +
  labs(x = "p Ljung–Box", y = "Плотность",
       title = "Распределение p-значений Ljung–Box") +
  theme_minimal(base_size = 11)
p_lb
p_heat <- summ %>%
  select(clim_param, `Ljung–Box p<0.05` = lb_bad,
         `Хоть один лаг вне границ` = any_sig,
         `Shapiro p<0.05` = sw_bad) %>%
  pivot_longer(-clim_param, names_to = "test", values_to = "share") %>%
  ggplot(aes(x = test, y = clim_param, fill = share)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = scales::percent(share, accuracy = 1)), size = 3.5) +
  scale_fill_gradient2(low = "white", mid = "#fee08b", high = "#d73027",
                       midpoint = 0.10, limits = c(0, NA),
                       labels = scales::percent) +
  labs(x = NULL, y = NULL, fill = "Доля\nитераций",
       title = "Доля итераций с проблемами в остатках") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_sens <- ggplot(df, aes(x = coef, y = clim_param, fill = lb_bad)) +
  geom_vline(xintercept = 0, linetype = 2, colour = "grey50") +
  geom_boxplot(outlier.size = 0.4, alpha = 0.7) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "#d73027"),
                    labels = c("Ljung–Box OK", "Ljung–Box p<0.05"),
                    name = NULL) +
  labs(x = "Коэффициент", y = NULL,
       title = "Оценки эффекта при адекватных и проблемных остатках") +
  theme_minimal(base_size = 12)

p_coef_phi <- ggplot(df, aes(x = phi, y = coef)) +
  geom_hex(bins = 40) +
  geom_smooth(method = "loess", colour = "red", se = FALSE) +
  facet_wrap(~ clim_param, scales = "free_y") +
  scale_fill_viridis_c(trans = "log10") +
  labs(x = expression(phi), y = "Коэффициент",
       title = "Связь оценки эффекта с оценкой AR1") +
  theme_minimal(base_size = 11)

fig_effects <- p_coef | p_share
fig_acf     <- (p_phi | p_acf) / (p_lag | p_heat)

fig_effects
fig_acf
ggsave("fig_effects1.png",  fig_effects, width = 13, height = 6,  dpi = 300)
ggsave("fig_autocorr1.png", fig_acf,     width = 14, height = 10, dpi = 300)
ggsave("fig_lb1.png",       p_lb,        width = 10, height = 8,  dpi = 300)
ggsave("fig_sens1.png",     p_sens / p_coef_phi + plot_layout(heights = c(1, 2)),
       width = 11, height = 12, dpi = 300)


####### Оформление таблиц с индикацией 

library(dplyr)
library(gt)

n_iter <- df %>% count(clim_param) %>% pull(n) %>% median()
K_lags <- max(df$lag_max, na.rm = TRUE)   # лучше задать явно: сколько лагов проверялось

lim_5   <- qbinom(0.975, n_iter, 0.05) / n_iter      # порог для LB и SW
exp_any <- 1 - 0.95^K_lags                            # ожидаемая доля any_sig
lim_any <- qbinom(0.975, n_iter, exp_any) / n_iter

mode_val <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

diag_tab <- df %>%
  group_by(clim_param) %>%
  summarise(
    N         = median(n_years),
    # --- эффект
    coef_m    = median(coef),
    coef_lo   = quantile(coef, 0.025),
    coef_hi   = quantile(coef, 0.975),
    sig       = mean(p < 0.05),
    sign_cons = max(mean(coef > 0), mean(coef < 0)),
    # --- автокорреляция
    phi_m     = median(phi, na.rm = TRUE),
    phi_lo    = quantile(phi, 0.25, na.rm = TRUE),
    phi_hi    = quantile(phi, 0.75, na.rm = TRUE),
    acf1_m    = median(acf1),
    acfmax_m  = median(acf_max),
    lag_mode  = mode_val(lag_max),
    lag_share = mean(lag_max == lag_mode),
    # --- тесты
    any_bad   = mean(any_sig),
    lb_bad    = mean(lb_p < 0.05),
    sw_bad    = mean(sw_p < 0.05),
    # --- чувствительность: значимость при «хороших» остатках
    sig_lbok  = mean(p[lb_p >= 0.05] < 0.05),
    .groups = "drop"
  ) %>%
  mutate(
    delta_sig = sig_lbok - sig,
    verdict = case_when(
      lb_bad > lim_5 & any_bad > lim_any ~ "AR(1) недостаточно",
      lb_bad > lim_5                     ~ "остаточная автокорр.",
      sw_bad > lim_5                     ~ "ненормальность",
      TRUE                               ~ "OK"
    )
  ) %>%
  arrange(desc(sig))

tab <- diag_tab %>%
  gt(rowname_col = "clim_param") %>%
  # интервалы объединяем в одну ячейку
  cols_merge(c(coef_m, coef_lo, coef_hi), pattern = "{1} [{2}; {3}]") %>%
  cols_merge(c(phi_m, phi_lo, phi_hi),    pattern = "{1} [{2}–{3}]") %>%
  cols_merge(c(lag_mode, lag_share),      pattern = "{1} ({2})") %>%
  fmt_number(c(coef_m, coef_lo, coef_hi, phi_m, phi_lo, phi_hi,
               acf1_m, acfmax_m), decimals = 2) %>%
  fmt_percent(c(sig, sign_cons, lag_share, any_bad, lb_bad, sw_bad,
                sig_lbok), decimals = 0) %>%
  fmt_percent(delta_sig, decimals = 0, force_sign = TRUE) %>%
  fmt_number(N, decimals = 0) %>%
  tab_spanner("Эффект",            c(coef_m, sig, sign_cons)) %>%
  tab_spanner("Автокорреляция",    c(phi_m, acf1_m, acfmax_m, lag_mode)) %>%
  tab_spanner("Доля срабатываний", c(any_bad, lb_bad, sw_bad)) %>%
  tab_spanner("Устойчивость",      c(sig_lbok, delta_sig, verdict)) %>%
  cols_label(
    N = "N лет", coef_m = "β [95%]", sig = "p<0.05", sign_cons = "Знак",
    phi_m = "φ [IQR]", acf1_m = "ACF₁", acfmax_m = "ACFmax",
    lag_mode = "Лаг (доля)", any_bad = "ACF вне границ",
    lb_bad = "Ljung–Box", sw_bad = "Shapiro",
    sig_lbok = "p<0.05 | LB ok", delta_sig = "Δ", verdict = "Вывод"
  ) %>%
  # подсветка превышений
  tab_style(cell_fill("#fdd0a2"),
            cells_body(lb_bad,  rows = lb_bad  > lim_5)) %>%
  tab_style(cell_fill("#fdd0a2"),
            cells_body(sw_bad,  rows = sw_bad  > lim_5)) %>%
  tab_style(cell_fill("#fdd0a2"),
            cells_body(any_bad, rows = any_bad > lim_any)) %>%
  tab_style(cell_text(weight = "bold"),
            cells_body(sig, rows = sig >= 0.5)) %>%
  tab_source_note(sprintf(
    "Итераций на параметр: %d. Подсвечены доли выше 97.5%% квантиля биномиального ожидания: %.1f%% для LB и SW, %.0f%% для ACF (K = %d лагов). Δ — изменение доли значимых итераций при исключении итераций с LB p<0.05.",
    n_iter, 100 * lim_5, 100 * lim_any, K_lags)) %>%
  tab_options(table.font.size = 11, data_row.padding = px(2))

tab
gtsave(tab, "diag_table1.html")
#gtsave(tab, "diag_table1.docx")   # требует gt >= 0.10

diag_tab %>%
  select(clim_param, coef_m, coef_lo, coef_hi, sig, phi_m, lb_bad, verdict) %>%
  gt(rowname_col = "clim_param") %>%
  cols_merge(c(coef_m, coef_lo, coef_hi), pattern = "{1} [{2}; {3}]") %>%
  fmt_number(c(coef_m, coef_lo, coef_hi, phi_m), decimals = 2) %>%
  fmt_percent(c(sig, lb_bad), decimals = 0) %>%
  cols_label(coef_m = "β [95%]", sig = "p<0.05", phi_m = "φ",
             lb_bad = "LB p<0.05", verdict = "Остатки") %>%
  tab_style(cell_fill("#fdd0a2"),
            cells_body(lb_bad, rows = lb_bad > lim_5))