library(tidyverse); library(tsibble); library(ggtime)

df <- read_csv("data/raw/ERA5Land_giornaliero.csv")
head(df, 10); names(df)

df_ts <- df |> 
  filter(round(lon, 1) == 12.2,
         round(lat, 1) == 42.4,
         between(year(data), 1960, 2025)) |> 
  select(data, t2m, tmin, tmax, td, prec)
head(df_ts); nrow(df_ts)
df_ts <- tsibble::as_tsibble(df_ts)


#==============================================================================
# VPD
es_T   <- function(t)     0.6108 * exp(17.27 * t / (t + 237.3))  # kPa, t in °C
vpd_rh <- function(t, rh) es_T(t) * (1 - rh / 100)               # rh in %
vpd_td <- function(t, td) es_T(t) - es_T(td)                     # td = rugiada, °C

df_ts <- df_ts |>
  mutate(ea      = es_T(td),        
         es      = (es_T(tmax) + es_T(tmin)) / 2, 
         vpd     = pmax(es - ea, 0),    
         vpd_max = pmax(es_T(tmax) - ea, 0))          


#==============================================================================
# Aggregazioni mensili e annuali
max_miss <- 10; max_run  <- 4
df_month <- df_ts |>
  group_by_key() |>
  index_by(month = yearmonth(data)) |>
  summarise(n_ok1 = sum(!is.na(t2m)), 
            run1  = with(rle(is.na(t2m)), max(0, lengths[values])),
            t2m  = mean(t2m, na.rm = TRUE),
            n_ok2 = sum(!is.na(tmin)), 
            run2  = with(rle(is.na(tmin)), max(0, lengths[values])),
            tmin  = mean(tmin, na.rm = TRUE),
            n_ok3 = sum(!is.na(tmax)), 
            run3  = with(rle(is.na(tmax)), max(0, lengths[values])),
            tmax  = mean(tmax, na.rm = TRUE),
            n_ok5 = sum(!is.na(vpd)), 
            run5  = with(rle(is.na(vpd)), max(0, lengths[values])),
            vpd  = mean(vpd, na.rm = TRUE),
            n_ok6 = sum(!is.na(vpd_max)), 
            run6  = with(rle(is.na(vpd_max)), max(0, lengths[values])),
            vpd_max  = mean(vpd_max, na.rm = TRUE),
            n_ok4 = sum(!is.na(prec)),
            prec  = sum(prec, na.rm = TRUE)) |>
  mutate(t2m = if_else(days_in_month(month) - n_ok1 <= max_miss & run1 <= max_run, t2m, NA),
         tmin = if_else(days_in_month(month) - n_ok2 <= max_miss & run2 <= max_run, tmin, NA),
         tmax = if_else(days_in_month(month) - n_ok3 <= max_miss & run3 <= max_run, tmax, NA),
         vpd = if_else(days_in_month(month) - n_ok5 <= max_miss & run5 <= max_run, vpd, NA),
         vpd_max = if_else(days_in_month(month) - n_ok6 <= max_miss & run6 <= max_run, vpd_max, NA),
         prec = if_else(n_ok4 == days_in_month(month), prec, NA))

df_year <- df_month |>
  group_by_key() |>
  index_by(year = year(month)) |>
  summarise(t2m = if_else(n() == 12, mean(t2m), NA),
            tmin = if_else(n() == 12, mean(tmin), NA),
            tmax = if_else(n() == 12, mean(tmax), NA),
            vpd = if_else(n() == 12, mean(vpd), NA),
            vpd_max = if_else(n() == 12, mean(vpd_max), NA),
            prec = if_else(n() == 12, sum(prec), NA))


#==============================================================================

# Trend sugli anni
range(df$data); nrow(df)
summary(df)

vars <- c("t2m", "tmin", "tmax", "vpd", "vpd_max", "prec")
df_year |>
  filter(year>2000) |> 
  pivot_longer(all_of(vars), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = vars)) |>        # pannelli nell'ordine di vars
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "firebrick") +
  facet_wrap(~ variable, ncol = 3, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_minimal()

#==============================================================================
write.csv(df_ts, "data/intermediate/days.csv")
write.csv(df_month, "data/intermediate/months.csv")
write.csv(df_year, "data/intermediate/years.csv")










