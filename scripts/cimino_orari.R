source("config/setup.R")
library(tidyverse); library(tsibble); library(lubridate)
library(SPEI)

# =============================================================================
# # Create hourly dataset from E5L (temp and relative humidity)
# list.files(file.path(dir_raw, "era_land_cimino"))
# files <- list.files(file.path(dir_raw, "era_land_cimino"),
#                     pattern = "^ERA5Land_orario_.*\\.csv$",
#                     full.names = TRUE)
# df_h <- NULL
# for (f in files) {
#   df_h <- rbind(df_h, read_csv(f, show_col_types = FALSE))}
# write.csv(df_h, file.path(dir_intermediate, "era5land_hourly_1960_2025.csv"), row.names = FALSE)


# =============================================================================
# Daily and monthly aggregates + precipitation
prec <- read_csv(file.path(dir_raw, "era_land_cimino/ERA5Land_prec_giornaliera.csv"))
prec <- as_tsibble(prec, index = data)
df_h <- read_csv(file.path(dir_intermediate, "era5land_hourly_1960_2025.csv"))
df_h <- as_tsibble(df_h, index = ora_utc)
head(df_h, 15)

df_d <- df_h |>
  group_by_key() |>
  index_by(data = as.Date(ora_utc)) |>
  summarise(tmax = max(t2m, na.rm =TRUE),
            tmin = min (t2m, na.rm =TRUE),
            t2m  = mean(t2m, na.rm = TRUE),
            rhmin  = min(rh, na.rm = TRUE),
            rhmax = max(rh, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE), 
            vpdmin = min(vpd, na.rm = TRUE),
            vpdmax = max(vpd, na.rm = TRUE),
            vpd = mean(vpd, na.rm = TRUE)) |> 
  left_join(prec, by = "data")
df_d

df_m <- df_d |>
  group_by_key() |>
  index_by(month = yearmonth(data)) |>
  summarise(t2m  = mean(t2m, na.rm = TRUE),                         
            rh  = mean(rh, na.rm = TRUE),
            vpd = mean(vpd, na.rm =TRUE),
            prec = sum(prec, na.rm = TRUE)) 
df_m


# =============================================================================
# SPEI
calcola_spei <- function(temp, prec, lat) {
  pet <- thornthwaite(temp, lat = lat, na.rm = TRUE, verbose = FALSE)
  bal <- prec - pet    
  s <- lapply(scale_spei, \(k) spei(bal, scale = k, na.rm = TRUE, verbose = FALSE,
                                    ref.start = c(ref[1], 1), ref.end = c(ref[2], 12)))
  setNames(s, paste0("spei_", scale_spei))}
s <- calcola_spei(as.ts(df_m, value = t2m), as.ts(df_m, value = prec), lat)
df_m[names(s)] <- lapply(s, \(x) as.numeric(x$fitted))   # una colonna per scala, accanto ai dati
df_m

df_m |>
  filter(year(month) >= 1996) |>
    ggplot(aes(x = month)) +
    geom_ribbon(aes(ymin = pmin(spei_12, 0), ymax = 0), fill = "firebrick") +
    geom_ribbon(aes(ymin = 0, ymax = pmax(spei_12, 0)), fill = "steelblue") +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_hline(yintercept = c(-1, -1.5, -2), linetype = "dashed",
               linewidth = 0.2, colour = "grey40") +
    labs(title = "SPEI-12", x = NULL, y = "SPEI-12 (z-values)") +
    theme_minimal()
df_m |>
  filter(year(month) >= 1996) |>
  ggplot(aes(x = month)) +
  geom_ribbon(aes(ymin = pmin(spei_6, 0), ymax = 0), fill = "firebrick") +
  geom_ribbon(aes(ymin = 0, ymax = pmax(spei_12, 0)), fill = "steelblue") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_hline(yintercept = c(-1, -1.5, -2), linetype = "dashed",
             linewidth = 0.2, colour = "grey40") +
  labs(title = "SPEI-6", x = NULL, y = "SPEI-6 (z-values)") +
  theme_minimal()



# =============================================================================
# SPEI classes for the period 1996-2025
df <- df_m |> 
  filter(between(year(month), 1996, 2025)); df

classes <- df |>
  as_tibble() |>
  pivot_longer(starts_with("spei_"), names_to = "scala", values_to = "spei") |>
  mutate(class = cut(spei, breaks = c(-Inf, -2, -1.5, -1),
                      labels = c("extreme", "severe", "moderate"))) |>
  filter(!is.na(class)) |>
  select(scala, month, spei, class) |>
  arrange(scala, class, month)

print(classes, n = Inf)
count(classes, scala, class)

# =============================================================================
# Selection of a "typical day"
# spei_12 2022 lug
df_jul22 <- df_h |> filter(year(ora_utc) == 2022, month(ora_utc) == 7)
giorno_tipo <- df_jul22 |>
  as_tibble() |>
  mutate(ora = hour(with_tz(ora_utc, "Europe/Rome"))) |>
  group_by(ora) |>
  summarise(across(c(t2m, rh), list(mean = mean, min = min, max = max))) |>
  pivot_longer(-ora, names_to = c("var", ".value"), names_sep = "_")

ggplot(giorno_tipo, aes(x = ora)) +
  geom_ribbon(aes(ymin = min, ymax = max), fill = "grey80") +
  geom_line(aes(y = mean), linewidth = 0.6) +
  facet_wrap(~ var, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(t2m = "Temperature (°C)", rh = "Relative Humidity (%)"))) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "July 2022", x = "Ora", y = NULL) +
  theme_minimal()
plot(df_jul22$ora_utc, df_jul22$t2m, type="l")






