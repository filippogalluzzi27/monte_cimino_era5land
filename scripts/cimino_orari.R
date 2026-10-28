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
  index_by(month = yearmonth(data)) |>
  summarise(t2m  = mean(t2m,  na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE),
            tmax = mean(tmax, na.rm = TRUE),
            rh   = mean(rh,   na.rm = TRUE),
            vpd  = mean(vpd,  na.rm = TRUE),
            prec = sum(prec,  na.rm = TRUE))
df_m




# =============================================================================
# SPEI
pet <- hargreaves(as.ts(df_m, value = tmin), as.ts(df_m, value = tmax),
                  lat = lat, na.rm = TRUE, verbose = FALSE)
bal <- as.ts(df_m, value = prec) - pet                         
s   <- spei(bal, scale = scala, na.rm = TRUE, verbose = FALSE,
            ref.start = c(ref[1], 1), ref.end = c(ref[2], 12))

df_m$pet  <- as.numeric(pet)
df_m$spei <- as.numeric(s$fitted)

df_m |>
  ggplot(aes(x = month)) +
  geom_ribbon(aes(ymin = pmin(spei, 0), ymax = 0), fill = "firebrick") +
  geom_ribbon(aes(ymin = 0, ymax = pmax(spei, 0)), fill = "steelblue") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_hline(yintercept = c(-1, -1.5, -2), linetype = "dashed",
             linewidth = 0.2, colour = "grey40") +
  labs(title = paste0("SPEI-", scala), x = NULL, y = "SPEI (z-values)") +
  theme_minimal()

# df_m |>
#   ggplot(aes(x = month)) +
#   geom_ribbon(aes(ymin = pmin(spei_6, 0), ymax = 0), fill = "firebrick") +
#   geom_ribbon(aes(ymin = 0, ymax = pmax(spei_12, 0)), fill = "steelblue") +
#   geom_hline(yintercept = 0, linewidth = 0.3) +
#   geom_hline(yintercept = c(-1, -1.5, -2), linetype = "dashed",
#              linewidth = 0.2, colour = "grey40") +
#   labs(title = "SPEI-6", x = NULL, y = "SPEI-6 (z-values)") +
#   theme_minimal()



# =============================================================================
# SPEI classes for the period 1996-2025
# Classi SPEI → mesi dell'esperimento
df_m <- df_m |> mutate(classe = cut(spei, breaks = soglie, labels = classi))

mesi_classe <- df_m |>
  as_tibble() |>
  filter(month(as.Date(month)) %in% mesi, !is.na(classe)) |>
  select(month, spei, classe)

count(mesi_classe, classe)
print(mesi_classe, n = Inf)


# =============================================================================
# VPD selection
df_m |>
  ggplot(aes(x = month)) +
  geom_ribbon(aes(ymin = pmin(vpd, 0), ymax = 0), fill = "firebrick") +
  geom_ribbon(aes(ymin = 0, ymax = pmax(vpd, 0)), fill = "steelblue") +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_hline(yintercept = c(0, 0.4, 0.8, 1.2, 1.6), linetype = "dashed",
             linewidth = 0.2, colour = "red") +
  labs(title = "VPD", x = NULL, y = "VPD (kPa)") +
  theme_minimal()

df_vpd <- df_m
classes_vpd <- df_vpd |>
  as_tibble() |>
  pivot_longer(starts_with("vpd"), names_to = "scala", values_to = "vpd") |>
  mutate(class = cut(vpd, breaks = c(0, 0.4, 0.8, 1.2, 1.6, +Inf))) |>
  filter(!is.na(class)) |>
  select(scala, month, vpd, class) |>
  arrange(scala, class, month)

print(classes_vpd, n = Inf)
count(classes_vpd, scala, class)




# =============================================================================
# Selection of a "typical day" during extreme drought
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


# =============================================================================
# =============================================================================
# Giorno / notte: elevazione del sole a ogni ora
# ore in UTC → ora solare calcolata dalla longitudine: nessun fuso o ora legale da gestire
# giorno = sole sopra l'orizzonte all'istante del dato (T e RH sono valori istantanei)
elev_sole <- function(t, lat, lon) {
  doy  <- yday(t)
  decl <- 23.45 * sin(2 * pi * (284 + doy) / 365)              # declinazione solare (°)
  B    <- 2 * pi * (doy - 81) / 364
  eot  <- 9.87 * sin(2 * B) - 7.53 * cos(B) - 1.5 * sin(B)     # equazione del tempo (min)
  ora_sol <- hour(t) + minute(t) / 60 + lon / 15 + eot / 60    # ora solare vera (h)
  w    <- 15 * (ora_sol - 12)                                 # angolo orario (°)
  r    <- pi / 180
  asin(sin(lat * r) * sin(decl * r) + cos(lat * r) * cos(decl * r) * cos(w * r)) / r
}

df_h <- df_h |>
  mutate(periodo = if_else(elev_sole(ora_utc, lat, lon) > 0, "giorno", "notte"))


# =============================================================================
# Valori diurni e notturni per classe SPEI (solo i mesi in mesi_classe)
es <- function(t) 0.61094 * exp(17.625 * t / (t + 243.04))

valori_classe <- df_h |>
  as_tibble() |>
  mutate(month = yearmonth(ora_utc)) |>
  inner_join(mesi_classe, by = "month") |>  
  group_by(classe, periodo) |>
  summarise(n_mesi        = n_distinct(month),
            ore_al_giorno = n() / n_distinct(as.Date(ora_utc)),
            t_p10 = quantile(t2m, 0.1), 
            t_p90 = quantile(t2m, 0.9),
            t2m   = mean(t2m),
            rh    = mean(rh),
            vpd   = mean(vpd),
            .groups = "drop") |>
  mutate(rh_da_vpd = 100 * (1 - vpd / es(t2m))) 

valori_classe





