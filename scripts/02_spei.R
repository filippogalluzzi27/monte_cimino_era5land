library(SPEI)
df_month <- read_csv("data/intermediate/months.csv", col_select = -1) |>
  mutate(month = yearmonth(parse_date(paste(month, "01"), "%Y %b %d", locale = locale("it")))) |>
  as_tsibble(index = month)


lat_sito  <- 42.4 
scale_spei <- c(3, 6, 12)      # mesi di cumulo
ref        <- c(1991, 2020)    # periodo di riferimento per la distribuzione (anni interi)

calcola_spei <- function(temp, prec, lat) {
  pet <- thornthwaite(temp, lat = lat, na.rm = TRUE, verbose = FALSE)    # evapotraspirazione potenziale, mm/mese
  bal <- prec - pet                                                      # bilancio idrico climatico D = P - PET
  s <- lapply(scale_spei, \(k) spei(bal, scale = k, na.rm = TRUE, verbose = FALSE,
                                    ref.start = c(ref[1], 1), ref.end = c(ref[2], 12)))
  setNames(s, paste0("spei_", scale_spei))                               # lista: spei_3, spei_6, spei_12
}


# =============================================================================
s_th <- s   # risultato con Thornthwaite, per il confronto

calcola_spei <- function(tmin, tmax, prec, lat) {
  pet <- hargreaves(tmin, tmax, lat = lat_sito, na.rm = TRUE, verbose = FALSE)   # PET da tmin e tmax, mm/mese
  bal <- prec - pet                                                         # D = P - PET
  s <- lapply(scale_spei, \(k) spei(bal, scale = k, na.rm = TRUE, verbose = FALSE,
                                    ref.start = c(ref[1], 1), ref.end = c(ref[2], 12)))
  setNames(s, paste0("spei_", scale_spei))
}

s <- calcola_spei(as.ts(df_month, value = tmin), as.ts(df_month, value = tmax),
                  as.ts(df_month, value = prec), lat_sito)
df_month[names(s)] <- lapply(s, \(x) as.numeric(x$fitted))

sapply(names(s), \(n) cor(s[[n]]$fitted, s_th[[n]]$fitted, use = "complete.obs"))   # somiglianza tra i due metodi


s <- calcola_spei(as.ts(df_month, value = t2m), as.ts(df_month, value = prec), lat_sito)
df_month[names(s)] <- lapply(s, \(x) as.numeric(x$fitted))   # una colonna per scala, accanto ai dati


# =============================================================================
soglie <- c(-1, -1.5, -2)   # siccità moderata, severa, estrema
df_month |>
  filter(year(month) >= 2000) |>
  pivot_longer(all_of(names(s)), names_to = "scala", values_to = "spei") |>
  mutate(scala  = factor(scala, levels = names(s)),                     # pannelli in ordine 3, 6, 12
         classe = cut(spei, c(-Inf, sort(soglie), Inf),
                      labels = c("estrema", "severa", "moderata", "-"))) |>
  ggplot(aes(x = month, y = spei, fill = classe)) +
  geom_col() +
  geom_hline(yintercept = soglie, linetype = "dashed", linewidth = 0.3) +
  scale_fill_manual(values = c(estrema = "darkred", severa = "red", moderata = "orange", "-" = "grey75")) +
  facet_wrap(~ scala, ncol = 1) +
  labs(x = NULL, y = "SPEI", fill = NULL) +
  scale_x_yearmonth(date_breaks = "2 years", date_labels = "%Y")+
  theme_minimal()
