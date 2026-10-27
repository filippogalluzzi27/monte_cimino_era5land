df <- read_csv("data/raw/ERA5Land_t2m_prec_giornaliero.csv")
df_ts <- tsibble::as_tsibble(df)
max_miss <- 0   # giorni mancanti ammessi
max_run  <- 0    # giorni mancanti consecutivi ammessi
df_month <- df_ts |>
  group_by_key() |>
  index_by(month = yearmonth(data)) |>
  summarise(n_ok1 = sum(!is.na(t2m)),                                # giorni con dato, PRIMA di sovrascrivere var1
            run1  = with(rle(is.na(t2m)), max(0, lengths[values])),  # buco consecutivo più lungo
            n_ok2 = sum(!is.na(prec)),
            var1  = mean(t2m, na.rm = TRUE),                         # stato  => media
            var2  = sum(prec, na.rm = TRUE)) |>                       # flusso => somma
  mutate(var1 = if_else(days_in_month(month) - n_ok1 <= max_miss & run1 <= max_run, var1, NA),
         var2 = if_else(n_ok2 == days_in_month(month), var2, NA))     # flusso: solo mesi completi


nome_sito <- "sito"                                        # prefisso dei file salvati
lat_sito  <- 44.42                                         # gradi decimali (serie puntuale)

# Indice
scale_spei <- c(3, 6, 12)      # mesi di cumulo
ref        <- c(1991, 2020)    # periodo di riferimento per la distribuzione (anni interi)


library(SPEI)

calcola_spei <- function(temp, prec, lat) {
  pet <- thornthwaite(temp, lat = lat, na.rm = TRUE, verbose = FALSE)    # evapotraspirazione potenziale, mm/mese
  bal <- prec - pet                                                      # bilancio idrico climatico D = P - PET
  s <- lapply(scale_spei, \(k) spei(bal, scale = k, na.rm = TRUE, verbose = FALSE,
                                    ref.start = c(ref[1], 1), ref.end = c(ref[2], 12)))
  setNames(s, paste0("spei_", scale_spei))                               # lista: spei_3, spei_6, spei_12
}


s <- calcola_spei(as.ts(df_month, value = var1), as.ts(df_month, value = var2), lat_sito)

df_month[names(s)] <- lapply(s, \(x) as.numeric(x$fitted))   # una colonna per scala, accanto ai dati
plot(s$spei_3)

names(s)

write_csv(df_month |> as_tibble() |> mutate(month = as.Date(month)),     # mese come data (giorno 1)
          file.path(dir_out, paste0(nome_sito, "_spei.csv")))


