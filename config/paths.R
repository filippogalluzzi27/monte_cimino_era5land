library(here); library(fs)

# Root del progetto
dir_project <- here()

# Dati
dir_data <- "B:/dati/monte_cimino"

dir_raw <- here(dir_data, "raw")
dir_intermediate <- here(dir_data, "intermediate")
dir_processed <- here(dir_data, "processed")

# Output
dir_outputs <- here("outputs")

# ===========================================================================
dir_era <- here(dir_raw, "era5land")
dir.create(dir_era, showWarnings = FALSE, recursive = TRUE)
#era_path <- file.path(dir_era, "era5land_2m_temperature_2021_08.nc")