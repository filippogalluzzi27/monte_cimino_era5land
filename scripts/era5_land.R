source("config/setup.R")


# ===========================================================================
# CHELSA
tas <- getChelsa("tas", 
                 extent = c(xmin, xmax, ymin, ymax),
                 startdate = start_date, 
                 enddate = end_date, 
                 dataset = "chelsa-daily")

# ===========================================================================
# E5L
area_dl  <- area + c(1, -1, -1, 1) * margin
days     <- seq(start_date, end_date, by = "day")
months   <- split(days, format(days, "%Y_%m"))
req_grid <- expand.grid(v = variables, m = names(months), stringsAsFactors = FALSE)

requests <- lapply(seq_len(nrow(req_grid)), function(i) {
  v <- req_grid$v[i]; d <- months[[req_grid$m[i]]]
  list(
    dataset_short_name = dataset,
    variable        = v,
    year            = format(d[1], "%Y"),
    month           = format(d[1], "%m"),
    day             = format(d, "%d"),
    daily_statistic = statistic,
    time_zone       = time_zone,
    frequency       = frequency,
    area            = area_dl,
    target          = sprintf("era5land_%s_%s_%s.zip", v, statistic, req_grid$m[i])
  )
})

zips <- wf_request_batch(requests, workers = workers, path = dir_era, time_out = timeout_era)
file <- unlist(lapply(zips, function(z) unzip(z, exdir = sub("\\.zip$", "", z))))

temp <- rast(file.path(dir_era, "era5land_2m_temperature_daily_mean_1960_01.nc"))
temp
plot(temp$t2m_31)


