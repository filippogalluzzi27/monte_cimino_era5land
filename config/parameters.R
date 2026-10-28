crs_geo <- 4326
lon <- 12.20; lat <- 42.40
buf <- 500
dlat <- buf / 111320; dlon <- buf / (111320 * cos(lat * pi / 180))
xmin <- lon - dlon; xmax = lon + dlon; ymin = lat - dlat; ymax = lat + dlat
bbox <- sf::st_bbox(c(xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax), crs = crs_geo)
bbox <- sf::st_as_sfc(sf::st_bbox(bbox))

# ERA5-Land
dataset    <- "derived-era5-land-daily-statistics"
variables  <- c("2m_temperature")
statistic  <- c("daily_mean")

start_date <- as.Date("1960-01-01")
end_date   <- as.Date("2025-07-31")
time_zone  <- "utc+01:00" 
frequency  <- "1_hourly"

area   <- c(ymax, xmin, ymin, xmax)
margin <- 0

dir_era     <- "era5land"
workers     <- 2                          # richieste in parallelo
timeout_era <- 3 * 3600                   # secondi massimi di attesa in coda


# CHELSA
pt <- data.frame(lon = lon, lat =  lat)


# SPEI
scala <- 12      # mesi di cumulo
ref        <- c(1991, 2020)
mesi   <- 6:8             # mesi dell'esperimento (giugno–agosto)
soglie <- c(-Inf, -2, -1.5, -1, 1)                        # limiti delle classi
classi <- c("extreme", "severe", "moderate", "normal")  


