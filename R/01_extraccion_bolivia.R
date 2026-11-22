# ==============================================================================
# Módulo 1: Adquisición y Normalización de Datos Climáticos de Bolivia
# Fuente: Open-Meteo Historical Weather API (datos observados/reanalisis ERA5)
# Cobertura: 10 ciudades principales (9 departamentos + El Alto), 2024-2025
# ==============================================================================

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

obtener_estaciones_bolivia <- function() {
  tibble::tribble(
    ~ciudad,        ~departamento, ~piso_ecologico, ~lat,     ~lon,      ~altitud_m,
    "La Paz",       "La Paz",      "Altiplano",       -16.5000, -68.1500,  3640,
    "El Alto",      "La Paz",      "Altiplano",       -16.5100, -68.1800,  4090,
    "Oruro",        "Oruro",       "Altiplano",       -17.9833, -67.1500,  3735,
    "Potosí",       "Potosí",      "Altiplano",       -19.5836, -65.7531,  4067,
    "Cochabamba",   "Cochabamba",  "Valles",          -17.3895, -66.1568,  2574,
    "Sucre",        "Chuquisaca",  "Valles",          -19.0333, -65.2627,  2790,
    "Tarija",       "Tarija",      "Valles",          -21.5355, -64.7296,  1854,
    "Santa Cruz",   "Santa Cruz",  "Llanos",          -17.7833, -63.1821,  416,
    "Trinidad",     "Beni",        "Llanos",          -14.8333, -64.9000,  155,
    "Cobija",       "Pando",       "Llanos",          -11.0267, -68.7692,  235
  )
}

descargar_ciudad_openmeteo <- function(fila, start_date = "2024-01-01", end_date = "2025-12-31") {
  url <- "https://archive-api.open-meteo.com/v1/archive"
  req <- httr2::request(url) |>
    httr2::req_url_query(
      latitude = fila$lat,
      longitude = fila$lon,
      start_date = start_date,
      end_date = end_date,
      daily = paste(
        c(
          "temperature_2m_max",
          "temperature_2m_min",
          "precipitation_sum",
          "wind_speed_10m_max",
          "relative_humidity_2m_mean",
          "surface_pressure_mean"
        ),
        collapse = ","
      ),
      timezone = "America/La_Paz"
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3)

  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) != 200) {
    stop("Error HTTP al consultar Open-Meteo para ", fila$ciudad)
  }

  raw_json <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  daily <- raw_json$daily

  tibble::tibble(
    fecha = as.Date(daily$time),
    ciudad = fila$ciudad,
    departamento = fila$departamento,
    piso_ecologico = fila$piso_ecologico,
    altitud_m = fila$altitud_m,
    lat = fila$lat,
    lon = fila$lon,
    temp_max = as.numeric(daily$temperature_2m_max),
    temp_min = as.numeric(daily$temperature_2m_min),
    amplitud_termica = as.numeric(daily$temperature_2m_max) - as.numeric(daily$temperature_2m_min),
    precipitacion_mm = as.numeric(daily$precipitation_sum),
    viento_max_kmh = as.numeric(daily$wind_speed_10m_max),
    humedad_pct = as.numeric(daily$relative_humidity_2m_mean),
    presion_hpa = as.numeric(daily$surface_pressure_mean)
  )
}

recolectar_datos_bolivia <- function(
    start_date = "2024-01-01",
    end_date = "2025-12-31",
    ruta_salida = "data/bolivia_clima_raw.rds"
) {
  dir.create(dirname(ruta_salida), recursive = TRUE, showWarnings = FALSE)
  estaciones <- obtener_estaciones_bolivia()

  lista_df <- lapply(seq_len(nrow(estaciones)), function(i) {
    fila <- estaciones[i, ]
    message(sprintf("[%d/%d] Descargando %s (%s)...", i, nrow(estaciones), fila$ciudad, fila$piso_ecologico))
    Sys.sleep(0.25)
    descargar_ciudad_openmeteo(fila, start_date = start_date, end_date = end_date)
  })

  df <- dplyr::bind_rows(lista_df) |>
    dplyr::mutate(
      id = sprintf("%s_%s", gsub("[^A-Za-z0-9]", "", ciudad), format(fecha, "%Y%m%d")),
      anio = lubridate::year(fecha),
      mes = lubridate::month(fecha)
    ) |>
    tidyr::drop_na(temp_max, temp_min, precipitacion_mm, viento_max_kmh, humedad_pct, presion_hpa)

  saveRDS(df, ruta_salida)
  utils::write.csv(df, sub("\\.rds$", ".csv", ruta_salida), row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("Dataset guardado: %d registros en %s", nrow(df), ruta_salida))
  df
}

if (sys.nframe() == 0) {
  recolectar_datos_bolivia()
}
