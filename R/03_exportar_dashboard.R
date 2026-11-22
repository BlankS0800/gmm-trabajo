# ==============================================================================
# Módulo 3: Agregación Estadística y Exportación JSON para Dashboard Web
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
})

calcular_matriz_jaccard <- function(df) {
  cols <- c("anom_gmm", "anom_lof", "anom_iforest", "anom_ocsvm", "anom_mahal")
  nombres <- c("GMM", "LOF", "Isolation Forest", "One-Class SVM", "Mahalanobis MCD")
  n <- length(cols)
  mat <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      a <- df[[cols[i]]]
      b <- df[[cols[j]]]
      inter <- sum(a & b)
      uni <- sum(a | b)
      mat[i, j] <- if (uni == 0) 0 else round(100 * inter / uni, 1)
    }
  }
  list(labels = nombres, matrix = mat)
}

exportar_json_dashboard <- function(
    ruta_resultados = "data/bolivia_anomalias_resultados.rds",
    ruta_json = "data/bolivia_anomalias.json"
) {
  res <- readRDS(ruta_resultados)
  df <- res$datos

  resumen_ciudad <- df |>
    dplyr::group_by(ciudad, departamento, piso_ecologico, altitud_m) |>
    dplyr::summarise(
      total = dplyr::n(),
      anom_gmm = sum(anom_gmm),
      anom_lof = sum(anom_lof),
      anom_iforest = sum(anom_iforest),
      anom_ocsvm = sum(anom_ocsvm),
      anom_mahal = sum(anom_mahal),
      anom_consenso = sum(anomalia_consenso),
      tasa_consenso_pct = round(100 * mean(anomalia_consenso), 2),
      temp_max_media = round(mean(temp_max), 2),
      temp_min_media = round(mean(temp_min), 2),
      precip_total_mm = round(sum(precipitacion_mm), 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(anom_consenso))

  resumen_piso <- df |>
    dplyr::group_by(piso_ecologico) |>
    dplyr::summarise(
      total = dplyr::n(),
      anom_gmm = sum(anom_gmm),
      anom_lof = sum(anom_lof),
      anom_iforest = sum(anom_iforest),
      anom_consenso = sum(anomalia_consenso),
      tasa_consenso_pct = round(100 * mean(anomalia_consenso), 2),
      .groups = "drop"
    )

  serie_mensual <- df |>
    dplyr::mutate(mes_fecha = format(fecha, "%Y-%m")) |>
    dplyr::group_by(mes_fecha) |>
    dplyr::summarise(
      total = dplyr::n(),
      anom_gmm = sum(anom_gmm),
      anom_lof = sum(anom_lof),
      anom_iforest = sum(anom_iforest),
      anom_ocsvm = sum(anom_ocsvm),
      anom_mahal = sum(anom_mahal),
      anom_consenso = sum(anomalia_consenso),
      .groups = "drop"
    ) |>
    dplyr::arrange(mes_fecha)

  # Muestra estratificada para el scatter PCA (todas las anomalias de consenso + muestra normal)
  # Para que el navegador sea instantáneo y exacto, incluimos todas las filas con voto >= 1
  # más una muestra representativa de normales (o todas si <= 2500)
  set.seed(42)
  df_anom <- df |> dplyr::filter(votos_anomalia >= 1L)
  df_norm <- df |> dplyr::filter(votos_anomalia == 0L) |>
    dplyr::slice_sample(n = min(1400L, sum(df$votos_anomalia == 0L)))
  df_scatter <- dplyr::bind_rows(df_anom, df_norm) |>
    dplyr::select(
      id, fecha, ciudad, departamento, piso_ecologico,
      temp_max, temp_min, amplitud_termica, precipitacion_mm,
      viento_max_kmh, humedad_pct, presion_hpa,
      pc1, pc2, gmm_cluster, gmm_uncertainty,
      score_gmm, score_lof, score_iforest, score_ocsvm, score_mahal, score_consenso,
      anom_gmm, anom_lof, anom_iforest, anom_ocsvm, anom_mahal,
      votos_anomalia, anomalia_consenso
    )

  top_anomalias <- df |>
    dplyr::filter(votos_anomalia >= 2L) |>
    dplyr::arrange(dplyr::desc(votos_anomalia), dplyr::desc(score_consenso)) |>
    dplyr::slice_head(n = 250) |>
    dplyr::select(
      id, fecha, ciudad, departamento, piso_ecologico,
      temp_max, temp_min, amplitud_termica, precipitacion_mm,
      viento_max_kmh, humedad_pct, presion_hpa,
      gmm_cluster, score_gmm, score_lof, score_iforest, score_ocsvm, score_mahal,
      score_consenso, anom_gmm, anom_lof, anom_iforest, anom_ocsvm, anom_mahal,
      votos_anomalia, anomalia_consenso
    )

  payload <- list(
    meta = list(
      titulo = "Detección de Anomalías Climáticas en Bolivia (GMM, LOF, Isolation Forest, OC-SVM, Mahalanobis MCD)",
      fuente = "Open-Meteo Historical Weather API (ERA5)",
      fecha_inicio = as.character(min(df$fecha)),
      fecha_fin = as.character(max(df$fecha)),
      total_registros = nrow(df),
      total_ciudades = length(unique(df$ciudad)),
      total_anomalias_consenso = sum(df$anomalia_consenso),
      tasa_consenso_pct = round(100 * mean(df$anomalia_consenso), 2),
      pca_var_exp = res$pca_var_exp,
      pca_loadings = as.data.frame(res$pca_loadings)
    ),
    modelos = unname(res$modelos),
    jaccard = calcular_matriz_jaccard(df),
    resumen_ciudad = resumen_ciudad,
    resumen_piso = resumen_piso,
    serie_mensual = serie_mensual,
    scatter_pca = df_scatter,
    top_anomalias = top_anomalias
  )

  jsonlite::write_json(payload, ruta_json, auto_unbox = TRUE, pretty = FALSE)
  message(sprintf("JSON exportado en %s (%.1f KB)", ruta_json, file.info(ruta_json)$size / 1024))
  invisible(payload)
}

if (sys.nframe() == 0) {
  exportar_json_dashboard()
}
