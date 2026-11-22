# ==============================================================================
# Módulo 2: Modelado y Detección de Anomalías Multivariadas en R
# Algoritmos: GMM (mclust), LOF (dbscan), Isolation Forest (solitude),
#             One-Class SVM (e1071) y Mahalanobis Robusta MCD (robustbase)
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(mclust)
  library(dbscan)
  library(solitude)
  library(e1071)
  library(robustbase)
})

preparar_matriz_caracteristicas <- function(df) {
  vars <- c(
    "temp_max",
    "temp_min",
    "precipitacion_mm",
    "viento_max_kmh",
    "humedad_pct",
    "presion_hpa"
  )
  X <- as.matrix(df[, vars])
  X_scaled <- scale(X)
  list(
    vars = vars,
    X_raw = X,
    X_scaled = X_scaled,
    center = attr(X_scaled, "scaled:center"),
    scale = attr(X_scaled, "scaled:scale")
  )
}

ejecutar_gmm <- function(X_scaled, contaminacion = 0.05) {
  t0 <- proc.time()[["elapsed"]]
  set.seed(42)
  modelo <- mclust::Mclust(X_scaled, G = 2:6, verbose = FALSE)
  log_dens <- mclust::dens(
    data = X_scaled,
    modelName = modelo$modelName,
    parameters = modelo$parameters,
    logarithm = TRUE
  )
  score <- -as.numeric(log_dens)
  umbral <- as.numeric(stats::quantile(score, probs = 1 - contaminacion, na.rm = TRUE))
  anom <- score >= umbral
  elapsed <- proc.time()[["elapsed"]] - t0

  list(
    nombre = "GMM",
    modelo = modelo,
    G = modelo$G,
    modelName = modelo$modelName,
    bic = modelo$bic,
    cluster = as.integer(modelo$classification),
    uncertainty = round(as.numeric(modelo$uncertainty), 4),
    score = score,
    umbral = umbral,
    anomalia = anom,
    tiempo_seg = round(elapsed, 3)
  )
}

ejecutar_lof <- function(X_scaled, minPts = 20, contaminacion = 0.05) {
  t0 <- proc.time()[["elapsed"]]
  score <- dbscan::lof(X_scaled, minPts = minPts)
  score[!is.finite(score)] <- max(score[is.finite(score)], na.rm = TRUE)
  umbral <- as.numeric(stats::quantile(score, probs = 1 - contaminacion, na.rm = TRUE))
  anom <- score >= umbral
  elapsed <- proc.time()[["elapsed"]] - t0

  list(
    nombre = "LOF",
    minPts = minPts,
    score = score,
    umbral = umbral,
    anomalia = anom,
    tiempo_seg = round(elapsed, 3)
  )
}

ejecutar_iforest <- function(X_scaled, contaminacion = 0.05) {
  t0 <- proc.time()[["elapsed"]]
  df_x <- as.data.frame(X_scaled)
  iso <- solitude::isolationForest$new(
    sample_size = min(256L, nrow(df_x)),
    num_trees = 200L,
    seed = 42L
  )
  iso$fit(df_x)
  pred <- iso$predict(df_x)
  score <- as.numeric(pred$anomaly_score)
  umbral <- as.numeric(stats::quantile(score, probs = 1 - contaminacion, na.rm = TRUE))
  anom <- score >= umbral
  elapsed <- proc.time()[["elapsed"]] - t0

  list(
    nombre = "Isolation Forest",
    score = score,
    umbral = umbral,
    anomalia = anom,
    tiempo_seg = round(elapsed, 3)
  )
}

ejecutar_ocsvm <- function(X_scaled, contaminacion = 0.05) {
  t0 <- proc.time()[["elapsed"]]
  set.seed(42)
  modelo <- e1071::svm(
    x = X_scaled,
    y = NULL,
    type = "one-classification",
    kernel = "radial",
    nu = contaminacion,
    scale = FALSE
  )
  pred <- stats::predict(modelo, X_scaled, decision.values = TRUE)
  dec_vals <- as.numeric(attr(pred, "decision.values"))
  score <- -dec_vals
  umbral <- 0
  anom <- !as.logical(pred)
  elapsed <- proc.time()[["elapsed"]] - t0

  list(
    nombre = "One-Class SVM",
    score = score,
    umbral = umbral,
    anomalia = anom,
    tiempo_seg = round(elapsed, 3)
  )
}

ejecutar_mahalanobis_mcd <- function(X_scaled, contaminacion = 0.05) {
  t0 <- proc.time()[["elapsed"]]
  set.seed(42)
  mcd <- robustbase::covMcd(X_scaled)
  dist_sq <- stats::mahalanobis(X_scaled, center = mcd$center, cov = mcd$cov)
  score <- sqrt(pmax(dist_sq, 0))
  umbral <- as.numeric(stats::quantile(score, probs = 1 - contaminacion, na.rm = TRUE))
  anom <- score >= umbral
  elapsed <- proc.time()[["elapsed"]] - t0

  list(
    nombre = "Mahalanobis MCD",
    score = score,
    umbral = umbral,
    anomalia = anom,
    tiempo_seg = round(elapsed, 3)
  )
}

ejecutar_pipeline_anomalias <- function(
    ruta_entrada = "data/bolivia_clima_raw.rds",
    ruta_salida = "data/bolivia_anomalias_resultados.rds",
    contaminacion = 0.05
) {
  df <- readRDS(ruta_entrada)
  prep <- preparar_matriz_caracteristicas(df)
  X_scaled <- prep$X_scaled

  message("1/5 Ejecutando GMM (mclust)...")
  res_gmm <- ejecutar_gmm(X_scaled, contaminacion)
  message(sprintf("   -> Modelo óptimo: %s con G=%d componentes (BIC=%.1f) en %.2fs",
                  res_gmm$modelName, res_gmm$G, res_gmm$bic, res_gmm$tiempo_seg))

  message("2/5 Ejecutando LOF (dbscan, minPts=20)...")
  res_lof <- ejecutar_lof(X_scaled, minPts = 20, contaminacion)

  message("3/5 Ejecutando Isolation Forest (solitude)...")
  res_iforest <- ejecutar_iforest(X_scaled, contaminacion)

  message("4/5 Ejecutando One-Class SVM (e1071)...")
  res_ocsvm <- ejecutar_ocsvm(X_scaled, contaminacion)

  message("5/5 Ejecutando Mahalanobis Robusta MCD (robustbase)...")
  res_mahal <- ejecutar_mahalanobis_mcd(X_scaled, contaminacion)

  # Proyección PCA para visualización 2D
  pca <- stats::prcomp(X_scaled, center = FALSE, scale. = FALSE)
  var_exp <- round(100 * (pca$sdev^2 / sum(pca$sdev^2)), 2)

  # Normalizar scores a [0, 1] por cuantiles empíricos para comparabilidad visual
  norm_rank <- function(s) round(rank(s, ties.method = "average") / length(s), 4)

  df_out <- df |>
    dplyr::mutate(
      pc1 = round(pca$x[, 1], 4),
      pc2 = round(pca$x[, 2], 4),
      gmm_cluster = res_gmm$cluster,
      gmm_uncertainty = res_gmm$uncertainty,
      score_gmm = round(res_gmm$score, 4),
      score_lof = round(res_lof$score, 4),
      score_iforest = round(res_iforest$score, 4),
      score_ocsvm = round(res_ocsvm$score, 4),
      score_mahal = round(res_mahal$score, 4),
      score_consenso = round((
        norm_rank(res_gmm$score) +
        norm_rank(res_lof$score) +
        norm_rank(res_iforest$score) +
        norm_rank(res_ocsvm$score) +
        norm_rank(res_mahal$score)
      ) / 5, 4),
      anom_gmm = res_gmm$anomalia,
      anom_lof = res_lof$anomalia,
      anom_iforest = res_iforest$anomalia,
      anom_ocsvm = res_ocsvm$anomalia,
      anom_mahal = res_mahal$anomalia,
      votos_anomalia = as.integer(anom_gmm) + as.integer(anom_lof) +
        as.integer(anom_iforest) + as.integer(anom_ocsvm) + as.integer(anom_mahal),
      anomalia_consenso = votos_anomalia >= 3L
    )

  resultados <- list(
    datos = df_out,
    vars = prep$vars,
    pca_var_exp = var_exp,
    pca_loadings = round(pca$rotation[, 1:2], 4),
    modelos = list(
      gmm = list(
        nombre = res_gmm$nombre,
        G = res_gmm$G,
        modelName = res_gmm$modelName,
        bic = round(res_gmm$bic, 2),
        umbral = round(res_gmm$umbral, 4),
        n_anomalias = sum(res_gmm$anomalia),
        pct_anomalias = round(100 * mean(res_gmm$anomalia), 2),
        tiempo_seg = res_gmm$tiempo_seg
      ),
      lof = list(
        nombre = res_lof$nombre,
        minPts = res_lof$minPts,
        umbral = round(res_lof$umbral, 4),
        n_anomalias = sum(res_lof$anomalia),
        pct_anomalias = round(100 * mean(res_lof$anomalia), 2),
        tiempo_seg = res_lof$tiempo_seg
      ),
      iforest = list(
        nombre = res_iforest$nombre,
        umbral = round(res_iforest$umbral, 4),
        n_anomalias = sum(res_iforest$anomalia),
        pct_anomalias = round(100 * mean(res_iforest$anomalia), 2),
        tiempo_seg = res_iforest$tiempo_seg
      ),
      ocsvm = list(
        nombre = res_ocsvm$nombre,
        umbral = round(res_ocsvm$umbral, 4),
        n_anomalias = sum(res_ocsvm$anomalia),
        pct_anomalias = round(100 * mean(res_ocsvm$anomalia), 2),
        tiempo_seg = res_ocsvm$tiempo_seg
      ),
      mahal = list(
        nombre = res_mahal$nombre,
        umbral = round(res_mahal$umbral, 4),
        n_anomalias = sum(res_mahal$anomalia),
        pct_anomalias = round(100 * mean(res_mahal$anomalia), 2),
        tiempo_seg = res_mahal$tiempo_seg
      )
    )
  )

  saveRDS(resultados, ruta_salida)
  utils::write.csv(df_out, sub("\\.rds$", ".csv", ruta_salida), row.names = FALSE, fileEncoding = "UTF-8")
  message(sprintf("Resultados guardados en %s (%d anomalías de consenso >= 3 votos)",
                  ruta_salida, sum(df_out$anomalia_consenso)))
  resultados
}

if (sys.nframe() == 0) {
  ejecutar_pipeline_anomalias()
}
