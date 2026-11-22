# Detección de Anomalías Multivariadas en Bolivia (GMM, LOF, Isolation Forest, OC-SVM & Mahalanobis MCD)

**Autor:** Univ. Perez Ovando Hernan Yazid  
**Página Web (GitHub Pages):** https://blanks0800.github.io/gmm-trabajo/  
**Repositorio:** https://github.com/BlankS0800/gmm-trabajo

## Descripción
Sistema reproducible en **R 4.6.1** para la adquisición de datos meteorológicos reales de 10 ciudades representativas de los tres pisos ecológicos de Bolivia (Altiplano, Valles y Llanos; periodo 2024–2025, 7 310 observaciones diarias) mediante la API histórica de Open-Meteo (ERA5), aplicación comparativa de cinco algoritmos de detección de anomalías multivariadas y visualización interactiva en **HTML5, CSS3 y JavaScript (Plotly.js)**.

## Estructura del Proyecto
- `R/01_extraccion_bolivia.R`: Descarga y normalización de series climáticas diarias para La Paz, El Alto, Oruro, Potosí, Cochabamba, Sucre, Tarija, Santa Cruz, Trinidad y Cobija.
- `R/02_modelos_anomalias.R`: Ejecución de **GMM** (`mclust`), **LOF** (`dbscan`), **Isolation Forest** (`solitude`), **One-Class SVM** (`e1071`), **Mahalanobis Robusta MCD** (`robustbase`), votación de consenso y proyección PCA 2D.
- `R/03_exportar_dashboard.R`: Cálculo de métricas agregadas, matriz de concordancia de Jaccard y exportación a `data/bolivia_anomalias.json`.
- `index.html`, `styles.css`, `app.js`: Dashboard web estático listo para GitHub Pages.

## Reproducción en R
```r
source("R/01_extraccion_bolivia.R")
source("R/02_modelos_anomalias.R")
source("R/03_exportar_dashboard.R")
```
