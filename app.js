let DATA = null;

const ALGO_COL = {
  consenso: "anomalia_consenso",
  gmm: "anom_gmm",
  lof: "anom_lof",
  iforest: "anom_iforest",
  ocsvm: "anom_ocsvm",
  mahal: "anom_mahal"
};

const ALGO_LABEL = {
  consenso: "Consenso (>= 3 votos)",
  gmm: "GMM (Gaussian Mixture)",
  lof: "LOF (Local Outlier Factor)",
  iforest: "Isolation Forest",
  ocsvm: "One-Class SVM",
  mahal: "Mahalanobis MCD"
};

const PACKAGES_MAP = {
  "GMM": { pkg: "mclust", desc: "Mezcla gaussiana multivariada, selección BIC (G=2..6)" },
  "LOF": { pkg: "dbscan", desc: "Densidad local relativa con k=20 vecinos (minPts=20)" },
  "Isolation Forest": { pkg: "solitude", desc: "200 árboles de aislamiento, submuestra=256" },
  "One-Class SVM": { pkg: "e1071", desc: "Kernel RBF radial, frontera nu=0.05" },
  "Mahalanobis MCD": { pkg: "robustbase", desc: "Estimador robusto Minimum Covariance Determinant" }
};

document.addEventListener("DOMContentLoaded", async () => {
  setupTabs();
  try {
    const resp = await fetch("data/bolivia_anomalias.json");
    if (!resp.ok) throw new Error("No se pudo cargar data/bolivia_anomalias.json");
    DATA = await resp.json();
    initDashboard();
  } catch (err) {
    document.getElementById("meta-periodo").textContent = "Error al cargar datos";
    console.error(err);
  }
});

function setupTabs() {
  const buttons = document.querySelectorAll(".tab-btn");
  const panels = document.querySelectorAll(".tab-panel");
  const activate = (tabName) => {
    buttons.forEach((b) => {
      const isMatch = b.dataset.tab === tabName;
      b.classList.toggle("active", isMatch);
      b.setAttribute("aria-selected", isMatch ? "true" : "false");
    });
    panels.forEach((p) => p.classList.toggle("active", p.id === `tab-${tabName}`));
    window.dispatchEvent(new Event("resize"));
  };
  buttons.forEach((btn) => btn.addEventListener("click", () => activate(btn.dataset.tab)));
  const urlTab = new URLSearchParams(window.location.search).get("tab");
  if (urlTab === "comparativa") activate("comparativa");
}

function initDashboard() {
  const { meta, resumen_ciudad } = DATA;
  document.getElementById("meta-periodo").textContent =
    `${meta.fecha_inicio} a ${meta.fecha_fin} · ${meta.total_registros.toLocaleString("es-BO")} obs.`;

  const selCiudad = document.getElementById("filtro-ciudad");
  resumen_ciudad
    .map((c) => c.ciudad)
    .sort()
    .forEach((ciudad) => {
      const opt = document.createElement("option");
      opt.value = ciudad;
      opt.textContent = ciudad;
      selCiudad.appendChild(opt);
    });

  ["filtro-piso", "filtro-ciudad", "filtro-algoritmo", "filtro-buscar"].forEach((id) => {
    const el = document.getElementById(id);
    el.addEventListener("input", renderExplorador);
    el.addEventListener("change", renderExplorador);
  });

  renderExplorador();
  renderComparativa();
}

function getFiltros() {
  return {
    piso: document.getElementById("filtro-piso").value,
    ciudad: document.getElementById("filtro-ciudad").value,
    algo: document.getElementById("filtro-algoritmo").value,
    buscar: document.getElementById("filtro-buscar").value.trim().toLowerCase()
  };
}

function renderExplorador() {
  const f = getFiltros();
  const col = ALGO_COL[f.algo];

  // KPIs
  const ciudadesFiltradas = DATA.resumen_ciudad.filter((c) => {
    if (f.piso !== "Todos" && c.piso_ecologico !== f.piso) return false;
    if (f.ciudad !== "Todas" && c.ciudad !== f.ciudad) return false;
    return true;
  });

  const totalObs = ciudadesFiltradas.reduce((acc, c) => acc + c.total, 0);
  const anomColMap = {
    consenso: "anom_consenso",
    gmm: "anom_gmm",
    lof: "anom_lof",
    iforest: "anom_iforest",
    ocsvm: "anom_ocsvm",
    mahal: "anom_mahal"
  };
  const keyCiudad = anomColMap[f.algo];
  const totalAnom = ciudadesFiltradas.reduce((acc, c) => acc + (c[keyCiudad] || 0), 0);
  const tasa = totalObs > 0 ? ((100 * totalAnom) / totalObs).toFixed(2) : "0.00";

  document.getElementById("kpi-total").textContent = totalObs.toLocaleString("es-BO");
  document.getElementById("kpi-ciudades").textContent = `${ciudadesFiltradas.length} estaciones seleccionadas`;
  document.getElementById("kpi-anom-label").textContent = `Anomalías (${ALGO_LABEL[f.algo]})`;
  document.getElementById("kpi-anomalias").textContent = totalAnom.toLocaleString("es-BO");
  document.getElementById("kpi-tasa").textContent = `${tasa}% del total filtrado`;

  const topC = [...ciudadesFiltradas].sort((a, b) => (b[keyCiudad] || 0) - (a[keyCiudad] || 0))[0];
  if (topC) {
    document.getElementById("kpi-top-ciudad").textContent = `${topC.ciudad} (${topC[keyCiudad]})`;
    document.getElementById("kpi-top-piso").textContent = `${topC.piso_ecologico} · ${topC.altitud_m} m s.n.m.`;
  }

  const gmmModel = DATA.modelos.find((m) => m.nombre === "GMM");
  if (gmmModel) {
    document.getElementById("kpi-gmm-bic").textContent = `${gmmModel.modelName} (G=${gmmModel.G})`;
    document.getElementById("kpi-gmm-desc").textContent = `BIC = ${gmmModel.bic.toLocaleString("es-BO")}`;
  }

  // Filtrar puntos PCA
  const pts = DATA.scatter_pca.filter((p) => {
    if (f.piso !== "Todos" && p.piso_ecologico !== f.piso) return false;
    if (f.ciudad !== "Todas" && p.ciudad !== f.ciudad) return false;
    if (f.buscar && !(`${p.fecha} ${p.ciudad} ${p.piso_ecologico}`).toLowerCase().includes(f.buscar)) return false;
    return true;
  });

  renderPCA(pts, col);
  renderCiudades(ciudadesFiltradas, keyCiudad);
  renderMensual(f.algo);
  renderTablaAnomalias(f, col);
}

function renderPCA(pts, col) {
  const norm = pts.filter((p) => !p[col]);
  const anom = pts.filter((p) => p[col]);

  const varExp = DATA.meta.pca_var_exp || [0, 0];
  const makeHover = (p) =>
    `<b>${p.ciudad}</b> (${p.fecha})<br>` +
    `Piso: ${p.piso_ecologico} | Cluster GMM: ${p.gmm_cluster}<br>` +
    `T.Máx: ${p.temp_max}°C | T.Mín: ${p.temp_min}°C<br>` +
    `Precip: ${p.precipitacion_mm} mm | Viento: ${p.viento_max_kmh} km/h<br>` +
    `Score GMM: ${p.score_gmm} | LOF: ${p.score_lof} | Votos: ${p.votos_anomalia}/5`;

  const traces = [
    {
      x: norm.map((p) => p.pc1),
      y: norm.map((p) => p.pc2),
      text: norm.map(makeHover),
      hoverinfo: "text",
      mode: "markers",
      type: "scatter",
      name: "Normal",
      marker: { size: 5, color: "#0284c7", opacity: 0.38 }
    },
    {
      x: anom.map((p) => p.pc1),
      y: anom.map((p) => p.pc2),
      text: anom.map(makeHover),
      hoverinfo: "text",
      mode: "markers",
      type: "scatter",
      name: "Anomalía",
      marker: {
        size: 7.5,
        color: "#dc2626",
        opacity: 0.88,
        line: { width: 0.8, color: "#ffffff" }
      }
    }
  ];

  Plotly.react("chart-pca", traces, {
    margin: { t: 10, r: 16, b: 44, l: 48 },
    paper_bgcolor: "transparent",
    plot_bgcolor: "#ffffff",
    xaxis: { title: `PC1 (${varExp[0]}% varianza)`, gridcolor: "#f1f5f9", zerolinecolor: "#cbd5e1" },
    yaxis: { title: `PC2 (${varExp[1]}% varianza)`, gridcolor: "#f1f5f9", zerolinecolor: "#cbd5e1" },
    showlegend: false,
    hovermode: "closest"
  }, { responsive: true, displaylogo: false });
}

function renderCiudades(ciudades, keyCiudad) {
  const sorted = [...ciudades].sort((a, b) => (a[keyCiudad] || 0) - (b[keyCiudad] || 0));
  Plotly.react("chart-ciudades", [{
    y: sorted.map((c) => c.ciudad),
    x: sorted.map((c) => c[keyCiudad] || 0),
    type: "bar",
    orientation: "h",
    marker: { color: "#1d4ed8" },
    hovertemplate: "%{y}: %{x} anomalías<extra></extra>"
  }], {
    margin: { t: 10, r: 20, b: 36, l: 96 },
    paper_bgcolor: "transparent",
    plot_bgcolor: "#ffffff",
    xaxis: { title: "Cantidad de anomalías", gridcolor: "#f1f5f9" },
    yaxis: { automargin: true }
  }, { responsive: true, displaylogo: false });
}

function renderMensual(algo) {
  const mapCol = {
    consenso: "anom_consenso",
    gmm: "anom_gmm",
    lof: "anom_lof",
    iforest: "anom_iforest",
    ocsvm: "anom_ocsvm",
    mahal: "anom_mahal"
  };
  const key = mapCol[algo];
  const serie = DATA.serie_mensual;
  Plotly.react("chart-mensual", [{
    x: serie.map((s) => s.mes_fecha),
    y: serie.map((s) => s[key]),
    type: "scatter",
    mode: "lines+markers",
    line: { color: "#dc2626", width: 2.2 },
    marker: { size: 5.5, color: "#dc2626" },
    fill: "tozeroy",
    fillcolor: "rgba(220, 38, 38, 0.08)",
    hovertemplate: "%{x}: %{y} anomalías<extra></extra>"
  }], {
    margin: { t: 10, r: 16, b: 44, l: 44 },
    paper_bgcolor: "transparent",
    plot_bgcolor: "#ffffff",
    xaxis: { title: "Mes", gridcolor: "#f1f5f9" },
    yaxis: { title: "Eventos anómalos", gridcolor: "#f1f5f9" }
  }, { responsive: true, displaylogo: false });
}

function renderTablaAnomalias(f, col) {
  const rows = DATA.top_anomalias.filter((r) => {
    if (f.piso !== "Todos" && r.piso_ecologico !== f.piso) return false;
    if (f.ciudad !== "Todas" && r.ciudad !== f.ciudad) return false;
    if (f.algo !== "consenso" && !r[col]) return false;
    if (f.buscar && !(`${r.fecha} ${r.ciudad} ${r.piso_ecologico}`).toLowerCase().includes(f.buscar)) return false;
    return true;
  });

  document.getElementById("table-count").textContent = `${rows.length} registros mostrados`;
  const tbody = document.getElementById("tabla-anomalias");
  tbody.innerHTML = rows.slice(0, 120).map((r) => {
    const cls = r.votos_anomalia >= 4 ? "voto-alto" : "voto-medio";
    return `
      <tr>
        <td>${r.fecha}</td>
        <td><strong>${r.ciudad}</strong></td>
        <td>${r.piso_ecologico}</td>
        <td>${r.temp_max.toFixed(1)}</td>
        <td>${r.temp_min.toFixed(1)}</td>
        <td>${r.precipitacion_mm.toFixed(1)}</td>
        <td>${r.viento_max_kmh.toFixed(1)}</td>
        <td>${Math.round(r.humedad_pct)}%</td>
        <td>Cluster ${r.gmm_cluster}</td>
        <td>${r.score_gmm.toFixed(2)}</td>
        <td>${r.score_lof.toFixed(2)}</td>
        <td><span class="badge-voto ${cls}">${r.votos_anomalia}/5</span></td>
      </tr>
    `;
  }).join("");
}

function renderComparativa() {
  // Matriz de Jaccard
  const jac = DATA.jaccard;
  Plotly.react("chart-jaccard", [{
    z: jac.matrix,
    x: jac.labels,
    y: jac.labels,
    type: "heatmap",
    colorscale: "Blues",
    text: jac.matrix.map((row) => row.map((v) => `${v}%`)),
    texttemplate: "%{text}",
    hovertemplate: "%{y} vs %{x}: %{z}% Jaccard<extra></extra>"
  }], {
    margin: { t: 10, r: 16, b: 70, l: 115 },
    paper_bgcolor: "transparent",
    yaxis: { autorange: "reversed" }
  }, { responsive: true, displaylogo: false });

  // Comparativa por piso ecológico
  const pisos = DATA.resumen_piso;
  Plotly.react("chart-pisos", [
    {
      x: pisos.map((p) => p.piso_ecologico),
      y: pisos.map((p) => p.anom_gmm),
      name: "GMM",
      type: "bar",
      marker: { color: "#1d4ed8" }
    },
    {
      x: pisos.map((p) => p.piso_ecologico),
      y: pisos.map((p) => p.anom_lof),
      name: "LOF",
      type: "bar",
      marker: { color: "#0284c7" }
    },
    {
      x: pisos.map((p) => p.piso_ecologico),
      y: pisos.map((p) => p.anom_iforest),
      name: "Isolation Forest",
      type: "bar",
      marker: { color: "#0f766e" }
    },
    {
      x: pisos.map((p) => p.piso_ecologico),
      y: pisos.map((p) => p.anom_consenso),
      name: "Consenso (>=3)",
      type: "bar",
      marker: { color: "#dc2626" }
    }
  ], {
    barmode: "group",
    margin: { t: 10, r: 16, b: 40, l: 48 },
    paper_bgcolor: "transparent",
    plot_bgcolor: "#ffffff",
    yaxis: { title: "Anomalías detectadas", gridcolor: "#f1f5f9" }
  }, { responsive: true, displaylogo: false });

  // Tabla modelos
  const tbody = document.getElementById("tabla-modelos");
  tbody.innerHTML = DATA.modelos.map((m) => {
    const info = PACKAGES_MAP[m.nombre] || { pkg: "R", desc: "" };
    const extra = m.nombre === "GMM" ? `Modelo ${m.modelName}, G=${m.G}, BIC=${m.bic}` : info.desc;
    return `
      <tr>
        <td><strong>${m.nombre}</strong></td>
        <td><code>${info.pkg}</code></td>
        <td>${m.umbral}</td>
        <td>${m.n_anomalias}</td>
        <td>${m.pct_anomalias}%</td>
        <td>${m.tiempo_seg} s</td>
        <td>${extra}</td>
      </tr>
    `;
  }).join("");
}
