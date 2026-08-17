# R/01_import_figures.R
# ------------------------------------------------------------
# Copia figuras já geradas pelo script original para a pasta
# local da apresentação Quarto.
# ------------------------------------------------------------

maps_dir <- file.path(figures_dir, "maps")
dir.create(maps_dir, recursive = TRUE, showWarnings = FALSE)

# Como o RProj está em .../TMI/tmi-latam-alap,
# a pasta antiga graficos_final deve estar em .../TMI/graficos_final
old_graph_dir <- normalizePath(
  file.path(project_dir, "..", "graficos_final"),
  winslash = "/",
  mustWork = FALSE
)

figs_to_try <- c(
  # níveis da TMI
  "choropleth_latam_paises_tmi_2000_brasil_regioes.png",
  "choropleth_latam_paises_tmi_2008_brasil_regioes.png",
  "choropleth_latam_paises_tmi_2016_brasil_regioes.png",
  "choropleth_latam_paises_tmi_2023_brasil_regioes.png",
  
  # delta 2000-2023
  "choropleth_latam_delta_abs_tmi_2000_2023_brasil_regioes.png",
  "choropleth_latam_delta_pct_tmi_2000_2023_brasil_regioes.png",
  
  # delta 2019-2023, se já tiver sido gerado
  "choropleth_latam_delta_abs_tmi_2019_2023_brasil_regioes.png",
  "choropleth_latam_delta_pct_tmi_2019_2023_brasil_regioes.png"
)

src <- file.path(old_graph_dir, figs_to_try)
dst <- file.path(maps_dir, figs_to_try)

ok <- file.exists(src)

if (any(ok)) {
  file.copy(src[ok], dst[ok], overwrite = TRUE)
  message("Figuras copiadas para figures/maps/:")
  message(paste0(" - ", figs_to_try[ok], collapse = "\n"))
}

if (any(!ok)) {
  message("Figuras ainda não encontradas em graficos_final/:")
  message(paste0(" - ", figs_to_try[!ok], collapse = "\n"))
}