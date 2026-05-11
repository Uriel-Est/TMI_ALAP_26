# comparativo_tmi_br_regioes_vs_latam.R
# ------------------------------------------------------------
# Objetivo:
#   - Comparar TMI (mortalidade infantil) das grandes regiões do Brasil
#     com países LATAM (ONU/WDI no seu CSV), com tabelas no console,
#     gráficos (PNG) e choropleths (Brasil regiões + LATAM países).
#
# Lê:
#   (1) brasil_00_24.xlsx  (sheet: "TMI")  -> Brasil + grandes regiões + UFs (na mesma aba)
#   (2) tmi_latam_onu_NOVO.csv            -> países LATAM (2000–2023)
#
# Gera:
#   - outputs_tmi_final/tmi_comparativo_final.xlsx
#   - graficos_final/*.png
# ------------------------------------------------------------

# =========================
# 0) Caminhos (AJUSTE AQUI)
# =========================
main_folder <- "SEU/CAMINHO"
main_folder <- normalizePath(main_folder, winslash = "/", mustWork = FALSE)

brasil_tmi <- file.path(main_folder, "brasil_00_24.xlsx")
latam_tmi  <- file.path(main_folder, "tmi_latam_onu_NOVO.csv")

out_dir      <- file.path(main_folder, "outputs_tmi_final")
graficos_dir <- file.path(main_folder, "graficos_final")

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
if (!dir.exists(graficos_dir)) dir.create(graficos_dir, recursive = TRUE)

# =========================
# 1) Pacotes
# =========================
pkgs <- c(
  "readxl", "readr", "dplyr", "tidyr", "stringr",
  "ggplot2", "openxlsx",
  "sf", "geobr",
  "rnaturalearth", "rnaturalearthdata",
  "ggrepel", "viridis", "patchwork", "purrr", "tibble"
)

missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Pacotes faltando: ", paste(missing_pkgs, collapse = ", "),
    "\nInstale com: install.packages(c(",
    paste0('"', missing_pkgs, '"', collapse = ", "), "))"
  )
}
invisible(lapply(pkgs, library, character.only = TRUE))

options(tibble.width = Inf)
options(dplyr.summarise.inform = FALSE)

# =========================
# 2) Estilo (fundo externo cinza + borda no painel + fonte)
# =========================
FONT_FAMILY <- "Times New Roman"
TITLE_SIZE  <- 11
LEG_SIZE    <- 10
CAP_SIZE    <- 9
BASE_SIZE   <- 10

theme_tmi <- function() {
  ggplot2::theme_minimal(base_size = BASE_SIZE, base_family = FONT_FAMILY) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(size = LEG_SIZE, family = FONT_FAMILY),
      legend.text       = ggplot2::element_text(size = LEG_SIZE, family = FONT_FAMILY),
      plot.title        = ggplot2::element_text(size = TITLE_SIZE, family = FONT_FAMILY),
      plot.subtitle     = ggplot2::element_text(size = BASE_SIZE, family = FONT_FAMILY),
      plot.caption      = ggplot2::element_text(size = CAP_SIZE, family = FONT_FAMILY),
      axis.title        = ggplot2::element_text(size = BASE_SIZE, family = FONT_FAMILY),
      axis.text         = ggplot2::element_text(size = BASE_SIZE, family = FONT_FAMILY),
      
      panel.border      = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.6),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.minor  = ggplot2::element_blank(),
      
      plot.background   = ggplot2::element_rect(fill = "grey92", color = NA),
      legend.background = ggplot2::element_rect(fill = "grey92", color = NA),
      
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(12, 12, 12, 12)
    )
}

theme_map_tmi_safe <- function() {
  ggplot2::theme_void(base_size = BASE_SIZE, base_family = FONT_FAMILY) +
    ggplot2::theme(
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(size = LEG_SIZE, family = FONT_FAMILY),
      legend.text       = ggplot2::element_text(size = LEG_SIZE, family = FONT_FAMILY),
      
      plot.title        = ggplot2::element_text(size = TITLE_SIZE, family = FONT_FAMILY),
      plot.subtitle     = ggplot2::element_text(size = BASE_SIZE, family = FONT_FAMILY),
      
      plot.caption      = ggplot2::element_text(
        size = CAP_SIZE, family = FONT_FAMILY, lineheight = 1.05,
        margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
      ),
      
      panel.border      = ggplot2::element_rect(fill = NA, color = "black", linewidth = 0.6),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      
      plot.background   = ggplot2::element_rect(fill = "grey92", color = NA),
      legend.background = ggplot2::element_rect(fill = "grey92", color = NA),
      
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      
      plot.margin = ggplot2::margin(14, 14, 24, 14)
    )
}

save_png <- function(plot, filename, width, height, dpi = 300) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "grey92"
  )
}

save_png_map <- function(plot, filename, width = 11, height = 8.6, dpi = 300) {
  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "grey92",
    limitsize = FALSE
  )
}

# =========================
# 3) Leitura e padronização
# =========================

# ---- Brasil (Excel) ----
br_wide <- readxl::read_excel(brasil_tmi, sheet = "TMI") |>
  dplyr::mutate(UF = stringr::str_squish(UF))

br_long <- br_wide |>
  tidyr::pivot_longer(
    cols = -UF,
    names_to = "year",
    values_to = "tmi"
  ) |>
  dplyr::mutate(
    year = stringr::str_replace_all(year, "\\*", ""),
    year = as.integer(year),
    tmi  = as.numeric(tmi),
    UF   = stringr::str_squish(UF)
  )

regioes <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

br_regioes <- br_long |>
  dplyr::filter(UF %in% regioes) |>
  dplyr::transmute(
    unit = UF,
    year = year,
    tmi  = tmi,
    type = "Região BR"
  )

br_brasil_total <- br_long |>
  dplyr::filter(UF == "Brasil") |>
  dplyr::transmute(
    unit = "Brasil (planilha)",
    year = year,
    tmi  = tmi,
    type = "Brasil (planilha)"
  )

# ---- LATAM (CSV) ----
latam <- readr::read_csv(latam_tmi, show_col_types = FALSE) |>
  dplyr::transmute(
    unit  = stringr::str_squish(country),
    iso3c = stringr::str_squish(iso3c),
    year  = as.integer(year),
    tmi   = as.numeric(SP.DYN.IMRT.IN),
    type  = "País LATAM"
  )

# =========================
# 4) Janela comum (comparação)
# =========================
start_common <- max(min(br_regioes$year, na.rm = TRUE), min(latam$year, na.rm = TRUE))
end_common   <- min(max(br_regioes$year, na.rm = TRUE), max(latam$year, na.rm = TRUE))
latest_year  <- end_common

br_regioes_c <- br_regioes |>
  dplyr::filter(year >= start_common, year <= end_common)

latam_c <- latam |>
  dplyr::filter(year >= start_common, year <= end_common)

pool <- dplyr::bind_rows(br_regioes_c, latam_c)

# =========================
# 5) Referência LATAM (mediana + IQR)
# =========================
latam_ref <- latam_c |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    latam_mean   = mean(tmi, na.rm = TRUE),
    latam_median = median(tmi, na.rm = TRUE),
    latam_p25    = unname(stats::quantile(tmi, 0.25, na.rm = TRUE)),
    latam_p75    = unname(stats::quantile(tmi, 0.75, na.rm = TRUE)),
    .groups = "drop"
  )

gap_regioes <- br_regioes_c |>
  dplyr::left_join(latam_ref, by = "year") |>
  dplyr::mutate(
    gap_to_latam_median = tmi - latam_median,
    gap_to_latam_mean   = tmi - latam_mean
  )

# =========================
# 6) Ranking e “wins” no último ano
# =========================
rank_latest <- pool |>
  dplyr::filter(year == latest_year) |>
  dplyr::arrange(tmi) |>
  dplyr::mutate(
    rank = dplyr::row_number(),
    n_total = dplyr::n()
  )

latam_latest <- latam_c |>
  dplyr::filter(year == latest_year)

wins_regioes <- br_regioes_c |>
  dplyr::filter(year == latest_year) |>
  dplyr::rowwise() |>
  dplyr::mutate(
    paises_com_tmi_maior = sum(latam_latest$tmi > tmi, na.rm = TRUE),
    paises_total_latam   = nrow(latam_latest)
  ) |>
  dplyr::ungroup() |>
  dplyr::select(unit, year, tmi, paises_com_tmi_maior, paises_total_latam)

# =========================
# 7) Movimento no tempo (2000 -> último ano; e 2019 -> último ano)
# =========================
change_long <- pool |>
  dplyr::group_by(unit, type) |>
  dplyr::summarise(
    tmi_start  = tmi[year == start_common][1],
    tmi_end    = tmi[year == latest_year][1],
    abs_change = tmi_end - tmi_start,
    pct_change = 100 * (tmi_end / tmi_start - 1),
    .groups = "drop"
  ) |>
  dplyr::arrange(pct_change)

if (start_common <= 2019 && latest_year >= 2019) {
  change_recent <- pool |>
    dplyr::filter(year %in% c(2019, latest_year)) |>
    dplyr::group_by(unit, type) |>
    dplyr::summarise(
      tmi_2019 = tmi[year == 2019][1],
      tmi_end  = tmi[year == latest_year][1],
      abs_change_2019 = tmi_end - tmi_2019,
      pct_change_2019 = 100 * (tmi_end / tmi_2019 - 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(desc(pct_change_2019))
} else {
  change_recent <- dplyr::tibble()
}

# =========================
# 7.1) Varredura exaustiva de janelas consecutivas
#      Estagnação/reversão dentro de trajetória geral de queda
# =========================

# Parâmetros principais
# Observação: a TMI está em óbitos <1 ano por 1.000 nascidos vivos.
# Assim, delta_tmi é variação absoluta da taxa, não ponto percentual.
limiar_estagnacao   <- 0.1    # |ΔTMI| <= 0.1 = estagnação prática
min_duracao_anos    <- 1      # intervalo mínimo: n -> n+1
max_duracao_anos    <- Inf    # pode limitar, ex.: 8

# Parâmetros para diagnóstico de sequência / comportamento assintótico empírico
# Como a série é finita, estes indicadores não provam limite matemático verdadeiro.
# Eles aproximam: estabilidade de cauda, monotonicidade, alternância e tendência log-linear.
tail_n              <- 5      # últimos anos usados como "cauda" da sequência
epsilon_cauda       <- 0.3    # amplitude máxima da cauda para diagnóstico tipo Cauchy empírico
limiar_slope_log    <- 0.002  # tolerância para inclinação log-linear próxima de zero

parametros_analise <- tibble::tibble(
  parametro = c(
    "limiar_estagnacao",
    "min_duracao_anos",
    "max_duracao_anos",
    "tail_n",
    "epsilon_cauda",
    "limiar_slope_log"
  ),
  valor = c(
    as.character(limiar_estagnacao),
    as.character(min_duracao_anos),
    as.character(max_duracao_anos),
    as.character(tail_n),
    as.character(epsilon_cauda),
    as.character(limiar_slope_log)
  ),
  interpretacao = c(
    "Limiar absoluto para classificar estagnação prática da TMI",
    "Menor intervalo temporal testado na varredura de janelas",
    "Maior intervalo temporal testado na varredura de janelas",
    "Tamanho da cauda usada para estabilidade/convergência empírica",
    "Amplitude máxima da cauda para classificar estabilidade tipo Cauchy empírica",
    "Tolerância para considerar a inclinação log-linear assintoticamente nula"
  )
)

# Base limpa
base_janelas <- pool |>
  dplyr::filter(!is.na(tmi), is.finite(tmi)) |>
  dplyr::arrange(type, unit, year) |>
  dplyr::group_by(type, unit) |>
  dplyr::mutate(row_id = dplyr::row_number()) |>
  dplyr::ungroup() |>
  dplyr::select(type, unit, row_id, year, tmi)

# Tendência geral da série completa
tendencia_geral <- base_janelas |>
  dplyr::group_by(type, unit) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::summarise(
    ano_inicial_serie = dplyr::first(year),
    ano_final_serie   = dplyr::last(year),
    tmi_inicial_serie = dplyr::first(tmi),
    tmi_final_serie   = dplyr::last(tmi),
    delta_geral_tmi   = tmi_final_serie - tmi_inicial_serie,
    delta_geral_pct   = 100 * (tmi_final_serie / tmi_inicial_serie - 1),
    trajetoria_geral  = dplyr::case_when(
      abs(delta_geral_tmi) <= limiar_estagnacao ~ "Estagnação geral",
      delta_geral_tmi < -limiar_estagnacao ~ "Queda geral",
      delta_geral_tmi > limiar_estagnacao ~ "Aumento geral",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  )

# Todas as janelas possíveis: (n,n+1), (n,n+2), ..., (n,n+k)
janelas_tmi <- base_janelas |>
  dplyr::inner_join(
    base_janelas,
    by = c("type", "unit"),
    suffix = c("_inicio", "_fim")
  ) |>
  dplyr::filter(year_fim > year_inicio) |>
  dplyr::mutate(
    duracao_anos = year_fim - year_inicio,
    n_obs_janela = row_id_fim - row_id_inicio + 1,
    janela_consecutiva = n_obs_janela == duracao_anos + 1
  ) |>
  dplyr::filter(
    janela_consecutiva,
    duracao_anos >= min_duracao_anos,
    duracao_anos <= max_duracao_anos
  ) |>
  dplyr::transmute(
    type,
    unit,
    ano_inicio = year_inicio,
    ano_fim    = year_fim,
    duracao_anos,
    tmi_inicio = tmi_inicio,
    tmi_fim    = tmi_fim,
    delta_tmi  = tmi_fim - tmi_inicio,
    delta_pct  = 100 * (tmi_fim / tmi_inicio - 1)
  ) |>
  dplyr::mutate(
    classificacao = dplyr::case_when(
      abs(delta_tmi) <= limiar_estagnacao ~ "Estagnação",
      delta_tmi > limiar_estagnacao ~ "Reversão",
      delta_tmi < -limiar_estagnacao ~ "Queda contínua",
      TRUE ~ "Indefinido"
    )
  ) |>
  dplyr::left_join(tendencia_geral, by = c("type", "unit")) |>
  dplyr::mutate(
    episodio_local_contra_tendencia = trajetoria_geral == "Queda geral" &
      classificacao %in% c("Estagnação", "Reversão")
  )

# Episódios locais relevantes:
# estagnação ou reversão dentro de uma trajetória geral de queda
episodios_locais <- janelas_tmi |>
  dplyr::filter(episodio_local_contra_tendencia) |>
  dplyr::arrange(type, unit, ano_inicio, ano_fim)

# Reversões principais por unidade
reversoes_principais <- episodios_locais |>
  dplyr::filter(classificacao == "Reversão") |>
  dplyr::group_by(type, unit) |>
  dplyr::arrange(desc(delta_tmi), desc(delta_pct), .by_group = TRUE) |>
  dplyr::slice_head(n = 10) |>
  dplyr::ungroup()

# Estagnações principais por unidade
# prioriza janelas mais longas e com menor variação absoluta
estagnacoes_principais <- episodios_locais |>
  dplyr::filter(classificacao == "Estagnação") |>
  dplyr::group_by(type, unit) |>
  dplyr::arrange(desc(duracao_anos), abs(delta_tmi), .by_group = TRUE) |>
  dplyr::slice_head(n = 10) |>
  dplyr::ungroup()

# Ranking dos anos iniciais mais recorrentes
# útil para descobrir se há outros anos além de 2019
ranking_anos_inicio <- episodios_locais |>
  dplyr::group_by(type, ano_inicio, classificacao) |>
  dplyr::summarise(
    n_unidades = dplyr::n_distinct(unit),
    n_janelas  = dplyr::n(),
    duracao_media = mean(duracao_anos, na.rm = TRUE),
    delta_medio_tmi = mean(delta_tmi, na.rm = TRUE),
    delta_medio_pct = mean(delta_pct, na.rm = TRUE),
    unidades = paste(sort(unique(unit)), collapse = "; "),
    .groups = "drop"
  ) |>
  dplyr::arrange(type, classificacao, desc(n_unidades), desc(n_janelas), ano_inicio)

# Versão apenas para as grandes regiões brasileiras
episodios_regioes <- episodios_locais |>
  dplyr::filter(type == "Região BR") |>
  dplyr::arrange(unit, ano_inicio, ano_fim)

ranking_anos_inicio_regioes <- ranking_anos_inicio |>
  dplyr::filter(type == "Região BR") |>
  dplyr::arrange(classificacao, desc(n_unidades), desc(n_janelas), ano_inicio)

episodios_resumo_unidade <- episodios_locais |>
  dplyr::group_by(type, unit) |>
  dplyr::summarise(
    n_janelas_locais = dplyr::n(),
    n_reversoes = sum(classificacao == "Reversão", na.rm = TRUE),
    n_estagnacoes = sum(classificacao == "Estagnação", na.rm = TRUE),
    maior_delta_reversao = if (any(classificacao == "Reversão")) {
      max(delta_tmi[classificacao == "Reversão"], na.rm = TRUE)
    } else NA_real_,
    janela_maior_reversao = if (any(classificacao == "Reversão")) {
      idx <- which.max(ifelse(classificacao == "Reversão", delta_tmi, -Inf))
      paste0(ano_inicio[idx], "–", ano_fim[idx])
    } else NA_character_,
    maior_duracao_estagnacao = if (any(classificacao == "Estagnação")) {
      max(duracao_anos[classificacao == "Estagnação"], na.rm = TRUE)
    } else NA_real_,
    janela_maior_estagnacao = if (any(classificacao == "Estagnação")) {
      idx <- which.max(ifelse(classificacao == "Estagnação", duracao_anos, -Inf))
      paste0(ano_inicio[idx], "–", ano_fim[idx])
    } else NA_character_,
    .groups = "drop"
  ) |>
  dplyr::arrange(type, desc(n_reversoes), desc(n_estagnacoes), unit)

# =========================
# 7.2) Diagnóstico de sequências e comportamento assintótico empírico
# =========================

classifica_monotonia <- function(x, tol = limiar_estagnacao) {
  dx <- diff(x)
  if (length(dx) == 0) return("Série curta")
  if (all(abs(dx) <= tol, na.rm = TRUE)) return("Aproximadamente constante")
  if (all(dx <= tol, na.rm = TRUE) && any(dx < -tol, na.rm = TRUE)) return("Monótona decrescente fraca")
  if (all(dx >= -tol, na.rm = TRUE) && any(dx > tol, na.rm = TRUE)) return("Monótona crescente fraca")
  "Não monotônica"
}

safe_lm_slope <- function(formula, data) {
  out <- tryCatch(
    stats::coef(stats::lm(formula, data = data))[2],
    error = function(e) NA_real_,
    warning = function(w) suppressWarnings(stats::coef(stats::lm(formula, data = data))[2])
  )
  unname(as.numeric(out))
}

calc_seq_diag <- function(df) {
  df <- df |>
    dplyr::filter(!is.na(tmi), is.finite(tmi), !is.na(year)) |>
    dplyr::arrange(year)
  
  n <- nrow(df)
  if (n < 2) {
    return(tibble::tibble(
      type = dplyr::first(df$type), unit = dplyr::first(df$unit),
      n_obs = n, ano_inicio = dplyr::first(df$year), ano_fim = dplyr::last(df$year),
      tmi_inicio = dplyr::first(df$tmi), tmi_fim = dplyr::last(df$tmi),
      delta_total = NA_real_, delta_total_pct = NA_real_,
      monotonia = "Série curta", n_alternancias_sinal = NA_integer_,
      prop_anos_reversao = NA_real_, prop_anos_estagnacao = NA_real_,
      limite_empirico_cauda = NA_real_, sd_cauda = NA_real_, amplitude_cauda = NA_real_,
      max_dif_cauda = NA_real_, cauchy_empirico = NA,
      slope_linear = NA_real_, slope_log = NA_real_, tail_slope_log = NA_real_,
      taxa_media_log_anual = NA_real_, razao_media_cauda = NA_real_,
      limite_modelo_loglinear = "Indefinido", classe_convergencia_empirica = "Série curta",
      comportamento_subsequencias = "Série curta"
    ))
  }
  
  dx <- diff(df$tmi)
  dx_class <- dplyr::case_when(
    abs(dx) <= limiar_estagnacao ~ 0,
    dx > limiar_estagnacao ~ 1,
    dx < -limiar_estagnacao ~ -1,
    TRUE ~ NA_real_
  )
  dx_nonzero <- dx_class[!is.na(dx_class) & dx_class != 0]
  n_alternancias <- if (length(dx_nonzero) >= 2) sum(dx_nonzero[-1] != dx_nonzero[-length(dx_nonzero)]) else 0L
  
  tail_df <- df |>
    dplyr::slice_tail(n = min(tail_n, n))
  
  n_tail <- nrow(tail_df)
  tail_range <- max(tail_df$tmi, na.rm = TRUE) - min(tail_df$tmi, na.rm = TRUE)
  max_dif_tail <- if (n_tail >= 2) max(abs(stats::dist(tail_df$tmi)), na.rm = TRUE) else NA_real_
  
  slope_linear <- safe_lm_slope(tmi ~ year, df)
  slope_log <- if (all(df$tmi > 0)) safe_lm_slope(log(tmi) ~ year, df) else NA_real_
  tail_slope_log <- if (n_tail >= 3 && all(tail_df$tmi > 0)) safe_lm_slope(log(tmi) ~ year, tail_df) else NA_real_
  
  razoes <- df$tmi[-1] / df$tmi[-n]
  razao_media_cauda <- if (length(razoes) >= 1) {
    mean(utils::tail(razoes, min(tail_n - 1, length(razoes))), na.rm = TRUE)
  } else NA_real_
  
  cauchy_empirico <- is.finite(tail_range) && tail_range <= epsilon_cauda
  
  limite_modelo_loglinear <- dplyr::case_when(
    is.na(slope_log) ~ "Indefinido",
    slope_log < -limiar_slope_log ~ "Convergente para 0 no modelo log-linear",
    abs(slope_log) <= limiar_slope_log ~ "Estável com limite positivo aproximado no modelo log-linear",
    slope_log > limiar_slope_log ~ "Divergente/crescente no modelo log-linear",
    TRUE ~ "Indefinido"
  )
  
  classe_convergencia <- dplyr::case_when(
    isTRUE(cauchy_empirico) && !is.na(tail_slope_log) && abs(tail_slope_log) <= limiar_slope_log ~
      "Cauda estável / convergência empírica",
    !is.na(tail_slope_log) && tail_slope_log > limiar_slope_log ~
      "Cauda com reversão ou divergência local",
    !is.na(tail_slope_log) && tail_slope_log < -limiar_slope_log ~
      "Ainda em queda / convergência em curso",
    isTRUE(cauchy_empirico) ~
      "Cauda estável por amplitude",
    TRUE ~ "Indefinido"
  )
  
  comportamento_subseq <- dplyr::case_when(
    classifica_monotonia(df$tmi) %in% c("Monótona decrescente fraca", "Aproximadamente constante") && isTRUE(cauchy_empirico) ~
      "Subsequências de cauda estáveis",
    n_alternancias >= 3 ~
      "Subsequências alternantes/irregulares",
    mean(dx_class == 1, na.rm = TRUE) > 0 ~
      "Subsequências com reversões locais",
    TRUE ~ "Subsequências majoritariamente descendentes"
  )
  
  tibble::tibble(
    type = dplyr::first(df$type),
    unit = dplyr::first(df$unit),
    n_obs = n,
    ano_inicio = dplyr::first(df$year),
    ano_fim = dplyr::last(df$year),
    tmi_inicio = dplyr::first(df$tmi),
    tmi_fim = dplyr::last(df$tmi),
    delta_total = dplyr::last(df$tmi) - dplyr::first(df$tmi),
    delta_total_pct = 100 * (dplyr::last(df$tmi) / dplyr::first(df$tmi) - 1),
    monotonia = classifica_monotonia(df$tmi),
    n_alternancias_sinal = as.integer(n_alternancias),
    prop_anos_reversao = mean(dx_class == 1, na.rm = TRUE),
    prop_anos_estagnacao = mean(dx_class == 0, na.rm = TRUE),
    limite_empirico_cauda = mean(tail_df$tmi, na.rm = TRUE),
    sd_cauda = if (n_tail >= 2) stats::sd(tail_df$tmi, na.rm = TRUE) else NA_real_,
    amplitude_cauda = tail_range,
    max_dif_cauda = max_dif_tail,
    cauchy_empirico = cauchy_empirico,
    slope_linear = slope_linear,
    slope_log = slope_log,
    tail_slope_log = tail_slope_log,
    taxa_media_log_anual = ifelse(is.na(slope_log), NA_real_, 100 * (exp(slope_log) - 1)),
    razao_media_cauda = razao_media_cauda,
    limite_modelo_loglinear = limite_modelo_loglinear,
    classe_convergencia_empirica = classe_convergencia,
    comportamento_subsequencias = comportamento_subseq
  )
}

diagnostico_sequencias <- base_janelas |>
  dplyr::group_by(type, unit) |>
  dplyr::group_split() |>
  purrr::map_dfr(calc_seq_diag) |>
  dplyr::left_join(tendencia_geral, by = c("type", "unit")) |>
  dplyr::arrange(type, unit)

diagnostico_sequencias_regioes <- diagnostico_sequencias |>
  dplyr::filter(type == "Região BR") |>
  dplyr::arrange(unit)

cat("\n--- Episódios locais de estagnação/reversão dentro de trajetória geral de queda ---\n")
print(episodios_locais, n = Inf)

cat("\n--- Episódios nas grandes regiões brasileiras ---\n")
print(episodios_regioes, n = Inf)

cat("\n--- Reversões principais por unidade ---\n")
print(reversoes_principais, n = Inf)

cat("\n--- Estagnações principais por unidade ---\n")
print(estagnacoes_principais, n = Inf)

cat("\n--- Ranking dos anos iniciais mais recorrentes — regiões brasileiras ---\n")
print(ranking_anos_inicio_regioes, n = Inf)

cat("\n--- Diagnóstico de sequências — grandes regiões brasileiras ---\n")
print(diagnostico_sequencias_regioes, n = Inf)

# =========================
# 8) Similaridade (Região BR ~ País LATAM): score composto
# =========================
series_feat <- pool |>
  dplyr::mutate(log_tmi = log(tmi)) |>
  dplyr::group_by(unit, type) |>
  dplyr::summarise(
    tmi_latest = tmi[year == latest_year][1],
    tmi_start  = tmi[year == start_common][1],
    pct_change = 100 * (tmi_latest / tmi_start - 1),
    slope_log  = stats::coef(stats::lm(log_tmi ~ year))[2],
    .groups = "drop"
  )

reg_feat <- series_feat |>
  dplyr::filter(type == "Região BR") |>
  dplyr::rename(region = unit)

cty_feat <- series_feat |>
  dplyr::filter(type == "País LATAM") |>
  dplyr::rename(country = unit) |>
  dplyr::select(country, tmi_latest, pct_change, slope_log)

pool_std <- pool |>
  dplyr::group_by(unit, type) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    log_tmi = log(tmi),
    growth  = log_tmi - dplyr::lag(log_tmi),
    z_log   = (log_tmi - mean(log_tmi, na.rm = TRUE)) / stats::sd(log_tmi, na.rm = TRUE)
  ) |>
  dplyr::ungroup()

reg_series <- pool_std |>
  dplyr::filter(type == "Região BR") |>
  dplyr::select(region = unit, year, reg_growth = growth, reg_z = z_log)

cty_series <- pool_std |>
  dplyr::filter(type == "País LATAM") |>
  dplyr::select(country = unit, iso3c, year, cty_growth = growth, cty_z = z_log)

# **FIX COMPAT**: sem relationship= (pra não quebrar em dplyr antigo)
pairs <- reg_series |>
  dplyr::inner_join(cty_series, by = "year") |>
  dplyr::group_by(region, country, iso3c) |>
  dplyr::summarise(
    rmse_z = sqrt(mean((reg_z - cty_z)^2, na.rm = TRUE)),
    corr_growth = suppressWarnings(stats::cor(reg_growth, cty_growth, use = "pairwise.complete.obs")),
    n_years = sum(!is.na(reg_z) & !is.na(cty_z)),
    .groups = "drop"
  ) |>
  dplyr::left_join(reg_feat, by = "region") |>
  dplyr::rename(reg_tmi_latest = tmi_latest, reg_pct_change = pct_change, reg_slope_log = slope_log) |>
  dplyr::left_join(cty_feat, by = "country") |>
  dplyr::rename(cty_tmi_latest = tmi_latest, cty_pct_change = pct_change, cty_slope_log = slope_log) |>
  dplyr::mutate(
    diff_level = abs(reg_tmi_latest - cty_tmi_latest),
    diff_pct   = abs(reg_pct_change - cty_pct_change),
    diff_slope = abs(reg_slope_log - cty_slope_log),
    corr_growth = dplyr::if_else(is.na(corr_growth), 0, corr_growth),
    corr_dist = 1 - corr_growth
  )

pairs_scored <- pairs |>
  dplyr::mutate(
    z_level = as.numeric(scale(diff_level)),
    z_pct   = as.numeric(scale(diff_pct)),
    z_slope = as.numeric(scale(diff_slope)),
    z_rmse  = as.numeric(scale(rmse_z)),
    z_corr  = as.numeric(scale(corr_dist)),
    score = z_level + z_pct + z_slope + z_rmse + z_corr
  ) |>
  dplyr::arrange(score)

top_matches_by_region <- pairs_scored |>
  dplyr::group_by(region) |>
  dplyr::arrange(score, .by_group = TRUE) |>
  dplyr::slice_head(n = 8) |>
  dplyr::ungroup() |>
  dplyr::select(
    region, country, iso3c, score,
    reg_tmi_latest, cty_tmi_latest, diff_level,
    reg_pct_change, cty_pct_change, diff_pct,
    rmse_z, corr_growth, n_years
  )

best_region_for_country <- pairs_scored |>
  dplyr::group_by(country, iso3c) |>
  dplyr::arrange(score, .by_group = TRUE) |>
  dplyr::slice_head(n = 1) |>
  dplyr::ungroup() |>
  dplyr::select(country, iso3c, best_region = region, score, diff_level, diff_pct, rmse_z, corr_growth)


# =========================
# 8.1) Comparação assintótica dentro dos clusters de semelhantes
# =========================

top3_matches_by_region <- top_matches_by_region |>
  dplyr::group_by(region) |>
  dplyr::arrange(score, .by_group = TRUE) |>
  dplyr::slice_head(n = 3) |>
  dplyr::ungroup()

seq_region <- diagnostico_sequencias |>
  dplyr::filter(type == "Região BR") |>
  dplyr::select(
    region = unit,
    reg_limite_cauda = limite_empirico_cauda,
    reg_amplitude_cauda = amplitude_cauda,
    reg_cauchy_empirico = cauchy_empirico,
    reg_slope_log_seq = slope_log,
    reg_tail_slope_log = tail_slope_log,
    reg_classe_convergencia = classe_convergencia_empirica,
    reg_limite_modelo_loglinear = limite_modelo_loglinear,
    reg_comportamento_subseq = comportamento_subsequencias,
    reg_monotonia = monotonia
  )

seq_country <- diagnostico_sequencias |>
  dplyr::filter(type == "País LATAM") |>
  dplyr::select(
    country = unit,
    cty_limite_cauda = limite_empirico_cauda,
    cty_amplitude_cauda = amplitude_cauda,
    cty_cauchy_empirico = cauchy_empirico,
    cty_slope_log_seq = slope_log,
    cty_tail_slope_log = tail_slope_log,
    cty_classe_convergencia = classe_convergencia_empirica,
    cty_limite_modelo_loglinear = limite_modelo_loglinear,
    cty_comportamento_subseq = comportamento_subsequencias,
    cty_monotonia = monotonia
  )

cluster_seq_comparison <- top3_matches_by_region |>
  dplyr::left_join(seq_region, by = "region") |>
  dplyr::left_join(seq_country, by = "country") |>
  dplyr::mutate(
    diff_limite_cauda = abs(reg_limite_cauda - cty_limite_cauda),
    diff_amplitude_cauda = abs(reg_amplitude_cauda - cty_amplitude_cauda),
    diff_slope_log_seq = abs(reg_slope_log_seq - cty_slope_log_seq),
    mesma_classe_convergencia = reg_classe_convergencia == cty_classe_convergencia,
    mesmo_limite_modelo_loglinear = reg_limite_modelo_loglinear == cty_limite_modelo_loglinear,
    mesma_monotonia = reg_monotonia == cty_monotonia
  ) |>
  dplyr::arrange(region, score)

epi_region <- episodios_resumo_unidade |>
  dplyr::filter(type == "Região BR") |>
  dplyr::select(
    region = unit,
    reg_n_janelas_locais = n_janelas_locais,
    reg_n_reversoes = n_reversoes,
    reg_n_estagnacoes = n_estagnacoes,
    reg_maior_delta_reversao = maior_delta_reversao,
    reg_janela_maior_reversao = janela_maior_reversao,
    reg_maior_duracao_estagnacao = maior_duracao_estagnacao,
    reg_janela_maior_estagnacao = janela_maior_estagnacao
  )

epi_country <- episodios_resumo_unidade |>
  dplyr::filter(type == "País LATAM") |>
  dplyr::select(
    country = unit,
    cty_n_janelas_locais = n_janelas_locais,
    cty_n_reversoes = n_reversoes,
    cty_n_estagnacoes = n_estagnacoes,
    cty_maior_delta_reversao = maior_delta_reversao,
    cty_janela_maior_reversao = janela_maior_reversao,
    cty_maior_duracao_estagnacao = maior_duracao_estagnacao,
    cty_janela_maior_estagnacao = janela_maior_estagnacao
  )

cluster_episode_comparison <- top3_matches_by_region |>
  dplyr::left_join(epi_region, by = "region") |>
  dplyr::left_join(epi_country, by = "country") |>
  tidyr::replace_na(list(
    reg_n_janelas_locais = 0L,
    reg_n_reversoes = 0L,
    reg_n_estagnacoes = 0L,
    cty_n_janelas_locais = 0L,
    cty_n_reversoes = 0L,
    cty_n_estagnacoes = 0L
  )) |>
  dplyr::mutate(
    diff_n_reversoes = abs(reg_n_reversoes - cty_n_reversoes),
    diff_n_estagnacoes = abs(reg_n_estagnacoes - cty_n_estagnacoes),
    mesmo_padrao_eventos = diff_n_reversoes <= 1 & diff_n_estagnacoes <= 2
  ) |>
  dplyr::arrange(region, score)

cat("\n--- Comparação assintótica entre regiões e top 3 países semelhantes ---\n")
print(cluster_seq_comparison, n = Inf)

cat("\n--- Comparação de episódios locais entre regiões e top 3 países semelhantes ---\n")
print(cluster_episode_comparison, n = Inf)

# =========================
# 9) GRÁFICOS (PNG) -> graficos/
# =========================
source_caption <- "Fontes: Brasil — SVSA/MS. Países — ONU/UN IGME."
source_caption <- stringr::str_wrap(source_caption, width = 95)

# (9.1) Ranking no último ano (com diferenciação visual: regiões tracejadas)
rank_latest_plot <- rank_latest |>
  dplyr::mutate(unit = stats::reorder(unit, tmi))

p_rank <- ggplot2::ggplot(
  rank_latest_plot,
  ggplot2::aes(x = tmi, y = unit, shape = type)
) +
  ggplot2::geom_segment(
    ggplot2::aes(x = 0, xend = tmi, yend = unit, linetype = type),
    alpha = 0.45,
    linewidth = 0.6
  ) +
  ggplot2::geom_point(size = 2.7) +
  ggplot2::scale_shape_manual(
    values = c("Região BR" = 16, "País LATAM" = 17, "Brasil (planilha)" = 15),
    breaks = c("Região BR", "País LATAM")
  ) +
  ggplot2::scale_linetype_manual(
    values = c("Região BR" = "twodash", "País LATAM" = "solid", "Brasil (planilha)" = "dashed"),
    breaks = c("Região BR", "País LATAM")
  ) +
  ggplot2::labs(
    x = paste0("TMI em ", latest_year),
    y = NULL,
    shape = NULL,
    linetype = NULL,
    title = paste0("Ranking de TMI (", latest_year, "): Regiões do Brasil vs Países LATAM"),
    caption = source_caption
  ) +
  theme_tmi()

save_png(p_rank, file.path(graficos_dir, "fig_ranking_latest_year.png"), 10, 9)

# (9.2) % mudança 2000 -> último ano (regiões + países)
chg_plot <- change_long |>
  dplyr::mutate(unit = stats::reorder(unit, pct_change))

p_chg <- ggplot2::ggplot(chg_plot, ggplot2::aes(x = pct_change, y = unit, fill = type)) +
  ggplot2::geom_col() +
  ggplot2::labs(
    x = paste0("% variação TMI (", start_common, " → ", latest_year, ")"),
    y = NULL,
    fill = NULL,
    title = "Mudança relativa da TMI no período (regiões + países)",
    caption = source_caption
  ) +
  theme_tmi()

save_png(p_chg, file.path(graficos_dir, "fig_pct_change_2000_to_latest.png"), 10, 11)

# (9.3) Séries “similares”: UMA figura por região (região + top3 países) + patchwork
# (9.3) Séries “similares”: UMA figura por região (região + top3 países)
#       - linetype diferente por linha (diversidade real)
#       - labels nos 4 pontos (início, fim, pico, vale), sem repetir quando coincide
#       - ponto final com a mesma cor da linha (endpoint)
#       - salva dois nomes (pra teu plug-and-play)
#       - guarda versões em plots_top3_patch p/ patchwork (9.6)

# guarda plots para patchwork (9.6)
plots_top3_patch <- list()

# top3 países por região (score)
top3 <- top_matches_by_region |>
  dplyr::mutate(
    region  = stringr::str_squish(region),
    country = stringr::str_squish(country)
  ) |>
  dplyr::group_by(region) |>
  dplyr::slice_head(n = 3) |>
  dplyr::ungroup()

linetypes_pool <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
shapes_pool    <- c(16, 17, 15, 18, 3, 7)

for (reg in regioes) {

  reg_key <- stringr::str_squish(reg)

  top_countries_reg <- top3 |>
    dplyr::filter(region == reg_key) |>
    dplyr::pull(country) |>
    stringr::str_squish()

  # região (robusto a espaços)
  df_reg <- br_regioes_c |>
    dplyr::mutate(unit = stringr::str_squish(unit)) |>
    dplyr::filter(unit == reg_key) |>
    dplyr::select(unit, year, tmi) |>
    dplyr::mutate(type = "Região BR")

  if (nrow(df_reg) == 0) {
    cat("\n[WARN] Região não encontrada em br_regioes_c:", reg, "\n")
    next
  }

  # fallback 1: se top3 vier vazio, pega 3 mais próximos no último ano
  if (length(top_countries_reg) == 0) {
    reg_last <- df_reg |>
      dplyr::filter(year == latest_year) |>
      dplyr::pull(tmi) |>
      dplyr::first()

    top_countries_reg <- latam_c |>
      dplyr::mutate(unit = stringr::str_squish(unit)) |>
      dplyr::filter(year == latest_year, !is.na(tmi)) |>
      dplyr::mutate(dist = abs(tmi - reg_last)) |>
      dplyr::arrange(dist) |>
      dplyr::slice_head(n = 3) |>
      dplyr::pull(unit)

    cat("\n[WARN] top3 vazio p/", reg, "→ fallback por proximidade no último ano.\n")
  }

  # países (robusto a espaços)
  df_cty <- latam_c |>
    dplyr::mutate(unit = stringr::str_squish(unit)) |>
    dplyr::filter(unit %in% top_countries_reg) |>
    dplyr::select(unit, year, tmi) |>
    dplyr::mutate(type = "País LATAM")

  # fallback 2: se não casou (encoding/nome), força 3 garantidos no último ano
  if (nrow(df_cty) == 0) {
    reg_last <- df_reg |>
      dplyr::filter(year == latest_year) |>
      dplyr::pull(tmi) |>
      dplyr::first()

    top_countries_reg <- latam_c |>
      dplyr::mutate(unit = stringr::str_squish(unit)) |>
      dplyr::filter(year == latest_year, !is.na(tmi)) |>
      dplyr::mutate(dist = abs(tmi - reg_last)) |>
      dplyr::arrange(dist) |>
      dplyr::slice_head(n = 3) |>
      dplyr::pull(unit)

    df_cty <- latam_c |>
      dplyr::mutate(unit = stringr::str_squish(unit)) |>
      dplyr::filter(unit %in% top_countries_reg) |>
      dplyr::select(unit, year, tmi) |>
      dplyr::mutate(type = "País LATAM")

    cat("\n[WARN] top3 não casou com latam_c p/", reg, "→ fallback (nomes/encoding).\n")
  }

  df_plot <- dplyr::bind_rows(df_reg, df_cty) |>
    dplyr::mutate(unit = as.character(unit)) |>
    dplyr::filter(!is.na(tmi))

  if (nrow(df_plot) == 0) {
    cat("\n[WARN] df_plot ficou vazio (tmi NA) para:", reg, "\n")
    next
  }

  # ==== FIX DO “GRÁFICO BRANCO” ====
  # garante que escalas manuais cobrem exatamente as unidades presentes no df_plot
  units_present <- unique(df_plot$unit)

  units_order <- c(reg_key, top_countries_reg)
  units_order <- units_order[!is.na(units_order) & units_order != ""]
  units_order <- unique(units_order)
  units_order <- c(units_order, setdiff(units_present, units_order)) # qualquer sobra

  # corta pools se precisar (não deveria passar de 4, mas fica robusto)
  lt_map <- stats::setNames(
    linetypes_pool[seq_len(min(length(linetypes_pool), length(units_order)))],
    units_order[seq_len(min(length(linetypes_pool), length(units_order)))]
  )
  sh_map <- stats::setNames(
    shapes_pool[seq_len(min(length(shapes_pool), length(units_order)))],
    units_order[seq_len(min(length(shapes_pool), length(units_order)))]
  )

  # usa factor para estabilizar ordem e legendas (quando usadas)
  df_plot <- df_plot |>
    dplyr::mutate(unit = factor(unit, levels = units_order))

  # keypoints NA-safe (início, fim, pico, vale) sem repetir quando coincide
  keypoints <- df_plot |>
    dplyr::group_by(unit) |>
    dplyr::arrange(year, .by_group = TRUE) |>
    dplyr::mutate(
      tmi_min_safe = dplyr::if_else(is.na(tmi), Inf, tmi),
      tmi_max_safe = dplyr::if_else(is.na(tmi), -Inf, tmi)
    ) |>
    dplyr::slice(c(
      1,
      dplyr::n(),
      which.min(tmi_min_safe),
      which.max(tmi_max_safe)
    )) |>
    dplyr::distinct(unit, year, .keep_all = TRUE) |>
    dplyr::mutate(lbl = sprintf("%.1f", tmi)) |>
    dplyr::ungroup()

  # endpoints (ponto final com a mesma cor da linha)
  endpoints <- df_plot |>
    dplyr::group_by(unit) |>
    dplyr::filter(year == max(year, na.rm = TRUE)) |>
    dplyr::ungroup()

  reg_file <- stringr::str_replace_all(stringr::str_to_lower(reg_key), "[^a-z0-9]+", "_")

  p_reg <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data = latam_ref,
      ggplot2::aes(x = year, ymin = latam_p25, ymax = latam_p75),
      alpha = 0.18
    ) +
    ggplot2::geom_line(
      data = latam_ref,
      ggplot2::aes(x = year, y = latam_median),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::geom_line(
      data = df_plot,
      ggplot2::aes(x = year, y = tmi, color = unit, linetype = unit),
      linewidth = 0.95
    ) +
    ggplot2::geom_point(
      data = endpoints,
      ggplot2::aes(x = year, y = tmi, color = unit),
      size = 2.6
    ) +
    ggplot2::geom_point(
      data = keypoints,
      ggplot2::aes(x = year, y = tmi, color = unit, shape = unit),
      size = 2.2
    ) +
    ggrepel::geom_label_repel(
      data = keypoints,
      ggplot2::aes(x = year, y = tmi, label = lbl, color = unit),
      family = FONT_FAMILY,
      size = 3.2,
      label.size = 0.2,
      min.segment.length = 0,
      box.padding = 0.25,
      point.padding = 0.15,
      max.overlaps = Inf,
      force = 2,
      seed = 123,
      fill = "white",
      alpha = 0.95
    ) +
    ggplot2::scale_linetype_manual(values = lt_map) +
    ggplot2::scale_shape_manual(values = sh_map) +
    ggplot2::labs(
      x = NULL,
      y = "TMI (óbitos <1 ano por 1.000 NV)",
      color = NULL,
      linetype = NULL,
      shape = NULL,
      title = paste0("TMI — ", reg_key, " e países mais próximos (", start_common, "–", latest_year, ")"),
      subtitle = "Faixa sombreada: P25–P75 LATAM | Linha tracejada fina: mediana LATAM",
      caption = source_caption
    ) +
    theme_tmi()

  # salva nos DOIS nomes
  save_png(
    p_reg,
    file.path(graficos_dir, paste0("fig_similares_regiao_", reg_file, "_top3_paises.png")),
    width = 11, height = 6.2
  )

  save_png(
    p_reg,
    file.path(graficos_dir, paste0("fig_timeseries_", reg_file, "_vs_latam_top3.png")),
    width = 11, height = 6.2
  )

  # ---- versão p/ patchwork (sem legenda + nome da linha no fim) ----
  p_reg_patch <- p_reg +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.22))) +
    ggrepel::geom_text_repel(
      data = endpoints,
      ggplot2::aes(x = year, y = tmi, label = unit, color = unit),
      family = FONT_FAMILY,
      size = 3.1,
      direction = "y",
      hjust = 0,
      nudge_x = 0.6,
      box.padding = 0.20,
      point.padding = 0.10,
      min.segment.length = 0,
      segment.alpha = 0.6,
      seed = 123,
      max.overlaps = Inf
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(legend.position = "none")

  plots_top3_patch[[reg_key]] <- p_reg_patch
}

# (9.4) Séries “tudo junto” (regiões + todos países)
df_all <- pool |>
  dplyr::mutate(type2 = dplyr::if_else(type == "Região BR", "Região BR", "País LATAM"))

regions_latest_pts <- df_all |>
  dplyr::filter(type2 == "Região BR", year == latest_year) |>
  dplyr::mutate(lbl = sprintf("%.1f", tmi))

p_all <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = latam_ref,
    ggplot2::aes(x = year, ymin = latam_p25, ymax = latam_p75),
    alpha = 0.10
  ) +
  ggplot2::geom_line(
    data = latam_ref,
    ggplot2::aes(x = year, y = latam_median),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  ggplot2::geom_line(
    data = df_all |> dplyr::filter(type2 == "País LATAM"),
    ggplot2::aes(x = year, y = tmi, group = unit),
    linewidth = 0.35,
    alpha = 0.20,
    color = "grey40"
  ) +
  ggplot2::geom_line(
    data = df_all |> dplyr::filter(type2 == "Região BR"),
    ggplot2::aes(x = year, y = tmi, color = unit),
    linewidth = 0.95
  ) +
  ggplot2::geom_point(
    data = df_all |> dplyr::filter(type2 == "Região BR", year == latest_year),
    ggplot2::aes(x = year, y = tmi, color = unit),
    size = 2.2
  ) +
  ggrepel::geom_label_repel(
    data = regions_latest_pts,
    ggplot2::aes(x = year, y = tmi, label = lbl, color = unit),
    family = FONT_FAMILY,
    size = 3.1,
    label.size = 0.2,
    box.padding = 0.25,
    point.padding = 0.15,
    min.segment.length = 0,
    max.overlaps = 30,
    fill = "white",
    alpha = 0.95
  ) +
  ggplot2::labs(
    x = NULL,
    y = "TMI",
    color = NULL,
    title = paste0("TMI — Regiões do Brasil e países LATAM (", start_common, "–", latest_year, ")"),
    subtitle = "Regiões em destaque; países em cinza; referência LATAM: mediana + P25–P75",
    caption = source_caption
  ) +
  theme_tmi()

save_png(p_all, file.path(graficos_dir, "fig_timeseries_all_regioes_all_paises.png"), 13, 7)

# (9.5) Trajetórias indexadas (2000=100): regiões + referência LATAM
pool_index <- df_all |>
  dplyr::group_by(unit, type2) |>
  dplyr::mutate(tmi_0 = tmi[year == start_common][1]) |>
  dplyr::ungroup() |>
  dplyr::mutate(index_2000 = 100 * (tmi / tmi_0))

latam_index_ref <- pool_index |>
  dplyr::filter(type2 == "País LATAM") |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    median_index = median(index_2000, na.rm = TRUE),
    p25_index    = unname(stats::quantile(index_2000, 0.25, na.rm = TRUE)),
    p75_index    = unname(stats::quantile(index_2000, 0.75, na.rm = TRUE)),
    .groups = "drop"
  )

p_idx <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data = latam_index_ref,
    ggplot2::aes(x = year, ymin = p25_index, ymax = p75_index),
    alpha = 0.18
  ) +
  ggplot2::geom_line(
    data = latam_index_ref,
    ggplot2::aes(x = year, y = median_index),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  ggplot2::geom_line(
    data = pool_index |> dplyr::filter(type2 == "Região BR"),
    ggplot2::aes(x = year, y = index_2000, color = unit),
    linewidth = 0.95
  ) +
  ggplot2::labs(
    x = NULL,
    y = paste0("Índice (", start_common, "=100)"),
    color = NULL,
    title = "Trajetórias (queda relativa) — Regiões BR vs referência LATAM",
    subtitle = "Faixa sombreada: P25–P75 LATAM (índice) | Linha tracejada fina: mediana LATAM",
    caption = source_caption
  ) +
  theme_tmi()

save_png(p_idx, file.path(graficos_dir, "fig_indexed_trajectories_regioes_vs_latam.png"), 11, 6.2)

# (9.5b) Episódios locais: principais reversões e estagnações nas regiões
# Evita plotar todas as janelas possíveis, que deixariam a figura poluída.
episodios_regioes_plot <- dplyr::bind_rows(
  reversoes_principais |>
    dplyr::filter(type == "Região BR") |>
    dplyr::mutate(grupo_plot = "Reversões principais"),
  estagnacoes_principais |>
    dplyr::filter(type == "Região BR") |>
    dplyr::mutate(grupo_plot = "Estagnações principais")
) |>
  dplyr::distinct(type, unit, ano_inicio, ano_fim, classificacao, .keep_all = TRUE) |>
  dplyr::arrange(unit, classificacao, ano_inicio, ano_fim)

if (nrow(episodios_regioes_plot) > 0) {
  p_episodios_regioes <- episodios_regioes_plot |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = ano_inicio,
        xend = ano_fim,
        y = unit,
        yend = unit,
        linetype = classificacao,
        linewidth = duracao_anos
      )
    ) +
    ggplot2::geom_segment(alpha = 0.85) +
    ggplot2::geom_point(ggplot2::aes(x = ano_inicio), size = 2) +
    ggplot2::geom_point(ggplot2::aes(x = ano_fim), size = 2) +
    ggplot2::scale_linewidth_continuous(range = c(0.6, 2.4)) +
    ggplot2::labs(
      x = NULL,
      y = NULL,
      linetype = NULL,
      linewidth = "Duração",
      title = "Episódios locais de estagnação/reversão da TMI — Grandes Regiões",
      subtitle = "Principais intervalos consecutivos dentro de trajetórias gerais de queda",
      caption = source_caption
    ) +
    theme_tmi()
  
  save_png(
    p_episodios_regioes,
    file.path(graficos_dir, "fig_episodios_locais_estagnacao_reversao_regioes.png"),
    width = 11,
    height = 6.2
  )
}

# (9.5c) Diagnóstico assintótico empírico das regiões
p_seq_regioes <- diagnostico_sequencias_regioes |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = stats::reorder(unit, limite_empirico_cauda),
      y = limite_empirico_cauda,
      fill = classe_convergencia_empirica
    )
  ) +
  ggplot2::geom_col() +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = paste0("Limite empírico da cauda (média dos últimos ", tail_n, " anos)"),
    fill = NULL,
    title = "Comportamento assintótico empírico da TMI — Grandes Regiões",
    subtitle = "Diagnóstico baseado na estabilidade da cauda e na inclinação log-linear recente",
    caption = source_caption
  ) +
  theme_tmi()

save_png(
  p_seq_regioes,
  file.path(graficos_dir, "fig_diagnostico_assintotico_regioes.png"),
  width = 11,
  height = 6.2
)

# (9.5d) Diferença do limite empírico de cauda entre cada região e seus top 3 países semelhantes
if (nrow(cluster_seq_comparison) > 0) {
  p_cluster_limite <- cluster_seq_comparison |>
    dplyr::mutate(country = stats::reorder(country, diff_limite_cauda)) |>
    ggplot2::ggplot(ggplot2::aes(x = country, y = diff_limite_cauda)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ region, scales = "free_y") +
    ggplot2::labs(
      x = NULL,
      y = "|limite empírico da região - limite empírico do país|",
      title = "Comparação assintótica nos clusters de trajetórias semelhantes",
      subtitle = "Top 3 países mais semelhantes a cada região, segundo o escore composto",
      caption = source_caption
    ) +
    theme_tmi()
  
  save_png(
    p_cluster_limite,
    file.path(graficos_dir, "fig_cluster_comparacao_limite_empirico.png"),
    width = 12,
    height = 7
  )
}


# (9.6) Patchwork: 3 em cima, 2 embaixo centralizados (robusto, sem string design)

ordem_regs <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")
ordem_regs <- stringr::str_squish(ordem_regs)

pA <- plots_top3_patch[[ordem_regs[1]]]
pB <- plots_top3_patch[[ordem_regs[2]]]
pC <- plots_top3_patch[[ordem_regs[3]]]
pD <- plots_top3_patch[[ordem_regs[4]]]
pE <- plots_top3_patch[[ordem_regs[5]]]

if (any(vapply(list(pA, pB, pC, pD, pE), is.null, logical(1)))) {
  stop("Faltou algum painel em plots_top3_patch. Veja names(plots_top3_patch).")
}

design_3_2_center <- c(
  A = patchwork::area(t = 1, l = 1, b = 1, r = 2),
  B = patchwork::area(t = 1, l = 3, b = 1, r = 4),
  C = patchwork::area(t = 1, l = 5, b = 1, r = 6),
  D = patchwork::area(t = 2, l = 2, b = 2, r = 3),
  E = patchwork::area(t = 2, l = 4, b = 2, r = 5)
)

p_patch <- patchwork::wrap_plots(A = pA, B = pB, C = pC, D = pD, E = pE) +
  patchwork::plot_layout(design = design_3_2_center) +
  patchwork::plot_annotation(
    title   = paste0("TMI — Regiões do Brasil e Top 3 países mais próximos (", start_common, "–", latest_year, ")"),
    caption = source_caption,
    theme = ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "grey92", color = NA),
      panel.background = ggplot2::element_rect(fill = "grey92", color = NA),
      text         = ggplot2::element_text(family = FONT_FAMILY),
      plot.title   = ggplot2::element_text(size = TITLE_SIZE, family = FONT_FAMILY),
      plot.caption = ggplot2::element_text(size = CAP_SIZE,  family = FONT_FAMILY),
      plot.margin  = ggplot2::margin(12, 12, 12, 12)
    )
  )

save_png(
  p_patch,
  file.path(graficos_dir, "fig_timeseries_top3_patchwork_5regioes.png"),
  width = 16, height = 9.2
)

# =========================
# 10) Choropleths parametrizados por ano
# =========================
normalize_region <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("-", " ") |>
    stringr::str_squish()
}

sf_label_points <- function(sfobj) {
  pts <- sf::st_point_on_surface(sfobj)
  sf::st_coordinates(pts) |> as.data.frame()
}

# Evita o erro da escala (sem viridis::scale_fill_viridis_c)
scale_fill_magma <- function(..., na.value = NULL) {
  has_ggplot_viridis <- exists("scale_fill_viridis_c", where = asNamespace("ggplot2"), inherits = FALSE)
  if (has_ggplot_viridis) {
    if (is.null(na.value)) ggplot2::scale_fill_viridis_c(option = "magma", ...)
    else ggplot2::scale_fill_viridis_c(option = "magma", na.value = na.value, ...)
  } else {
    if (is.null(na.value)) viridis::scale_fill_viridis(option = "magma", discrete = FALSE, ...)
    else viridis::scale_fill_viridis(option = "magma", discrete = FALSE, na.value = na.value, ...)
  }
}

make_choropleths_tmi <- function(
    year_sel = latest_year,
    year_base = start_common,
    label_values = TRUE,
    width_abs = 11,
    height_abs = 8.6,
    width_delta = 11,
    height_delta = 8.6
) {
  set.seed(123)
  
  # BRASIL por regiões (ABS)
  br_states_sf <- geobr::read_state(year = 2020, simplified = TRUE)
  
  br_regions_sf <- br_states_sf |>
    dplyr::group_by(name_region) |>
    dplyr::summarise(geom = sf::st_union(geom), .groups = "drop") |>
    dplyr::mutate(region_key = normalize_region(name_region)) |>
    sf::st_transform(4326)
  
  br_reg_y <- br_regioes_c |>
    dplyr::filter(year == year_sel) |>
    dplyr::mutate(region_key = normalize_region(unit)) |>
    dplyr::select(region_label = unit, region_key, tmi)
  
  br_map_abs <- br_regions_sf |>
    dplyr::left_join(br_reg_y, by = "region_key")
  
  br_lab_abs <- br_map_abs |>
    dplyr::mutate(lbl = sprintf("%.1f", tmi))
  
  if (label_values) {
    xy <- sf_label_points(br_lab_abs)
    br_lab_abs <- br_lab_abs |>
      dplyr::mutate(x = xy$X, y = xy$Y)
  }
  
  p_br_abs <- ggplot2::ggplot(br_map_abs) +
    ggplot2::geom_sf(ggplot2::aes(fill = tmi), color = "black", linewidth = 0.9) +
    scale_fill_magma(na.value = "grey85") +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("Brasil — TMI por Grande Região (", year_sel, ")"),
      fill = "TMI",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_br_abs <- p_br_abs +
      ggrepel::geom_label_repel(
        data = br_lab_abs,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 3.0,
        family = FONT_FAMILY,
        label.size = 0.2,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.05,
        seed = 123,
        fill = "white",
        alpha = 0.95
      )
  }
  
  save_png_map(
    p_br_abs,
    file.path(graficos_dir, paste0("choropleth_br_regioes_tmi_", year_sel, ".png")),
    width_abs, height_abs
  )
  
  # Mundo/LATAM base
  world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    dplyr::mutate(
      iso3 = dplyr::if_else(iso_a3 == "-99" & !is.na(iso_a3_eh), iso_a3_eh, iso_a3)
    ) |>
    sf::st_transform(4326)
  
  bb <- sf::st_bbox(c(xmin = -120, xmax = -30, ymin = -60, ymax = 35), crs = sf::st_crs(4326))
  
  latam_y <- latam_c |>
    dplyr::filter(year == year_sel) |>
    dplyr::select(iso3c, unit, tmi)
  
  latam_world_abs <- world_sf |>
    dplyr::inner_join(latam_y, by = c("iso3" = "iso3c")) |>
    sf::st_crop(bb)
  
  latam_lab_abs <- latam_world_abs |>
    dplyr::mutate(lbl = sprintf("%.1f", tmi))
  
  if (label_values) {
    xy <- sf_label_points(latam_lab_abs)
    latam_lab_abs <- latam_lab_abs |>
      dplyr::mutate(x = xy$X, y = xy$Y)
  }
  
  # LATAM ABS (Brasil como país)
  p_latam_abs_bra <- ggplot2::ggplot(latam_world_abs) +
    ggplot2::geom_sf(ggplot2::aes(fill = tmi), color = "black", linewidth = 0.25) +
    scale_fill_magma() +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("LATAM — TMI por país (", year_sel, ") | Brasil como país"),
      fill = "TMI",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_latam_abs_bra <- p_latam_abs_bra +
      ggrepel::geom_label_repel(
        data = latam_lab_abs,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.4,
        family = FONT_FAMILY,
        label.size = 0.15,
        min.segment.length = 0,
        box.padding = 0.12,
        point.padding = 0.03,
        seed = 123,
        fill = "white",
        alpha = 0.90,
        max.overlaps = Inf
      )
  }
  
  save_png_map(
    p_latam_abs_bra,
    file.path(graficos_dir, paste0("choropleth_latam_paises_tmi_", year_sel, "_brasil_pais.png")),
    width_abs, height_abs
  )
  
  # LATAM ABS (Brasil por regiões)
  latam_world_no_bra_abs <- latam_world_abs |>
    dplyr::filter(iso3 != "BRA")
  
  br_regions_crop_abs <- br_map_abs |>
    sf::st_crop(bb)
  
  br_lab_abs2 <- br_regions_crop_abs |>
    dplyr::mutate(lbl = sprintf("%.1f", tmi))
  
  if (label_values) {
    xy <- sf_label_points(br_lab_abs2)
    br_lab_abs2 <- br_lab_abs2 |>
      dplyr::mutate(x = xy$X, y = xy$Y)
  }
  
  p_latam_abs_bra_regions <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = latam_world_no_bra_abs,
      ggplot2::aes(fill = tmi),
      color = "black",
      linewidth = 0.25
    ) +
    ggplot2::geom_sf(
      data = br_regions_crop_abs,
      ggplot2::aes(fill = tmi),
      color = "black",
      linewidth = 0.70
    ) +
    scale_fill_magma() +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("LATAM — TMI (", year_sel, ") | Brasil por Grandes Regiões"),
      fill = "TMI",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_latam_abs_bra_regions <- p_latam_abs_bra_regions +
      ggrepel::geom_label_repel(
        data = latam_lab_abs |> dplyr::filter(iso3 != "BRA"),
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.3,
        family = FONT_FAMILY,
        label.size = 0.15,
        min.segment.length = 0,
        box.padding = 0.12,
        point.padding = 0.03,
        seed = 123,
        fill = "white",
        alpha = 0.90,
        max.overlaps = Inf
      ) +
      ggrepel::geom_label_repel(
        data = br_lab_abs2,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.7,
        family = FONT_FAMILY,
        label.size = 0.18,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.05,
        seed = 123,
        fill = "white",
        alpha = 0.95
      )
  }
  
  save_png_map(
    p_latam_abs_bra_regions,
    file.path(graficos_dir, paste0("choropleth_latam_paises_tmi_", year_sel, "_brasil_regioes.png")),
    width_abs, height_abs
  )
  
  # ---------------------------------------------------------
  # PATCHWORKS ABSOLUTOS: ano inicial vs ano final
  # Mantém as mesmas regras dos choropleths flat,
  # mas coloca start_common e latest_year lado a lado.
  # ---------------------------------------------------------
  
  years_abs_pair <- c(year_base, year_sel)
  
  # =========================
  # A) Brasil por Grandes Regiões: year_base vs year_sel
  # =========================
  
  br_abs_pair_data <- br_regioes_c |>
    dplyr::filter(year %in% years_abs_pair) |>
    dplyr::mutate(region_key = normalize_region(unit)) |>
    dplyr::select(year, region_label = unit, region_key, tmi)
  
  br_abs_limits <- range(br_abs_pair_data$tmi, na.rm = TRUE)
  
  make_br_abs_panel <- function(ano) {
    
    br_reg_y_i <- br_abs_pair_data |>
      dplyr::filter(year == ano)
    
    br_map_i <- br_regions_sf |>
      dplyr::left_join(br_reg_y_i, by = "region_key")
    
    br_lab_i <- br_map_i |>
      dplyr::mutate(lbl = sprintf("%.1f", tmi))
    
    if (label_values) {
      xy <- sf_label_points(br_lab_i)
      br_lab_i <- br_lab_i |>
        dplyr::mutate(x = xy$X, y = xy$Y)
    }
    
    p_i <- ggplot2::ggplot(br_map_i) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = tmi),
        color = "black",
        linewidth = 0.9
      ) +
      scale_fill_magma(
        limits = br_abs_limits,
        na.value = "grey85"
      ) +
      ggplot2::coord_sf(expand = FALSE, clip = "off") +
      ggplot2::labs(
        title = paste0(ano),
        fill = "TMI"
      ) +
      theme_map_tmi_safe() +
      ggplot2::theme(
        plot.caption = ggplot2::element_blank()
      )
    
    if (label_values) {
      p_i <- p_i +
        ggrepel::geom_label_repel(
          data = br_lab_i,
          ggplot2::aes(x = x, y = y, label = lbl),
          size = 3.0,
          family = FONT_FAMILY,
          label.size = 0.2,
          min.segment.length = 0,
          box.padding = 0.15,
          point.padding = 0.05,
          seed = 123,
          fill = "white",
          alpha = 0.95
        )
    }
    
    p_i
  }
  
  p_br_abs_pair <- make_br_abs_panel(year_base) +
    make_br_abs_panel(year_sel) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0("Brasil — TMI por Grande Região (", year_base, " e ", year_sel, ")"),
      caption = source_caption,
      theme = ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "grey92", color = NA),
        text = ggplot2::element_text(family = FONT_FAMILY),
        plot.title = ggplot2::element_text(size = TITLE_SIZE, family = FONT_FAMILY),
        plot.caption = ggplot2::element_text(size = CAP_SIZE, family = FONT_FAMILY),
        legend.position = "bottom"
      )
    ) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "grey92", color = NA)
    )
  
  save_png_map(
    p_br_abs_pair,
    file.path(
      graficos_dir,
      paste0("choropleth_br_regioes_tmi_", year_base, "_", year_sel, "_patchwork.png")
    ),
    width = 16,
    height = 8.6
  )
  
  # =========================
  # B) LATAM: Brasil como país — year_base vs year_sel
  # =========================
  
  latam_abs_pair_data <- latam_c |>
    dplyr::filter(year %in% years_abs_pair) |>
    dplyr::select(year, iso3c, unit, tmi)
  
  latam_abs_limits <- range(latam_abs_pair_data$tmi, na.rm = TRUE)
  
  make_latam_abs_bra_panel <- function(ano) {
    
    latam_y_i <- latam_abs_pair_data |>
      dplyr::filter(year == ano) |>
      dplyr::select(iso3c, unit, tmi)
    
    latam_world_i <- world_sf |>
      dplyr::inner_join(latam_y_i, by = c("iso3" = "iso3c")) |>
      sf::st_crop(bb)
    
    latam_lab_i <- latam_world_i |>
      dplyr::mutate(lbl = sprintf("%.1f", tmi))
    
    if (label_values) {
      xy <- sf_label_points(latam_lab_i)
      latam_lab_i <- latam_lab_i |>
        dplyr::mutate(x = xy$X, y = xy$Y)
    }
    
    p_i <- ggplot2::ggplot(latam_world_i) +
      ggplot2::geom_sf(
        ggplot2::aes(fill = tmi),
        color = "black",
        linewidth = 0.25
      ) +
      scale_fill_magma(
        limits = latam_abs_limits
      ) +
      ggplot2::coord_sf(expand = FALSE, clip = "off") +
      ggplot2::labs(
        title = paste0(ano),
        fill = "TMI"
      ) +
      theme_map_tmi_safe() +
      ggplot2::theme(
        plot.caption = ggplot2::element_blank()
      )
    
    if (label_values) {
      p_i <- p_i +
        ggrepel::geom_label_repel(
          data = latam_lab_i,
          ggplot2::aes(x = x, y = y, label = lbl),
          size = 2.4,
          family = FONT_FAMILY,
          label.size = 0.15,
          min.segment.length = 0,
          box.padding = 0.12,
          point.padding = 0.03,
          seed = 123,
          fill = "white",
          alpha = 0.90,
          max.overlaps = Inf
        )
    }
    
    p_i
  }
  
  p_latam_abs_bra_pair <- make_latam_abs_bra_panel(year_base) +
    make_latam_abs_bra_panel(year_sel) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0("LATAM — TMI por país (", year_base, " e ", year_sel, ") | Brasil como país"),
      caption = source_caption,
      theme = ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "grey92", color = NA),
        text = ggplot2::element_text(family = FONT_FAMILY),
        plot.title = ggplot2::element_text(size = TITLE_SIZE, family = FONT_FAMILY),
        plot.caption = ggplot2::element_text(size = CAP_SIZE, family = FONT_FAMILY),
        legend.position = "bottom"
      )
    ) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "grey92", color = NA)
    )
  
  save_png_map(
    p_latam_abs_bra_pair,
    file.path(
      graficos_dir,
      paste0("choropleth_latam_paises_tmi_", year_base, "_", year_sel, "_brasil_pais_patchwork.png")
    ),
    width = 16,
    height = 8.6
  )
  
  # =========================
  # C) LATAM: Brasil por Grandes Regiões — year_base vs year_sel
  # =========================
  
  latam_no_bra_abs_pair_data <- latam_c |>
    dplyr::filter(year %in% years_abs_pair) |>
    dplyr::select(year, iso3c, unit, tmi)
  
  br_regions_abs_pair_data <- br_regioes_c |>
    dplyr::filter(year %in% years_abs_pair) |>
    dplyr::mutate(region_key = normalize_region(unit)) |>
    dplyr::select(year, region_label = unit, region_key, tmi)
  
  latam_bra_regions_abs_limits <- range(
    c(
      latam_no_bra_abs_pair_data$tmi,
      br_regions_abs_pair_data$tmi
    ),
    na.rm = TRUE
  )
  
  make_latam_abs_bra_regions_panel <- function(ano) {
    
    latam_y_i <- latam_no_bra_abs_pair_data |>
      dplyr::filter(year == ano) |>
      dplyr::select(iso3c, unit, tmi)
    
    latam_world_i <- world_sf |>
      dplyr::inner_join(latam_y_i, by = c("iso3" = "iso3c")) |>
      dplyr::filter(iso3 != "BRA") |>
      sf::st_crop(bb)
    
    latam_lab_i <- latam_world_i |>
      dplyr::mutate(lbl = sprintf("%.1f", tmi))
    
    if (label_values) {
      xy <- sf_label_points(latam_lab_i)
      latam_lab_i <- latam_lab_i |>
        dplyr::mutate(x = xy$X, y = xy$Y)
    }
    
    br_reg_y_i <- br_regions_abs_pair_data |>
      dplyr::filter(year == ano)
    
    br_regions_i <- br_regions_sf |>
      dplyr::left_join(br_reg_y_i, by = "region_key") |>
      sf::st_crop(bb)
    
    br_lab_i <- br_regions_i |>
      dplyr::mutate(lbl = sprintf("%.1f", tmi))
    
    if (label_values) {
      xy <- sf_label_points(br_lab_i)
      br_lab_i <- br_lab_i |>
        dplyr::mutate(x = xy$X, y = xy$Y)
    }
    
    p_i <- ggplot2::ggplot() +
      ggplot2::geom_sf(
        data = latam_world_i,
        ggplot2::aes(fill = tmi),
        color = "black",
        linewidth = 0.25
      ) +
      ggplot2::geom_sf(
        data = br_regions_i,
        ggplot2::aes(fill = tmi),
        color = "black",
        linewidth = 0.70
      ) +
      scale_fill_magma(
        limits = latam_bra_regions_abs_limits
      ) +
      ggplot2::coord_sf(expand = FALSE, clip = "off") +
      ggplot2::labs(
        title = paste0(ano),
        fill = "TMI"
      ) +
      theme_map_tmi_safe() +
      ggplot2::theme(
        plot.caption = ggplot2::element_blank()
      )
    
    if (label_values) {
      p_i <- p_i +
        ggrepel::geom_label_repel(
          data = latam_lab_i,
          ggplot2::aes(x = x, y = y, label = lbl),
          size = 2.3,
          family = FONT_FAMILY,
          label.size = 0.15,
          min.segment.length = 0,
          box.padding = 0.12,
          point.padding = 0.03,
          seed = 123,
          fill = "white",
          alpha = 0.90,
          max.overlaps = Inf
        ) +
        ggrepel::geom_label_repel(
          data = br_lab_i,
          ggplot2::aes(x = x, y = y, label = lbl),
          size = 2.7,
          family = FONT_FAMILY,
          label.size = 0.18,
          min.segment.length = 0,
          box.padding = 0.15,
          point.padding = 0.05,
          seed = 123,
          fill = "white",
          alpha = 0.95
        )
    }
    
    p_i
  }
  
  p_latam_abs_bra_regions_pair <- make_latam_abs_bra_regions_panel(year_base) +
    make_latam_abs_bra_regions_panel(year_sel) +
    patchwork::plot_layout(guides = "collect") +
    patchwork::plot_annotation(
      title = paste0("LATAM — TMI (", year_base, " e ", year_sel, ") | Brasil por Grandes Regiões"),
      caption = source_caption,
      theme = ggplot2::theme(
        plot.background = ggplot2::element_rect(fill = "grey92", color = NA),
        text = ggplot2::element_text(family = FONT_FAMILY),
        plot.title = ggplot2::element_text(size = TITLE_SIZE, family = FONT_FAMILY),
        plot.caption = ggplot2::element_text(size = CAP_SIZE, family = FONT_FAMILY),
        legend.position = "bottom"
      )
    ) &
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(fill = "grey92", color = NA)
    )
  
  save_png_map(
    p_latam_abs_bra_regions_pair,
    file.path(
      graficos_dir,
      paste0("choropleth_latam_paises_tmi_", year_base, "_", year_sel, "_brasil_regioes_patchwork.png")
    ),
    width = 16,
    height = 8.6
  )
  
  # DELTAS (ABS e %)
  latam_delta <- latam_c |>
    dplyr::filter(year %in% c(year_base, year_sel)) |>
    dplyr::group_by(unit, iso3c) |>
    dplyr::summarise(
      tmi_base = tmi[year == year_base][1],
      tmi_sel  = tmi[year == year_sel][1],
      delta_abs = tmi_sel - tmi_base,
      delta_pct = 100 * (tmi_sel / tmi_base - 1),
      .groups = "drop"
    )
  
  latam_world_delta <- world_sf |>
    dplyr::inner_join(latam_delta, by = c("iso3" = "iso3c")) |>
    sf::st_crop(bb)
  
  latam_lab_delta_abs <- latam_world_delta |>
    dplyr::mutate(lbl = sprintf("%+.1f", delta_abs))
  
  latam_lab_delta_pct <- latam_world_delta |>
    dplyr::mutate(lbl = sprintf("%+.0f%%", delta_pct))
  
  if (label_values) {
    xy1 <- sf_label_points(latam_lab_delta_abs)
    latam_lab_delta_abs <- latam_lab_delta_abs |> dplyr::mutate(x = xy1$X, y = xy1$Y)
    
    xy2 <- sf_label_points(latam_lab_delta_pct)
    latam_lab_delta_pct <- latam_lab_delta_pct |> dplyr::mutate(x = xy2$X, y = xy2$Y)
  }
  
  br_delta <- br_regioes_c |>
    dplyr::filter(year %in% c(year_base, year_sel)) |>
    dplyr::group_by(unit) |>
    dplyr::summarise(
      tmi_base = tmi[year == year_base][1],
      tmi_sel  = tmi[year == year_sel][1],
      delta_abs = tmi_sel - tmi_base,
      delta_pct = 100 * (tmi_sel / tmi_base - 1),
      .groups = "drop"
    ) |>
    dplyr::mutate(region_key = normalize_region(unit))
  
  br_map_delta <- br_regions_sf |>
    dplyr::left_join(br_delta, by = "region_key")
  
  br_lab_delta_abs <- br_map_delta |>
    dplyr::mutate(lbl = sprintf("%+.1f", delta_abs))
  
  br_lab_delta_pct <- br_map_delta |>
    dplyr::mutate(lbl = sprintf("%+.0f%%", delta_pct))
  
  if (label_values) {
    xy3 <- sf_label_points(br_lab_delta_abs)
    br_lab_delta_abs <- br_lab_delta_abs |> dplyr::mutate(x = xy3$X, y = xy3$Y)
    
    xy4 <- sf_label_points(br_lab_delta_pct)
    br_lab_delta_pct <- br_lab_delta_pct |> dplyr::mutate(x = xy4$X, y = xy4$Y)
  }
  
  p_br_delta_abs <- ggplot2::ggplot(br_map_delta) +
    ggplot2::geom_sf(ggplot2::aes(fill = delta_abs), color = "black", linewidth = 0.9) +
    scale_fill_magma(na.value = "grey85") +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("Brasil — ΔTMI por Grande Região (", year_base, "→", year_sel, ")"),
      subtitle = "Valores negativos indicam queda no período",
      fill = "Δ TMI",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_br_delta_abs <- p_br_delta_abs +
      ggrepel::geom_label_repel(
        data = br_lab_delta_abs,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 3.0,
        family = FONT_FAMILY,
        label.size = 0.2,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.05,
        seed = 123,
        fill = "white",
        alpha = 0.95
      )
  }
  
  save_png_map(
    p_br_delta_abs,
    file.path(graficos_dir, paste0("choropleth_br_regioes_delta_abs_tmi_", year_base, "_", year_sel, ".png")),
    width_delta, height_delta
  )
  
  p_br_delta_pct <- ggplot2::ggplot(br_map_delta) +
    ggplot2::geom_sf(ggplot2::aes(fill = delta_pct), color = "black", linewidth = 0.9) +
    scale_fill_magma(na.value = "grey85") +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("Brasil — ΔTMI (%) por Grande Região (", year_base, "→", year_sel, ")"),
      subtitle = "Valores negativos indicam queda no período",
      fill = "Δ %",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_br_delta_pct <- p_br_delta_pct +
      ggrepel::geom_label_repel(
        data = br_lab_delta_pct,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 3.0,
        family = FONT_FAMILY,
        label.size = 0.2,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.05,
        seed = 123,
        fill = "white",
        alpha = 0.95
      )
  }
  
  save_png_map(
    p_br_delta_pct,
    file.path(graficos_dir, paste0("choropleth_br_regioes_delta_pct_tmi_", year_base, "_", year_sel, ".png")),
    width_delta, height_delta
  )
  
  # LATAM Δ ABS/% (Brasil como país)
  p_latam_delta_abs_bra <- ggplot2::ggplot(latam_world_delta) +
    ggplot2::geom_sf(ggplot2::aes(fill = delta_abs), color = "black", linewidth = 0.25) +
    scale_fill_magma() +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("LATAM — ΔTMI (", year_base, "→", year_sel, ") | Brasil como país"),
      subtitle = "Valores negativos indicam queda no período",
      fill = "Δ TMI",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_latam_delta_abs_bra <- p_latam_delta_abs_bra +
      ggrepel::geom_label_repel(
        data = latam_lab_delta_abs,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.4,
        family = FONT_FAMILY,
        label.size = 0.15,
        min.segment.length = 0,
        box.padding = 0.12,
        point.padding = 0.03,
        seed = 123,
        fill = "white",
        alpha = 0.90,
        max.overlaps = Inf
      )
  }
  
  save_png_map(
    p_latam_delta_abs_bra,
    file.path(graficos_dir, paste0("choropleth_latam_delta_abs_tmi_", year_base, "_", year_sel, "_brasil_pais.png")),
    width_delta, height_delta
  )
  
  p_latam_delta_pct_bra <- ggplot2::ggplot(latam_world_delta) +
    ggplot2::geom_sf(ggplot2::aes(fill = delta_pct), color = "black", linewidth = 0.25) +
    scale_fill_magma() +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("LATAM — ΔTMI (%) (", year_base, "→", year_sel, ") | Brasil como país"),
      subtitle = "Valores negativos indicam queda no período",
      fill = "Δ %",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_latam_delta_pct_bra <- p_latam_delta_pct_bra +
      ggrepel::geom_label_repel(
        data = latam_lab_delta_pct,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.4,
        family = FONT_FAMILY,
        label.size = 0.15,
        min.segment.length = 0,
        box.padding = 0.12,
        point.padding = 0.03,
        seed = 123,
        fill = "white",
        alpha = 0.90,
        max.overlaps = Inf
      )
  }
  
  save_png_map(
    p_latam_delta_pct_bra,
    file.path(graficos_dir, paste0("choropleth_latam_delta_pct_tmi_", year_base, "_", year_sel, "_brasil_pais.png")),
    width_delta, height_delta
  )
  
  # LATAM Δ ABS/% (Brasil por regiões)
  latam_world_no_bra_delta <- latam_world_delta |>
    dplyr::filter(iso3 != "BRA")
  
  br_regions_delta_crop <- br_map_delta |>
    sf::st_crop(bb)
  
  br_lab_delta_abs2 <- br_regions_delta_crop |>
    dplyr::mutate(lbl = sprintf("%+.1f", delta_abs))
  
  br_lab_delta_pct2 <- br_regions_delta_crop |>
    dplyr::mutate(lbl = sprintf("%+.0f%%", delta_pct))
  
  if (label_values) {
    xy5 <- sf_label_points(br_lab_delta_abs2)
    br_lab_delta_abs2 <- br_lab_delta_abs2 |> dplyr::mutate(x = xy5$X, y = xy5$Y)
    
    xy6 <- sf_label_points(br_lab_delta_pct2)
    br_lab_delta_pct2 <- br_lab_delta_pct2 |> dplyr::mutate(x = xy6$X, y = xy6$Y)
  }
  
  p_latam_delta_abs_bra_regions <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = latam_world_no_bra_delta,
      ggplot2::aes(fill = delta_abs),
      color = "black",
      linewidth = 0.25
    ) +
    ggplot2::geom_sf(
      data = br_regions_delta_crop,
      ggplot2::aes(fill = delta_abs),
      color = "black",
      linewidth = 0.70
    ) +
    scale_fill_magma() +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("LATAM — ΔTMI (", year_base, "→", year_sel, ") | Brasil por Grandes Regiões"),
      subtitle = "Valores negativos indicam queda no período",
      fill = "Δ TMI",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_latam_delta_abs_bra_regions <- p_latam_delta_abs_bra_regions +
      ggrepel::geom_label_repel(
        data = latam_lab_delta_abs |> dplyr::filter(iso3 != "BRA"),
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.3,
        family = FONT_FAMILY,
        label.size = 0.15,
        min.segment.length = 0,
        box.padding = 0.12,
        point.padding = 0.03,
        seed = 123,
        fill = "white",
        alpha = 0.90,
        max.overlaps = Inf
      ) +
      ggrepel::geom_label_repel(
        data = br_lab_delta_abs2,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.7,
        family = FONT_FAMILY,
        label.size = 0.18,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.05,
        seed = 123,
        fill = "white",
        alpha = 0.95
      )
  }
  
  save_png_map(
    p_latam_delta_abs_bra_regions,
    file.path(graficos_dir, paste0("choropleth_latam_delta_abs_tmi_", year_base, "_", year_sel, "_brasil_regioes.png")),
    width_delta, height_delta
  )
  
  p_latam_delta_pct_bra_regions <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = latam_world_no_bra_delta,
      ggplot2::aes(fill = delta_pct),
      color = "black",
      linewidth = 0.25
    ) +
    ggplot2::geom_sf(
      data = br_regions_delta_crop,
      ggplot2::aes(fill = delta_pct),
      color = "black",
      linewidth = 0.70
    ) +
    scale_fill_magma() +
    ggplot2::coord_sf(expand = FALSE, clip = "off") +
    ggplot2::labs(
      title = paste0("LATAM — ΔTMI (%) (", year_base, "→", year_sel, ") | Brasil por Grandes Regiões"),
      subtitle = "Valores negativos indicam queda no período",
      fill = "Δ %",
      caption = source_caption
    ) +
    theme_map_tmi_safe()
  
  if (label_values) {
    p_latam_delta_pct_bra_regions <- p_latam_delta_pct_bra_regions +
      ggrepel::geom_label_repel(
        data = latam_lab_delta_pct |> dplyr::filter(iso3 != "BRA"),
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.3,
        family = FONT_FAMILY,
        label.size = 0.15,
        min.segment.length = 0,
        box.padding = 0.12,
        point.padding = 0.03,
        seed = 123,
        fill = "white",
        alpha = 0.90,
        max.overlaps = Inf
      ) +
      ggrepel::geom_label_repel(
        data = br_lab_delta_pct2,
        ggplot2::aes(x = x, y = y, label = lbl),
        size = 2.7,
        family = FONT_FAMILY,
        label.size = 0.18,
        min.segment.length = 0,
        box.padding = 0.15,
        point.padding = 0.05,
        seed = 123,
        fill = "white",
        alpha = 0.95
      )
  }
  
  save_png_map(
    p_latam_delta_pct_bra_regions,
    file.path(graficos_dir, paste0("choropleth_latam_delta_pct_tmi_", year_base, "_", year_sel, "_brasil_regioes.png")),
    width_delta, height_delta
  )
  
  invisible(TRUE)
}

#year_base <- 2019

make_choropleths_tmi(year_sel = latest_year, year_base = start_common, label_values = TRUE)

# =========================
# 11) Exportar tabelas (Excel) -> outputs_tmi_final/
# =========================
wb <- openxlsx::createWorkbook()

write_sheet <- function(wb, sheet_name, data) {
  sheet_name <- substr(sheet_name, 1, 31)
  openxlsx::addWorksheet(wb, sheet_name)
  if (is.null(data) || nrow(data) == 0) {
    openxlsx::writeData(wb, sheet_name, tibble::tibble(mensagem = "Sem registros para os critérios definidos."))
  } else {
    openxlsx::writeData(wb, sheet_name, data)
  }
}

write_sheet(wb, "Parametros_analise", parametros_analise)
write_sheet(wb, "BR_regioes_series", br_regioes_c)
write_sheet(wb, "LATAM_series", latam_c)
write_sheet(wb, paste0("Ranking_", latest_year), rank_latest)
write_sheet(wb, "Gaps_regioes_vs_LATAM", gap_regioes)
write_sheet(wb, "Wins_regioes_ultimo", wins_regioes)
write_sheet(wb, paste0("Change_", start_common, "_", latest_year), change_long)

if (nrow(change_recent) > 0) {
  write_sheet(wb, paste0("Change_2019_", latest_year), change_recent)
}

write_sheet(wb, "Janelas_tmi", janelas_tmi)
write_sheet(wb, "Episodios_locais", episodios_locais)
write_sheet(wb, "Episodios_regioes", episodios_regioes)
write_sheet(wb, "Reversoes_princip", reversoes_principais)
write_sheet(wb, "Estagnacoes_princip", estagnacoes_principais)
write_sheet(wb, "Ranking_inicio", ranking_anos_inicio)
write_sheet(wb, "Ranking_inicio_reg", ranking_anos_inicio_regioes)
write_sheet(wb, "Episodios_resumo", episodios_resumo_unidade)

write_sheet(wb, "Seq_diag", diagnostico_sequencias)
write_sheet(wb, "Seq_diag_regioes", diagnostico_sequencias_regioes)

write_sheet(wb, "Similares_por_regiao", top_matches_by_region)
write_sheet(wb, "Melhor_regiao_por_pais", best_region_for_country)
write_sheet(wb, "Cluster_seq_comp", cluster_seq_comparison)
write_sheet(wb, "Cluster_event_comp", cluster_episode_comparison)

openxlsx::saveWorkbook(
  wb,
  file = file.path(out_dir, "tmi_comparativo_final.xlsx"),
  overwrite = TRUE
)

# =========================
# 12) Tabelas no console (RStudio/Positron)
# =========================
cat("\n==============================\n")
cat("TMI — COMPARATIVO BR (REGIÕES) vs LATAM\n")
cat("Janela comum:", start_common, "–", latest_year, "\n")
cat("==============================\n")

latam_latest_stats <- latam_latest |>
  dplyr::summarise(
    year = latest_year,
    n_countries = dplyr::n_distinct(unit),
    mean   = mean(tmi, na.rm = TRUE),
    sd     = stats::sd(tmi, na.rm = TRUE),
    min    = min(tmi, na.rm = TRUE),
    p25    = unname(stats::quantile(tmi, 0.25, na.rm = TRUE)),
    median = stats::median(tmi, na.rm = TRUE),
    p75    = unname(stats::quantile(tmi, 0.75, na.rm = TRUE)),
    max    = max(tmi, na.rm = TRUE)
  )

cat("\n--- LATAM: estatísticas descritivas (último ano comum) ---\n")
print(latam_latest_stats)

latam_ref_latest <- latam_ref |>
  dplyr::filter(year == latest_year)

regions_latest <- br_regioes_c |>
  dplyr::filter(year == latest_year) |>
  dplyr::left_join(latam_ref_latest, by = "year") |>
  dplyr::rowwise() |>
  dplyr::mutate(
    rank_among_latam = 1L + sum(latam_latest$tmi < tmi, na.rm = TRUE),
    percentile_latam = 100 * mean(latam_latest$tmi <= tmi, na.rm = TRUE),
    paises_com_tmi_maior = sum(latam_latest$tmi > tmi, na.rm = TRUE),
    paises_total_latam   = nrow(latam_latest),
    gap_to_latam_median  = tmi - latam_median,
    gap_to_latam_mean    = tmi - latam_mean
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    unit, year, tmi,
    gap_to_latam_median, gap_to_latam_mean,
    percentile_latam, rank_among_latam,
    paises_com_tmi_maior, paises_total_latam
  ) |>
  dplyr::arrange(tmi)

cat("\n--- Regiões BR no último ano (comparado à distribuição LATAM) ---\n")
print(regions_latest, n = Inf)

cat("\n--- Movimento 2000 → último ano (Regiões BR) ---\n")
print(change_long |> dplyr::filter(type == "Região BR") |> dplyr::arrange(pct_change), n = Inf)

cat("\n--- Movimento 2000 → último ano (Países LATAM: 15 primeiras linhas ordenadas) ---\n")
print(change_long |> dplyr::filter(type == "País LATAM") |> dplyr::arrange(pct_change) |> dplyr::slice_head(n = 15), n = Inf)

if (nrow(change_recent) > 0) {
  cat("\n--- Movimento 2019 → último ano (Regiões BR) ---\n")
  print(change_recent |> dplyr::filter(type == "Região BR") |> dplyr::arrange(desc(pct_change_2019)), n = Inf)
  
  cat("\n--- Movimento 2019 → último ano (Países LATAM: top 10 piora e top 10 melhora) ---\n")
  cat("\nTop 10 PIORA:\n")
  print(change_recent |> dplyr::filter(type == "País LATAM") |> dplyr::arrange(desc(pct_change_2019)) |> dplyr::slice_head(n = 10), n = Inf)
  cat("\nTop 10 MELHORA:\n")
  print(change_recent |> dplyr::filter(type == "País LATAM") |> dplyr::arrange(pct_change_2019) |> dplyr::slice_head(n = 10), n = Inf)
}

cat("\n--- Países LATAM mais parecidos com cada região (top 8 por score) ---\n")
print(top_matches_by_region, n = Inf)

cat("\n--- Para cada país, qual região BR ele mais parece (ranking por score) ---\n")
print(best_region_for_country |> dplyr::arrange(score), n = Inf)

# =========================
# 13) Prints finais
# =========================
cat("\nOK!\n")
cat("Excel em:", normalizePath(out_dir, winslash = "/"), "\n")
cat("Gráficos em:", normalizePath(graficos_dir, winslash = "/"), "\n")
cat("Janela comum:", start_common, "–", latest_year, "\n")
cat("Arquivo Excel:", file.path(out_dir, "tmi_comparativo_final.xlsx"), "\n")
