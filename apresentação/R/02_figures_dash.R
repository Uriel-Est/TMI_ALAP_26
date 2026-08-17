# ============================================================
# R/02_figures_dash.R
# Figuras para a apresentação Quarto/reveal.js — TMI LATAM/ALAP
# Gera PNG/PDF em figures/dash/ com mapas estáticos rotulados.
# ============================================================

# ---------- Pacotes ----------
required_pkgs <- c(
  "dplyr", "tidyr", "stringr", "readxl", "readr", "ggplot2", "sf",
  "geobr", "rnaturalearth", "rnaturalearthdata", "scales", "forcats", "ggrepel", "purrr", "tibble"
)
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Pacotes ausentes: ", paste(missing_pkgs, collapse = ", "),
    "\nInstale com install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))"
  )
}

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(readr)
library(ggplot2)
library(sf)
library(geobr)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(forcats)
library(ggrepel)
library(purrr)
library(tibble)

# ---------- Caminhos ----------
if (!exists("project_dir")) {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (basename(project_dir) == "R") project_dir <- dirname(project_dir)
}
if (!exists("data_dir")) data_dir <- file.path(project_dir, "data")
if (!exists("figures_dir")) figures_dir <- file.path(project_dir, "figures")

dash_fig_dir <- file.path(figures_dir, "dash")
dir.create(dash_fig_dir, recursive = TRUE, showWarnings = FALSE)

find_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0) return(NA_character_)
  hit[[1]]
}

br_file <- find_first_existing(c(
  file.path(data_dir, "brasil_00_24.xlsx"),
  file.path(project_dir, "brasil_00_24.xlsx"),
  file.path(dirname(project_dir), "brasil_00_24.xlsx")
))

latam_file <- find_first_existing(c(
  file.path(data_dir, "tmi_latam_onu_NOVO.csv"),
  file.path(project_dir, "tmi_latam_onu_NOVO.csv"),
  file.path(dirname(project_dir), "tmi_latam_onu_NOVO.csv")
))

if (is.na(br_file)) stop("Não encontrei brasil_00_24.xlsx em data/, na raiz do projeto, ou na pasta acima.")
if (is.na(latam_file)) stop("Não encontrei tmi_latam_onu_NOVO.csv em data/, na raiz do projeto, ou na pasta acima.")

# ---------- Parâmetros visuais ----------
FONT_FAMILY <- "sans"

pal <- list(
  navy = "#102A43", cyan = "#1CA7C9", teal = "#0F766E",
  rose = "#F6E8E8", orange = "#E69F00", red = "#D55E00",
  gray = "#64748B", grid = "#E2E8F0", light = "#F8FAFC",
  white = "#FFFFFF"
)

pal_okabe <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#000000", "#F0E442")
linetypes_accessible <- c("solid", "longdash", "dotdash", "dashed", "twodash", "dotted", "solid", "longdash")
shapes_accessible <- c(16, 17, 15, 18, 8, 3, 7, 4)

source_caption <- "Fontes: Brasil — SVSA/MS; países — ONU/UN IGME. Elaboração própria."
regioes <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
start_common <- 2000L
latest_year <- 2023L
limiar_estagnacao <- 0.1


# ============================================================
# Escalas fixas para comparabilidade visual
# ============================================================

tmi_fixed_limits <- c(0, 60)
tmi_fixed_breaks <- seq(0, 60, by = 10)

tmi_fixed_colors <- c(
  "#FFF7BC",
  "#FEC44F",
  "#FE9929",
  "#EC7014",
  "#CC4C02",
  "#8C2D04",
  "#3B0F70"
)

scale_fill_tmi_fixed <- function(name = "TMI") {
  ggplot2::scale_fill_gradientn(
    colours = c(
      "#fff7bc", "#fee391", "#fec44f", "#fdae6b", "#f16913",
      "#dd3497", "#ae017e", "#7a0177", "#49006a", "#000000"
    ),
    limits = tmi_fixed_limits,
    breaks = tmi_fixed_breaks,
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ","
    ),
    oob = scales::squish,
    name = name,
    na.value = "#F1F5F9",
    guide = ggplot2::guide_colourbar(
      nbin = 200,
      title.position = "top",
      barwidth = grid::unit(7.2, "cm"),
      barheight = grid::unit(0.30, "cm")
    )
  )
}

scale_y_tmi_fixed <- function(name = "TMI") {
  ggplot2::scale_y_continuous(
    limits = tmi_fixed_limits,
    breaks = tmi_fixed_breaks,
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = ggplot2::expansion(mult = c(0.02, 0.04)),
    name = name
  )
}

scale_x_similarity_score_fixed <- function(name = "Score composto") {
  ggplot2::scale_x_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, by = 1),
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ","
    ),
    expand = ggplot2::expansion(mult = c(0, 0.08)),
    name = name
  )
}

delta_abs_fixed_limits <- c(-3.2, 1.6)
delta_abs_fixed_breaks <- c(-3, -2, -1, 0, 1)

delta_pct_fixed_limits <- c(-50, 40)
delta_pct_fixed_breaks <- c(-50, -25, 0, 25, 40)

scale_fill_delta_abs_fixed <- function(name = "Δ TMI") {
  ggplot2::scale_fill_gradient2(
    low = "#313695",
    mid = "#F8FAFC",
    high = "#D73027",
    midpoint = 0,
    limits = delta_abs_fixed_limits,
    breaks = delta_abs_fixed_breaks,
    labels = scales::label_number(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    oob = scales::squish,
    name = name,
    na.value = "#F1F5F9"
  )
}

scale_fill_delta_pct_fixed <- function(name = "Δ %") {
  ggplot2::scale_fill_gradient2(
    low = "#313695",
    mid = "#F8FAFC",
    high = "#D73027",
    midpoint = 0,
    limits = delta_pct_fixed_limits,
    breaks = delta_pct_fixed_breaks,
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ",",
      suffix = "%"
    ),
    oob = scales::squish,
    name = name,
    na.value = "#F1F5F9"
  )
}

scale_x_delta_abs_recent_fixed <- function(name = "Δ TMI, 2019–2023") {
  ggplot2::scale_x_continuous(
    limits = c(-0.6, 1.6),
    breaks = c(-0.5, 0, 0.5, 1.0, 1.5),
    labels = scales::label_number(
      accuracy = 0.1,
      decimal.mark = ","
    ),
    oob = scales::squish,
    expand = ggplot2::expansion(mult = c(0, 0.12)),
    name = name
  )
}

scale_x_delta_pct_recent_fixed <- function(name = "Δ %, 2019–2023") {
  ggplot2::scale_x_continuous(
    limits = c(-10, 40),
    breaks = c(-10, 0, 10, 20, 30, 40),
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ",",
      suffix = "%"
    ),
    oob = scales::squish,
    expand = ggplot2::expansion(mult = c(0, 0.12)),
    name = name
  )
}

reduction_abs_fixed_limits <- c(0, 45)
reduction_abs_fixed_breaks <- seq(0, 45, by = 10)

reduction_pct_fixed_limits <- c(0, 75)
reduction_pct_fixed_breaks <- seq(0, 75, by = 15)

reduction_fixed_colors <- c(
  "#F7FCF5",
  "#D9F0A3",
  "#ADDD8E",
  "#78C679",
  "#31A354",
  "#006837"
)

scale_fill_reduction_abs_fixed <- function(name = "Queda TMI") {
  ggplot2::scale_fill_stepsn(
    colours = reduction_fixed_colors,
    limits = reduction_abs_fixed_limits,
    breaks = reduction_abs_fixed_breaks,
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ","
    ),
    oob = scales::squish,
    name = name,
    na.value = "#F1F5F9"
  )
}

scale_fill_reduction_pct_fixed <- function(name = "Queda %") {
  ggplot2::scale_fill_stepsn(
    colours = reduction_fixed_colors,
    limits = reduction_pct_fixed_limits,
    breaks = reduction_pct_fixed_breaks,
    labels = scales::label_number(
      accuracy = 1,
      decimal.mark = ",",
      suffix = "%"
    ),
    oob = scales::squish,
    name = name,
    na.value = "#F1F5F9"
  )
}


# ---------- Utilitários ----------
clean_names <- function(x) {
  x |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

norm_txt <- function(x) {
  x |>
    as.character() |>
    iconv(from = "UTF-8", to = "ASCII//TRANSLIT") |>
    stringr::str_to_lower() |>
    stringr::str_squish()
}

slug <- function(x) {
  x |>
    norm_txt() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

norm01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0, length(x)))
  (x - rng[1]) / diff(rng)
}

safe_cor <- function(x, y) {
  out <- suppressWarnings(stats::cor(x, y, use = "complete.obs"))
  ifelse(is.finite(out), out, 0)
}

safe_slope <- function(df) {
  df <- df |> dplyr::filter(is.finite(tmi), tmi > 0)
  if (nrow(df) < 4) return(NA_real_)
  unname(stats::coef(stats::lm(log(tmi) ~ year, data = df))[2])
}

crop_sf <- function(x, bbox) suppressWarnings(sf::st_crop(x, bbox))

sf_label_points <- function(x) {
  suppressWarnings(sf::st_point_on_surface(x))
}

save_dash_plot <- function(p, filename, width = 11, height = 6.2, dpi = 320) {
  png_path <- file.path(dash_fig_dir, paste0(filename, ".png"))
  pdf_path <- file.path(dash_fig_dir, paste0(filename, ".pdf"))
  
  ggplot2::ggsave(
    filename = png_path,
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    bg = "transparent",
    units = "in",
    limitsize = FALSE
  )
  
  # PDF é secundário aqui. Se o Windows implicar com fonte/dispositivo,
  # não deixa o script inteiro morrer.
  tryCatch(
    {
      if (capabilities("cairo")) {
        ggplot2::ggsave(
          filename = pdf_path,
          plot = p,
          width = width,
          height = height,
          bg = "transparent",
          units = "in",
          limitsize = FALSE,
          device = grDevices::cairo_pdf
        )
      } else {
        ggplot2::ggsave(
          filename = pdf_path,
          plot = p,
          width = width,
          height = height,
          bg = "transparent",
          units = "in",
          limitsize = FALSE
        )
      }
    },
    error = function(e) {
      message("PDF não gerado para ", filename, ": ", conditionMessage(e))
      message("PNG foi gerado normalmente: ", png_path)
    }
  )
  
  invisible(png_path)
}

theme_dash_base <- function(base_size = 13, legend_position = "bottom", show_caption = TRUE) {
  ggplot2::theme_minimal(base_size = base_size, base_family = FONT_FAMILY) +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(color = pal$navy, size = base_size - 1),
      axis.text = ggplot2::element_text(color = pal$navy, size = base_size - 2),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = pal$grid, linewidth = 0.35),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(color = pal$navy, size = base_size - 3, face = "bold"),
      legend.text = ggplot2::element_text(color = pal$navy, size = base_size - 3),
      plot.caption = if (show_caption) {
        ggplot2::element_text(color = pal$gray, size = base_size - 4, hjust = 1)
      } else {
        ggplot2::element_blank()
      },
      plot.background = ggplot2::element_rect(fill = NA, color = NA),
      panel.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      plot.margin = ggplot2::margin(10, 16, 10, 10)
    )
}

theme_dash_map <- function(base_size = 12) {
  ggplot2::theme_void(base_size = base_size, base_family = FONT_FAMILY) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = pal$navy, size = base_size - 2, face = "bold"),
      legend.text = ggplot2::element_text(color = pal$navy, size = base_size - 3),
      plot.caption = ggplot2::element_text(color = pal$gray, size = base_size - 4, hjust = 1),
      plot.background = ggplot2::element_rect(fill = NA, color = NA),
      panel.background = ggplot2::element_rect(fill = NA, color = NA),
      legend.background = ggplot2::element_rect(fill = NA, color = NA),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

# ---------- Leitura dos dados ----------
read_brazil_tmi <- function(path) {
  br_wide <- readxl::read_excel(path, sheet = "TMI") |>
    dplyr::mutate(UF = stringr::str_squish(as.character(UF)))
  
  br_long <- br_wide |>
    tidyr::pivot_longer(
      cols = -UF,
      names_to = "year",
      values_to = "tmi"
    ) |>
    dplyr::mutate(
      year = stringr::str_replace_all(as.character(year), "\\*", ""),
      year = as.integer(year),
      tmi = suppressWarnings(as.numeric(tmi)),
      UF = stringr::str_squish(as.character(UF))
    )
  
  br_long |>
    dplyr::filter(
      UF %in% regioes,
      year >= start_common,
      year <= latest_year,
      is.finite(tmi)
    ) |>
    dplyr::transmute(
      type = "Região BR",
      unit = UF,
      year = year,
      tmi = tmi
    )
}

latam_lookup <- tibble::tribble(
  ~iso3, ~unit,
  "ARG", "Argentina",
  "BOL", "Bolívia",
  "BRA", "Brasil",
  "CHL", "Chile",
  "COL", "Colômbia",
  "CRI", "Costa Rica",
  "CUB", "Cuba",
  "DOM", "República Dominicana",
  "ECU", "Equador",
  "SLV", "El Salvador",
  "GTM", "Guatemala",
  "HND", "Honduras",
  "MEX", "México",
  "NIC", "Nicarágua",
  "PAN", "Panamá",
  "PRY", "Paraguai",
  "PER", "Peru",
  "URY", "Uruguai",
  "VEN", "Venezuela"
)

read_latam_tmi <- function(path) {
  raw <- suppressMessages(
    readr::read_csv(path, show_col_types = FALSE, guess_max = 10000)
  )
  
  names(raw) <- clean_names(names(raw))
  
  # Garante compatibilidade caso o lookup esteja com iso3c em vez de iso3
  lookup <- latam_lookup
  if ("iso3c" %in% names(lookup) && !"iso3" %in% names(lookup)) {
    lookup <- lookup |> dplyr::rename(iso3 = iso3c)
  }
  
  # Conversor simples e tolerante para número
  to_num <- function(x) {
    x_chr <- as.character(x)
    x_chr <- stringr::str_replace_all(x_chr, ",", ".")
    suppressWarnings(as.numeric(x_chr))
  }
  
  # ============================================================
  # CASO 1: CSV LONGO, que é o teu caso original
  # Esperado depois de clean_names():
  # country | iso3c | year | sp_dyn_imrt_in
  # ============================================================
  
  code_col_long <- names(raw)[names(raw) %in% c("iso3c", "iso3", "country_code", "code")]
  year_col_long <- names(raw)[names(raw) %in% c("year", "ano")]
  tmi_col_long  <- names(raw)[names(raw) %in% c(
    "sp_dyn_imrt_in",
    "tmi",
    "imr",
    "infant_mortality",
    "infant_mortality_rate",
    "value",
    "valor"
  )]
  
  if (
    length(code_col_long) > 0 &&
    length(year_col_long) > 0 &&
    length(tmi_col_long) > 0
  ) {
    out <- raw |>
      dplyr::transmute(
        iso3 = stringr::str_squish(as.character(.data[[code_col_long[1]]])),
        year = as.integer(.data[[year_col_long[1]]]),
        tmi = to_num(.data[[tmi_col_long[1]]])
      ) |>
      dplyr::filter(iso3 %in% lookup$iso3) |>
      dplyr::left_join(lookup, by = "iso3") |>
      dplyr::filter(
        year >= start_common,
        year <= latest_year,
        is.finite(tmi)
      ) |>
      dplyr::mutate(type = "País LATAM") |>
      dplyr::select(type, iso3, unit, year, tmi)
    
    if (nrow(out) == 0) {
      stop("O CSV LATAM foi lido em formato longo, mas nenhum país/ano da lista foi encontrado.")
    }
    
    return(out)
  }
  
  # ============================================================
  # CASO 2: CSV WIDE, fallback
  # country_code | indicator_code | 2000 | 2001 | ...
  # ============================================================
  
  code_col <- names(raw)[names(raw) %in% c(
    "country_code",
    "codigo_pais",
    "code",
    "iso3",
    "iso3c"
  )]
  
  ind_col <- names(raw)[names(raw) %in% c(
    "indicator_code",
    "codigo_indicador",
    "series_code"
  )]
  
  if (length(code_col) == 0) {
    stop(
      paste0(
        "Não encontrei coluna de código do país no CSV LATAM. Colunas encontradas: ",
        paste(names(raw), collapse = ", ")
      )
    )
  }
  
  if (length(ind_col) > 0) {
    raw <- raw |>
      dplyr::filter(
        .data[[ind_col[1]]] %in% c(
          "SP.DYN.IMRT.IN",
          "sp_dyn_imrt_in"
        )
      )
  }
  
  year_cols <- names(raw)[
    stringr::str_detect(names(raw), "^x?20[0-9]{2}$|^20[0-9]{2}$")
  ]
  
  if (length(year_cols) == 0) {
    stop(
      paste0(
        "Não consegui identificar colunas de ano no CSV LATAM. Colunas encontradas: ",
        paste(names(raw), collapse = ", ")
      )
    )
  }
  
  out <- raw |>
    dplyr::mutate(
      iso3 = stringr::str_squish(as.character(.data[[code_col[1]]]))
    ) |>
    dplyr::filter(iso3 %in% lookup$iso3) |>
    dplyr::select(iso3, dplyr::all_of(year_cols)) |>
    tidyr::pivot_longer(
      cols = -iso3,
      names_to = "year_raw",
      values_to = "tmi"
    ) |>
    dplyr::mutate(
      year = as.integer(stringr::str_extract(year_raw, "20[0-9]{2}")),
      tmi = to_num(tmi)
    ) |>
    dplyr::left_join(lookup, by = "iso3") |>
    dplyr::filter(
      year >= start_common,
      year <= latest_year,
      is.finite(tmi)
    ) |>
    dplyr::mutate(type = "País LATAM") |>
    dplyr::select(type, iso3, unit, year, tmi)
  
  if (nrow(out) == 0) {
    stop("O CSV LATAM foi lido, mas nenhum país/ano da lista foi encontrado.")
  }
  
  out
}

br_regioes_c <- read_brazil_tmi(br_file) |>
  dplyr::mutate(
    type = "Região BR",
    unit = stringr::str_squish(as.character(unit)),
    year = as.integer(year),
    tmi = suppressWarnings(as.numeric(tmi))
  ) |>
  dplyr::filter(
    unit %in% regioes,
    year >= start_common,
    year <= latest_year,
    is.finite(tmi)
  ) |>
  dplyr::summarise(
    tmi = mean(tmi, na.rm = TRUE),
    .by = c(type, unit, year)
  )

latam_c <- read_latam_tmi(latam_file) |>
  dplyr::mutate(
    type = "País LATAM",
    iso3 = stringr::str_squish(as.character(iso3)),
    unit = stringr::str_squish(as.character(unit)),
    year = as.integer(year),
    tmi = suppressWarnings(as.numeric(tmi))
  ) |>
  dplyr::filter(
    iso3 %in% latam_lookup$iso3,
    year >= start_common,
    year <= latest_year,
    is.finite(tmi)
  ) |>
  dplyr::summarise(
    tmi = mean(tmi, na.rm = TRUE),
    .by = c(type, iso3, unit, year)
  )

pool <- dplyr::bind_rows(br_regioes_c, latam_c) |>
  dplyr::mutate(
    unit = stringr::str_squish(as.character(unit)),
    type = stringr::str_squish(as.character(type)),
    year = as.integer(year),
    tmi  = suppressWarnings(as.numeric(tmi))
  ) |>
  dplyr::filter(is.finite(tmi), !is.na(year), !is.na(unit), unit != "") |>
  dplyr::group_by(type, unit, year) |>
  dplyr::summarise(
    iso3 = dplyr::first(iso3[!is.na(iso3) & iso3 != ""], default = NA_character_),
    tmi = mean(tmi, na.rm = TRUE),
    .groups = "drop"
  )

latam_ref <- latam_c |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    latam_median = stats::median(tmi, na.rm = TRUE),
    latam_q10 = unname(stats::quantile(tmi, 0.10, na.rm = TRUE)),
    latam_p25 = unname(stats::quantile(tmi, 0.25, na.rm = TRUE)),
    latam_p75 = unname(stats::quantile(tmi, 0.75, na.rm = TRUE)),
    latam_q90 = unname(stats::quantile(tmi, 0.90, na.rm = TRUE)),
    .groups = "drop"
  )

change_recent <- pool |>
  dplyr::filter(year %in% c(2019L, latest_year)) |>
  dplyr::select(type, unit, year, tmi) |>
  dplyr::summarise(
    tmi = mean(as.numeric(tmi), na.rm = TRUE),
    .by = c(type, unit, year)
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = tmi,
    names_prefix = "tmi_",
    values_fn = list(tmi = ~ mean(as.numeric(.x), na.rm = TRUE))
  ) |>
  dplyr::filter(
    !is.na(.data[["tmi_2019"]]),
    !is.na(.data[[paste0("tmi_", latest_year)]])
  ) |>
  dplyr::mutate(
    abs_change_2019 = as.numeric(.data[[paste0("tmi_", latest_year)]]) -
      as.numeric(.data[["tmi_2019"]]),
    pct_change_2019 = 100 * (
      as.numeric(.data[[paste0("tmi_", latest_year)]]) /
        as.numeric(.data[["tmi_2019"]]) - 1
    )
  )

queda_box_units <- c(
  "Sul", "Sudeste", "Centro-Oeste", "Nordeste", "Norte",
  "México", "Colômbia", "Peru", "Paraguai", "Guatemala"
)

queda_box_2023 <- dplyr::bind_rows(
  br_regioes_c |>
    dplyr::filter(year == latest_year, unit %in% queda_box_units) |>
    dplyr::select(type, unit, tmi),
  
  latam_c |>
    dplyr::filter(year == latest_year, unit %in% queda_box_units) |>
    dplyr::select(type, unit, tmi)
) |>
  dplyr::mutate(order = match(unit, queda_box_units)) |>
  dplyr::arrange(order) |>
  dplyr::select(type, unit, tmi)

readr::write_csv(
  queda_box_2023,
  file.path(dash_fig_dir, "data_queda_box_2023.csv")
)

# ---------- Similaridade ----------
score_one_pair <- function(region_name, country_name) {
  r <- br_regioes_c |> dplyr::filter(unit == region_name) |> dplyr::arrange(year)
  c <- latam_c |> dplyr::filter(unit == country_name) |> dplyr::arrange(year)
  joined <- dplyr::inner_join(
    r |> dplyr::select(year, tmi_r = tmi),
    c |> dplyr::select(year, tmi_c = tmi),
    by = "year"
  )
  if (nrow(joined) < 6) return(NULL)
  
  r_start <- joined$tmi_r[joined$year == start_common][1]
  r_end   <- joined$tmi_r[joined$year == latest_year][1]
  c_start <- joined$tmi_c[joined$year == start_common][1]
  c_end   <- joined$tmi_c[joined$year == latest_year][1]
  
  tibble::tibble(
    region = region_name,
    country = country_name,
    raw_level = abs(r_end - c_end),
    raw_pct = abs(100 * (r_end / r_start - 1) - 100 * (c_end / c_start - 1)),
    raw_slope = abs(safe_slope(tibble::tibble(year = joined$year, tmi = joined$tmi_r)) -
                      safe_slope(tibble::tibble(year = joined$year, tmi = joined$tmi_c))),
    raw_rmse = sqrt(mean((as.numeric(scale(joined$tmi_r)) - as.numeric(scale(joined$tmi_c)))^2, na.rm = TRUE)),
    raw_corr = 1 - safe_cor(diff(joined$tmi_r), diff(joined$tmi_c))
  )
}

pairs_scored <- purrr::map_dfr(regioes, function(reg) {
  purrr::map_dfr(unique(latam_c$unit), function(cty) score_one_pair(reg, cty))
}) |>
  dplyr::group_by(region) |>
  dplyr::mutate(
    comp_level = norm01(raw_level),
    comp_pct = norm01(raw_pct),
    comp_slope = norm01(raw_slope),
    comp_rmse = norm01(raw_rmse),
    comp_corr = norm01(raw_corr),
    score_dash = comp_level + comp_pct + comp_slope + comp_rmse + comp_corr
  ) |>
  dplyr::ungroup()

top_matches_by_region <- pairs_scored |>
  dplyr::arrange(region, score_dash) |>
  dplyr::group_by(region) |>
  dplyr::mutate(rank = dplyr::row_number()) |>
  dplyr::ungroup()

readr::write_csv(top_matches_by_region, file.path(dash_fig_dir, "data_top_matches_by_region.csv"))
readr::write_csv(pairs_scored, file.path(dash_fig_dir, "data_pairs_scored.csv"))
readr::write_csv(change_recent, file.path(dash_fig_dir, "data_change_recent.csv"))

# ---------- Geometrias ----------
bbox_latam <- sf::st_bbox(c(xmin = -123, xmax = -31, ymin = -58, ymax = 35), crs = sf::st_crs(4326))

world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_make_valid() |>
  dplyr::mutate(iso3 = dplyr::coalesce(.data$iso_a3, .data$adm0_a3)) |>
  sf::st_transform(4326)

br_states_sf <- geobr::read_state(year = 2020, simplified = TRUE, showProgress = FALSE) |>
  sf::st_make_valid() |>
  sf::st_transform(4326)

if (!"name_region" %in% names(br_states_sf)) {
  br_states_sf <- br_states_sf |>
    dplyr::mutate(
      abbrev_state = as.character(.data$abbrev_state),
      name_region = dplyr::case_when(
        abbrev_state %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
        abbrev_state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
        abbrev_state %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
        abbrev_state %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
        abbrev_state %in% c("PR", "RS", "SC") ~ "Sul",
        TRUE ~ NA_character_
      )
    )
}

br_regions_sf <- br_states_sf |>
  dplyr::mutate(
    abbrev_state = as.character(.data$abbrev_state),
    region_key = dplyr::case_when(
      abbrev_state %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      abbrev_state %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      abbrev_state %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      abbrev_state %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      abbrev_state %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ NA_character_
    )
  ) |>
  dplyr::filter(!is.na(region_key)) |>
  dplyr::group_by(region_key) |>
  dplyr::summarise(geometry = sf::st_union(geom), .groups = "drop") |>
  sf::st_make_valid()

# ---------- Mapas ----------
make_latam_level_map <- function(year_sel = latest_year) {
  latam_y <- latam_c |> dplyr::filter(year == year_sel)
  br_y <- br_regioes_c |> dplyr::filter(year == year_sel) |> dplyr::rename(region_key = unit)
  
  latam_world <- world_sf |>
    dplyr::inner_join(latam_y, by = "iso3") |>
    dplyr::filter(iso3 != "BRA") |>
    crop_sf(bbox_latam)
  
  br_map <- br_regions_sf |>
    dplyr::left_join(br_y, by = "region_key") |>
    crop_sf(bbox_latam) |>
    dplyr::mutate(unit = region_key, type = "Região BR")
  
  label_countries <- latam_world |>
    sf_label_points() |>
    dplyr::mutate(
      label = paste0(unit, "\n", scales::number(tmi, accuracy = 0.1, decimal.mark = ","))
    )
  
  label_regions <- br_map |>
    sf_label_points() |>
    dplyr::mutate(
      label = paste0(region_key, "\n", scales::number(tmi, accuracy = 0.1, decimal.mark = ","))
    )
  
  label_df <- dplyr::bind_rows(
    label_countries |> dplyr::select(unit, label, geometry),
    label_regions |> dplyr::select(unit, label, geometry)
  )
  
  label_xy <- cbind(label_df, sf::st_coordinates(label_df)) |>
    sf::st_drop_geometry()
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = crop_sf(world_sf, bbox_latam), fill = "#F8FAFC", color = "#CBD5E1", linewidth = 0.18) +
    ggplot2::geom_sf(data = latam_world, ggplot2::aes(fill = tmi), color = "#102A43", linewidth = 0.32) +
    ggplot2::geom_sf(data = br_map, ggplot2::aes(fill = tmi), color = "#102A43", linewidth = 0.42) +
    ggrepel::geom_label_repel(
      data = label_xy,
      ggplot2::aes(x = X, y = Y, label = label),
      family = FONT_FAMILY,
      size = 2.45,
      label.size = 0.15,
      label.padding = ggplot2::unit(0.11, "lines"),
      fill = ggplot2::alpha("white", 0.86),
      color = pal$navy,
      segment.color = ggplot2::alpha(pal$navy, 0.45),
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 123
    ) +
    scale_fill_tmi_fixed(name = "TMI") +
    ggplot2::coord_sf(xlim = c(bbox_latam["xmin"], bbox_latam["xmax"]), ylim = c(bbox_latam["ymin"], bbox_latam["ymax"]), expand = FALSE) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top", barwidth = grid::unit(7.2, "cm"), barheight = grid::unit(0.30, "cm"))) +
    ggplot2::labs(caption = source_caption) +
    theme_dash_map(base_size = 12)
}

make_latam_delta_map <- function(year_base = 2019L, year_sel = latest_year, type_delta = c("abs", "pct")) {
  type_delta <- match.arg(type_delta)
  
  latam_delta <- latam_c |>
    dplyr::filter(year %in% c(year_base, year_sel)) |>
    dplyr::select(iso3, unit, year, tmi) |>
    dplyr::summarise(
      tmi = mean(as.numeric(tmi), na.rm = TRUE),
      .by = c(iso3, unit, year)
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = tmi,
      names_prefix = "tmi_",
      values_fn = list(tmi = ~ mean(as.numeric(.x), na.rm = TRUE))
    ) |>
    dplyr::filter(
      !is.na(.data[[paste0("tmi_", year_base)]]),
      !is.na(.data[[paste0("tmi_", year_sel)]])
    ) |>
    dplyr::mutate(
      delta_abs = as.numeric(.data[[paste0("tmi_", year_sel)]]) -
        as.numeric(.data[[paste0("tmi_", year_base)]]),
      delta_pct = 100 * (
        as.numeric(.data[[paste0("tmi_", year_sel)]]) /
          as.numeric(.data[[paste0("tmi_", year_base)]]) - 1
      )
    )
  
  br_delta <- br_regioes_c |>
    dplyr::filter(year %in% c(year_base, year_sel)) |>
    dplyr::select(region_key = unit, year, tmi) |>
    dplyr::summarise(
      tmi = mean(as.numeric(tmi), na.rm = TRUE),
      .by = c(region_key, year)
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = tmi,
      names_prefix = "tmi_",
      values_fn = list(tmi = ~ mean(as.numeric(.x), na.rm = TRUE))
    ) |>
    dplyr::filter(
      !is.na(.data[[paste0("tmi_", year_base)]]),
      !is.na(.data[[paste0("tmi_", year_sel)]])
    ) |>
    dplyr::mutate(
      delta_abs = as.numeric(.data[[paste0("tmi_", year_sel)]]) -
        as.numeric(.data[[paste0("tmi_", year_base)]]),
      delta_pct = 100 * (
        as.numeric(.data[[paste0("tmi_", year_sel)]]) /
          as.numeric(.data[[paste0("tmi_", year_base)]]) - 1
      )
    )
  
  value_col <- if (type_delta == "abs") "delta_abs" else "delta_pct"
  legend_name <- if (type_delta == "abs") "Δ TMI" else "Δ %"
  acc <- if (type_delta == "abs") 0.1 else 1
  suffix <- if (type_delta == "abs") "" else "%"
  
  latam_world <- world_sf |>
    dplyr::inner_join(latam_delta, by = "iso3") |>
    dplyr::filter(iso3 != "BRA") |>
    crop_sf(bbox_latam) |>
    dplyr::mutate(value = as.numeric(.data[[value_col]]))
  
  br_map <- br_regions_sf |>
    dplyr::left_join(br_delta, by = "region_key") |>
    crop_sf(bbox_latam) |>
    dplyr::mutate(
      unit = region_key,
      value = as.numeric(.data[[value_col]])
    )
  
  label_units <- c(
    "Cuba", "Costa Rica", "Chile", "Panamá", "Nicarágua", "Bolívia",
    "Centro-Oeste", "Norte", "Sudeste", "Nordeste", "Sul"
  )
  
  label_df <- dplyr::bind_rows(
    latam_world |>
      dplyr::filter(unit %in% label_units) |>
      sf_label_points() |>
      dplyr::mutate(
        label = paste0(
          unit, "\n",
          scales::number(value, accuracy = acc, decimal.mark = ","),
          suffix
        )
      ) |>
      dplyr::select(unit, label, geometry),
    
    br_map |>
      dplyr::filter(region_key %in% label_units) |>
      sf_label_points() |>
      dplyr::mutate(
        label = paste0(
          region_key, "\n",
          scales::number(value, accuracy = acc, decimal.mark = ","),
          suffix
        )
      ) |>
      dplyr::select(unit, label, geometry)
  )
  
  label_xy <- cbind(label_df, sf::st_coordinates(label_df)) |>
    sf::st_drop_geometry()
  
  lim <- max(abs(c(latam_world$value, br_map$value)), na.rm = TRUE)
  if (!is.finite(lim) || lim == 0) lim <- 1
  
  fill_scale <- if (type_delta == "abs") {
    scale_fill_delta_abs_fixed(name = legend_name)
  } else {
    scale_fill_delta_pct_fixed(name = legend_name)
  }
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = crop_sf(world_sf, bbox_latam),
      fill = "#F8FAFC",
      color = "#CBD5E1",
      linewidth = 0.18
    ) +
    ggplot2::geom_sf(
      data = latam_world,
      ggplot2::aes(fill = value),
      color = "#102A43",
      linewidth = 0.32
    ) +
    ggplot2::geom_sf(
      data = br_map,
      ggplot2::aes(fill = value),
      color = "#102A43",
      linewidth = 0.42
    ) +
    ggrepel::geom_label_repel(
      data = label_xy,
      ggplot2::aes(x = X, y = Y, label = label),
      family = FONT_FAMILY,
      size = 2.55,
      label.size = 0.15,
      label.padding = ggplot2::unit(0.11, "lines"),
      fill = ggplot2::alpha("white", 0.86),
      color = pal$navy,
      segment.color = ggplot2::alpha(pal$navy, 0.45),
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 123
    ) +
    fill_scale +
    ggplot2::coord_sf(
      xlim = c(bbox_latam["xmin"], bbox_latam["xmax"]),
      ylim = c(bbox_latam["ymin"], bbox_latam["ymax"]),
      expand = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(5.8, "cm"),
        barheight = grid::unit(0.30, "cm"),
        ticks = TRUE,
        draw.ulim = TRUE,
        draw.llim = TRUE
      )
    ) +
    ggplot2::labs(caption = NULL) +
    theme_dash_map(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(color = pal$navy, size = 10, face = "bold"),
      legend.text = ggplot2::element_text(color = pal$navy, size = 9),
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      plot.caption = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(3, 10, 26, 10)
    )
}

# ---------- Séries ----------
make_global_series <- function() {
  endpoints <- br_regioes_c |>
    dplyr::filter(year == latest_year) |>
    dplyr::mutate(lbl = paste0(unit, " — ", scales::number(tmi, accuracy = 0.1, decimal.mark = ",")))
  
  col_map <- stats::setNames(pal_okabe[seq_along(regioes)], regioes)
  lt_map <- stats::setNames(linetypes_accessible[seq_along(regioes)], regioes)
  shape_map <- stats::setNames(shapes_accessible[seq_along(regioes)], regioes)
  
  ymax <- max(c(latam_ref$latam_q90, br_regioes_c$tmi), na.rm = TRUE) * 1.14
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = latam_ref, ggplot2::aes(x = year, ymin = latam_q10, ymax = latam_q90), fill = "#94A3B8", alpha = 0.20) +
    ggplot2::geom_ribbon(data = latam_ref, ggplot2::aes(x = year, ymin = latam_p25, ymax = latam_p75), fill = "#64748B", alpha = 0.18) +
    ggplot2::geom_line(data = latam_ref, ggplot2::aes(x = year, y = latam_median), color = "#334155", linetype = "dashed", linewidth = 0.95) +
    ggplot2::geom_line(data = br_regioes_c, ggplot2::aes(x = year, y = tmi, color = unit, linetype = unit), linewidth = 1.16) +
    ggplot2::geom_point(data = br_regioes_c |> dplyr::filter(year %in% c(start_common, latest_year)), ggplot2::aes(x = year, y = tmi, color = unit, shape = unit), size = 2.4) +
    ggrepel::geom_text_repel(data = endpoints, ggplot2::aes(x = year, y = tmi, label = lbl, color = unit), family = FONT_FAMILY, size = 3.5, nudge_x = 0.55, direction = "y", min.segment.length = 0, segment.alpha = 0.5, seed = 123, max.overlaps = Inf, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = col_map) +
    ggplot2::scale_linetype_manual(values = lt_map) +
    ggplot2::scale_shape_manual(values = shape_map) +
    ggplot2::scale_x_continuous(breaks = seq(start_common, latest_year, by = 4), expand = ggplot2::expansion(mult = c(0.01, 0.18))) +
    scale_y_tmi_fixed(name = "TMI") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "TMI", color = NULL, linetype = NULL, shape = NULL, caption = "Faixa clara: q10–q90 LATAM; faixa escura: q25–q75 LATAM; linha tracejada: mediana LATAM. Fontes: Brasil — SVSA/MS; países — ONU/UN IGME.") +
    theme_dash_base(base_size = 14, legend_position = "none", show_caption = TRUE) +
    ggplot2::theme(plot.margin = ggplot2::margin(18, 70, 12, 12))
}

make_similarity_series <- function(reg) {
  reg_key <- stringr::str_squish(reg)
  top_countries <- top_matches_by_region |>
    dplyr::filter(region == reg_key) |>
    dplyr::arrange(score_dash) |>
    dplyr::slice_head(n = 3) |>
    dplyr::pull(country)
  
  df_plot <- dplyr::bind_rows(
    br_regioes_c |> dplyr::filter(unit == reg_key) |> dplyr::mutate(series_type = "Região BR"),
    latam_c |> dplyr::filter(unit %in% top_countries) |> dplyr::mutate(series_type = "País LATAM")
  ) |>
    dplyr::mutate(unit = factor(unit, levels = c(reg_key, top_countries)))
  
  endpoints <- df_plot |> dplyr::filter(year == latest_year) |> dplyr::mutate(lbl = paste0(unit, " — ", scales::number(tmi, accuracy = 0.1, decimal.mark = ",")))
  units <- levels(df_plot$unit)
  col_map <- stats::setNames(c("#D55E00", pal_okabe[1:3]), units)
  lt_map <- stats::setNames(c("solid", "longdash", "dotdash", "dashed"), units)
  shape_map <- stats::setNames(c(16, 17, 15, 18), units)
  
  ymax <- max(df_plot$tmi, latam_ref$latam_p75, na.rm = TRUE) * 1.12
  
  ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = latam_ref, ggplot2::aes(x = year, ymin = latam_p25, ymax = latam_p75), fill = "#CBD5E1", alpha = 0.55) +
    ggplot2::geom_line(data = latam_ref, ggplot2::aes(x = year, y = latam_median), color = "#334155", linetype = "dashed", linewidth = 0.75) +
    ggplot2::geom_line(data = df_plot, ggplot2::aes(x = year, y = tmi, color = unit, linetype = unit), linewidth = 1.12) +
    ggplot2::geom_point(data = df_plot |> dplyr::filter(year %in% c(start_common, latest_year)), ggplot2::aes(x = year, y = tmi, color = unit, shape = unit), size = 2.5) +
    ggrepel::geom_text_repel(data = endpoints, ggplot2::aes(x = year, y = tmi, label = lbl, color = unit), family = FONT_FAMILY, size = 3.2, nudge_x = 0.45, direction = "y", min.segment.length = 0, segment.alpha = 0.5, seed = 123, max.overlaps = Inf, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = col_map) +
    ggplot2::scale_linetype_manual(values = lt_map) +
    ggplot2::scale_shape_manual(values = shape_map) +
    ggplot2::scale_x_continuous(breaks = seq(start_common, latest_year, by = 4), expand = ggplot2::expansion(mult = c(0.01, 0.18))) +
    scale_y_tmi_fixed(name = "TMI") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(x = NULL, y = "TMI", color = NULL, linetype = NULL, shape = NULL, caption = source_caption) +
    theme_dash_base(base_size = 13, legend_position = "bottom", show_caption = TRUE) +
    ggplot2::theme(plot.margin = ggplot2::margin(14, 60, 12, 12))
}

make_similarity_ranking <- function(reg) {
  reg_key <- stringr::str_squish(reg)
  df <- top_matches_by_region |>
    dplyr::filter(region == reg_key) |>
    dplyr::arrange(score_dash) |>
    dplyr::slice_head(n = 8) |>
    dplyr::mutate(country = forcats::fct_reorder(country, score_dash, .desc = TRUE))
  
  ggplot2::ggplot(df, ggplot2::aes(x = score_dash, y = country)) +
    ggplot2::geom_col(fill = "#1CA7C9", alpha = 0.85, width = 0.70) +
    ggplot2::geom_text(ggplot2::aes(label = scales::number(score_dash, accuracy = 0.01, decimal.mark = ",")), hjust = -0.15, family = FONT_FAMILY, size = 3.2, color = pal$navy, fontface = "bold") +
    scale_x_similarity_score_fixed(name = "Score composto") +
    ggplot2::labs(x = "Score composto", y = NULL, caption = "Menor score = maior similaridade relativa.") +
    theme_dash_base(base_size = 12, legend_position = "none", show_caption = TRUE) +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(), axis.text.y = ggplot2::element_text(face = "bold"), plot.margin = ggplot2::margin(8, 22, 8, 8))
}

make_similarity_components <- function(reg) {
  reg_key <- stringr::str_squish(reg)
  
  df <- top_matches_by_region |>
    dplyr::filter(region == reg_key) |>
    dplyr::arrange(score_dash) |>
    dplyr::slice_head(n = 5) |>
    dplyr::select(country, Nível = comp_level, Queda = comp_pct, Inclinação = comp_slope, Formato = comp_rmse, Ritmo = comp_corr) |>
    tidyr::pivot_longer(cols = -country, names_to = "component", values_to = "value") |>
    dplyr::mutate(
      country = forcats::fct_rev(factor(country, levels = unique(country))),
      component = factor(component, levels = c("Nível", "Queda", "Inclinação", "Formato", "Ritmo")),
      text_col = dplyr::if_else(value >= 0.55, "white", "#102A43")
    )
  
  ggplot2::ggplot(df, ggplot2::aes(x = component, y = country, fill = value)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = scales::number(value, accuracy = 0.01, decimal.mark = ","), color = text_col), size = 3.5, family = FONT_FAMILY, fontface = "bold") +
    ggplot2::scale_color_identity() +
    ggplot2::scale_fill_gradient(low = "#F8FAFC", high = "#102A43", limits = c(0, 1), oob = scales::squish, name = "Distância relativa") +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = grid::unit(7.2, "cm"), barheight = grid::unit(0.34, "cm"))) +
    ggplot2::labs(x = NULL, y = NULL, caption = NULL) +
    theme_dash_base(base_size = 13, legend_position = "bottom", show_caption = FALSE) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(), axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, face = "bold"), axis.text.y = ggplot2::element_text(face = "bold"), legend.margin = ggplot2::margin(t = 3, b = 10), legend.box.margin = ggplot2::margin(t = 2, b = 8), plot.margin = ggplot2::margin(18, 14, 28, 14))
}

make_recent_ranking <- function(type_delta = c("abs", "pct")) {
  type_delta <- match.arg(type_delta)
  
  value_col <- if (type_delta == "abs") "abs_change_2019" else "pct_change_2019"
  x_label <- if (type_delta == "abs") "Δ TMI, 2019–2023" else "Δ %, 2019–2023"
  acc <- if (type_delta == "abs") 0.1 else 1
  suffix <- if (type_delta == "abs") "" else "%"
  
  df <- change_recent |>
    dplyr::mutate(value = as.numeric(.data[[value_col]])) |>
    dplyr::arrange(dplyr::desc(value)) |>
    dplyr::slice_head(n = 10) |>
    dplyr::mutate(
      unit = forcats::fct_reorder(unit, value, .desc = TRUE),
      label_value = dplyr::if_else(
        value >= 0,
        paste0("+", scales::number(value, accuracy = acc, decimal.mark = ","), suffix),
        paste0(scales::number(value, accuracy = acc, decimal.mark = ","), suffix)
      )
    )
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = value, y = unit, fill = type)) +
    ggplot2::geom_col(width = 0.70, alpha = 0.88) +
    ggplot2::geom_text(
      ggplot2::aes(label = label_value),
      hjust = -0.12,
      family = FONT_FAMILY,
      size = 3.1,
      color = pal$navy,
      fontface = "bold"
    ) +
    ggplot2::scale_fill_manual(
      values = c("Região BR" = "#D55E00", "País LATAM" = "#0072B2"),
      name = NULL
    ) +
    ggplot2::labs(
      x = x_label,
      y = NULL,
      caption = source_caption
    ) +
    theme_dash_base(base_size = 12, legend_position = "bottom", show_caption = TRUE) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(face = "bold"),
      plot.margin = ggplot2::margin(8, 24, 8, 8)
    )
  
  if (type_delta == "abs") {
    p + scale_x_delta_abs_recent_fixed(name = x_label)
  } else {
    p + scale_x_delta_pct_recent_fixed(name = x_label)
  }
}

make_timeline_events <- function() {
  yoy <- pool |>
    dplyr::arrange(type, unit, year) |>
    dplyr::group_by(type, unit) |>
    dplyr::mutate(
      delta_yoy = tmi - dplyr::lag(tmi),
      status_yoy = dplyr::case_when(
        is.na(delta_yoy) ~ NA_character_,
        delta_yoy > limiar_estagnacao ~ "Reversão",
        abs(delta_yoy) <= limiar_estagnacao ~ "Estagnação",
        TRUE ~ "Queda"
      )
    ) |>
    dplyr::ungroup()
  
  context_units <- unique(c(regioes, "Chile", "Costa Rica", "Cuba", "República Dominicana", "Peru", "Guatemala", "Paraguai", "El Salvador", "Bolívia"))
  
  event_df <- yoy |>
    dplyr::filter(unit %in% context_units, status_yoy %in% c("Estagnação", "Reversão")) |>
    dplyr::mutate(
      unit_type_order = dplyr::if_else(unit %in% regioes, 1L, 2L),
      lbl = dplyr::if_else(status_yoy == "Reversão" & abs(delta_yoy) >= 0.4, sprintf("%+.1f", delta_yoy), "")
    )
  
  unit_order <- event_df |>
    dplyr::distinct(unit, unit_type_order) |>
    dplyr::arrange(unit_type_order, unit) |>
    dplyr::pull(unit)
  
  event_df <- event_df |> dplyr::mutate(unit = factor(unit, levels = rev(unit_order)))
  
  ggplot2::ggplot(event_df, ggplot2::aes(x = year, y = unit)) +
    ggplot2::geom_point(ggplot2::aes(color = status_yoy, shape = status_yoy), size = 3.9, alpha = 0.96) +
    ggrepel::geom_text_repel(data = event_df |> dplyr::filter(lbl != ""), ggplot2::aes(label = lbl, color = status_yoy), family = FONT_FAMILY, size = 3.1, box.padding = 0.16, point.padding = 0.10, min.segment.length = 0, seed = 123, max.overlaps = Inf, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = c("Estagnação" = "#E69F00", "Reversão" = "#D55E00"), name = NULL) +
    ggplot2::scale_shape_manual(values = c("Estagnação" = 21, "Reversão" = 24), name = NULL) +
    ggplot2::scale_x_continuous(limits = c(start_common, latest_year), breaks = seq(start_common, latest_year, by = 2), labels = as.character(seq(start_common, latest_year, by = 2)), expand = ggplot2::expansion(mult = c(0.01, 0.035))) +
    ggplot2::scale_y_discrete(expand = ggplot2::expansion(mult = c(0.08, 0.14))) +
    ggplot2::labs(x = "Ano", y = NULL, caption = "Cada ponto indica o status anual da variação da TMI: estagnação quando |ΔTMI| ≤ 0,1; reversão quando ΔTMI > 0,1. Fontes: Brasil — SVSA/MS; países — ONU/UN IGME.") +
    theme_dash_base(base_size = 14, legend_position = "none", show_caption = TRUE) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "#E2E8F0",
        linewidth = 0.35
      ),
      axis.text.x = ggplot2::element_text(
        size = 11,
        face = "bold",
        color = "#475569"
      ),
      axis.title.x = ggplot2::element_text(
        size = 11,
        face = "bold",
        color = "#64748B",
        margin = ggplot2::margin(t = 6)
      ),
      axis.text.y = ggplot2::element_text(face = "bold"),
      legend.margin = ggplot2::margin(t = 4, b = 2),
      legend.box.margin = ggplot2::margin(t = 2, b = 2),
      plot.margin = ggplot2::margin(14, 10, 28, 10)
    )
}


# ============================================================
# Mapa de queda acumulada 2000–2023
# ============================================================

make_latam_reduction_map <- function(year_base = start_common, year_sel = latest_year, type_reduction = c("abs", "pct")) {
  type_reduction <- match.arg(type_reduction)
  
  latam_red <- latam_c |>
    dplyr::filter(year %in% c(year_base, year_sel)) |>
    dplyr::select(iso3, unit, year, tmi) |>
    dplyr::summarise(
      tmi = mean(as.numeric(tmi), na.rm = TRUE),
      .by = c(iso3, unit, year)
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = tmi,
      names_prefix = "tmi_",
      values_fn = list(tmi = ~ mean(as.numeric(.x), na.rm = TRUE))
    ) |>
    dplyr::filter(
      !is.na(.data[[paste0("tmi_", year_base)]]),
      !is.na(.data[[paste0("tmi_", year_sel)]])
    ) |>
    dplyr::mutate(
      reduction_abs = as.numeric(.data[[paste0("tmi_", year_base)]]) -
        as.numeric(.data[[paste0("tmi_", year_sel)]]),
      reduction_pct = 100 * (
        1 - as.numeric(.data[[paste0("tmi_", year_sel)]]) /
          as.numeric(.data[[paste0("tmi_", year_base)]])
      )
    )
  
  br_red <- br_regioes_c |>
    dplyr::filter(year %in% c(year_base, year_sel)) |>
    dplyr::select(region_key = unit, year, tmi) |>
    dplyr::summarise(
      tmi = mean(as.numeric(tmi), na.rm = TRUE),
      .by = c(region_key, year)
    ) |>
    tidyr::pivot_wider(
      names_from = year,
      values_from = tmi,
      names_prefix = "tmi_",
      values_fn = list(tmi = ~ mean(as.numeric(.x), na.rm = TRUE))
    ) |>
    dplyr::filter(
      !is.na(.data[[paste0("tmi_", year_base)]]),
      !is.na(.data[[paste0("tmi_", year_sel)]])
    ) |>
    dplyr::mutate(
      reduction_abs = as.numeric(.data[[paste0("tmi_", year_base)]]) -
        as.numeric(.data[[paste0("tmi_", year_sel)]]),
      reduction_pct = 100 * (
        1 - as.numeric(.data[[paste0("tmi_", year_sel)]]) /
          as.numeric(.data[[paste0("tmi_", year_base)]])
      )
    )
  
  value_col <- if (type_reduction == "abs") "reduction_abs" else "reduction_pct"
  legend_name <- if (type_reduction == "abs") "Queda TMI" else "Queda %"
  acc <- if (type_reduction == "abs") 0.1 else 1
  suffix <- if (type_reduction == "abs") "" else "%"
  
  fill_scale <- if (type_reduction == "abs") {
    scale_fill_reduction_abs_fixed(name = legend_name)
  } else {
    scale_fill_reduction_pct_fixed(name = legend_name)
  }
  
  latam_world <- world_sf |>
    dplyr::inner_join(latam_red, by = "iso3") |>
    dplyr::filter(iso3 != "BRA") |>
    crop_sf(bbox_latam) |>
    dplyr::mutate(value = as.numeric(.data[[value_col]]))
  
  br_map <- br_regions_sf |>
    dplyr::left_join(br_red, by = "region_key") |>
    crop_sf(bbox_latam) |>
    dplyr::mutate(
      unit = region_key,
      value = as.numeric(.data[[value_col]])
    )
  
  label_units <- c(
    "México", "Guatemala", "Colômbia", "Peru", "Paraguai",
    "Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul"
  )
  
  label_df <- dplyr::bind_rows(
    latam_world |>
      dplyr::filter(unit %in% label_units) |>
      sf_label_points() |>
      dplyr::mutate(
        label = paste0(
          unit, "\n",
          scales::number(value, accuracy = acc, decimal.mark = ","),
          suffix
        )
      ) |>
      dplyr::select(unit, label, geometry),
    
    br_map |>
      dplyr::filter(region_key %in% label_units) |>
      sf_label_points() |>
      dplyr::mutate(
        label = paste0(
          region_key, "\n",
          scales::number(value, accuracy = acc, decimal.mark = ","),
          suffix
        )
      ) |>
      dplyr::select(unit, label, geometry)
  )
  
  label_xy <- cbind(label_df, sf::st_coordinates(label_df)) |>
    sf::st_drop_geometry()
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = crop_sf(world_sf, bbox_latam),
      fill = "#F8FAFC",
      color = "#CBD5E1",
      linewidth = 0.18
    ) +
    ggplot2::geom_sf(
      data = latam_world,
      ggplot2::aes(fill = value),
      color = "#102A43",
      linewidth = 0.32
    ) +
    ggplot2::geom_sf(
      data = br_map,
      ggplot2::aes(fill = value),
      color = "#102A43",
      linewidth = 0.42
    ) +
    ggrepel::geom_label_repel(
      data = label_xy,
      ggplot2::aes(x = X, y = Y, label = label),
      family = FONT_FAMILY,
      size = 2.55,
      label.size = 0.15,
      label.padding = ggplot2::unit(0.11, "lines"),
      fill = ggplot2::alpha("white", 0.86),
      color = pal$navy,
      segment.color = ggplot2::alpha(pal$navy, 0.45),
      min.segment.length = 0,
      max.overlaps = Inf,
      seed = 123
    ) +
    fill_scale +
    ggplot2::coord_sf(
      xlim = c(bbox_latam["xmin"], bbox_latam["xmax"]),
      ylim = c(bbox_latam["ymin"], bbox_latam["ymax"]),
      expand = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_colorbar(
        title.position = "top",
        barwidth = grid::unit(7.2, "cm"),
        barheight = grid::unit(0.30, "cm")
      )
    ) +
    ggplot2::labs(caption = source_caption) +
    theme_dash_map(base_size = 12)
}


# ---------- Gerador principal ----------
generate_all_dash_figures <- function() {
  message("Gerando figuras em: ", dash_fig_dir)
  
  for (yr in c(2000L, 2008L, 2016L, latest_year)) {
    save_dash_plot(make_latam_level_map(yr), paste0("queda_choropleth_tmi_", yr), width = 11.6, height = 8.25, dpi = 320)
  }
  
  save_dash_plot(make_global_series(), "queda_series_regioes_vs_latam", width = 12.6, height = 7.2, dpi = 320)
  
  for (reg in regioes) {
    reg_slug <- slug(reg)
    save_dash_plot(make_similarity_series(reg), paste0("similaridade_", reg_slug, "_serie_top3"), width = 11.2, height = 6.35, dpi = 320)
    save_dash_plot(make_similarity_ranking(reg), paste0("similaridade_", reg_slug, "_ranking_top8"), width = 6.4, height = 5.2, dpi = 320)
    save_dash_plot(make_similarity_components(reg), paste0("similaridade_", reg_slug, "_componentes_score_backstage"), width = 7.8, height = 6.2, dpi = 320)
  }
  
  save_dash_plot(make_timeline_events(), "estagnacao_timeline_principais_eventos", width = 13.2, height = 7.8, dpi = 320)
  
  save_dash_plot(make_latam_delta_map(2019L, latest_year, "abs"), "reversao_choropleth_delta_abs_2019_2023", width = 8.4, height = 8.4, dpi = 320)
  save_dash_plot(make_latam_delta_map(2019L, latest_year, "pct"), "reversao_choropleth_delta_pct_2019_2023", width = 8.4, height = 8.4, dpi = 320)
  save_dash_plot(make_recent_ranking("abs"), "reversao_ranking_aumentos_2019_2023", width = 6.8, height = 5.7, dpi = 320)
  save_dash_plot(make_recent_ranking("pct"), "reversao_ranking_aumentos_pct_2019_2023", width = 6.8, height = 5.7, dpi = 320)
  
  save_dash_plot(make_latam_delta_map(start_common, latest_year, "abs"), "mudanca_choropleth_delta_abs_2000_2023", width = 10.2, height = 8.1, dpi = 320)
  save_dash_plot(make_latam_delta_map(start_common, latest_year, "pct"), "mudanca_choropleth_delta_pct_2000_2023", width = 10.2, height = 8.1, dpi = 320)
  save_dash_plot(make_latam_reduction_map(start_common, latest_year, "abs"), "queda_acumulada_choropleth_abs_2000_2023", width = 10.2, height = 8.1, dpi = 320)
  save_dash_plot(make_latam_reduction_map(start_common, latest_year, "pct"), "queda_acumulada_choropleth_pct_2000_2023", width = 10.2, height = 8.1, dpi = 320)
  
  message("Pronto. Figuras geradas em figures/dash/.")
  invisible(TRUE)
}

generate_all_dash_figures()

# ============================================================
# Dados auxiliares para interatividade: queda por ano
# ============================================================

norm_unit <- function(x) {
  x |>
    as.character() |>
    iconv(to = "ASCII//TRANSLIT") |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_|_$", "")
}

queda_years <- c(2000L, 2008L, 2016L, 2023L)

queda_units_labels <- unique(c(
  "Sul", "Sudeste", "Centro-Oeste", "Nordeste", "Norte",
  latam_c |>
    dplyr::filter(unit != "Brasil") |>
    dplyr::distinct(unit) |>
    dplyr::arrange(unit) |>
    dplyr::pull(unit)
))

queda_units_lookup <- tibble::tibble(
  unit_label = queda_units_labels,
  unit_order = seq_along(queda_units_labels)
) |>
  dplyr::mutate(unit_key = norm_unit(unit_label))

queda_box_years <- dplyr::bind_rows(
  br_regioes_c |>
    dplyr::filter(year %in% queda_years) |>
    dplyr::select(type, unit, year, tmi),
  
  latam_c |>
    dplyr::filter(year %in% queda_years) |>
    dplyr::select(type, unit, year, tmi)
) |>
  dplyr::mutate(
    unit_key = norm_unit(unit),
    year = as.integer(year),
    tmi = as.numeric(tmi)
  ) |>
  dplyr::inner_join(queda_units_lookup, by = "unit_key") |>
  dplyr::filter(is.finite(tmi)) |>
  dplyr::arrange(year, unit_order) |>
  dplyr::select(year, type, unit = unit_label, tmi, unit_order)

readr::write_csv(
  queda_box_years,
  file.path(dash_fig_dir, "data_queda_box_years.csv")
)


# ============================================================
# Dados auxiliares para interatividade: queda acumulada 2000–2023
# ============================================================

queda_acumulada_2000_2023 <- dplyr::bind_rows(
  br_regioes_c |>
    dplyr::filter(year %in% c(start_common, latest_year)) |>
    dplyr::select(type, unit, year, tmi),
  
  latam_c |>
    dplyr::filter(year %in% c(start_common, latest_year)) |>
    dplyr::select(type, unit, year, tmi)
) |>
  dplyr::mutate(
    unit_key = norm_unit(unit),
    year = as.integer(year),
    tmi = as.numeric(tmi)
  ) |>
  dplyr::inner_join(queda_units_lookup, by = "unit_key") |>
  dplyr::filter(is.finite(tmi)) |>
  dplyr::select(type, unit = unit_label, unit_order, year, tmi) |>
  dplyr::summarise(
    tmi = mean(tmi, na.rm = TRUE),
    .by = c(type, unit, unit_order, year)
  ) |>
  tidyr::pivot_wider(
    names_from = year,
    values_from = tmi,
    names_prefix = "tmi_"
  ) |>
  dplyr::mutate(
    queda_abs = .data[[paste0("tmi_", start_common)]] -
      .data[[paste0("tmi_", latest_year)]],
    queda_pct = 100 * (
      1 - .data[[paste0("tmi_", latest_year)]] /
        .data[[paste0("tmi_", start_common)]]
    )
  ) |>
  dplyr::arrange(unit_order)

readr::write_csv(
  queda_acumulada_2000_2023,
  file.path(dash_fig_dir, "data_queda_acumulada_2000_2023.csv")
)

