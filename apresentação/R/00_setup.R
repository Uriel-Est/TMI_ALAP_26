# R/00_setup.R
# ------------------------------------------------------------
# Setup geral da apresentação interativa TMI LATAM / ALAP
# ------------------------------------------------------------

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
library(patchwork)
library(scales)
library(glue)
library(ggiraph)
library(htmltools)

# Caminho raiz do projeto Quarto
project_dir <- normalizePath(
  "C:/Users/uriel/Documents/UFPB Estatística/ALAP 2026/TMI/tmi-latam-alap",
  winslash = "/",
  mustWork = FALSE
)

data_dir    <- file.path(project_dir, "data")
figures_dir <- file.path(project_dir, "figures")
styles_dir  <- file.path(project_dir, "styles")
R_dir       <- file.path(project_dir, "R")

# Caminho da logo
logo_alap <- file.path(figures_dir, "logo_alap.png")

# Paleta inicial da apresentação
pal <- list(
  navy   = "#102A43",
  cyan   = "#1CA7C9",
  teal   = "#0F766E",
  rose   = "#F6E8E8",
  orange = "#F59E0B",
  red    = "#C2410C",
  gray   = "#64748B",
  light  = "#F8FAFC",
  white  = "#FFFFFF"
)

theme_tmi_slide <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold",
        color = pal$navy,
        size = base_size + 3
      ),
      plot.subtitle = element_text(
        color = pal$gray,
        size = base_size
      ),
      plot.caption = element_text(
        color = pal$gray,
        size = base_size - 3
      ),
      axis.title = element_text(color = pal$navy),
      axis.text = element_text(color = pal$navy),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 2),
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA)
    )
}

source_caption <- "Fontes: Brasil — SVSA/MS. Países — ONU/UN IGME."