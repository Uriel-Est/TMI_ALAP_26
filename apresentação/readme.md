# Gradientes de Mortalidade Infantil no Brasil em Perspectiva Latino-Americana

Este repositório contém o código usado para gerar as figuras e a apresentação interativa em Quarto/reveal.js do trabalho **“Gradientes de Mortalidade Infantil no Brasil em Perspectiva Latino-Americana”**.

O pipeline combina dados de mortalidade infantil das regiões brasileiras e de países latino-americanos para produzir mapas, séries temporais, rankings de similaridade, eventos de estagnação, reversões recentes e painéis auxiliares usados na apresentação.

## Produto final

Ao final da execução, o projeto gera:

- uma apresentação HTML em reveal.js, a partir de `apresentacao.qmd`;
- mapas e gráficos em `figures/dash/`, salvos em `.png` e `.pdf`;
- arquivos `.csv` auxiliares usados pelos slides interativos;
- slides interativos para queda continental, queda acumulada, similaridade, estagnação e reversão recente.

O produto principal esperado é:

```text
apresentacao.html
```

Os principais produtos intermediários ficam em:

```text
figures/dash/
```

## Estrutura esperada do projeto

A estrutura recomendada é:

```text
tmi-latam-alap/
├── tmi-latam-alap.Rproj
├── apresentacao.qmd
├── R/
│   ├── 00_setup.R
│   └── 02_figures_dash.R
├── data/
│   ├── brasil_00_24.xlsx
│   └── tmi_latam_onu_NOVO.csv
├── figures/
│   ├── logo_alap.png
│   └── dash/
├── partials/
│   └── nav.html
├── styles/
│   └── custom.scss
└── outputs/
```

A pasta `figures/dash/` é criada automaticamente pelo script `R/02_figures_dash.R`, caso ainda não exista.

## Dados necessários

Antes de rodar o pipeline, coloque os seguintes arquivos na pasta `data/`:

```text
data/brasil_00_24.xlsx
data/tmi_latam_onu_NOVO.csv
```

O script também procura esses arquivos na raiz do projeto e na pasta imediatamente acima da raiz, mas a forma recomendada é manter ambos em `data/`.

### `brasil_00_24.xlsx`

Arquivo com as taxas de mortalidade infantil das regiões brasileiras. O script espera uma aba chamada:

```text
TMI
```

### `tmi_latam_onu_NOVO.csv`

Arquivo com as taxas de mortalidade infantil dos países latino-americanos.

## Requisitos

### Programas

- R, preferencialmente versão 4.2 ou superior;
- RStudio, opcional, mas recomendado;
- Quarto instalado e disponível no sistema;
- acesso à internet na primeira execução, especialmente para baixar/consultar geometrias via `geobr` e `rnaturalearth`, caso ainda não estejam em cache.

### Pacotes R

O script verifica automaticamente se os pacotes necessários estão instalados. Caso algum esteja ausente, ele interrompe a execução e mostra uma mensagem com os pacotes faltantes.

Para instalar tudo manualmente, rode:

```r
install.packages(c(
  "dplyr", "tidyr", "stringr", "readxl", "readr", "ggplot2",
  "sf", "geobr", "rnaturalearth", "rnaturalearthdata", "scales",
  "forcats", "ggrepel", "purrr", "tibble"
))
```

Para renderizar a apresentação via R, também é útil ter o pacote `quarto`:

```r
install.packages("quarto")
```

## Ordem de execução

### 1. Abrir o projeto

Abra o arquivo `.Rproj` no RStudio, ou defina manualmente o diretório de trabalho para a raiz do projeto:

```r
setwd("caminho/para/tmi-latam-alap")
```

A raiz correta é a pasta que contém `apresentacao.qmd`, `R/`, `data/`, `figures/` e `styles/`.

### 2. Conferir os dados de entrada

Confirme que estes arquivos existem:

```text
data/brasil_00_24.xlsx
data/tmi_latam_onu_NOVO.csv
```

Também confirme que o logo usado na capa existe:

```text
figures/logo_alap.png
```

### 3. Gerar figuras e arquivos auxiliares

Rode o script principal de figuras:

```r
source("R/02_figures_dash.R")
```

Esse script lê os dados de entrada, prepara as bases e gera os arquivos necessários para a apresentação.

### 4. Renderizar a apresentação

Depois de gerar as figuras, renderize o Quarto:

```r
quarto::quarto_render("apresentacao.qmd")
```

Alternativamente, use o botão **Render** do RStudio com o arquivo `apresentacao.qmd` aberto.

## Principais saídas geradas

O script `R/02_figures_dash.R` gera mapas, gráficos e arquivos auxiliares em `figures/dash/`.

### Mapas de queda continental por ano

```text
figures/dash/queda_choropleth_tmi_2000.png
figures/dash/queda_choropleth_tmi_2008.png
figures/dash/queda_choropleth_tmi_2016.png
figures/dash/queda_choropleth_tmi_2023.png
```

Esses arquivos alimentam o slide interativo de queda continental.

### Série temporal geral

```text
figures/dash/queda_series_regioes_vs_latam.png
```

### Figuras de similaridade por região

Para cada região brasileira, o script gera uma série temporal e um ranking de países similares:

```text
figures/dash/similaridade_norte_serie_top3.png
figures/dash/similaridade_norte_ranking_top8.png
figures/dash/similaridade_nordeste_serie_top3.png
figures/dash/similaridade_nordeste_ranking_top8.png
figures/dash/similaridade_centro_oeste_serie_top3.png
figures/dash/similaridade_centro_oeste_ranking_top8.png
figures/dash/similaridade_sudeste_serie_top3.png
figures/dash/similaridade_sudeste_ranking_top8.png
figures/dash/similaridade_sul_serie_top3.png
figures/dash/similaridade_sul_ranking_top8.png
```

Esses arquivos alimentam o slide interativo de similaridade.

### Timeline de estagnação

```text
figures/dash/estagnacao_timeline_principais_eventos.png
```

### Reversão recente

```text
figures/dash/reversao_choropleth_delta_abs_2019_2023.png
figures/dash/reversao_choropleth_delta_pct_2019_2023.png
figures/dash/reversao_ranking_aumentos_2019_2023.png
figures/dash/reversao_ranking_aumentos_pct_2019_2023.png
```

### Queda acumulada entre 2000 e 2023

```text
figures/dash/queda_acumulada_choropleth_abs_2000_2023.png
figures/dash/queda_acumulada_choropleth_pct_2000_2023.png
```

### Arquivos auxiliares para interatividade

```text
figures/dash/data_queda_box_2023.csv
figures/dash/data_queda_box_years.csv
figures/dash/data_queda_acumulada_2000_2023.csv
figures/dash/data_top_matches_by_region.csv
figures/dash/data_pairs_scored.csv
figures/dash/data_change_recent.csv
```

Esses `.csv` são usados por chunks do `apresentacao.qmd` para preencher cards, rankings e abas interativas.

## Fluxo resumido

```text
1. Colocar dados em data/
2. Rodar R/02_figures_dash.R
3. Conferir figuras em figures/dash/
4. Renderizar apresentacao.qmd
5. Abrir apresentacao.html
```

Em R:

```r
source("R/02_figures_dash.R")
quarto::quarto_render("apresentacao.qmd")
```

## Observações sobre reprodução

- O script assume que o diretório de trabalho é a raiz do projeto.
- Se o script for executado a partir da pasta `R/`, ele tenta ajustar automaticamente `project_dir` para a pasta acima.
- Os arquivos gerados em `figures/dash/` podem ser apagados e recriados a qualquer momento rodando novamente `source("R/02_figures_dash.R")`.
- A apresentação depende dos arquivos gerados em `figures/dash/`; portanto, rode o script de figuras antes de renderizar o Quarto.

## Problemas comuns

### Erro: `Não encontrei brasil_00_24.xlsx`

Confirme se o arquivo está em:

```text
data/brasil_00_24.xlsx
```

ou na raiz do projeto.

### Erro: `Não encontrei tmi_latam_onu_NOVO.csv`

Confirme se o arquivo está em:

```text
data/tmi_latam_onu_NOVO.csv
```

ou na raiz do projeto.

### Figuras antigas continuam aparecendo

Apague a pasta `figures/dash/` ou rode novamente:

```r
source("R/02_figures_dash.R")
```

Depois renderize de novo:

```r
quarto::quarto_render("apresentacao.qmd")
```

### O Quarto não renderiza

Verifique se o Quarto está instalado no sistema. No R:

```r
quarto::quarto_version()
```

Se o comando falhar, instale o Quarto pelo site oficial e reinicie o RStudio.

## Versionamento no GitHub

Para garantir que a apresentação renderize em outro computador, recomenda-se versionar:

```text
apresentacao.qmd
R/02_figures_dash.R
styles/custom.scss
partials/nav.html
figures/logo_alap.png
```

Se os dados puderem ser redistribuídos, inclua também:

```text
data/brasil_00_24.xlsx
data/tmi_latam_onu_NOVO.csv
```

Se os dados não puderem ser redistribuídos, mantenha apenas a estrutura da pasta `data/` e documente como obtê-los.

Os arquivos em `figures/dash/` podem ser versionados quando o objetivo for disponibilizar diretamente a apresentação renderizada, mas também podem ser regenerados a partir do script.

