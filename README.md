---

# Material Suplementar – Mortalidade Infantil Brasil vs América Latina (2000–2023)

Este repositório contém os scripts e arquivos auxiliares utilizados no estudo comparativo da **Taxa de Mortalidade Infantil (TMI)** entre as grandes regiões do Brasil e países da América Latina, no período 2000–2023. O material suplementar garante transparência e reprodutibilidade dos resultados apresentados no artigo.

## 📂 Estrutura

- `comparativo_tmi_br_regioes_vs_latam.R`  
  Script principal em R para leitura, processamento e análise dos dados.  
  - Gera tabelas comparativas (Excel)  
  - Produz gráficos em PNG  
  - Constrói mapas (choropleths) para Brasil (regiões) e países latino-americanos  

- `TMI_ONU.R`  
  Script auxiliar para baixar os dados da TMI diretamente da API do **World Bank/WDI** (2000–2023) para países da América Latina.  
  - Gera os arquivos `tmi_latam_onu_NOVO.csv` e `tmi_latam_onu_NOVO.parquet`  
  - Deve ser executado **antes** do script principal, pois fornece os dados internacionais.  

- **Dados de entrada:**  
  - `brasil_00_24.xlsx` (sheet: "TMI") – Brasil, grandes regiões e UFs (incluído no repositório)  
  - `tmi_latam_onu_NOVO.csv` – países da América Latina (2000–2023, gerado pelo script `get_latam_tmi_wdi.R`)  

- **Outputs gerados automaticamente:**  
  - `outputs_tmi_final/tmi_comparativo_final.xlsx`  
  - `graficos_final/*.png`  

## ⚙️ Dependências

O script utiliza os seguintes pacotes R:

```
readxl, readr, dplyr, tidyr, stringr,
ggplot2, openxlsx,
sf, geobr,
rnaturalearth, rnaturalearthdata,
ggrepel, viridis, patchwork, purrr, tibble,
WDI, arrow
```

Instale-os com:

```r
install.packages(c(
  "readxl","readr","dplyr","tidyr","stringr",
  "ggplot2","openxlsx","sf","geobr",
  "rnaturalearth","rnaturalearthdata",
  "ggrepel","viridis","patchwork","purrr","tibble",
  "WDI","arrow"
))
```

## ▶️ Como executar

1. **Baixar dados internacionais:**  
   - Edite o caminho da pasta em `TMI_ONU.R`  
   - Execute o script para gerar `tmi_latam_onu_NOVO.csv` e `tmi_latam_onu_NOVO.parquet`  

2. **Rodar análise comparativa:**  
   - Ajuste o caminho principal (`main_folder`) em `comparativo_tmi_br_regioes_vs_latam.R`  
   - Execute o script  
   - Os resultados serão salvos automaticamente nas pastas `outputs_tmi_final` e `graficos_final`  

## 📊 Principais análises

- Comparação da TMI entre regiões brasileiras e países latino-americanos.  
- Ranking e posicionamento relativo em 2000 e 2023.  
- Variação acumulada (2000–2023) e recente (2019–2023).  
- Detecção sistemática de episódios de **estagnação** e **reversão**.  
- Identificação de países com trajetórias semelhantes às regiões brasileiras (índice de similaridade).  

## 🔗 Referência

Este repositório é material suplementar do artigo:  
**“Gradientes de Mortalidade Infantil no Brasil em Perspectiva Latino-Americana”**  

---
