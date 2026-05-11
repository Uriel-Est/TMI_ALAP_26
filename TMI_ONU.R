library(WDI)
library(arrow)
library(readr)

pasta <- "CAMINHO" # Edite para a sua pasta
paises_latam <- c("AR","BO","BR","CL","CO","CR","CU","DO","EC","GT","HN","MX","NI","PA","PE","PY","SV","UY","VE")

# Buscar e salvar os dados
dados_novos <- WDI(country = paises_latam, indicator = "SP.DYN.IMRT.IN", start = 2000, end = 2023)

write_parquet(dados_novos, paste0(pasta, "tmi_latam_onu_NOVO.parquet"))
write_csv(dados_novos, paste0(pasta, "tmi_latam_onu_NOVO.csv"))

# Verificar a Bolívia nos NOVOS arquivos
tmp_novo <- read_parquet(paste0(pasta, "tmi_latam_onu_NOVO.parquet"))
unique(tmp_novo$country)
tmp_novo %>% filter(iso2c == "BO") %>% head()