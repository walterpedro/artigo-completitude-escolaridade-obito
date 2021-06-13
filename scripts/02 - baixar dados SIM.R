# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(read.dbc)
library(httr)

# Função ------------------------------------------------------------------

get_sim_data <- function(base, uf, ano) {
  
  url <- paste0(
    "ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/",
    base,
    uf,
    ano, 
    ".dbc"
  )
  
  httr::GET(url, httr::write_disk("temp.dbc", overwrite = TRUE))
  
  df <- read.dbc(file = "temp.dbc")
  file.remove("temp.dbc")
  df %>% mutate(uf = uf, ano = ano)
}

# Dados -------------------------------------------------------------------
# Obs.: É necessário ter baixado o arquivo uf.txt em "arquivos_auxiliares"

grid <- expand.grid(
  estados = readLines("arquivos_auxiliares/uf.txt"),
  anos = 2010
)

grid_data <- grid %>%
  mutate(data = map2(estados, anos, ~get_sim_data("DO", .x, .y)))

saveRDS(grid_data, "arquivos-resultados/grid_data.rds")
