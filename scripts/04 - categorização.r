# Pacotes -----------------------------------------------------------------

library(tidyverse)

# Funções -----------------------------------------------------------------

source("scripts/3 - transformações.R")

transformar_df <- function (df, ano) {
  if (ano <= 2005) {
    funcs <- transf_ate_2005
  } else {
    funcs <- transf
  }
  
  cat(paste(names(df)[! names(df) %in% names(funcs)], "= function(x) {x}, \n"))
  
  df <- imap_dfc(df, ~funcs[[.y]](.x))
  df <- df %>% mutate_if(is.factor, function(x) {fct_explicit_na(x, "(missing)")})
  
  df <- df %>%
    mutate(
      gretarioQ = criar_gretarioQ(IDADE),
      racacor2 = criar_racacor2(RACACOR),
      estciv2 = criar_estciv2(ESTCIV),
      capitulo_CID = criar_capitulo_CID(CAUSABAS),
      capitulo_CID2 = criar_capitulo_CID2(capitulo_CID),
      escfaltante = criar_escfaltante(ESC),
      esc2 = criar_esc2(ESC)
    )
  
  df
}

# Ler e categorizar -------------------------------------------------------

grid_data <- readRDS("arquivos-resultados/grid_data.rds")

final <- grid_data %>%
  mutate(data_transf = purrr::map2(data, anos, ~transformar_df(.x, .y))) %>%
  select(-data)

write_rds(final, "arquivos-resultados/final.rds")

ano2010 <- final %>% filter(anos == 2010)
ano2010 <- data.table::rbindlist(ano2010$data, fill = TRUE)

write_rds(ano2010, "arquivos-resultados/ano2010.rds")

ano2010_adultos <- ano2010 %>% filter((IDADE >= 25 & IDADE < 60) | is.na(IDADE))

write_rds(ano2010_adultos, "arquivos-resultados/ano2010_adultos.rds")

