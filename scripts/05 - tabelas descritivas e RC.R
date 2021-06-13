# Pacotes -----------------------------------------------------------------

library(xlsx)
library(tidyverse)

# Tabelas -----------------------------------------------------------------

df <- read_rds("arquivos-resultados/ano2010_adultos.rds")
uf_regiao <- readxl::read_excel("arquivos_auxiliares/uf-regiao.xlsx")
df <- df %>% left_join(uf_regiao, by = "uf")

# Especificação das tabelas -----------------------------------------------

univariadas <- c("regiao", "uf", "SEXO", "gretarioQ",
                 "racacor2", "estciv2", "LOCOCOR",
                 "capitulo_CID2", "capitulo_CID", "esc2", "escfaltante")


variaveis <- c("regiao", "uf", "SEXO", "gretarioQ",
                 "racacor2", "estciv2", "LOCOCOR",
                 "capitulo_CID2","capitulo_CID")
referencias <- c("norte", "AM", "feminino", "25 a 30 anos", "branca",
                 "solteiro", "domicílio", "II. Neoplasmas [tumores]", "I. Algumas doenças infecciosas e parasitárias")



# Funções auxiliares ------------------------------------------------------

add_title <- function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

add_table <- function(sheet, df, row, col, col_style, row_style) {
  addDataFrame(
    df, 
    sheet, 
    startRow=row, 
    startColumn=col, 
    colnamesStyle = col_style,
    rownamesStyle = row_style
  )
}

criar_freqs <- function(df, var) {
  df %>%
    group_by_(var) %>%
    summarise(n = n()) %>%
    mutate(`%` = n/sum(n))
}

criar_tabela2 <- function(df, var, referencia) {
  aux <- df %>%
    group_by_("escfaltante", var) %>%
    summarise(n = n()) %>%
    group_by_(var) %>%
    mutate(`%` = n/sum(n)) %>%
    ungroup()
  
  qtd <- aux %>% select(-`%`) %>% 
    spread(escfaltante, n)
  
  perc <- aux %>% select(-`n`) %>% 
    spread(escfaltante, `%`)
  
  perc2 <- aux %>%
    filter(escfaltante == 1) %>%
    mutate(`escfaltante (sim) %` = n/sum(n)) 
  
  tabela <- bind_cols(
    qtd %>% select(var, `0`),
    perc %>% select(`%0` = `0`),
    qtd %>% select(`1`),
    perc %>% select(`%1`= `1`),
    perc2 %>% select(`escfaltante (sim) %`)
  )
  
  ref <- tabela[tabela[[var]] == referencia & !is.na(tabela[[var]]),]
  ref_vl <- as.numeric(ref[,5]/(1-ref[,5]))
  
  tabela <- tabela %>%
    mutate(
      RC = `%1`/`%0`/ref_vl
    )
  tabela <- as.data.frame(tabela)
  rownames(tabela) <- ifelse(is.na(tabela[[var]]), "(missing)", as.character(tabela[[var]]))
  tabela[[var]] <- NULL
  
  tabela
}


# Criar workbook ----------------------------------------------------------

wb <- createWorkbook(type="xlsx")

# estilos

title <- CellStyle(wb) + 
  Font(wb, heightInPoints = 16, color="blue", isBold=TRUE, underline=1)

sub_title <- CellStyle(wb) + 
  Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)

tbl_rownames <- CellStyle(wb) + Font(wb, isBold=TRUE)
tbl_colnames <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(
    color="black", position=c("TOP", "BOTTOM"), 
    pen=c("BORDER_THIN", "BORDER_THICK")
  ) 

# Descritiva univariada ---------------------------------------------------

sheet <- createSheet(wb, sheetName = "Univariadas")

# escrever

i <- 1
for (j in univariadas) {
  
  tabela <- as.data.frame(criar_freqs(df, j))
  print(tabela)
  rownames(tabela) <- ifelse(is.na(tabela[[j]]), "(missing)", as.character(tabela[[j]]))
  tabela[[j]] <- NULL
  
  # Parâmetros
  titulo <- j
  linha_titulo <- i 
  linha_tabela <- linha_titulo + 2
  i <- linha_tabela + nrow(tabela) + 2
  
  # Add elementos
  add_title(sheet, linha_titulo, titulo, title)
  add_table(
    sheet, tabela, linha_tabela, 1, 
    col_style = tbl_colnames, row_style = tbl_rownames
  )
  
}


# Descritivas 2 -----------------------------------------------------------

sheet <- createSheet(wb, sheetName = "Razões de chance")

# escrever


i <- 1
for (j in 1:length(variaveis)) {
  
  tabela <- as.data.frame(criar_tabela2(df, variaveis[j], referencias[j]))
  print(tabela)
  
  # Parâmetros
  titulo <- variaveis[j]
  linha_titulo <- i 
  linha_tabela <- linha_titulo + 2
  i <- linha_tabela + nrow(tabela) + 2
  
  # Add elementos
  add_title(sheet, linha_titulo, titulo, title)
  add_table(
    sheet, tabela, linha_tabela, 1, 
    col_style = tbl_colnames, row_style = tbl_rownames
  )
  
}



saveWorkbook(wb, "arquivos-resultados/tabelas_descritivas_e_RC.xlsx")
