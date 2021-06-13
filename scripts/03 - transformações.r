# Transformações atuais ---------------------------------------------------

transf <- list(
  
  NUMERODO = function(x) {x},
  
  TIPOBITO = function(x) {
    fct_recode(
      x,
      "óbito fetal" = "1",
      "óbito não fetal" = "2"
    ) 
  },
  
  DTOBITO = function(x) {
    lubridate::dmy(as.character(x))
  },
  
  HORAOBITO = function(x) {x},
  
  DTNASC = function(x) {
    lubridate::dmy(as.character(x))
  },
  
  NATURAL = function(x) {x},
  
  IDADE = function(x) { 
    
    # mudar tudo para anos
    aux <- str_split(x, "", n = 2)
    
    sub_1 <- aux %>% map_chr(~ .x[1])
    sub_2 <- aux %>% map_chr(~ .x[2])
    
    resultado <- rep(NA_real_, times = length(x))
    
    resultado[x == "000" & !is.na(x)] <- NA
    resultado[sub_1 == "0" & !is.na(sub_1) & sub_2 != "00"] <- as.numeric(sub_2[sub_1 == "0" & !is.na(sub_1) & sub_2 != "00"])/(365*24*60)
    resultado[sub_1 == "1" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "1" & !is.na(sub_1)])/(365*24)
    resultado[sub_1 == "2" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "2" & !is.na(sub_1)])/(365)
    resultado[sub_1 == "3" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "3" & !is.na(sub_1)])/(12)
    resultado[sub_1 == "4" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "4" & !is.na(sub_1)])
    resultado[sub_1 == "5" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "5" & !is.na(sub_1)]) + 100
    
    resultado
  },
  
  SEXO = function(x) {
    fct_recode(
      x,
      "(missing)" = "0",
      "masculino" = "1",
      "feminino" = "2",
      "(missing)" = "9"
    )
  },
  
  RACACOR = function(x) {
    fct_recode(
      x,
      "branca" = "1",
      "preta" = "2",
      "amarela" = "3",
      "parda" = "4",
      "indígena" = "5"
    )
  },
  
  ESTCIV = function(x) {
    fct_recode(
      x,
      "solteiro" = "1",
      "casado" = "2",
      "viúvo" = "3",
      "separado judicialmente" = "4",
      "outros" = "5",
      "(missing)" = "9"
    )
  },
  
  ESC = function(x) {
    fct_recode(
      x,
      "nenhuma" = "1",
      "1 a 3 anos" = "2",
      "4 a 7 anos" = "3",
      "8 a 11 anos" = "4",
      "12 e mais" = "5",
      "(missing)" = "9",
      "(missing)" = "0"
    )
  },
  
  OCUP = function(x) {x},
  
  CODMUNRES = function(x) {x},
  
  CODBAIRES = function(x) {x},
  
  LOCOCOR = function(x) {
    # Local de ocorrência do óbito, conforme a tabela:
    # 9: Ignorado
    # 1: Hospital
    # 2: Outro estab saúde
    # 3: Domicílio
    # 4: Via Pública
    # 5: Outros
    fct_recode(
      x,
      "(missing)" = "9",
      "hospital" = "1",
      "outro estab saúde" = "2",
      "domicílio" = "3",
      "via pública" = "4",
      "outros" = "5"
    )
  },
  
  CODESTAB = function(x) {x},
  
  CODMUNOCOR = function(x) {x},
  
  CODBAIOCOR = function(x) {x},
  
  IDADEMAE = function(x) {x},
  
  ESCMAE = function(x) {
    # Escolaridade, Anos de estudo concluídos:
    # 1: Nenhuma
    # 2: 1 a 3 anos
    # 3: 4 a 7 anos
    # 4: 8 a 11 anos
    # 5: 12 e mais
    # 9: Ignorado
    fct_recode(
      x,
      "nenhuma" = "1",
      "1 a 3 anos" = "2",
      "4 a 7 anos" = "3",
      "8 a 11 anos" = "4",
      "12 e mais" = "5",
      "(missing)" = "9"
    )
  },
  
  OCUPMAE = function(x) {x},
  
  QTDFILVIVO = function(x) {x},
  
  QTDFILMORT = function(x) {x},
  
  GRAVIDEZ = function(x) {
    # Tipo de gravidez, conforme a tabela:
    # 9: Ignorado
    # 1: Única
    # 2: Dupla
    # 3: Tripla e mais
    fct_recode(
      x,
      "(missing)" = "9",
      "única" = "1",
      "dupla" = "2",
      "tripla e mais" = "3"
    )
  },
  
  GESTACAO = function(x) {
    # Semanas de gestação, conforme as tabelas:
    # 9: Ignorado
    # 1: Menos de 22 semanas
    # 2: 22 a 27 semanas
    # 3: 28 a 31 semanas
    # 4: 32 a 36 semanas
    # 5: 37 a 41 semanas
    # 6: 42 semanas e mais
    fct_recode(
      x,
      "(missing)" = "9",
      "menos de 22 semanas" = "1",
      "22 a 27 semanas" = "2",
      "28 a 31 semanas" = "3",
      "32 a 36 semanas" = "4",
      "37 a 41 semanas" = "5",
      "42 semanas e mais" = "6"
    )
  },
  
  PARTO = function(x) {
    # 9: Ignorado
    # 1: Vaginal
    # 2: Cesáreo
    fct_recode(
      x,
      "(missing)" = "9",
      "vaginal" = "1",
      "cesáreo" = "2"
    )
  },
  
  OBITOPARTO = function(x) {
    # Morte em relação ao parto, conforme tabela:
    # 9: Ignorado
    # 1: Antes
    # 2: Durante
    # 3: Depois
    fct_recode(
      x,
      "(missing)" = "9",
      "antes" = "1",
      "durante" = "2",
      "depois" = "3"
    )
  },
  
  PESO = function(x) {x},
  
  NUMERODN = function(x) {x},
  
  OBITOGRAV = function(x) {
    # Morte durante a Gravidez conforme tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  OBITOPUERP = function(x) {
    # Morte durante o puerpério, conforme tabela:
    # 9: Ignorado
    # 1: Sim, ate 42 dias
    # 2: Sim, de 43 dias a 01 ano
    # 3: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim, até 42 dias" = "1",
      "sim, de 43 dias a 01 ano" = "2",
      "não" = "3"
    )
  },
  
  ASSISTMED = function(x) {
    # Indica se houve assistência medica, conforme a tabela:
    # 9: Ignorado
    # 1: Com assistência
    # 2: Sem assistência
    fct_recode(
      x,
      "(missing)" = "9",
      "com assistência" = "1",
      "sem assistência" = "2"
    )
  },
  
  EXAME = function(x) {
    # Indica se houve exame complementar, conforme a tabela:
    #   9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  CIRURGIA = function(x) {
    # Indica se houve cirurgia, conforme a tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  NECROPSIA = function(x) {
    # Indica se houve necrópsia, conforme a tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  LINHAA = function(x) {x},
  
  LINHAB = function(x) {x},
  
  LINHAC = function(x) {x},
  
  LINHAD = function(x) {x},
  
  LINHAII = function(x) {x},
  
  CAUSABAS = function(x) {x},
  
  TPASSINA = function(x) {x},
  
  DTATESTADO = function(x) {
    lubridate::dmy(x)
  },
  
  CIRCOBITO = function(x) {
    # Indica o tipo de acidente, se cabível:
    # 9: Ignorado
    # 1: Acidente
    # 2: Suicídio
    # 3: Homicídio
    # 4: Outros
    fct_recode(
      x,
      "(missing)" = "9",
      "acidente" = "1",
      "suicídio" = "2",
      "homicídio" = "3",
      "outros" = "4"
    )
  },
  
  ACIDTRAB = function(x) {
    # Indica se foi acidente de trabalho, conforme a tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  FONTE = function(x) {
    # Fonte da informação, conforme a tabela:
    # 9: Ignorado
    # 1: Boletim de Ocorrência
    # 2: Hospital
    # 3: Família
    # 4: Outra
    fct_recode(
      x,
      "(missing)" = "9",
      "boletim de ocorrência" = "1",
      "hospital" = "2",
      "família" = "3",
      "outra" = "4"
    )
  },
  
  TPPOS = function(x) {
    # Óbito investigado 1-Sim, 2- Não
    fct_recode(
      x,
      "sim" = "1",
      "não" = "2"
    )
  },
  
  DTINVESTIG = function(x) {
    lubridate::dmy(x)
  },
  
  CAUSABAS_O = function(x) {x},
  
  DTCADASTRO = function(x) {
    lubridate::dmy(x)
  },
  
  ATESTANTE = function(x) {
    # Indica se o medico que assina atendeu o paciente
    # 1: Sim
    # 2: Substituto
    # 3: IML
    # 4: SVO
    # 5: Outros
    fct_recode(
      x,
      "sim" = "1",
      "substituto" = "2",
      "IML" = "3",
      "SVO" = "4",
      "outros" = "5"
    )
  },
  
  FONTEINV = function(x) {
    # Fonte de investigação
    # 1 Comitê de Morte Materna e/ou Infantil
    # 2 Visita domiciliar / Entrevista família
    # 3 Estab Saúde / Prontuário
    # 4 Relacion com outros bancos de dados
    # 5 S V O
    # 6 I M L
    # 7 Outra fonte
    # 8 Múltiplas fontes
    # 9 Ignorado
    fct_recode(
      x,
      "Comitê de Morte Materna e/ou Infantil" = "1",
      "Visita domiciliar / Entrevista família" = "2",
      "Estab Saúde / Prontuário" = "3",
      "Relacion com outros bancos de dados" = "4",
      "SVO" = "5",
      "IML" = "6",
      "Outra fonte" = "7",
      "Múltiplas fontes" = "8",
      "(missing)" = "9"
    )
  },
  
  DTRECEBIM = function(x) {
    lubridate::dmy(x)
  },
  
  UFINFORM = function(x) {x},
  
  CODINST = function(x) {x},
  
  CB_PRE = function(x) {x},
  
  uf = function(x) {x},
  
  ano = function(x) {x},
  
  ORIGEM = function(x) {x} ,
  MORTEPARTO = function(x) {x} ,
  DTCADINF = function(x) {x},   
  TPOBITOCOR = function(x) {x},
  DTCADINV = function(x) {x},
  
  NUMERODV = function(x) {x}, 
  NUMSUS = function(x) {x}, 
  CODMUNNATU = function(x) {x}, 
  ESC2010 = function(x) {x}, 
  SERIESCFAL = function(x) {x}, 
  ESTABDESCR = function(x) {x}, 
  ESCMAE2010 = function(x) {x}, 
  SERIESCMAE = function(x) {x}, 
  SEMAGESTAC = function(x) {x}, 
  TPMORTEOCO = function(x) {x}, 
  CRM = function(x) {x}, 
  COMUNSVOIM = function(x) {x}, 
  NUMEROLOTE = function(x) {x}, 
  STCODIFICA = function(x) {x}, 
  CODIFICADO = function(x) {x}, 
  VERSAOSIST = function(x) {x}, 
  VERSAOSCB = function(x) {x}, 
  ATESTADO = function(x) {x}, 
  DTRECORIGA = function(x) {x}, 
  CAUSAMAT = function(x) {x}, 
  ESCMAEAGR1 = function(x) {x}, 
  ESCFALAGR1 = function(x) {x}, 
  STDOEPIDEM = function(x) {x}, 
  STDONOVA = function(x) {x}, 
  DIFDATA = function(x) {x}, 
  NUDIASOBCO = function(x) {x}, 
  NUDIASOBIN = function(x) {x}, 
  DTCONINV = function(x) {x}, 
  FONTES = function(x) {x}, 
  TPRESGINFO = function(x) {x}, 
  TPNIVELINV = function(x) {x}, 
  NUDIASINF = function(x) {x}, 
  DTCONCASO = function(x) {x}, 
  FONTESINF = function(x) {x}, 
  ALTCAUSA = function(x) {x},
  CODMUNCART = function(x) {x}, 
  CODCART = function(x) {x}, 
  NUMREGCART = function(x) {x}, 
  DTREGCART = function(x) {x}, 
  DTRECORIG = function(x) {x},
  EXPDIFDATA = function(x) {x},
  contador = function(x) {x},
  CONTADOR = function(x) {x},
  " " = function(x) {x}
  
)


# Transformações até 2005 -------------------------------------------------

transf_ate_2005 <- list(
  
  NUMERODO = function(x) {x},
  
  TIPOBITO = function(x) {
    fct_recode(
      x,
      "óbito fetal" = "1",
      "óbito não fetal" = "2"
    )
  },
  
  DTOBITO = function(x) {
    lubridate::dmy(as.character(x))
  },
  
  DTNASC = function(x) {
    lubridate::dmy(as.character(x))
  },
  
  NATURAL = function(x) {x},
  
  IDADE = function(x) { 
    
    # mudar tudo para anos
    aux <- str_split(x, "", n = 2)
    
    sub_1 <- aux %>% map_chr(~ .x[1])
    sub_2 <- aux %>% map_chr(~ .x[2])
    
    resultado <- rep(NA_real_, times = length(x))
    
    resultado[x == "000" & !is.na(x)] <- NA
    resultado[sub_1 == "0" & !is.na(sub_1) & sub_2 != "00"] <- as.numeric(sub_2[sub_1 == "0" & !is.na(sub_1) & sub_2 != "00"])/(365*24*60)
    resultado[sub_1 == "1" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "1" & !is.na(sub_1)])/(365*24)
    resultado[sub_1 == "2" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "2" & !is.na(sub_1)])/(365)
    resultado[sub_1 == "3" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "3" & !is.na(sub_1)])/(12)
    resultado[sub_1 == "4" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "4" & !is.na(sub_1)])
    resultado[sub_1 == "5" & !is.na(sub_1)] <- as.numeric(sub_2[sub_1 == "5" & !is.na(sub_1)]) + 100
    
    resultado
  },
  
  SEXO = function(x) {
    fct_recode(
      x,
      "(missing)" = "0",
      "masculino" = "1",
      "feminino" = "2",
      "(missing)" = "9"
    )
  },
  
  RACACOR = function(x) {
    fct_recode(
      x,
      "branca" = "1",
      "preta" = "2",
      "amarela" = "3",
      "parda" = "4",
      "indígena" = "5"
    )
  },
  
  ESTCIV = function(x) {
    fct_recode(
      x,
      "solteiro" = "1",
      "casado" = "2",
      "viúvo" = "3",
      "separado judicialmente" = "4",
      "outros" = "5",
      "(missing)" = "9"
    )
  },
  
  ESC = function(x) {
    fct_recode(
      x,
      "nenhuma" = "1",
      "1 a 3 anos" = "2",
      "4 a 7 anos" = "3",
      "8 a 11 anos" = "4",
      "12 e mais" = "5",
      "(missing)" = "9"
    )
  },
  
  CODBAIRES = function(x) {x},
  
  OCUP = function(x) {x},
  
  CODMUNRES = function(x) {x},
  
  LOCOCOR = function(x) {
    # Local de ocorrência do óbito, conforme a tabela:
    # 9: Ignorado
    # 1: Hospital
    # 2: Outro estab saúde
    # 3: Domicílio
    # 4: Via Pública
    # 5: Outros
    fct_recode(
      x,
      "(missing)" = "9",
      "hospital" = "1",
      "outro estab saúde" = "2",
      "domicílio" = "3",
      "via pública" = "4",
      "outros" = "5"
    )
  },
  
  CODESTAB = function(x) {x},
  
  CODMUNOCOR = function(x) {x},
  
  IDADEMAE = function(x) {x},
  
  ESCMAE = function(x) {
    # Escolaridade, Anos de estudo concluídos:
    # 1: Nenhuma
    # 2: 1 a 3 anos
    # 3: 4 a 7 anos
    # 4: 8 a 11 anos
    # 5: 12 e mais
    # 9: Ignorado
    fct_recode(
      x,
      "nenhuma" = "1",
      "1 a 3 anos" = "2",
      "4 a 7 anos" = "3",
      "8 a 11 anos" = "4",
      "12 e mais" = "5",
      "(missing)" = "9"
    )
  },
  
  OCUPMAE = function(x) {x},
  
  QTDFILVIVO = function(x) {x},
  
  QTDFILMORT = function(x) {x},
  
  GRAVIDEZ = function(x) {
    # Tipo de gravidez, conforme a tabela:
    # 9: Ignorado
    # 1: Única
    # 2: Dupla
    # 3: Tripla e mais
    fct_recode(
      x,
      "(missing)" = "9",
      "única" = "1",
      "dupla" = "2",
      "tripla e mais" = "3"
    )
  },
  
  GESTACAO = function(x) {
    # Semanas de gestação, conforme as tabelas:
    # 9: Ignorado
    # 1: Menos de 22 semanas
    # 2: 22 a 27 semanas
    # 3: 28 a 31 semanas
    # 4: 32 a 36 semanas
    # 5: 37 a 41 semanas
    # 6: 42 semanas e mais
    fct_recode(
      x,
      "(missing)" = "9",
      "menos de 22 semanas" = "1",
      "22 a 27 semanas" = "2",
      "28 a 31 semanas" = "3",
      "32 a 36 semanas" = "4",
      "37 a 41 semanas" = "5",
      "42 semanas e mais" = "6"
    )
  },
  
  PARTO = function(x) {
    # 9: Ignorado
    # 1: Vaginal
    # 2: Cesáreo
    fct_recode(
      x,
      "(missing)" = "9",
      "vaginal" = "1",
      "cesáreo" = "2"
    )
  },
  
  OBITOPARTO = function(x) {
    # Morte em relação ao parto, conforme tabela:
    # 9: Ignorado
    # 1: Antes
    # 2: Durante
    # 3: Depois
    fct_recode(
      x,
      "(missing)" = "9",
      "antes" = "1",
      "durante" = "2",
      "depois" = "3"
    )
  },
  
  PESO = function(x) {x},
  
  OBITOGRAV = function(x) {
    # Morte durante a Gravidez conforme tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  OBITOPUERP = function(x) {
    # Morte durante o puerpério, conforme tabela:
    # 9: Ignorado
    # 1: Sim, ate 42 dias
    # 2: Sim, de 43 dias a 01 ano
    # 3: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim, até 42 dias" = "1",
      "sim, de 43 dias a 01 ano" = "2",
      "não" = "3"
    )
  },
  
  ASSISTMED = function(x) {
    # Indica se houve assistência medica, conforme a tabela:
    # 9: Ignorado
    # 1: Com assistência
    # 2: Sem assistência
    fct_recode(
      x,
      "(missing)" = "9",
      "com assistência" = "1",
      "sem assistência" = "2"
    )
  },
  
  EXAME = function(x) {
    # Indica se houve exame complementar, conforme a tabela:
    #   9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  CIRURGIA = function(x) {
    # Indica se houve cirurgia, conforme a tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  NECROPSIA = function(x) {
    # Indica se houve necrópsia, conforme a tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  CAUSABAS = function(x) {x},
  
  LINHAA = function(x) {x},
  
  LINHAB = function(x) {x},
  
  LINHAC = function(x) {x},
  
  LINHAD = function(x) {x},
  
  LINHAII = function(x) {x},
  
  ATESTANTE = function(x) {
    # Indica se o medico que assina atendeu o paciente
    # 1: Sim
    # 2: Substituto
    # 3: IML
    # 4: SVO
    # 5: Outros
    fct_recode(
      x,
      "sim" = "1",
      "substituto" = "2",
      "IML" = "3",
      "SVO" = "4",
      "outros" = "5"
    )
  },
  
  CIRCOBITO = function(x) {
    # Indica o tipo de acidente, se cabível:
    # 9: Ignorado
    # 1: Acidente
    # 2: Suicídio
    # 3: Homicídio
    # 4: Outros
    fct_recode(
      x,
      "(missing)" = "9",
      "acidente" = "1",
      "suicídio" = "2",
      "homicídio" = "3",
      "outros" = "4"
    )
  },
  
  ACIDTRAB = function(x) {
    # Indica se foi acidente de trabalho, conforme a tabela:
    # 9: Ignorado
    # 1: Sim
    # 2: Não
    fct_recode(
      x,
      "(missing)" = "9",
      "sim" = "1",
      "não" = "2"
    )
  },
  
  FONTE = function(x) {
    # Fonte da informação, conforme a tabela:
    # 9: Ignorado
    # 1: Boletim de Ocorrência
    # 2: Hospital
    # 3: Família
    # 4: Outra
    fct_recode(
      x,
      "(missing)" = "9",
      "boletim de ocorrência" = "1",
      "hospital" = "2",
      "família" = "3",
      "outra" = "4"
    )
  },
  
  UFINFORM = function(x) {x},
  
  uf = function(x) {x},
  
  ano = function(x) {x},
  
  contador = function(x) {x},
  
  CONTADOR = function(x) {x},
  " " = function(x) {x}
  
)

# Criação de variáveis ----------------------------------------------------

criar_gretarioQ <- function(x) {
  # gretarioQ: Idade em grupos etários quinquenais, exceto nas primeiras idades que fica de 0 a 1
  # (exclusive) e 1 a 5 (exclusive) a partir da variável “idade”.
  case_when(
    x < 1 ~ "0 a 1 ano",
    x >= 1 & x < 5 ~ "1 a 4 anos",
    x >= 5 & x < 10 ~ "5 a 9 anos",
    x >= 10 & x < 15 ~ "10 a 14 anos",
    x >= 15 & x < 20 ~ "15 a 19 anos",
    x >= 20 & x < 25 ~ "20 a 24 anos",
    x >= 25 & x < 30 ~ "25 a 29 anos",
    x >= 30 & x < 35 ~ "30 a 34 anos",
    x >= 35 & x < 40 ~ "35 a 39 anos",
    x >= 40 & x < 45 ~ "40 a 44 anos",
    x >= 45 & x < 50 ~ "45 a 49 anos",
    x >= 50 & x < 55 ~ "50 a 54 anos",
    x >= 55 & x < 60 ~ "55 a 59 anos",
    x >= 60 & x < 65 ~ "60 a 64 anos",
    x >= 65 & x < 70 ~ "65 a 69 anos",
    x >= 70 & x < 75 ~ "70 a 74 anos",
    x >= 75 & x < 80 ~ "75 a 79 anos",
    x >= 80  ~ "80 anos ou mais",
    TRUE ~ NA_character_
  )
}

criar_racacor2 <- function(x) {
  # racacor2: cor/raça a partir da variável “racacor”. Juntar Amarelo e indígena numa única
  # categoria.
  fct_collapse(
    x,
    "amarela/indígena" = c("amarela", "indígena")
  )
}

criar_estciv2 <- function(x) {
  # -estciv2: estado civil a partir da variável “estciv”. Juntar divorciado e a categoria outros numa
  # única.
  fct_collapse(x, "divorciado/outros" = c("separado judicialmente", "outros"))
}

criar_capitulo_CID <- function(x) {
  
  capitulos_cid <- readxl::read_excel("arquivos_auxiliares/capitulos-cid-10.xlsx") %>%
    slice(-22) %>%
    mutate(capitulo_CID = paste0(capitulo, ". ", descricao))
  
  capitulos_cid2 <- readxl::read_excel("arquivos_auxiliares/causabas-capitulo_cid.xlsx")

  result <- data.frame(CAUSABAS = x) %>%
    mutate(CAUSABAS = as.character(CAUSABAS)) %>%
    left_join(
      capitulos_cid2,
      by = c("CAUSABAS")
    ) %>%
    with(capitulo_CID)
  
  factor(result, levels = capitulos_cid$capitulo_CID)
}

criar_capitulo_CID2 <- function(x) {
  
  manter <- c("II. Neoplasmas [tumores]", "IX. Doenças do aparelho circulatório", 
              "X. Doenças do aparelho respiratório", "XX. Causas externas de morbidade e de mortalidade")
  
  remover <- levels(x)[! levels(x) %in% manter]
  
  fct_collapse(
    x,
    "Outros" = remover
  )
}

criar_escfaltante <- function(x) {
  as.integer(x == "(missing)")
}

criar_esc2 <- function(x) {
  fct_collapse(
    x,
    "baixa escolaridade" = c("nenhuma", "1 a 3 anos", "4 a 7 anos"),
    "média escolaridade" = c("8 a 11 anos"),
    "alta escolaridade" = c("12 e mais")
  )
}




