
# Pacotes
library(tidyverse)
library(readxl)
library(tidyr)  

# Carregamento dos dicionarios -------------------------------------------------
acg <- read_excel("ACG.xlsx")

keys = c( 'uuid:d7fd569d-3b36-4591-bd0a-2708f236eaef', 
          'uuid:c201bf55-14dc-4fa7-83f2-20e33fdc028b', 
          'uuid:45755755-f1b0-4833-ad90-c8cc1f8c2f19', 
          'uuid:d55dea39-b610-4e81-ae67-251adf11645b', 
          'uuid:9e443ce1-a03b-4cd8-b8f1-7c840c1d71b8', 
          'uuid:2997fa7b-6325-4889-ba77-9961876d8d94', 
          'uuid:a52e2e72-2f28-4bc9-aa64-d0a6488fbb31', 
          'uuid:d6fb31da-fe05-42eb-b8b1-54e123221a38', 
          'uuid:20a5b2e0-a51e-41d2-a065-5c3fa578fb7f', 
          'uuid:342f9b91-ee2a-43b9-802d-062d7be0d8bc', 
          'uuid:4b757cba-f011-4c02-922c-f44e0fb99638', 
          'uuid:426d56a5-4236-4739-858b-4d07dba8a50c', 
          'uuid:11d87b80-512b-4802-858c-df84b7367637', 
          'uuid:c448a320-dce3-4149-9cc3-4905d22470bf', 
          'uuid:5fc1f240-eec4-4491-aaa6-41b44f8e846b', 
          'uuid:7e5299f3-3fea-4f04-8351-c4288879b76d', 
          'uuid:2b3fb8f5-10fe-43a8-bf34-0ce5f818e6e8', 
          'uuid:bbc88994-6415-476e-ac7b-1c9889eab4c1', 
          'uuid:0e16fe50-f7d4-451e-aa15-2e24247fede5', 
          'uuid:16f10a6e-a93a-4669-bce8-8146da4b7471', 
          'uuid:f664b5cb-99a4-494f-ab61-54e0109cecc4', 
          'uuid:f982f941-b491-47fa-aea6-d45637bdc7a3', 
          'uuid:593d3d99-9877-4f0c-ba79-c351d310e01a' 
)

acg_filtrado <- acg %>%
  # 1) Filtrar registros com SubmissionDate preenchido
  filter(!is.na(SubmissionDate) & SubmissionDate != "") %>%
  
  # 2) Remover KEY == "uuid:"
  filter(KEY != "uuid:") %>%
  
  # 3) Remover KEYs da lista de banidos
  filter(!(KEY %in% keys))


# =======================================================
# de para
# =======================================================

# 1) Funções auxiliares de recodificação --------------------------------------

# Binário 1/2 -> 1/0; 77 (se existir) vira 0
recode_12_to_10 <- function(x) dplyr::case_when(
  x == 1 ~ 1.00,
  x == 2 ~ 0.00,
  x == 77 ~ 0.00,
  TRUE   ~ NA_real_
)

# Likert 1..4 -> 0, 0.33, 0.66, 1; 77 (se existir) -> 0
recode_1to4_to_0to1 <- function(x) dplyr::case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.33,
  x == 3 ~ 0.66,
  x == 4 ~ 1.00,
  x == 77 ~ 0.00,
  TRUE    ~ NA_real_
)

# Tri-nível 1/2/3 -> 0, 0.5, 1; 77 (se existir) -> 0
recode_1to3_to_0_0.5_1 <- function(x) dplyr::case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.50,
  x == 3 ~ 1.00,
  x == 77 ~ 0.00,
  TRUE    ~ NA_real_
)

# Itens 0/1 -> 0/1
recode_01_same <- function(x) dplyr::case_when(
  x == 0 ~ 0.00,
  x == 1 ~ 1.00,
  TRUE   ~ NA_real_
)

# Itens "_77" (flags) -> sempre 0
recode_77_flag_to_zero <- function(x) dplyr::case_when(
  is.na(x) ~ NA_real_,  # mantém NA como NA
  TRUE     ~ 0.00
)

# 2) Listas de variáveis por padrão (com base no .sql) ------------------------
# Obs.: usei any_of() para não quebrar se alguma coluna não existir na sua planilha.

# Binário 1/2 -> 1/0
bin_12_to_10 <- c(
  "V01","V02","V03","V05","V13","V20","V24","V29","V30","V31",
  "V66A","V67A","V78A","V81","V82","V83","V97","V104","V105"
)

# Likert 1..4 -> 0..1 (muitos têm 77->0 no .sql)
likert_1to4 <- c(
  "V14","V16","V18","V26","V33","V34","V35","V40","V41","V43","V44","V45",
  "V51","V53","V54","V72","V73","V75","V76","V78","V79","V87","V89","V90",
  "V91","V92","V93","V94","V100","V106","V107","V108","V109","V110","V111",
  "V112","V115","V116"
)

# Tri 1/2/3 -> 0 / 0.5 / 1
tri_1to3 <- c("V11","V39","V80A","V113")

# Famílias 0/1 -> 0/1
fam_01 <- c(
  paste0("V08_", c(1:12)), paste0("V09_", c(1:7)), paste0("V10_", c(1:7)),
  paste0("V21_", c(1:6)),
  paste0("V38_", c(1:7)),
  paste0("V56_", c(1:6)),
  paste0("V57_", c(1:6)),
  paste0("V63_", c(1:17)),
  paste0("V64_", c(1:17)),
  paste0("V65_", c(1:4)),
  paste0("V67_", c(1:13)),
  paste0("V68_", c(1:12)),
  paste0("V74_", c(1:5)),
  paste0("V80_", c(1:10))
)

# Sufixos "_77" que no .sql são sempre 0
flags_77 <- c(
  "V08_77","V09_77","V10_77","V21_77","V38_77","V56_77","V57_77","V63_77",
  "V64_77","V65_77","V67_77","V68_77","V74_77","V80_77","V95_77","V99_77",
  "V101_77","V102_77"
)

# Casos específicos com regras distintas no .sql (exemplos selecionados)
casos_espec <- list(
  V06  = c(1,0.00, 2,0.33, 3,0.66, 4,1.00, 77,0.00),
  V07  = c(1,1.00, 2,0.00, 3,0.00),
  V11  = c(1,0.00, 2,0.50, 3,1.00, 77,0.00),  # também coberto por tri_1to3
  V33  = c(1,0.00, 2,0.33, 3,0.66, 4,1.00, 77,0.00),
  V34  = c(1,0.00, 2,0.33, 3,0.66),
  V39  = c(1,0.00, 2,0.50, 3,1.00, 77,0.00),  # também no tri_1to3
  V44  = c(1,0.00, 2,0.33, 3,0.66, 4,1.00, 77,0.00),
  V45  = c(1,0.00, 2,0.33, 3,0.66, 4,1.00, 77,0.00),
  V51  = c(1,0.00, 2,0.33, 3,0.66, 77,0.00),
  V53  = c(1,0.00, 2,0.33, 3,0.66, 4,1.00, 77,0.00),
  V54  = c(1,0.00, 2,0.33, 3,0.66, 4,1.00, 77,0.00),
  V88  = c(1,0.66, 2,0.00, 3,1.00),
  V95_1= c(0,0.00, 1,1.00),
  V95_2= c(0,0.00, 1,1.00),
  V95_3= c(0,0.00, 1,1.00),
  V95_4= c(0,0.00, 1,1.00),
  V95_5= c(0,0.00, 1,1.00),
  V95_6= c(0,0.00, 1,1.00),
  V97  = c(1,1.00, 2,0.00),
  V101_1=c(0,0.00, 1,1.00),
  V101_2=c(0,0.00, 1,1.00),
  V101_3=c(0,0.00, 1,1.00),
  V101_4=c(0,0.00, 1,1.00),
  V101_5=c(0,0.00, 1,1.00),
  V101_6=c(0,0.00, 1,1.00),
  V101_7=c(0,0.00, 1,1.00),
  V101_8=c(0,0.00, 1,1.00),
  V101_9=c(0,0.00, 1,1.00),
  V104 = c(1,1.00, 2,0.00, 77,0.00),
  V105 = c(1,1.00, 2,0.00),
  V113 = c(1,0.00, 2,0.50, 3,1.00)
)

# 3) Aplicar de-para (mutate) --------------------------------------------------

acg_depara <- acg_filtrado %>%
  # SEE_rotulo
  mutate(
    SEE_rotulo = dplyr::case_when(
      see == 1 ~ "1. SEE Ceará",
      see == 2 ~ "2. SEE Espírito Santo",
      see == 3 ~ "3. SEE Goiás",
      see == 4 ~ "4. SEE Piauí",
      TRUE     ~ NA_character_
    )
  ) %>%
  # Binário 1/2 -> 1/0
  mutate(across(any_of(bin_12_to_10), ~ recode_12_to_10(.x), .names = "{.col}_ajustada")) %>%
  # Likert 1..4 -> 0..1
  mutate(across(any_of(likert_1to4), ~ recode_1to4_to_0to1(.x), .names = "{.col}_ajustada")) %>%
  # Tri 1..3 -> 0 / 0.5 / 1
  mutate(across(any_of(tri_1to3), ~ recode_1to3_to_0_0.5_1(.x), .names = "{.col}_ajustada")) %>%
  # Famílias 0/1 -> 0/1
  mutate(across(any_of(fam_01), ~ recode_01_same(.x), .names = "{.col}_ajustada")) %>%
  # Sufixos _77 -> sempre 0
  mutate(across(any_of(flags_77), ~ recode_77_flag_to_zero(.x), .names = "{.col}_ajustada")) %>%
  # Casos específicos que no .sql têm regras próprias
  {
    out <- .
    for (nm in names(casos_espec)) {
      vec <- casos_espec[[nm]]
      # constrói pares valor -> score
      vals <- vec[c(TRUE, FALSE)]
      scrs <- vec[c(FALSE, TRUE)]
      out <- out %>%
        mutate("{nm}_ajustada" := dplyr::case_when(
          .data[[nm]] %in% vals ~ scrs[match(.data[[nm]], vals)],
          TRUE ~ NA_real_
        ))
    }
    out
  }

# =======================================================
# criaçao de numeradores e denominadores
# =======================================================


# 1) Liste todas as colunas que terminam com "_ajustada"
cols_ajustadas <- names(acg_depara)[grepl("_ajustada$", names(acg_depara))]

# 2) Crie numeradores (NA -> 0) e denominadores (NA -> 0; não-NA -> 1)
acg_nd <- acg_depara %>%
  mutate(
    # Numerador: o próprio valor quando não-NA, senão 0
    across(all_of(cols_ajustadas),
           ~ replace_na(as.numeric(.x), 0),
           .names = "{.col}_numerador"),
    # Denominador: 1 quando não-NA; 0 quando NA
    across(all_of(cols_ajustadas),
           ~ ifelse(is.na(.x), 0, 1),
           .names = "{.col}_denominador")
  )

# =======================================================
# média por questões multiplas
# =======================================================

# ==== Função auxiliar: média segura de um conjunto de itens ====
# Usa os pares *_ajustada_{numerador,denominador}. Ignora itens que não existirem.
make_media <- function(df, bases, new_name) {
  num_cols <- paste0(bases, "_ajustada_numerador")
  den_cols <- paste0(bases, "_ajustada_denominador")
  num_cols <- intersect(num_cols, names(df))
  den_cols <- intersect(den_cols, names(df))
  if (length(den_cols) == 0) {
    df[[new_name]] <- 0
    return(df)
  }
  df %>%
    rowwise() %>%
    mutate("{new_name}" := {
      num <- sum(c_across(all_of(num_cols)), na.rm = TRUE)
      den <- sum(c_across(all_of(den_cols)), na.rm = TRUE)
      if (den != 0) num / den else 0
    }) %>%
    ungroup()
}

# ==== Grupos conforme 3_calcula_media_de_questoes_multiplas.sql ====
grupos <- list(
  "V08_1_10_11_3_8_media"      = c("V08_1","V08_10","V08_11","V08_3","V08_8"),
  "V08_2_4_media"              = c("V08_2","V08_4"),
  "V08_6_7_media"              = c("V08_6","V08_7"),
  "V08_9_media"                = c("V08_9"),
  "V10_1_2_3_4_5_6_7_media"    = paste0("V10_", 1:7),
  "V101_1_2_3_4_5_6_7_8_9_media" = paste0("V101_", 1:9),
  "V102_1_2_3_media"           = paste0("V102_", 1:3),
  "V114_1_2_3_4_5_6_7_media"   = paste0("V114_", 1:7),
  "V19_1_2_3_4_5_6_media"      = paste0("V19_", 1:6),
  # Nota: no SQL aparece "V27_1_2_3_3_4_5_6_7_media"; aqui removemos a duplicata de V27_3
  "V27_1_2_3_4_5_6_7_media"    = paste0("V27_", c(1,2,3,4,5,6,7)),
  "V38_1_2_3_4_5_6_7_media"    = paste0("V38_", 1:7),
  
  # Blocos V63
  "V63_1_3_media"              = paste0("V63_", c(1,3)),
  "V63_10_media"               = "V63_10",
  "V63_11_12_media"            = paste0("V63_", c(11,12)),
  "V63_13_14_15_media"         = paste0("V63_", 13:15),
  "V63_16_media"               = "V63_16",
  "V63_2_media"                = "V63_2",
  "V63_4_media"                = "V63_4",
  "V63_5_media"                = "V63_5",
  "V63_6_media"                = "V63_6",
  "V63_7_8_media"              = paste0("V63_", c(7,8)),
  "V63_9_media"                = "V63_9",
  
  # Blocos V64
  "V64_1_3_media"              = paste0("V64_", c(1,3)),
  "V64_10_media"               = "V64_10",
  "V64_11_12_media"            = paste0("V64_", c(11,12)),
  "V64_13_14_15_media"         = paste0("V64_", 13:15),
  "V64_16_media"               = "V64_16",
  "V64_2_media"                = "V64_2",
  "V64_4_media"                = "V64_4",
  "V64_5_media"                = "V64_5",
  "V64_6_media"                = "V64_6",
  "V64_7_8_media"              = paste0("V64_", c(7,8)),
  "V64_9_media"                = "V64_9",
  
  "V65_1_2_3_4_media"          = paste0("V65_", 1:4),
  
  # Blocos V67
  "V67_1_media"                = "V67_1",
  "V67_1_12_media"             = paste0("V67_", c(1,12)),
  "V67_10_11_7_media"          = paste0("V67_", c(10,11,7)),
  "V67_2_media"                = "V67_2",
  "V67_3_4_5_6_media"          = paste0("V67_", 3:6),
  "V67_8_9_media"              = paste0("V67_", c(8,9)),
  
  # Blocos V68
  "V68_1_media"                = "V68_1",
  "V68_1_11_media"             = paste0("V68_", c(1,11)),
  "V68_10_11_7_media"          = paste0("V68_", c(10,11,7)),
  "V68_2_media"                = "V68_2",
  "V68_3_4_5_6_media"          = paste0("V68_", 3:6),
  "V68_8_9_media"              = paste0("V68_", c(8,9)),
  
  # Blocos V80 / V85 / V95 / V86
  "V80_1_3_6_8_9_media"        = paste0("V80_", c(1,3,6,8,9)),
  "V80_5_media"                = "V80_5",
  "V80_7_media"                = "V80_7",
  "V85_1_2_3_4_5_6_7_media"    = paste0("V85_", 1:7),
  "V95_1_2_3_4_5_6_media"      = paste0("V95_", 1:6),
  "V86_1_2_3_media"            = paste0("V86_", 1:3)
)
# (Todos os agrupamentos acima foram extraídos do seu .sql da etapa 3.)  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20media%20de%20questoes%20multiplas.sql)

# ==== Aplicar os grupos nas linhas ====
acg_medias <- acg_nd
for (nm in names(grupos)) {
  acg_medias <- make_media(acg_medias, grupos[[nm]], nm)
}

# defina a lista de colunas uma vez, fora do mutate
v98_nums <- intersect(paste0("V98_", 1:6, "_ajustada_numerador"), names(acg_medias))

acg_medias <- acg_medias %>%
  mutate(
    # 1 se _77 == 0; senão 0
    V21_media = ifelse(!is.na(V21_77_ajustada) & V21_77_ajustada == 0, 1.0, 0.0),
    V56_media = ifelse(!is.na(V56_77_ajustada) & V56_77_ajustada == 0, 1.0, 0.0),
    V57_media = ifelse(!is.na(V57_77_ajustada) & V57_77_ajustada == 0, 1.0, 0.0),
    V99_media = ifelse(!is.na(V99_77_ajustada) & V99_77_ajustada == 0, 1.0, 0.0),
    
    # V98_media: 1 se algum numerador V98_1..V98_6 != 0; senão 0
    V98_media = if (length(v98_nums) == 0) {
      0
    } else {
      as.numeric(if_any(all_of(v98_nums), ~ coalesce(.x, 0) != 0))
    }
  )

#=========================================================================
# agregação - ator see
#=========================================================================

# Definimos os campos exatamente como no SQL
# (nomes de entrada e como cada um será chamado na saída)
campos_agregar <- c(
  "V01_ajustada" = "V01_ajustada",
  "V02_ajustada" = "V02_ajustada",
  "V03_ajustada" = "V03_ajustada",
  "V04_ajustada" = "V04_ajustada",
  "V06_ajustada" = "V06_ajustada",
  "V07_ajustada" = "V07_ajustada",
  "V08_1_10_11_3_8_media" = "V08_1_10_11_3_8_ajustada",
  "V08_2_4_media"         = "V08_2_4_ajustada",
  "V08_6_7_media"         = "V08_6_7_ajustada",
  "V08_9_media"           = "V08_9_ajustada",
  "V10_1_2_3_4_5_6_7_media" = "V10_1_2_3_4_5_6_7_ajustada",
  "V100_ajustada" = "V100_ajustada",
  "V101_1_2_3_4_5_6_7_8_9_media" = "V101_1_2_3_4_5_6_7_8_9_ajustada",
  "V102_1_2_3_media" = "V102_1_2_3_ajustada",
  "V103_ajustada" = "V103_ajustada",
  "V104_ajustada" = "V104_ajustada",
  "V105_ajustada" = "V105_ajustada",
  "V106_ajustada" = "V106_ajustada",
  "V107_ajustada" = "V107_ajustada",
  "V108_ajustada" = "V108_ajustada",
  "V109_ajustada" = "V109_ajustada",
  "V11_ajustada"  = "V11_ajustada",
  "V110_ajustada" = "V110_ajustada",
  "V111_ajustada" = "V111_ajustada",
  "V112_ajustada" = "V112_ajustada",
  "V113_ajustada" = "V113_ajustada",
  "V114_1_2_3_4_5_6_7_media" = "V114_1_2_3_4_5_6_7_ajustada",
  "V115_ajustada" = "V115_ajustada",
  "V116_ajustada" = "V116_ajustada",
  "V14_ajustada"  = "V14_ajustada",
  "V16_ajustada"  = "V16_ajustada",
  "V18_ajustada"  = "V18_ajustada",
  "V19_1_2_3_4_5_6_media" = "V19_1_2_3_4_5_6_ajustada",
  "V20_ajustada" = "V20_ajustada",
  "V21_media"    = "V21_ajustada",
  "V24_ajustada" = "V24_ajustada",
  "V26_ajustada" = "V26_ajustada",
  # No seu SQL aparece "V27_1_2_3_3_4_5_6_7_media" (duplicando o 3);
  # aqui usamos o nome criado na etapa 3 sem duplicata, se for o seu caso:
  "V27_1_2_3_4_5_6_7_media" = "V27_1_2_3_3_4_5_6_7_ajustada",
  "V29_ajustada" = "V29_ajustada",
  "V31_ajustada" = "V31_ajustada",
  "V33_ajustada" = "V33_ajustada",
  "V34_ajustada" = "V34_ajustada",
  "V35_ajustada" = "V35_ajustada",
  "V38_1_2_3_4_5_6_7_media" = "V38_1_2_3_4_5_6_7_ajustada",
  "V39_ajustada" = "V39_ajustada",
  "V40_ajustada" = "V40_ajustada",
  "V41_ajustada" = "V41_ajustada",
  "V43_ajustada" = "V43_ajustada",
  "V44_ajustada" = "V44_ajustada",
  "V45_ajustada" = "V45_ajustada",
  "V51_ajustada" = "V51_ajustada",
  "V53_ajustada" = "V53_ajustada",
  "V54_ajustada" = "V54_ajustada",
  "V56_media"    = "V56_ajustada",
  "V57_media"    = "V57_ajustada",
  "V63_1_3_media"     = "V63_1_3_ajustada",
  "V63_10_media"      = "V63_10_ajustada",
  "V63_11_12_media"   = "V63_11_12_ajustada",
  "V63_13_14_15_media"= "V63_13_14_15_ajustada",
  "V63_16_media"      = "V63_16_ajustada",
  "V63_2_media"       = "V63_2_ajustada",
  "V63_4_media"       = "V63_4_ajustada",
  "V63_5_media"       = "V63_5_ajustada",
  "V63_6_media"       = "V63_6_ajustada",
  "V63_7_8_media"     = "V63_7_8_ajustada",
  "V63_9_media"       = "V63_9_ajustada",
  "V64_1_3_media"     = "V64_1_3_ajustada",
  "V64_10_media"      = "V64_10_ajustada",
  "V64_11_12_media"   = "V64_11_12_ajustada",
  "V64_13_14_15_media"= "V64_13_14_15_ajustada",
  "V64_16_media"      = "V64_16_ajustada",
  "V64_2_media"       = "V64_2_ajustada",
  "V64_4_media"       = "V64_4_ajustada",
  "V64_5_media"       = "V64_5_ajustada",
  "V64_6_media"       = "V64_6_ajustada",
  "V64_7_8_media"     = "V64_7_8_ajustada",
  "V64_9_media"       = "V64_9_ajustada",
  "V65_1_2_3_4_media" = "V65_1_2_3_4_ajustada",
  "V66_ajustada" = "V66_ajustada",
  "V67_1_media"       = "V67_1_ajustada",
  "V67_1_12_media"    = "V67_1_12_ajustada",
  "V67_10_11_7_media" = "V67_10_11_7_ajustada",
  "V67_2_media"       = "V67_2_ajustada",
  "V67_3_4_5_6_media" = "V67_3_4_5_6_ajustada",
  "V67_8_9_media"     = "V67_8_9_ajustada",
  "V68_1_media"       = "V68_1_ajustada",
  "V68_1_11_media"    = "V68_1_11_ajustada",
  "V68_10_11_7_media" = "V68_10_11_7_ajustada",
  "V68_2_media"       = "V68_2_ajustada",
  "V68_3_4_5_6_media" = "V68_3_4_5_6_ajustada",
  "V68_8_9_media"     = "V68_8_9_ajustada",
  "V72_ajustada" = "V72_ajustada",
  "V73_ajustada" = "V73_ajustada",
  "V75_ajustada" = "V75_ajustada",
  "V76_ajustada" = "V76_ajustada",
  "V78_ajustada" = "V78_ajustada",
  "V78A_ajustada"= "V78_A_ajustada",
  "V79_ajustada" = "V79_ajustada",
  "V80_1_3_6_8_9_media" = "V80_1_3_6_8_9_ajustada",
  "V80_5_media" = "V80_5_ajustada",
  "V80_7_media" = "V80_7_ajustada",
  "V80A_ajustada"= "V80_A_ajustada",
  "V85_1_2_3_4_5_6_7_media" = "V85_1_2_3_4_5_6_7_ajustada",
  "V86_1_2_3_media" = "V86_1_2_3_ajustada",
  "V87_ajustada" = "V87_ajustada",
  "V88_ajustada" = "V88_ajustada",
  "V89_ajustada" = "V89_ajustada",
  "V91_ajustada" = "V91_ajustada",
  "V92_ajustada" = "V92_ajustada",
  "V93_ajustada" = "V93_ajustada",
  "V95_1_2_3_4_5_6_media" = "V95_1_2_3_4_5_6_ajustada",
  "V97_ajustada" = "V97_ajustada",
  "V98_media"    = "V98_ajustada",
  "V99_media"    = "V99_ajustada"
)

# 2) Agregação por SEE_rotulo: média simples por coluna, como AVG no SQL
# campos_agregar: vetor nomeado "coluna_entrada" = "alias_saida" (como você definiu)
# 1) Garante trabalhar apenas com as colunas que existem na base:

cols_existentes <- intersect(names(campos_agregar), names(acg_medias))

agregado_por_see <- acg_medias %>%
  mutate(ATOR = "3. ACG") %>%
  rename(SEE = SEE_rotulo) %>%
  group_by(ATOR, SEE) %>%
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # renomeia cada coluna de entrada para o alias de saída do vetor 'campos_agregar'
  rename_with(
    ~ unname(campos_agregar[.x]),
    .cols = all_of(cols_existentes)
  )


# Identifique todas as colunas *_ajustada (inclusive nomes com _A_, como V78_A_ajustada)
ajustadas_cols <- grep("_ajustada$", names(agregado_por_see), value = TRUE)

# 2) Crie numeradores e denominadores (NA -> 0 para numerador; NA -> 0 e não-NA -> 1 para denominador)
agregado_nd <- agregado_por_see %>%
  mutate(
    across(
      all_of(ajustadas_cols),
      ~ dplyr::coalesce(as.numeric(.x), 0),
      .names = "{.col}_numerador"
    ),
    across(
      all_of(ajustadas_cols),
      ~ ifelse(is.na(.x), 0, 1),
      .names = "{.col}_denominador"
    )
  )


#==================================================
# Cacula descritor
#==================================================

# 1) Função: dado um vetor de "bases" (ex.: c("V53", "V54")), calcula:
#    (soma de *_ajustada_numerador) / (soma de *_ajustada_denominador),
#    retornando 0 quando denominador total = 0.
add_descritor <- function(df, bases, new_name) {
  # monta nomes completos para numeradores/denominadores
  num_cols <- paste0(bases, "_ajustada_numerador")
  den_cols <- paste0(bases, "_ajustada_denominador")
  
  # só usa as colunas que existem
  num_cols <- intersect(num_cols, names(df))
  den_cols <- intersect(den_cols, names(df))
  
  # se nada existir, cria a coluna com 0 e retorna
  if (length(num_cols) == 0 || length(den_cols) == 0) {
    df[[new_name]] <- 0
    return(df)
  }
  
  df %>%
    rowwise() %>%
    mutate("{new_name}" := {
      num <- sum(c_across(all_of(num_cols)), na.rm = TRUE)
      den <- sum(c_across(all_of(den_cols)), na.rm = TRUE)
      if (den != 0) num / den else 0
    }) %>%
    ungroup()
}

# 2) Mapeamento dos descritores (bases sem sufixo "_ajustada_*")
#    OBS.: nomes espelham exatamente o seu SQL.
descritores <- list(
  A2D3_6  = c("V53"),
  A2D3_7  = c("V54"),
  A3D1_1  = c("V56"),
  A3D1_2  = c("V57"),
  A3D2_4  = c("V79"),
  A3D2_5  = c("V67_1","V68_1"),
  A3D4_9  = c("V63_16","V64_16"),
  A4D1_2  = c("V11"),
  A4D1_3  = c("V80_A"),
  A4D1_4  = c("V91"),
  A4D1_5  = c("V16"),
  A4D1_7  = c("V86_1_2_3","V87","V88"),
  A4D2_9  = c("V89"),
  A4D3_11 = c("V103"),
  A4D3_12 = c("V26"),
  A4D4_19 = c("V63_4","V64_4"),
  A4D4_20 = c("V63_6","V64_6"),
  A4D4_21 = c("V63_7_8","V64_7_8"),
  
  E1D1_4  = c("V06"),
  E1D1_5  = c("V01","V02"),
  E1D2_10 = c("V14"),
  # No SQL, V08_6_7 aparece duas vezes; aqui usamos uma vez só:
  E1D2_6  = c("V08_6_7"),
  E1D2_7  = c("V78"),
  E1D2_8  = c("V80_5"),
  E1D4_15 = c("V07"),
  
  E2D1_11 = c("V08_1_10_11_3_8"),
  E2D1_12 = c("V80_1_3_6_8_9"),
  E2D1_3  = c("V03"),
  E2D1_5  = c("V11"),
  E2D1_6  = c("V80_A"),
  E2D1_8  = c("V08_9"),
  E2D1_9  = c("V80_7"),
  
  E2D2_14 = c("V10_1_2_3_4_5_6_7"),
  E2D2_15 = c("V85_1_2_3_4_5_6_7"),
  E2D2_16 = c("V67_2","V68_2"),
  
  E2D3_21 = c("V72"),
  
  E2D4_22 = c("V04","V51","V78_A"),
  E2D4_24 = c("V63_1_3","V64_1_3"),
  E2D4_25 = c("V65_1_2_3_4"),
  E2D4_26 = c("V66"),
  E2D4_28 = c("V08_2_4"),
  
  E3D1_10 = c("V91"),
  E3D1_2  = c("V92"),
  E3D1_5  = c("V19_1_2_3_4_5_6","V20","V21"),
  E3D1_6  = c("V100","V95_1_2_3_4_5_6","V97","V98"),
  E3D1_8  = c("V16"),
  E3D1_9  = c("V86_1_2_3","V87","V88"),
  
  E3D2_12 = c("V18"),
  E3D2_13 = c("V93"),
  E3D2_20 = c("V99"),
  
  E3D3_22 = c("V67_1_12","V68_1_11"),
  E3D3_23 = c("V73","V75","V76"),
  
  E3D4_27 = c("V63_5","V64_5"),
  E3D4_28 = c("V63_2","V64_2"),
  E3D4_30 = c("V63_4","V64_4"),
  E3D4_31 = c("V63_6","V64_6"),
  
  E4D1_1  = c("V101_1_2_3_4_5_6_7_8_9"),
  # SQL usa V27_1_2_3_3_4_5_6_7 (com 3 repetido). Use esse alias se ele existe no seu agregado:
  E4D1_2  = c("V27_1_2_3_3_4_5_6_7"),
  E4D2_5  = c("V67_3_4_5_6","V68_3_4_5_6"),
  E4D2_8  = c("V24"),
  E4D2_9  = c("V102_1_2_3"),
  
  E4D3_15 = c("V103"),
  E4D3_16 = c("V26"),
  E4D4_18 = c("V63_10","V64_10"),
  E4D4_19 = c("V63_7_8","V64_7_8"),
  E4D4_20 = c("V63_9","V64_9"),
  
  E5D1_1  = c("V110"),
  E5D1_4  = c("V110"),
  E5D2_10 = c("V34"),
  E5D2_11 = c("V105"),
  E5D2_12 = c("V67_8_9","V68_8_9"),
  E5D2_5  = c("V33"),
  E5D2_6  = c("V106"),
  E5D2_8  = c("V35"),
  E5D2_9  = c("V109"),
  E5D3_14 = c("V107","V108"),
  E5D3_15 = c("V104","V29"),
  E5D3_18 = c("V31"),
  E5D4_19 = c("V63_11_12","V64_11_12"),
  
  E6D1_1  = c("V111","V112"),
  E6D1_2  = c("V44","V45"),
  E6D2_11 = c("V40"),
  E6D2_12 = c("V115"),
  E6D2_14 = c("V41"),
  E6D2_15 = c("V116"),
  E6D2_17 = c("V67_10_11_7","V68_10_11_7"),
  E6D2_4  = c("V113"),
  E6D2_5  = c("V39"),
  E6D2_8  = c("V38_1_2_3_4_5_6_7"),
  E6D2_9  = c("V114_1_2_3_4_5_6_7"),
  E6D3_18 = c("V72"),
  E6D4_22 = c("V63_13_14_15","V64_13_14_15"),
  E6D4_24 = c("V43")
)
# (Todos os pares e combinações acima vêm do seu arquivo "3 calcula descritor.sql".)  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20descritor.sql)

# 3) Aplicar aos dados agregados (com numeradores/denominadores já criados)
descritores_por_see <- agregado_nd %>%
  mutate(SRE = "Agregado SEE")  # como no SELECT ... 'Agregado SEE' AS SRE

for (nm in names(descritores)) {
  bases <- unique(descritores[[nm]]) # evita duplicatas caso apareçam
  descritores_por_see <- add_descritor(descritores_por_see, bases, nm)
}

# Resultado: ATOR, SEE, SRE e todos os descritores A* e E* calculados

# =======================================================
# pivotagem
# =======================================================
# 1) Lista de descritores exatamente como no SQL (ordem não afeta o resultado)
descritor_cols <- c(
  "A2D3_6","A2D3_7","A3D1_1","A3D1_2","A3D2_4","A3D2_5","A3D4_9",
  "A4D1_2","A4D1_3","A4D1_4","A4D1_5","A4D1_7","A4D2_9","A4D3_11","A4D3_12",
  "A4D4_19","A4D4_20","A4D4_21",
  "E1D1_4","E1D1_5","E1D2_10","E1D2_6","E1D2_7","E1D2_8","E1D4_15",
  "E2D1_11","E2D1_12","E2D1_3","E2D1_5","E2D1_6","E2D1_8","E2D1_9",
  "E2D2_14","E2D2_15","E2D2_16","E2D3_21",
  "E2D4_22","E2D4_24","E2D4_25","E2D4_26","E2D4_28",
  "E3D1_10","E3D1_2","E3D1_5","E3D1_6","E3D1_8","E3D1_9",
  "E3D2_12","E3D2_13","E3D2_20","E3D3_22","E3D3_23",
  "E3D4_27","E3D4_28","E3D4_30","E3D4_31",
  "E4D1_1","E4D1_2","E4D2_5","E4D2_8","E4D2_9","E4D3_15","E4D3_16",
  "E4D4_18","E4D4_19","E4D4_20",
  "E5D1_1","E5D1_4","E5D2_10","E5D2_11","E5D2_12","E5D2_5","E5D2_6","E5D2_8","E5D2_9",
  "E5D3_14","E5D3_15","E5D3_18","E5D4_19",
  "E6D1_1","E6D1_2","E6D2_11","E6D2_12","E6D2_14","E6D2_15","E6D2_17","E6D2_4","E6D2_5",
  "E6D2_8","E6D2_9","E6D3_18","E6D4_22","E6D4_24"
)

# 2) Garante que só usa colunas que existem (caso alguma não tenha sido gerada)
descritor_cols_exist <- intersect(descritor_cols, names(descritores_por_see))

# 3) UNPIVOT em R: wide -> long
descritores_long <- descritores_por_see %>%
  pivot_longer(
    cols = all_of(descritor_cols_exist),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  mutate(valor = as.numeric(valor)) %>%
  arrange(ATOR, SEE, SRE, descritor)

# Resultado: ATOR, SEE, SRE, descritor, valor

# =======================================================
# ator sre see
# =======================================================

# (reutilize o mesmo 'campos_agregar' que você já definiu antes)
# Se ainda não renomeou, garanta que 'SEE' e 'SRE' existam:
acg_medias_sre <- acg_medias %>%
  rename(SEE = SEE_rotulo) %>%   # se já for SEE, pode remover
  mutate(ATOR = "3. ACG")        # constante como no SQL

# Quais colunas de fato existem? (robustez)
cols_existentes <- intersect(names(campos_agregar), names(acg_medias_sre))

agregado_por_sre <- acg_medias_sre %>%
  group_by(ATOR, SEE, sre) %>%   # <-- AQUI está a mudança: inclui 'sre'
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  # renomeia para os aliases (…_ajustada) exatamente como no SQL
  rename_with(~ unname(campos_agregar[.x]), .cols = all_of(cols_existentes))

# Pega todas as colunas *_ajustada
ajustadas_cols_sre <- grep("_ajustada$", names(agregado_por_sre), value = TRUE)

agregado_sre_nd <- agregado_por_sre %>%
  mutate(
    across(
      all_of(ajustadas_cols_sre),
      ~ dplyr::coalesce(as.numeric(.x), 0),
      .names = "{.col}_numerador"
    ),
    across(
      all_of(ajustadas_cols_sre),
      ~ ifelse(is.na(.x), 0, 1),
      .names = "{.col}_denominador"
    )
  )

# reutilize a add_descritor() e a lista 'descritores' que já definimos antes

descritores_por_sre <- agregado_sre_nd %>%
  mutate(SRE = sre)   # mantém a identificação da SRE na coluna SRE

for (nm in names(descritores)) {
  bases <- unique(descritores[[nm]])
  descritores_por_sre <- add_descritor(descritores_por_sre, bases, nm)
}

# Reaproveite 'descritor_cols' da etapa anterior ou detecte pelo padrão:
descritor_cols_exist_sre <- names(descritores_por_sre)[grepl("^[AE]\\d", names(descritores_por_sre))]

descritores_long_sre <- descritores_por_sre %>%
  pivot_longer(
    cols = all_of(descritor_cols_exist_sre),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  mutate(valor = as.numeric(valor)) %>%
  arrange(ATOR, SEE, sre, SRE, descritor)












