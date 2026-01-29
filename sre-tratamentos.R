
# Pacotes
library(tidyverse)
library(readxl)

# Carregamento dos dicionarios -------------------------------------------------
tec.sre <- read_excel("Tecnico SRE.xlsx")
# Se quiser conferir nomes de colunas antes:
# names(tec.sre)

# 1) (opcional) tentar parsear data de forma robusta
tec.sre <- tec.sre %>%
  mutate(
    SubmissionDate_parsed = suppressWarnings(parse_date_time(
      SubmissionDate,
      orders = c("Ymd HMS","Y-m-d H:M:S","d/m/Y H:M:S","d/m/Y","Y-m-d")
    ))
  )

# 2) Aplicar filtros do SQL
tec_sre_exclusao <- tec.sre %>%
  # Regra "SubmissionDate > ''" -> manter linhas com data não vazia / parseável
  filter(
    (!is.na(SubmissionDate) & SubmissionDate != "") | !is.na(SubmissionDate_parsed)
  ) %>%
  # Remover KEY == 'uuid:'
  filter(KEY != "uuid:")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# de para
# =-=-=-=-=-=-=-=-=-=-=-==-=-=-=--=

# Etapa 2: de-para de valores (pass-through)
# O SQL faz apenas SELECT * FROM a etapa anterior, então no R só propagamos o objeto.

tec_sre_base <- tec_sre_exclusao

# ---------------------------
# Helpers de mapeamento (vetorizados)
# ---------------------------
make_mapper <- function(map_named_num) {
  # map_named_num: ex. c("1"=1, "2"=0, "77"=0)
  function(x) {
    out <- unname(map_named_num[as.character(x)])
    as.numeric(out)  # valores não mapeados retornam NA
  }
}

# 0/1 para múltipla (0->0; 1->1)
f_dummy01      <- make_mapper(c("0"=0, "1"=1))
# "_77" para múltipla (0->0; 1->0)
f_dummy77_zero <- make_mapper(c("0"=0, "1"=0))

# Binária 1→1, 2→0, 77→0
f_bin_1_2_77zero <- make_mapper(c("1"=1, "2"=0, "77"=0))
# Binária 1→1, 2→0 (sem 77 no SQL)
f_bin_1_2_no77   <- make_mapper(c("1"=1, "2"=0))
# Binária 1→1, 2→0, 3→0 (sem 77)
f_bin_1_vs_2or3_zero <- make_mapper(c("1"=1, "2"=0, "3"=0))

# 4 pontos ascendente (1→0; 2→.33; 3→.66; 4→1) + 77→0
f_lik4_77  <- make_mapper(c("1"=0, "2"=0.33, "3"=0.66, "4"=1, "77"=0))
# 4 pontos ascendente sem 77
f_lik4     <- make_mapper(c("1"=0, "2"=0.33, "3"=0.66, "4"=1))

# 3 pontos (1→0; 2→0.5; 3→1) + 77→0
f_lik3_77  <- make_mapper(c("1"=0, "2"=0.5, "3"=1, "77"=0))

# 4 pontos invertida (V38: 1→1; 2→.66; 3→.33; 4→0)
f_lik4_rev <- make_mapper(c("1"=1, "2"=0.66, "3"=0.33, "4"=0))

# 1→0, 2→1, 77→0 (ex.: V47)
f_inv12_77 <- make_mapper(c("1"=0, "2"=1, "77"=0))
# 1→0, 2→1, 3→0 (ex.: V63)
f_1to0_2to1_3to0 <- make_mapper(c("1"=0, "2"=1, "3"=0))

# ---------------------------
# 1) SEE_rotulo (igual ao SQL)
# ---------------------------
tec_sre_aplica <- tec_sre_base %>%
  mutate(
    SEE_rotulo = case_when(
      see == 1 ~ "1. SEE Ceará",
      see == 2 ~ "2. SEE Espírito Santo",
      see == 3 ~ "3. SEE Goiás",
      see == 4 ~ "4. SEE Piauí",
      TRUE ~ NA_character_
    )
  )
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# ---------------------------
# 2) Grupos de variáveis por regra (extraídos do SQL)
# ---------------------------

# Binária 1→1, 2→0, 77→0
grp_bin_1_2_77 <- c("V01","V02","V08","V13","V15","V21",
                    "V49","V50","V51","V52","V54",
                    "V66","V79","V80","V81","V82",
                    "V86","V87","V90","V92","V94","V100")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# Binária 1→1, 2→0 (sem 77 no SQL)
grp_bin_1_2    <- c("V03","V04","V33")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# Binária 1→1, 2→0, 3→0
grp_bin_1_vs_2or3 <- c("V05","V06")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# 4 pontos asc (com 77→0)
grp_lik4_77 <- c("V07","V14","V17","V18","V19","V25","V27",
                 "V31","V32","V48","V56","V57","V58",
                 "V60","V61","V62","V64",
                 "V67","V68","V69","V70","V71","V72","V73","V74","V75","V76",
                 "V83","V84","V85","V88","V89","V98","V99")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# 4 pontos asc (sem 77 no SQL)
grp_lik4 <- c("V34","V39","V40","V41","V42","V43")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# 3 pontos 0/.5/1 (com 77→0)
grp_lik3_77 <- c("V12","V29","V37")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# 4 pontos invertida (V38)
grp_lik4_rev <- c("V38")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# 1→0, 2→1, 77→0 (V47)
grp_inv12_77 <- c("V47")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# 1→0, 2→1, 3→0 (V63)
grp_1to0_2to1_3to0 <- c("V63")
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# Múltiplas 0/1 (dummy) — grupos conforme o SQL
grp_V09  <- paste0("V09_",  c(1:12))
grp_V10  <- paste0("V10_",  c(1:7))
grp_V11  <- paste0("V11_",  c(1:7))
grp_V16  <- paste0("V16_",  c(1:9))
grp_V20  <- paste0("V20_",  c(1:6))
grp_V22  <- paste0("V22_",  c(1:6))
grp_V23  <- paste0("V23_",  c(1:6))
grp_V24  <- paste0("V24_",  c(1:5))
grp_V28  <- paste0("V28_",  c(1:7))
grp_V30  <- paste0("V30_",  c(1:9))
grp_V35  <- paste0("V35_",  c(1:7))
grp_V36  <- paste0("V36_",  c(1:7))
grp_V44  <- c("V44_1","V44_2","V44_3","V44_4","V44_5","V44_8")
grp_V45  <- paste0("V45_",  c(1:21))
grp_V46  <- paste0("V46_",  c(1:21))
grp_V53  <- paste0("V53_",  c(1:13))
grp_V55  <- paste0("V55_",  c(1:13))
grp_V59  <- paste0("V59_",  c(1:4))
grp_V93  <- paste0("V93_",  c(1:6))
grp_V95  <- paste0("V95_",  c(1:6))
grp_V96  <- paste0("V96_",  c(1:6))
grp_V97  <- paste0("V97_",  c(1:6))
grp_V101 <- paste0("V101_", c(1:6))
grp_V102 <- paste0("V102_", c(1:6))

# "_77" das múltiplas (sempre 0 quando 0/1; NA caso contrário)
grp_77 <- c("V09_77","V10_77","V11_77","V16_77","V20_77","V23_77","V24_77",
            "V28_77","V30_77","V35_77","V36_77","V44_77","V45_77","V46_77",
            "V53_77","V55_77","V59_77","V93_77","V95_77","V96_77","V97_77",
            "V101_77","V102_77")

# ---------------------------
# 3) Aplicar mapeamentos (criando *_ajustada)
# ---------------------------
apply_if_exist <- function(df, vars, fun, suffix = "_ajustada") {
  vars_ex <- intersect(vars, names(df))
  if (length(vars_ex)) {
    df <- df %>%
      mutate(across(all_of(vars_ex), ~ fun(.x), .names = paste0("{.col}", suffix)))
  }
  df
}

tec_sre_aplica <- tec_sre_aplica %>%
  # Binárias
  { apply_if_exist(., grp_bin_1_2_77,  f_bin_1_2_77zero) } %>%
  { apply_if_exist(., grp_bin_1_2,     f_bin_1_2_no77) } %>%
  { apply_if_exist(., grp_bin_1_vs_2or3, f_bin_1_vs_2or3_zero) } %>%
  # 4 pontos (asc)
  { apply_if_exist(., grp_lik4_77,     f_lik4_77) } %>%
  { apply_if_exist(., grp_lik4,        f_lik4) } %>%
  # 3 pontos (0/.5/1)
  { apply_if_exist(., grp_lik3_77,     f_lik3_77) } %>%
  # Escalas especiais
  { apply_if_exist(., grp_lik4_rev,    f_lik4_rev) } %>%
  { apply_if_exist(., grp_inv12_77,    f_inv12_77) } %>%
  { apply_if_exist(., grp_1to0_2to1_3to0, f_1to0_2to1_3to0) } %>%
  # Múltiplas 0/1
  { apply_if_exist(., grp_V09,  f_dummy01) } %>%
  { apply_if_exist(., grp_V10,  f_dummy01) } %>%
  { apply_if_exist(., grp_V11,  f_dummy01) } %>%
  { apply_if_exist(., grp_V16,  f_dummy01) } %>%
  { apply_if_exist(., grp_V20,  f_dummy01) } %>%
  { apply_if_exist(., grp_V22,  f_dummy01) } %>%
  { apply_if_exist(., grp_V23,  f_dummy01) } %>%
  { apply_if_exist(., grp_V24,  f_dummy01) } %>%
  { apply_if_exist(., grp_V28,  f_dummy01) } %>%
  { apply_if_exist(., grp_V30,  f_dummy01) } %>%
  { apply_if_exist(., grp_V35,  f_dummy01) } %>%
  { apply_if_exist(., grp_V36,  f_dummy01) } %>%
  { apply_if_exist(., grp_V44,  f_dummy01) } %>%
  { apply_if_exist(., grp_V45,  f_dummy01) } %>%
  { apply_if_exist(., grp_V46,  f_dummy01) } %>%
  { apply_if_exist(., grp_V53,  f_dummy01) } %>%
  { apply_if_exist(., grp_V55,  f_dummy01) } %>%
  { apply_if_exist(., grp_V59,  f_dummy01) } %>%
  { apply_if_exist(., grp_V93,  f_dummy01) } %>%
  { apply_if_exist(., grp_V95,  f_dummy01) } %>%
  { apply_if_exist(., grp_V96,  f_dummy01) } %>%
  { apply_if_exist(., grp_V97,  f_dummy01) } %>%
  { apply_if_exist(., grp_V101, f_dummy01) } %>%
  { apply_if_exist(., grp_V102, f_dummy01) } %>%
  # "_77" sempre zero quando 0/1
  { apply_if_exist(., grp_77,   f_dummy77_zero) }

# Observações de fidelidade ao SQL:
# - V28_3 tem linhas repetidas no CASE, mas o mapeamento é 0->0, 1->1 (mantido).
# - Itens como V34/V39/V40/V41/V42/V43 não trazem 77 no SQL -> aqui também não aplicamos 77.
# - V38 usa escala invertida; V47 usa 1->0/2->1/77->0; V63 usa 1->0/2->1/3->0.
# - "_77" de múltiplas sempre 0 (0 ou 1), senão NA — não contribuem a somatórios.
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)


# =-=-=-=-==-=-=-=-=-=-=
# criaão de numeradores e denominadores
# =-=-=-=-=-=-=-=-=-=-=-=

# ponto de partida
sre_depara <- tec_sre_aplica

# Prefixos de MÚLTIPLAS exatamente como no SQL desta etapa
prefixos_multiplas <- c(
  "V09_", "V10_", "V11_", "V16_", "V20_", "V22_", "V23_", "V24_",
  "V28_", "V30_", "V35_", "V36_", "V44_", "V45_", "V46_", "V53_",
  "V55_", "V59_", "V93_", "V95_", "V96_", "V97_", "V101_"
)

# Selecionar todas as colunas *_ajustada das múltiplas (preservando tudo que existir na base)
multiplas_aj_cols <- names(sre_depara)
multiplas_aj_cols <- multiplas_aj_cols[
  grepl("_ajustada$", multiplas_aj_cols) &
    grepl(paste0("^(", paste(prefixos_multiplas, collapse="|"), ")"), multiplas_aj_cols)
]

# (opcional) apagar pares antigos para evitar conflito
sre_depara <- sre_depara %>%
  select(-any_of(paste0(multiplas_aj_cols, "_numerador")),
         -any_of(paste0(multiplas_aj_cols, "_denominador")))

# Criar numerador = valor quando não-NA; senão 0.0
# Criar denominador = 1.0 quando não-NA; senão 0.0
sre_nd_multiplas <- sre_depara %>%
  mutate(
    across(
      all_of(multiplas_aj_cols),
      ~ ifelse(!is.na(.x), as.numeric(.x), 0.0),
      .names = "{.col}_numerador"
    ),
    across(
      all_of(multiplas_aj_cols),
      ~ ifelse(!is.na(.x), 1.0, 0.0),
      .names = "{.col}_denominador"
    )
  )

# =-=-=-=-=-=-=-=-==-=-=-=-=-=
# media das questoes multiplas
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=

# ---------------------------
# Helpers (SRE)
# ---------------------------

# (1) Média = (∑ num) / (∑ den); devolve 0.0 quando ∑den == 0
#     - bases_ajustada: vetor COM "_ajustada" (ex.: c("V09_1_ajustada","V09_10_ajustada", ...))
#     - duplicidades são preservadas (se repetir o mesmo item no vetor, conta 2x)
add_ratio_zero <- function(df, bases_ajustada, new_name) {
  df_base <- as.data.frame(df)
  
  num_cols_full <- paste0(bases_ajustada, "_numerador")
  den_cols_full <- paste0(bases_ajustada, "_denominador")
  
  # mantém ordem e duplicidades; remove apenas nomes inexistentes
  num_cols <- num_cols_full[num_cols_full %in% names(df_base)]
  den_cols <- den_cols_full[den_cols_full %in% names(df_base)]
  
  num_sum <- if (length(num_cols)) rowSums(df_base[, num_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  den_sum <- if (length(den_cols)) rowSums(df_base[, den_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  
  df[[new_name]] <- ifelse(den_sum != 0, num_sum / den_sum, 0.0)
  df
}

# (2) "Marcou alguma" pelo SOMATÓRIO DE NUMERADORES: se ∑num <> 0 -> 1; senão -> 0
add_any_marked_by_num <- function(df, bases_ajustada, new_name) {
  df_base <- as.data.frame(df)
  num_cols_full <- paste0(bases_ajustada, "_numerador")
  num_cols <- num_cols_full[num_cols_full %in% names(df_base)]
  s_num <- if (length(num_cols)) rowSums(df_base[, num_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  df[[new_name]] <- ifelse(s_num != 0, 1.0, 0.0)
  df
}

# (3) Flag "==0 então 1; caso contrário 0" em uma coluna *_ajustada (igual ao SQL; NA -> 0)
add_eq_zero_flag <- function(df, col_ajustada, new_name) {
  x <- df[[col_ajustada]]
  df[[new_name]] <- ifelse(!is.na(x) & x == 0, 1.0, 0.0)
  df
}

# ---------------------------
# Aplicação literal do seu SQL
# ---------------------------
sre_medias <- sre_nd_multiplas

# --- V09 ---
sre_medias <- add_ratio_zero(sre_medias, c("V09_1_ajustada","V09_10_ajustada","V09_11_ajustada","V09_3_ajustada","V09_8_ajustada"), "V09_1_10_11_3_8_media")
sre_medias <- add_ratio_zero(sre_medias, c("V09_2_ajustada","V09_4_ajustada"), "V09_2_4_media")
sre_medias <- add_ratio_zero(sre_medias, c("V09_6_ajustada","V09_7_ajustada"), "V09_6_7_media")
sre_medias <- add_ratio_zero(sre_medias, c("V09_9_ajustada"), "V09_9_media")

# --- V101 (e alias V103_3_media conforme SQL) ---
sre_medias <- add_ratio_zero(sre_medias, c("V101_2_ajustada"), "V101_2_media")
sre_medias <- add_ratio_zero(sre_medias, c("V101_5_ajustada"), "V101_5_media")
sre_medias <- add_ratio_zero(sre_medias, c("V101_6_ajustada"), "V101_6_media")
sre_medias <- add_ratio_zero(sre_medias, c("V101_3_ajustada"), "V103_3_media")   # alias exatamente como no SQL
sre_medias <- add_ratio_zero(sre_medias, c("V101_4_ajustada"), "V101_4_media")

# --- V11 ---
sre_medias <- add_ratio_zero(sre_medias, paste0("V11_", 1:7, "_ajustada"), "V11_1_2_3_4_5_6_7_media")

# --- V16 ---
sre_medias <- add_eq_zero_flag(sre_medias, "V16_77_ajustada", "V16_media")

# --- V20 ---
sre_medias <- add_ratio_zero(sre_medias, paste0("V20_", 1:6, "_ajustada"), "V20_1_2_3_4_5_6_media")

# --- V22 (marcou alguma via soma de numeradores) ---
sre_medias <- add_any_marked_by_num(sre_medias, paste0("V22_", 1:6, "_ajustada"), "V22_media")

# --- V23 ---
sre_medias <- add_eq_zero_flag(sre_medias, "V23_77_ajustada", "V23_media")

# --- V24 (com duplicidades) ---
sre_medias <- add_ratio_zero(sre_medias,
                             c("V24_3_ajustada","V24_3_ajustada",
                               "V24_4_ajustada","V24_4_ajustada",
                               "V24_5_ajustada","V24_5_ajustada"),
                             "V24_3_4_5_media")

# --- V28 (com duplicidade do 3) ---
sre_medias <- add_ratio_zero(sre_medias,
                             c("V28_1_ajustada","V28_2_ajustada",
                               "V28_3_ajustada","V28_3_ajustada",
                               "V28_4_ajustada","V28_5_ajustada",
                               "V28_6_ajustada","V28_7_ajustada"),
                             "V28_1_2_3_3_4_5_6_7_media")

# --- V30 / V35 / V44 / V93 / V95 / V96 / V97 -> flag "_77 == 0" ---
sre_medias <- add_eq_zero_flag(sre_medias, "V30_77_ajustada", "V30_media")
sre_medias <- add_eq_zero_flag(sre_medias, "V35_77_ajustada", "V35_media")
sre_medias <- add_eq_zero_flag(sre_medias, "V44_77_ajustada", "V44_media")

# --- V36 ---
sre_medias <- add_ratio_zero(sre_medias, paste0("V36_", 1:7, "_ajustada"), "V36_1_2_3_4_5_6_7_media")

# --- V45 (com várias duplicidades em alguns blocos) ---
sre_medias <- add_ratio_zero(sre_medias, c("V45_1_ajustada","V45_2_ajustada","V45_7_ajustada"), "V45_1_2_7_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_10_ajustada","V45_11_ajustada","V45_12_ajustada"), "V45_10_11_12_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_13_ajustada","V45_14_ajustada"), "V45_13_14_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_15_ajustada","V45_17_ajustada"), "V45_15_17_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_16_ajustada"), "V45_16_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_20_ajustada"), "V45_20_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_3_ajustada"), "V45_3_media")
sre_medias <- add_ratio_zero(sre_medias, c("V45_4_ajustada","V45_4_ajustada"), "V45_4_media")   # duplicado
sre_medias <- add_ratio_zero(sre_medias, c("V45_5_ajustada"), "V45_5_media")
sre_medias <- add_ratio_zero(sre_medias,
                             c("V45_8_ajustada","V45_8_ajustada","V45_9_ajustada","V45_9_ajustada"),
                             "V45_8_9_media")  # duplicado

# --- V46 (com várias duplicidades em alguns blocos) ---
sre_medias <- add_ratio_zero(sre_medias, c("V46_1_ajustada","V46_2_ajustada","V46_7_ajustada"), "V46_1_2_7_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_10_ajustada","V46_11_ajustada","V46_12_ajustada"), "V46_10_11_12_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_13_ajustada","V46_14_ajustada"), "V46_13_14_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_15_ajustada","V46_17_ajustada"), "V46_15_17_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_16_ajustada"), "V46_16_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_20_ajustada"), "V46_20_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_3_ajustada"), "V46_3_media")
sre_medias <- add_ratio_zero(sre_medias, c("V46_4_ajustada","V46_4_ajustada"), "V46_4_media")  # duplicado
sre_medias <- add_ratio_zero(sre_medias, c("V46_5_ajustada"), "V46_5_media")
sre_medias <- add_ratio_zero(sre_medias,
                             c("V46_8_ajustada","V46_8_ajustada","V46_9_ajustada","V46_9_ajustada"),
                             "V46_8_9_media")  # duplicado

# --- V53 ---
sre_medias <- add_ratio_zero(sre_medias, c("V53_1_ajustada","V53_12_ajustada"), "V53_1_12_media")
sre_medias <- add_ratio_zero(sre_medias, c("V53_10_ajustada","V53_11_ajustada","V53_7_ajustada"), "V53_10_11_7_media")
sre_medias <- add_ratio_zero(sre_medias, c("V53_2_ajustada"), "V53_2_media")
sre_medias <- add_ratio_zero(sre_medias, c("V53_3_ajustada","V53_4_ajustada","V53_5_ajustada","V53_6_ajustada"), "V53_3_4_6_media")
sre_medias <- add_ratio_zero(sre_medias, c("V53_8_ajustada","V53_9_ajustada"), "V53_8_9_media")

# --- V55 ---
sre_medias <- add_ratio_zero(sre_medias, c("V55_1_ajustada","V55_12_ajustada"), "V55_1_12_media")
sre_medias <- add_ratio_zero(sre_medias, c("V55_10_ajustada","V55_11_ajustada","V55_7_ajustada"), "V55_10_11_7_media")
sre_medias <- add_ratio_zero(sre_medias, c("V55_12_ajustada"), "V55_12_media")
sre_medias <- add_ratio_zero(sre_medias, c("V55_2_ajustada"), "V55_2_media")
sre_medias <- add_ratio_zero(sre_medias, c("V55_3_ajustada","V55_4_ajustada","V55_5_ajustada","V55_6_ajustada"), "V55_3_4_6_media")
sre_medias <- add_ratio_zero(sre_medias, c("V55_8_ajustada","V55_9_ajustada"), "V55_8_9_media")

# --- V59 ---
sre_medias <- add_ratio_zero(sre_medias, paste0("V59_", 1:4, "_ajustada"), "V59_1_2_3_4_media")

# --- Flags baseadas em *_77 == 0 ---
sre_medias <- add_eq_zero_flag(sre_medias, "V93_77_ajustada", "V93_media")
sre_medias <- add_eq_zero_flag(sre_medias, "V95_77_ajustada", "V95_media")
sre_medias <- add_eq_zero_flag(sre_medias, "V96_77_ajustada", "V96_media")
sre_medias <- add_eq_zero_flag(sre_medias, "V97_77_ajustada", "V97_media")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Agregação ator see 
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ----------------------------
# 0) Mapeamento entrada -> alias final (igual ao SQL)
#    (quando a entrada é ..._media, o alias vira ..._ajustada)
# ----------------------------
campos_sre_agregar <- c(
  # simples (já _ajustada)
  "V01_ajustada"="V01_ajustada","V02_ajustada"="V02_ajustada","V03_ajustada"="V03_ajustada",
  "V04_ajustada"="V04_ajustada","V05_ajustada"="V05_ajustada","V06_ajustada"="V06_ajustada",
  "V07_ajustada"="V07_ajustada","V08_ajustada"="V08_ajustada",
  "V12_ajustada"="V12_ajustada","V13_ajustada"="V13_ajustada","V14_ajustada"="V14_ajustada",
  "V15_ajustada"="V15_ajustada","V17_ajustada"="V17_ajustada","V18_ajustada"="V18_ajustada",
  "V19_ajustada"="V19_ajustada","V21_ajustada"="V21_ajustada","V25_ajustada"="V25_ajustada",
  "V27_ajustada"="V27_ajustada","V31_ajustada"="V31_ajustada","V32_ajustada"="V32_ajustada",
  "V33_ajustada"="V33_ajustada","V34_ajustada"="V34_ajustada","V37_ajustada"="V37_ajustada",
  "V38_ajustada"="V38_ajustada","V39_ajustada"="V39_ajustada","V40_ajustada"="V40_ajustada",
  "V42_ajustada"="V42_ajustada","V43_ajustada"="V43_ajustada","V47_ajustada"="V47_ajustada",
  "V48_ajustada"="V48_ajustada","V49_ajustada"="V49_ajustada","V50_ajustada"="V50_ajustada",
  "V51_ajustada"="V51_ajustada","V56_ajustada"="V56_ajustada","V57_ajustada"="V57_ajustada",
  "V58_ajustada"="V58_ajustada","V60_ajustada"="V60_ajustada","V61_ajustada"="V61_ajustada",
  "V62_ajustada"="V62_ajustada","V63_ajustada"="V63_ajustada","V65_ajustada"="V65_ajustada",
  "V66_ajustada"="V66_ajustada","V67_ajustada"="V67_ajustada","V68_ajustada"="V68_ajustada",
  "V69_ajustada"="V69_ajustada","V70_ajustada"="V70_ajustada","V71_ajustada"="V71_ajustada",
  "V72_ajustada"="V72_ajustada","V74_ajustada"="V74_ajustada","V75_ajustada"="V75_ajustada",
  "V76_ajustada"="V76_ajustada","V77_ajustada"="V77_ajustada","V78_ajustada"="V78_ajustada",
  "V80_ajustada"="V80_ajustada","V81_ajustada"="V81_ajustada","V82_ajustada"="V82_ajustada",
  "V83_ajustada"="V83_ajustada","V84_ajustada"="V84_ajustada","V85_ajustada"="V85_ajustada",
  "V86_ajustada"="V86_ajustada","V87_ajustada"="V87_ajustada","V88_ajustada"="V88_ajustada",
  "V89_ajustada"="V89_ajustada","V90_ajustada"="V90_ajustada","V92_ajustada"="V92_ajustada",
  "V94_ajustada"="V94_ajustada","V98_ajustada"="V98_ajustada","V99_ajustada"="V99_ajustada",
  
  # entradas que são médias (_media) -> alias final _ajustada
  "V09_1_10_11_3_8_media"="V09_1_10_11_3_8_ajustada",
  "V09_2_4_media"="V09_2_4_ajustada",
  "V09_6_7_media"="V09_6_7_ajustada",
  "V09_9_media"="V09_9_ajustada",
  
  "V101_2_media"="V101_2_ajustada",
  "V101_4_media"="V101_4_ajustada",
  "V101_5_media"="V101_5_ajustada",
  "V101_6_media"="V101_6_ajustada",
  "V103_3_media"="V103_3_ajustada",
  
  "V11_1_2_3_4_5_6_7_media"="V11_1_2_3_4_5_6_7_ajustada",
  "V16_media"="V16_ajustada",
  "V20_1_2_3_4_5_6_media"="V20_1_2_3_4_5_6_ajustada",
  "V22_media"="V22_ajustada",
  "V23_media"="V23_ajustada",
  "V24_3_4_5_media"="V24_3_4_5_ajustada",
  "V28_1_2_3_3_4_5_6_7_media"="V28_1_2_3_3_4_5_6_7_ajustada",
  "V30_media"="V30_ajustada",
  "V35_media"="V35_ajustada",
  "V36_1_2_3_4_5_6_7_media"="V36_1_2_3_4_5_6_7_ajustada",
  "V44_media"="V44_ajustada",
  
  "V45_1_2_7_media"="V45_1_2_7_ajustada",
  "V45_10_11_12_media"="V45_10_11_12_ajustada",
  "V45_13_14_media"="V45_13_14_ajustada",
  "V45_15_17_media"="V45_15_17_ajustada",
  "V45_16_media"="V45_16_ajustada",
  "V45_20_media"="V45_20_ajustada",
  "V45_3_media"="V45_3_ajustada",
  "V45_4_media"="V45_4_ajustada",
  "V45_5_media"="V45_5_ajustada",
  "V45_8_9_media"="V45_8_9_ajustada",
  
  "V46_1_2_7_media"="V46_1_2_7_ajustada",
  "V46_10_11_12_media"="V46_10_11_12_ajustada",
  "V46_13_14_media"="V46_13_14_ajustada",
  "V46_15_17_media"="V46_15_17_ajustada",
  "V46_16_media"="V46_16_ajustada",
  "V46_20_media"="V46_20_ajustada",
  "V46_3_media"="V46_3_ajustada",
  "V46_4_media"="V46_4_ajustada",
  "V46_5_media"="V46_5_ajustada",
  "V46_8_9_media"="V46_8_9_ajustada",
  
  "V53_1_12_media"="V53_1_12_ajustada",
  "V53_10_11_7_media"="V53_10_11_7_ajustada",
  "V53_2_media"="V53_2_ajustada",
  "V53_3_4_6_media"="V53_3_4_6_ajustada",
  "V53_8_9_media"="V53_8_9_ajustada",
  
  "V55_1_12_media"="V55_1_12_ajustada",
  "V55_10_11_7_media"="V55_10_11_7_ajustada",
  "V55_12_media"="V55_12_ajustada",
  "V55_2_media"="V55_2_ajustada",
  "V55_3_4_6_media"="V55_3_4_6_ajustada",
  "V55_8_9_media"="V55_8_9_ajustada",
  
  "V59_1_2_3_4_media"="V59_1_2_3_4_ajustada",
  
  "V93_media"="V93_ajustada",
  "V95_media"="V95_ajustada",
  "V96_media"="V96_ajustada",
  "V97_media"="V97_ajustada"
)

# ----------------------------
# 1) Quais colunas existem na base atual
# ----------------------------
cols_existentes <- intersect(names(campos_sre_agregar), names(sre_medias))

# ----------------------------
# 2) Agregar por SEE (AVG), fixando ATOR como no SQL
# ----------------------------
agregado_por_see_sre <- sre_medias %>%
  mutate(ATOR = "2. Técnico SRE") %>%   # igual ao SELECT '2. Técnico SRE' AS ATOR
  select(-any_of("SEE")) %>%            # evita colisão caso SEE já exista
  rename(SEE = SEE_rotulo) %>%
  group_by(ATOR, SEE) %>%
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ----------------------------
# 3) Renomear colunas agregadas para os aliases finais (por posição)
# ----------------------------
alias_vec <- unname(campos_sre_agregar[cols_existentes])
stopifnot(length(intersect(alias_vec, c("ATOR","SEE"))) == 0)

posicoes <- match(cols_existentes, names(agregado_por_see_sre))
names(agregado_por_see_sre)[posicoes] <- alias_vec

# garantir nomes únicos
stopifnot(anyDuplicated(names(agregado_por_see_sre)) == 0)


# =-=-=-=-=-=-=-=---=-=-=-
# criar numeradores
# -=-=-=-=-=-=--=-=-=-=--=
# Ponto de partida: saída da etapa "1 agrega calculando item" (SRE)
# objeto: agregado_por_see_sre

# 1) Capturar TODAS as colunas que terminam com _ajustada
ajustadas_cols_see_sre <- grep("_ajustada$", names(agregado_por_see_sre), value = TRUE)

# (opcional) limpar pares antigos para evitar colisões
agregado_por_see_sre <- agregado_por_see_sre %>%
  select(-any_of(paste0(ajustadas_cols_see_sre, "_numerador")),
         -any_of(paste0(ajustadas_cols_see_sre, "_denominador")))

# 2) Criar num/den exatamente como no SQL do SRE (ELSE 0.0 em ambos)
agregado_por_see_sre_nd <- agregado_por_see_sre %>%
  mutate(
    across(
      all_of(ajustadas_cols_see_sre),
      ~ ifelse(!is.na(.x), as.numeric(.x), 0.0),
      .names = "{.col}_numerador"
    ),
    across(
      all_of(ajustadas_cols_see_sre),
      ~ ifelse(!is.na(.x), 1.0, 0.0),
      .names = "{.col}_denominador"
    )
  )


# =-=-=-=-=-=-=-=-=-=-=-
# calcular descritor
# -=-=-=-=-=-=-=-=-=-=-=

# ---------------------------
# Helper: média com fallback 0.0 quando ∑den == 0 (mantém duplicidades)
add_descritor_ratio_zero <- function(df, bases_ajustada, new_name) {
  df_base <- as.data.frame(df)
  num_cols <- paste0(bases_ajustada, "_numerador")
  den_cols <- paste0(bases_ajustada, "_denominador")
  
  # Remove nomes inexistentes, mas preserva ordem e duplicidades
  num_cols <- num_cols[num_cols %in% names(df_base)]
  den_cols <- den_cols[den_cols %in% names(df_base)]
  
  num_sum <- if (length(num_cols)) rowSums(df_base[, num_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  den_sum <- if (length(den_cols)) rowSums(df_base[, den_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  
  df[[new_name]] <- ifelse(den_sum != 0, num_sum / den_sum, 0.0)
  df
}

# ---------------------------
# Aplicação literal do seu SQL
# ---------------------------
descritores_sre_see <- agregado_por_see_sre_nd %>%
  mutate(SRE = "Agregado SEE")  # conforme o SELECT ... 'Agregado SEE' AS SRE
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20descritor.sql)

# ----- A2*
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V87_ajustada"), "A2D1_2")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V85_ajustada"), "A2D2_3")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V63_ajustada"), "A2D2_8")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V88_ajustada"), "A2D3_6")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V89_ajustada","V90_ajustada"), "A2D3_7")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V86_ajustada"), "A2D3_9")

# ----- A3*
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V92_ajustada","V93_ajustada","V95_ajustada"), "A3D1_1")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V96_ajustada","V97_ajustada"), "A3D1_2")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V98_ajustada"), "A3D2_3")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V55_12_ajustada"), "A3D2_5")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V94_ajustada"), "A3D3_7")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V99_ajustada"), "A3D4_8")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V56_ajustada","V57_ajustada","V58_ajustada"), "A3D4_9")

# ----- A4*
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V12_ajustada"), "A4D1_2")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V70_ajustada"), "A4D1_3")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V71_ajustada"), "A4D1_4")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V17_ajustada"), "A4D1_5")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V74_ajustada"), "A4D1_7")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V77_ajustada"), "A4D2_9")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V27_ajustada"), "A4D3_12")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V24_3_4_5_ajustada"), "A4D3_14")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V45_4_ajustada","V46_4_ajustada"), "A4D4_18")
# Trinca repetida no SQL com rótulos distintos
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V56_ajustada","V57_ajustada","V58_ajustada"), "A4D4_19")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V56_ajustada","V57_ajustada","V58_ajustada"), "A4D4_20")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V56_ajustada","V57_ajustada","V58_ajustada"), "A4D4_21")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V45_8_9_ajustada"), "A4D4_22")

# ----- E1*
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V78_ajustada"), "E1D1_1")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V80_ajustada"), "E1D1_2")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V07_ajustada"), "E1D1_4")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V01_ajustada","V02_ajustada","V03_ajustada"), "E1D1_5")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V14_ajustada"), "E1D2_10")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V09_6_7_ajustada"), "E1D2_6")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V68_ajustada"), "E1D2_7")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V08_ajustada"), "E1D4_15")

# ----- E2*
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V05_ajustada","V81_ajustada"), "E2D1_1")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V09_1_10_11_3_8_ajustada"), "E2D1_11")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V69_ajustada"), "E2D1_12")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V04_ajustada"), "E2D1_3")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V12_ajustada"), "E2D1_5")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V70_ajustada"), "E2D1_6")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V09_9_ajustada"), "E2D1_8")

descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V11_1_2_3_4_5_6_7_ajustada"), "E2D2_14")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V53_2_ajustada","V55_2_ajustada"), "E2D2_16")

descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V45_1_2_7_ajustada"), "E2D3_17")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V83_ajustada"), "E2D3_19")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V15_ajustada"), "E2D3_20")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V60_ajustada"), "E2D3_21")

descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V06_ajustada","V82_ajustada"), "E2D4_22")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V45_3_ajustada","V46_3_ajustada","V47_ajustada"), "E2D4_23")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V56_ajustada","V57_ajustada","V58_ajustada"), "E2D4_24")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V59_1_2_3_4_ajustada"), "E2D4_25")
# Duplicidade (2x) no SQL para V09_2_4
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V09_2_4_ajustada","V09_2_4_ajustada"), "E2D4_28")

descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V16_ajustada"), "E2D5_30")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V101_2_ajustada"), "E2D5_31")

# ----- E3*
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V71_ajustada"), "E3D1_10")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V18_ajustada"), "E3D1_3")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V20_1_2_3_4_5_6_ajustada","V21_ajustada","V22_ajustada"), "E3D1_5")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V17_ajustada"), "E3D1_8")
descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V74_ajustada"), "E3D1_9")

descritores_sre_see <- add_descritor_ratio_zero(descritores_sre_see, c("V19_ajustada"), "E3D2_12")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# pivotagem ator see
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

sre_descritores_agregado <- descritores_sre_see

# Lista de descritores exatamente como no UNPIVOT do SQL
descritores_sql_sre <- c(
  "A2D1_2","A2D2_3","A2D2_8","A2D3_6","A2D3_7","A2D3_9",
  "A3D1_1","A3D1_2","A3D2_3","A3D2_5","A3D3_7","A3D4_8","A3D4_9",
  "A4D1_2","A4D1_3","A4D1_4","A4D1_5","A4D1_7","A4D2_9","A4D3_12","A4D3_14",
  "A4D4_18","A4D4_19","A4D4_20","A4D4_21","A4D4_22",
  "E1D1_1","E1D1_2","E1D1_4","E1D1_5","E1D2_10","E1D2_6","E1D2_7","E1D4_15",
  "E2D1_1","E2D1_11","E2D1_12","E2D1_3","E2D1_5","E2D1_6","E2D1_8",
  "E2D2_14","E2D2_16",
  "E2D3_17","E2D3_19","E2D3_20","E2D3_21",
  "E2D4_22","E2D4_23","E2D4_24","E2D4_25","E2D4_28",
  "E2D5_30","E2D5_31",
  "E3D1_10","E3D1_3","E3D1_5","E3D1_8","E3D1_9",
  "E3D2_12","E3D2_18",
  "E3D3_22","E3D3_24",
  "E3D4_26","E3D4_27","E3D4_28","E3D4_29","E3D4_30","E3D4_31",
  "E3D5_33","E3D5_36",
  "E4D1_1","E4D1_2",
  "E4D2_5","E4D2_8",
  "E4D3_14","E4D3_16","E4D3_17",
  "E4D4_19","E4D4_20","E4D4_21","E4D4_22",
  "E4D5_24","E4D5_25",
  "E5D1_1","E5D1_3",
  "E5D2_10","E5D2_12","E5D2_5","E5D2_6","E5D2_8",
  "E5D3_13","E5D3_14","E5D3_15","E5D3_16","E5D3_18",
  "E5D4_19","E5D4_20",
  "E5D5_22","E5D5_23",
  "E6D1_2",
  "E6D2_11","E6D2_14","E6D2_16","E6D2_17","E6D2_4","E6D2_5","E6D2_8",
  "E6D3_18",
  "E6D4_21","E6D4_23",
  "E6D5_26","E6D5_27"
)

# Apenas os descritores que realmente existem no seu objeto (robusto a faltas)
descritores_exist_sre <- intersect(descritores_sql_sre, names(sre_descritores_agregado))

# Pivot: wide -> long (equivalente ao UNPIVOT do SQL)
sre_descritores_long_see <- sre_descritores_agregado %>%
  pivot_longer(
    cols      = all_of(descritores_exist_sre),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  arrange(ATOR, SEE, SRE, descritor)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Ator sre see
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# ponto de entrada (ajuste se o seu objeto tiver outro nome)
sre_medias_input <- sre_medias

# ----------------------------
# 0) Mapeamento entrada -> alias final (igual ao SQL)
#    (quando a entrada é ..._media, o alias vira ..._ajustada)
# ----------------------------
campos_sre_agregar_sre <- c(
  # simples (_ajustada já vem pronta)
  "V01_ajustada"="V01_ajustada","V02_ajustada"="V02_ajustada","V03_ajustada"="V03_ajustada",
  "V04_ajustada"="V04_ajustada","V05_ajustada"="V05_ajustada","V06_ajustada"="V06_ajustada",
  "V07_ajustada"="V07_ajustada","V08_ajustada"="V08_ajustada",
  "V12_ajustada"="V12_ajustada","V13_ajustada"="V13_ajustada","V14_ajustada"="V14_ajustada",
  "V15_ajustada"="V15_ajustada","V17_ajustada"="V17_ajustada","V18_ajustada"="V18_ajustada",
  "V19_ajustada"="V19_ajustada","V21_ajustada"="V21_ajustada","V25_ajustada"="V25_ajustada",
  "V27_ajustada"="V27_ajustada","V31_ajustada"="V31_ajustada","V32_ajustada"="V32_ajustada",
  "V33_ajustada"="V33_ajustada","V34_ajustada"="V34_ajustada","V37_ajustada"="V37_ajustada",
  "V38_ajustada"="V38_ajustada","V39_ajustada"="V39_ajustada","V40_ajustada"="V40_ajustada",
  "V42_ajustada"="V42_ajustada","V43_ajustada"="V43_ajustada","V47_ajustada"="V47_ajustada",
  "V48_ajustada"="V48_ajustada","V49_ajustada"="V49_ajustada","V50_ajustada"="V50_ajustada",
  "V51_ajustada"="V51_ajustada","V56_ajustada"="V56_ajustada","V57_ajustada"="V57_ajustada",
  "V58_ajustada"="V58_ajustada","V60_ajustada"="V60_ajustada","V61_ajustada"="V61_ajustada",
  "V62_ajustada"="V62_ajustada","V63_ajustada"="V63_ajustada","V65_ajustada"="V65_ajustada",
  "V66_ajustada"="V66_ajustada","V67_ajustada"="V67_ajustada","V68_ajustada"="V68_ajustada",
  "V69_ajustada"="V69_ajustada","V70_ajustada"="V70_ajustada","V71_ajustada"="V71_ajustada",
  "V72_ajustada"="V72_ajustada","V74_ajustada"="V74_ajustada","V75_ajustada"="V75_ajustada",
  "V76_ajustada"="V76_ajustada","V77_ajustada"="V77_ajustada","V78_ajustada"="V78_ajustada",
  "V80_ajustada"="V80_ajustada","V81_ajustada"="V81_ajustada","V82_ajustada"="V82_ajustada",
  "V83_ajustada"="V83_ajustada","V84_ajustada"="V84_ajustada","V85_ajustada"="V85_ajustada",
  "V86_ajustada"="V86_ajustada","V87_ajustada"="V87_ajustada","V88_ajustada"="V88_ajustada",
  "V89_ajustada"="V89_ajustada","V90_ajustada"="V90_ajustada","V92_ajustada"="V92_ajustada",
  "V94_ajustada"="V94_ajustada","V98_ajustada"="V98_ajustada","V99_ajustada"="V99_ajustada",
  
  # entradas que são médias (_media) -> alias final _ajustada
  "V09_1_10_11_3_8_media"="V09_1_10_11_3_8_ajustada",
  "V09_2_4_media"="V09_2_4_ajustada",
  "V09_6_7_media"="V09_6_7_ajustada",
  "V09_9_media"="V09_9_ajustada",
  
  "V101_2_media"="V101_2_ajustada",
  "V101_4_media"="V101_4_ajustada",
  "V101_5_media"="V101_5_ajustada",
  "V101_6_media"="V101_6_ajustada",
  "V103_3_media"="V103_3_ajustada",
  
  "V11_1_2_3_4_5_6_7_media"="V11_1_2_3_4_5_6_7_ajustada",
  "V16_media"="V16_ajustada",
  "V20_1_2_3_4_5_6_media"="V20_1_2_3_4_5_6_ajustada",
  "V22_media"="V22_ajustada",
  "V23_media"="V23_ajustada",
  "V24_3_4_5_media"="V24_3_4_5_ajustada",
  "V28_1_2_3_3_4_5_6_7_media"="V28_1_2_3_3_4_5_6_7_ajustada",
  "V30_media"="V30_ajustada",
  "V35_media"="V35_ajustada",
  "V36_1_2_3_4_5_6_7_media"="V36_1_2_3_4_5_6_7_ajustada",
  "V44_media"="V44_ajustada",
  
  "V45_1_2_7_media"="V45_1_2_7_ajustada",
  "V45_10_11_12_media"="V45_10_11_12_ajustada",
  "V45_13_14_media"="V45_13_14_ajustada",
  "V45_15_17_media"="V45_15_17_ajustada",
  "V45_16_media"="V45_16_ajustada",
  "V45_20_media"="V45_20_ajustada",
  "V45_3_media"="V45_3_ajustada",
  "V45_4_media"="V45_4_ajustada",
  "V45_5_media"="V45_5_ajustada",
  "V45_8_9_media"="V45_8_9_ajustada",
  
  "V46_1_2_7_media"="V46_1_2_7_ajustada",
  "V46_10_11_12_media"="V46_10_11_12_ajustada",
  "V46_13_14_media"="V46_13_14_ajustada",
  "V46_15_17_media"="V46_15_17_ajustada",
  "V46_16_media"="V46_16_ajustada",
  "V46_20_media"="V46_20_ajustada",
  "V46_3_media"="V46_3_ajustada",
  "V46_4_media"="V46_4_ajustada",
  "V46_5_media"="V46_5_ajustada",
  "V46_8_9_media"="V46_8_9_ajustada",
  
  "V53_1_12_media"="V53_1_12_ajustada",
  "V53_10_11_7_media"="V53_10_11_7_ajustada",
  "V53_2_media"="V53_2_ajustada",
  "V53_3_4_6_media"="V53_3_4_6_ajustada",
  "V53_8_9_media"="V53_8_9_ajustada",
  
  "V55_1_12_media"="V55_1_12_ajustada",
  "V55_10_11_7_media"="V55_10_11_7_ajustada",
  "V55_12_media"="V55_12_ajustada",
  "V55_2_media"="V55_2_ajustada",
  "V55_3_4_6_media"="V55_3_4_6_ajustada",
  "V55_8_9_media"="V55_8_9_ajustada",
  
  "V59_1_2_3_4_media"="V59_1_2_3_4_ajustada",
  
  "V93_media"="V93_ajustada",
  "V95_media"="V95_ajustada",
  "V96_media"="V96_ajustada",
  "V97_media"="V97_ajustada"
)

# ----------------------------
# 1) Quais colunas existem na base atual
# ----------------------------
cols_existentes <- intersect(names(campos_sre_agregar_sre), names(sre_medias_input))

# ----------------------------
# 2) Agregar por SEE + SRE (AVG), fixando ATOR como no SQL
# ----------------------------
agregado_por_see_sre <- sre_medias_input %>%
  mutate(ATOR = "2. Técnico SRE") %>%    # SELECT '2. Técnico SRE' AS ATOR
  select(-any_of(c("SEE","SRE"))) %>%    # evita colisão de nomes
  rename(SEE = SEE_rotulo, SRE = sre) %>%# SEE_rotulo AS SEE; sre AS SRE
  group_by(ATOR, SEE, SRE) %>%           # GROUP BY ATOR, SEE, SRE
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ----------------------------
# 3) Renomear para os aliases finais (por posição)
# ----------------------------
alias_vec <- unname(campos_sre_agregar_sre[cols_existentes])
stopifnot(length(intersect(alias_vec, c("ATOR","SEE","SRE"))) == 0)

posicoes <- match(cols_existentes, names(agregado_por_see_sre))
names(agregado_por_see_sre)[posicoes] <- alias_vec

# garantir nomes únicos
stopifnot(anyDuplicated(names(agregado_por_see_sre)) == 0)

# ----------------------------
# 4) Amostra rápida
# ----------------------------
agregado_por_see_sre %>%
  select(ATOR, SEE, SRE,
         V09_1_10_11_3_8_ajustada, V20_1_2_3_4_5_6_ajustada,
         V45_8_9_ajustada, V46_8_9_ajustada,
         V53_1_12_ajustada, V55_1_12_ajustada,
         V59_1_2_3_4_ajustada) %>%
  head()


#=-=-==-=-=-=-=-=-=-=-=-=-=-=-=-
# calcular descritor
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# ponto de partida
sre_nd <- agregado_por_see_sre_nd  # o seu objeto atual

# chaves presentes na base (pode ser ATOR+SEE ou ATOR+SEE+SRE)
key_cols <- intersect(c("ATOR","SEE","SRE"), names(sre_nd))

sre_descritores_agregado <- sre_nd %>%
  # usa APENAS as chaves que existem
  select(all_of(key_cols)) %>%
  bind_cols(
    sre_nd %>% select(-any_of(key_cols))  # garante acesso às colunas ND
  ) %>%
  { # --- A* (A2..A4) ---
    . |> add_descritor_ratio(c("V87_ajustada"),                                   "A2D1_2")   |>
      add_descritor_ratio(c("V85_ajustada"),                                   "A2D2_3")   |>
      add_descritor_ratio(c("V63_ajustada"),                                   "A2D2_8")   |>
      add_descritor_ratio(c("V88_ajustada"),                                   "A2D3_6")   |>
      add_descritor_ratio(c("V89_ajustada","V90_ajustada"),                    "A2D3_7")   |>
      add_descritor_ratio(c("V86_ajustada"),                                   "A2D3_9")   |>
      
      add_descritor_ratio(c("V92_ajustada","V93_ajustada","V95_ajustada"),     "A3D1_1")   |>
      add_descritor_ratio(c("V96_ajustada","V97_ajustada"),                    "A3D1_2")   |>
      add_descritor_ratio(c("V98_ajustada"),                                   "A3D2_3")   |>
      add_descritor_ratio(c("V55_12_ajustada"),                                "A3D2_5")   |>
      add_descritor_ratio(c("V94_ajustada"),                                   "A3D3_7")   |>
      add_descritor_ratio(c("V99_ajustada"),                                   "A3D4_8")   |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "A3D4_9")   |>
      
      add_descritor_ratio(c("V12_ajustada"),                                   "A4D1_2")   |>
      add_descritor_ratio(c("V70_ajustada"),                                   "A4D1_3")   |>
      add_descritor_ratio(c("V71_ajustada"),                                   "A4D1_4")   |>
      add_descritor_ratio(c("V17_ajustada"),                                   "A4D1_5")   |>
      add_descritor_ratio(c("V74_ajustada"),                                   "A4D1_7")   |>
      
      add_descritor_ratio(c("V77_ajustada"),                                   "A4D2_9")   |>
      add_descritor_ratio(c("V27_ajustada"),                                   "A4D3_12")  |>
      add_descritor_ratio(c("V24_3_4_5_ajustada"),                             "A4D3_14")  |>
      add_descritor_ratio(c("V45_4_ajustada","V46_4_ajustada"),                "A4D4_18")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "A4D4_19")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "A4D4_20")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "A4D4_21")  |>
      add_descritor_ratio(c("V45_8_9_ajustada"),                               "A4D4_22")
  } %>%
  { # --- E* (E1..E6) ---
    . |> add_descritor_ratio(c("V78_ajustada"),                                   "E1D1_1")   |>
      add_descritor_ratio(c("V80_ajustada"),                                   "E1D1_2")   |>
      add_descritor_ratio(c("V07_ajustada"),                                   "E1D1_4")   |>
      add_descritor_ratio(c("V01_ajustada","V02_ajustada","V03_ajustada"),     "E1D1_5")   |>
      add_descritor_ratio(c("V14_ajustada"),                                   "E1D2_10")  |>
      add_descritor_ratio(c("V09_6_7_ajustada"),                               "E1D2_6")   |>
      add_descritor_ratio(c("V68_ajustada"),                                   "E1D2_7")   |>
      add_descritor_ratio(c("V08_ajustada"),                                   "E1D4_15")  |>
      
      add_descritor_ratio(c("V05_ajustada","V81_ajustada"),                    "E2D1_1")   |>
      add_descritor_ratio(c("V09_1_10_11_3_8_ajustada"),                       "E2D1_11")  |>
      add_descritor_ratio(c("V69_ajustada"),                                   "E2D1_12")  |>
      add_descritor_ratio(c("V04_ajustada"),                                   "E2D1_3")   |>
      add_descritor_ratio(c("V12_ajustada"),                                   "E2D1_5")   |>
      add_descritor_ratio(c("V70_ajustada"),                                   "E2D1_6")   |>
      add_descritor_ratio(c("V09_9_ajustada"),                                 "E2D1_8")   |>
      
      add_descritor_ratio(c("V11_1_2_3_4_5_6_7_ajustada"),                     "E2D2_14")  |>
      add_descritor_ratio(c("V53_2_ajustada","V55_2_ajustada"),                "E2D2_16")  |>
      
      add_descritor_ratio(c("V45_1_2_7_ajustada"),                              "E2D3_17")  |>
      add_descritor_ratio(c("V83_ajustada"),                                   "E2D3_19")  |>
      add_descritor_ratio(c("V15_ajustada"),                                   "E2D3_20")  |>
      add_descritor_ratio(c("V60_ajustada"),                                   "E2D3_21")  |>
      
      add_descritor_ratio(c("V06_ajustada","V82_ajustada"),                    "E2D4_22")  |>
      add_descritor_ratio(c("V45_3_ajustada","V46_3_ajustada","V47_ajustada"), "E2D4_23")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E2D4_24")  |>
      add_descritor_ratio(c("V59_1_2_3_4_ajustada"),                            "E2D4_25")  |>
      add_descritor_ratio(c("V09_2_4_ajustada","V09_2_4_ajustada"),            "E2D4_28")  |>
      
      add_descritor_ratio(c("V16_ajustada"),                                   "E2D5_30")  |>
      add_descritor_ratio(c("V101_2_ajustada"),                                "E2D5_31")  |>
      
      add_descritor_ratio(c("V71_ajustada"),                                   "E3D1_10")  |>
      add_descritor_ratio(c("V18_ajustada"),                                   "E3D1_3")   |>
      add_descritor_ratio(c("V20_1_2_3_4_5_6_ajustada","V21_ajustada","V22_ajustada"),
                          "E3D1_5")   |>
      add_descritor_ratio(c("V17_ajustada"),                                   "E3D1_8")   |>
      add_descritor_ratio(c("V74_ajustada"),                                   "E3D1_9")   |>
      
      add_descritor_ratio(c("V19_ajustada"),                                   "E3D2_12")  |>
      add_descritor_ratio(c("V13_ajustada"),                                   "E3D2_18")  |>
      
      add_descritor_ratio(c("V53_1_12_ajustada"),                               "E3D3_22")  |>
      add_descritor_ratio(c("V61_ajustada"),                                   "E3D3_24")  |>
      
      add_descritor_ratio(c("V45_5_ajustada","V46_5_ajustada"),                 "E3D4_26")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E3D4_27")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E3D4_28")  |>
      add_descritor_ratio(c("V45_4_ajustada","V46_4_ajustada"),                 "E3D4_29")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E3D4_30")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E3D4_31")  |>
      
      add_descritor_ratio(c("V23_ajustada"),                                   "E3D5_33")  |>
      add_descritor_ratio(c("V103_3_ajustada"),                                 "E3D5_36")  |>
      
      add_descritor_ratio(c("V72_ajustada"),                                   "E4D1_1")   |>
      add_descritor_ratio(c("V28_1_2_3_3_4_5_6_7_ajustada"),                    "E4D1_2")   |>
      
      add_descritor_ratio(c("V53_3_4_6_ajustada","V55_3_4_6_ajustada"),         "E4D2_5")   |>
      add_descritor_ratio(c("V25_ajustada"),                                   "E4D2_8")   |>
      
      add_descritor_ratio(c("V24_3_4_5_ajustada"),                             "E4D3_14")  |>
      add_descritor_ratio(c("V27_ajustada"),                                   "E4D3_16")  |>
      add_descritor_ratio(c("V62_ajustada"),                                   "E4D3_17")  |>
      
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E4D4_19")  |>
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E4D4_20")  |>
      add_descritor_ratio(c("V45_8_9_ajustada","V46_8_9_ajustada"),             "E4D4_21")  |>
      add_descritor_ratio(c("V45_10_11_12_ajustada","V46_10_11_12_ajustada"),   "E4D4_22")  |>
      
      add_descritor_ratio(c("V30_ajustada"),                                   "E4D5_24")  |>
      add_descritor_ratio(c("V101_4_ajustada"),                                 "E4D5_25")  |>
      
      add_descritor_ratio(c("V65_ajustada"),                                   "E5D1_1")   |>
      add_descritor_ratio(c("V31_ajustada"),                                   "E5D1_3")   |>
      
      add_descritor_ratio(c("V33_ajustada"),                                   "E5D2_10")  |>
      add_descritor_ratio(c("V53_8_9_ajustada","V55_8_9_ajustada"),             "E5D2_12")  |>
      add_descritor_ratio(c("V32_ajustada"),                                   "E5D2_5")   |>
      add_descritor_ratio(c("V76_ajustada"),                                   "E5D2_6")   |>
      add_descritor_ratio(c("V34_ajustada"),                                   "E5D2_8")   |>
      
      add_descritor_ratio(c("V45_20_ajustada","V46_20_ajustada","V50_ajustada"),"E5D3_13") |>
      add_descritor_ratio(c("V67_ajustada"),                                   "E5D3_14")  |>
      add_descritor_ratio(c("V48_ajustada"),                                   "E5D3_15")  |>
      add_descritor_ratio(c("V51_ajustada"),                                   "E5D3_16")  |>
      add_descritor_ratio(c("V49_ajustada","V66_ajustada"),                     "E5D3_18")  |>
      
      add_descritor_ratio(c("V56_ajustada","V57_ajustada","V58_ajustada"),     "E5D4_19")  |>
      add_descritor_ratio(c("V45_13_14_ajustada","V46_13_14_ajustada"),         "E5D4_20")  |>
      
      add_descritor_ratio(c("V35_ajustada"),                                   "E5D5_22")  |>
      add_descritor_ratio(c("V101_5_ajustada"),                                 "E5D5_23")  |>
      
      add_descritor_ratio(c("V42_ajustada","V43_ajustada"),                     "E6D1_2")   |>
      
      add_descritor_ratio(c("V38_ajustada"),                                   "E6D2_11")  |>
      add_descritor_ratio(c("V39_ajustada","V40_ajustada"),                     "E6D2_14")  |>
      add_descritor_ratio(c("V45_16_ajustada","V46_16_ajustada"),               "E6D2_16")  |>
      add_descritor_ratio(c("V53_10_11_7_ajustada","V55_10_11_7_ajustada"),     "E6D2_17")  |>
      add_descritor_ratio(c("V75_ajustada"),                                   "E6D2_4")   |>
      add_descritor_ratio(c("V37_ajustada"),                                   "E6D2_5")   |>
      add_descritor_ratio(c("V36_1_2_3_4_5_6_7_ajustada"),                      "E6D2_8")   |>
      
      add_descritor_ratio(c("V60_ajustada"),                                   "E6D3_18")  |>
      
      add_descritor_ratio(c("V45_15_17_ajustada","V46_15_17_ajustada"),         "E6D4_21")  |>
      add_descritor_ratio(c("V84_ajustada"),                                   "E6D4_23")  |>
      
      add_descritor_ratio(c("V44_ajustada"),                                   "E6D5_26")  |>
      add_descritor_ratio(c("V101_6_ajustada"),                                 "E6D5_27")
  }

#=-=-=-=-=-=-=-=-=-=-
# pivotagem
#=-=-=-=-=-=-=-=-=-=-

# Use a saída do passo "3 calcula descritor" (agregado). Ex.: sre_descritores_agregado
# Se o nome for outro, ajuste a linha abaixo:
descritores_df <- sre_descritores_agregado

# Chaves presentes (pode ser ATOR+SEE ou ATOR+SEE+SRE)
key_cols <- intersect(c("ATOR","SEE","SRE"), names(descritores_df))

# Lista de descritores exatamente como no UNPIVOT do seu 4 pivota 3.sql
descritores_sql_sre <- c(
  "A2D1_2","A2D2_3","A2D2_8","A2D3_6","A2D3_7","A2D3_9",
  "A3D1_1","A3D1_2","A3D2_3","A3D2_5","A3D3_7","A3D4_8","A3D4_9",
  "A4D1_2","A4D1_3","A4D1_4","A4D1_5","A4D1_7","A4D2_9","A4D3_12","A4D3_14",
  "A4D4_18","A4D4_19","A4D4_20","A4D4_21","A4D4_22",
  "E1D1_1","E1D1_2","E1D1_4","E1D1_5","E1D2_10","E1D2_6","E1D2_7","E1D4_15",
  "E2D1_1","E2D1_11","E2D1_12","E2D1_3","E2D1_5","E2D1_6","E2D1_8",
  "E2D2_14","E2D2_16",
  "E2D3_17","E2D3_19","E2D3_20","E2D3_21",
  "E2D4_22","E2D4_23","E2D4_24","E2D4_25","E2D4_28",
  "E2D5_30","E2D5_31",
  "E3D1_10","E3D1_3","E3D1_5","E3D1_8","E3D1_9",
  "E3D2_12","E3D2_18",
  "E3D3_22","E3D3_24",
  "E3D4_26","E3D4_27","E3D4_28","E3D4_29","E3D4_30","E3D4_31",
  "E3D5_33","E3D5_36",
  "E4D1_1","E4D1_2",
  "E4D2_5","E4D2_8",
  "E4D3_14","E4D3_16","E4D3_17",
  "E4D4_19","E4D4_20","E4D4_21","E4D4_22",
  "E4D5_24","E4D5_25",
  "E5D1_1","E5D1_3",
  "E5D2_10","E5D2_12","E5D2_5","E5D2_6","E5D2_8",
  "E5D3_13","E5D3_14","E5D3_15","E5D3_16","E5D3_18",
  "E5D4_19","E5D4_20",
  "E5D5_22","E5D5_23",
  "E6D1_2",
  "E6D2_11","E6D2_14","E6D2_16","E6D2_17","E6D2_4","E6D2_5","E6D2_8",
  "E6D3_18",
  "E6D4_21","E6D4_23",
  "E6D5_26","E6D5_27"
)
# (lista transcrita do IN (...) do UNPIVOT)  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/4%20pivota%203.sql)

# Apenas os descritores que realmente existem no objeto (robustez)
descritores_exist <- intersect(descritores_sql_sre, names(descritores_df))

# Pivot wide -> long
sre_descritores_long <- descritores_df %>%
  pivot_longer(
    cols      = all_of(descritores_exist),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  arrange(across(all_of(key_cols)), descritor)
library(dplyr)
library(tidyr)

# >>> OBJETO DE ENTRADA:
# Use a saída do passo "3 calcula descritor" (agregado). Ex.: sre_descritores_agregado
# Se o nome for outro, ajuste a linha abaixo:
descritores_df <- sre_descritores_agregado

# Chaves presentes (pode ser ATOR+SEE ou ATOR+SEE+SRE)
key_cols <- intersect(c("ATOR","SEE","SRE"), names(descritores_df))

# Lista de descritores exatamente como no UNPIVOT do seu 4 pivota 3.sql
descritores_sql_sre <- c(
  "A2D1_2","A2D2_3","A2D2_8","A2D3_6","A2D3_7","A2D3_9",
  "A3D1_1","A3D1_2","A3D2_3","A3D2_5","A3D3_7","A3D4_8","A3D4_9",
  "A4D1_2","A4D1_3","A4D1_4","A4D1_5","A4D1_7","A4D2_9","A4D3_12","A4D3_14",
  "A4D4_18","A4D4_19","A4D4_20","A4D4_21","A4D4_22",
  "E1D1_1","E1D1_2","E1D1_4","E1D1_5","E1D2_10","E1D2_6","E1D2_7","E1D4_15",
  "E2D1_1","E2D1_11","E2D1_12","E2D1_3","E2D1_5","E2D1_6","E2D1_8",
  "E2D2_14","E2D2_16",
  "E2D3_17","E2D3_19","E2D3_20","E2D3_21",
  "E2D4_22","E2D4_23","E2D4_24","E2D4_25","E2D4_28",
  "E2D5_30","E2D5_31",
  "E3D1_10","E3D1_3","E3D1_5","E3D1_8","E3D1_9",
  "E3D2_12","E3D2_18",
  "E3D3_22","E3D3_24",
  "E3D4_26","E3D4_27","E3D4_28","E3D4_29","E3D4_30","E3D4_31",
  "E3D5_33","E3D5_36",
  "E4D1_1","E4D1_2",
  "E4D2_5","E4D2_8",
  "E4D3_14","E4D3_16","E4D3_17",
  "E4D4_19","E4D4_20","E4D4_21","E4D4_22",
  "E4D5_24","E4D5_25",
  "E5D1_1","E5D1_3",
  "E5D2_10","E5D2_12","E5D2_5","E5D2_6","E5D2_8",
  "E5D3_13","E5D3_14","E5D3_15","E5D3_16","E5D3_18",
  "E5D4_19","E5D4_20",
  "E5D5_22","E5D5_23",
  "E6D1_2",
  "E6D2_11","E6D2_14","E6D2_16","E6D2_17","E6D2_4","E6D2_5","E6D2_8",
  "E6D3_18",
  "E6D4_21","E6D4_23",
  "E6D5_26","E6D5_27"
)
# (lista transcrita do IN (...) do UNPIVOT)  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/4%20pivota%203.sql)

# Apenas os descritores que realmente existem no objeto (robustez)
descritores_exist <- intersect(descritores_sql_sre, names(descritores_df))

# Pivot wide -> long
sre_descritores_long <- descritores_df %>%
  pivot_longer(
    cols      = all_of(descritores_exist),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  arrange(across(all_of(key_cols)), descritor)




























