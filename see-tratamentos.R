
# Pacotes
library(tidyverse)
library(readxl)

# Carregamento dos dicionarios -------------------------------------------------
tec.see <- read_excel("Tecnico SEE.xlsx")


# Se SubmissionDate já vier como Date/POSIXct, o parse é desnecessário
tec.see <- tec.see %>%
  mutate(
    SubmissionDate_parsed = suppressWarnings(parse_date_time(SubmissionDate, orders = c("Ymd HMS", "Y-m-d H:M:S", "d/m/Y H:M:S", "d/m/Y", "Y-m-d")))
  )

# Vetor com os UUIDs de teste (do seu SQL)
uuids_teste <- c(
  "uuid:676e71e1-0c70-4952-9d8f-e92fce0faf78",
  "uuid:b59fe2aa-2417-4535-bcd5-0d5c44b07ad3"
)

# Aplicar os quatro filtros do SQL
tec_exclusao <- tec.see %>%
  # 1) SubmissionDate preenchido (no SQL é > '', aqui usamos não-NA e não string vazia;
  #    se preferir, use a coluna parseada se ela existir)
  filter(
    (!is.na(SubmissionDate) & SubmissionDate != "") |
      (!is.na(SubmissionDate_parsed))
  ) %>%
  # 2) KEY != 'uuid:'
  filter(KEY != "uuid:") %>%
  # 3) tirar rede SEE 5 (Minas Gerais)
  filter(see != 5.0) %>%
  # 4) tirar testes (dois UUIDs)
  filter(!(KEY %in% uuids_teste))

#=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-=-=-=--=-=-=
# aplicação de para
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# --------------------------------
# Funções auxiliares de recodificação
# --------------------------------

# Likert 1..4 -> 0, .33, .66, 1
rec_1to4 <- function(x) case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.33,
  x == 3 ~ 0.66,
  x == 4 ~ 1.00,
  TRUE   ~ NA_real_
)

# Tri 1..3 -> 0, .5, 1
rec_1to3_0_05_1 <- function(x) case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.50,
  x == 3 ~ 1.00,
  TRUE   ~ NA_real_
)

# Binário 1/2 -> 1/0
rec_bin12_10 <- function(x) case_when(
  x == 1 ~ 1.00,
  x == 2 ~ 0.00,
  TRUE   ~ NA_real_
)

# Múltipla 0/1 com "gate" _77 (só vale quando flag == 0)
# Se flag == 0: 0 -> 0; 1 -> 1; outros -> NA
# Se flag != 0 ou NA -> NA
rec_01_gated <- function(x, flag) case_when(
  flag == 0 & x == 0 ~ 0.00,
  flag == 0 & x == 1 ~ 1.00,
  flag == 0          ~ NA_real_,
  TRUE               ~ NA_real_
)

# --------------------------------
# Listas de variáveis por padrão  (conforme seu SQL)
# --------------------------------

# 1) Tri 1..3 -> 0/.5/1
tri_1to3 <- c("V14","V31","V37")  # mapeadas explicitamente no SQL

# 2) Likert 1..4 -> 0/.33/.66/1
likert_1to4 <- c(
  "V01","V02","V05","V06","V09","V11","V17","V19","V20","V25","V27",
  "V32","V33","V34","V35","V36","V42","V44","V46","V48","V51","V51_A",
  "V54","V54_A","V55","V56","V57","V58","V59","V60","V61","V61_A","V62","V62_A",
  "V63","V64","V64_1","V65","V66","V67","V68","V69","V70","V71","V72","V73","V74",
  "V75","V76","V77","V78","V79","V81","V83","V84","V86","V87","V89",
  "V93","V94","V95","V96","V97","V98","V101","V102","V103","V104","V105","V106",
  "V107","V108","V109","V111","V112","V112_A","V118","V119","V123","V125","V125_B",
  "V128","V129","V130"
)

# 3) Binário 1/2 -> 1/0
bin_12_10 <- c(
  "V03","V04","V08","V10","V12","V22","V39","V43","V47","V49","V50",
  "V51_B","V85","V90","V99","V100","V110","V114","V116","V121"
)

# 4) Famílias 0/1 "gated" por *_77
fam_gated <- list(
  V07  = paste0("V07_", 1:5),
  V13  = paste0("V13_", 1:10),
  V15  = paste0("V15_", 1:8),
  V16  = paste0("V16_", 1:8),
  V18  = c(paste0("V18_", 1:3), paste0("V18_", c(5,6,7,8))),  # 4º não existe; 5..8 existem
  V21  = paste0("V21_", 1:7),
  V24  = paste0("V24_", 1:6),
  V28  = paste0("V28_", 1:8),
  V29  = paste0("V29_", 1:9),
  V30  = paste0("V30_", 1:8),
  V38  = c(paste0("V38_", 1:5), "V38_8"),                     # no SQL não há _6/_7
  V40  = paste0("V40_", 1:20),
  V41  = paste0("V41_", 1:20),
  V53  = paste0("V53_", 1:7),
  V91  = paste0("V91_", 1:6),
  V92  = paste0("V92_", 1:6),
  V122 = paste0("V122_", 1:6),
  V124 = paste0("V124_", 1:6)
)

# --------------------------------
# Aplicação do de-para (Tecnico SEE)
# --------------------------------

tec_depara <- tec_exclusao %>%
  # SEE_rotulo
  mutate(
    SEE_rotulo = case_when(
      see == 1 ~ "1. SEE Ceará",
      see == 2 ~ "2. SEE Espírito Santo",
      see == 3 ~ "3. SEE Goiás",
      see == 4 ~ "4. SEE Piauí",
      TRUE     ~ NA_character_
    )
  ) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Likert 1..4
  mutate(across(any_of(likert_1to4), rec_1to4, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Tri 1..3 (0/.5/1)
  mutate(across(any_of(tri_1to3), rec_1to3_0_05_1, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Binário 1/2 -> 1/0
  mutate(across(any_of(bin_12_10), rec_bin12_10, .names = "{.col}_ajustada"))       # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)

# ---- Famílias gated (precisam do *_77 == 0) ----
# Vamos aplicar família a família para acessar o flag correto.

# helper para aplicar uma família gated dada a "raiz" (ex.: "V07")
aplica_familia_gated <- function(df, raiz, itens) {
  flag_col <- paste0(raiz, "_77")
  if (!flag_col %in% names(df)) return(df)  # se o flag não existir, não faz nada
  # só processa itens que existem no df
  cols <- intersect(itens, names(df))
  if (length(cols) == 0) return(df)
  df %>%
    mutate(across(
      all_of(cols),
      ~ rec_01_gated(.x, .data[[flag_col]]),
      .names = "{.col}_ajustada"
    ))
}

for (raiz in names(fam_gated)) {
  tec_depara <- aplica_familia_gated(tec_depara, raiz, fam_gated[[raiz]])
}

# --- Correção pontual do V18_5 (typo no SQL: '01' -> '0') ---
if (all(c("V18_5","V18_77") %in% names(tec_exclusao))) {
  tec_depara <- tec_depara %>%
    mutate(V18_5_ajustada = ifelse(V18_77 == 0,
                                   case_when(V18_5 == 0 ~ 0, V18_5 == 1 ~ 1, TRUE ~ NA_real_),
                                   NA_real_))
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# cria numeradores e denominadores
# -==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# 1) Identificar todas as colunas *_ajustada
ajustadas_cols_tec <- grep("_ajustada$", names(tec_depara), value = TRUE)

# (opcional) se por algum motivo já existirem colunas _numerador/_denominador antigas, remova
tec_depara <- tec_depara %>%
  select(-matches("_ajustada_(numerador|denominador)$"))

# 2) Criar numeradores e denominadores EXATAMENTE como no SQL:
#    - numerador: valor quando não-NA; senão NA (NULL)
#    - denominador: 1 quando não-NA; senão NA (NULL)
tec_nd <- tec_depara %>%
  mutate(
    across(
      all_of(ajustadas_cols_tec),
      ~ ifelse(!is.na(.x), as.numeric(.x), NA_real_),
      .names = "{.col}_numerador"
    ),
    across(
      all_of(ajustadas_cols_tec),
      ~ ifelse(!is.na(.x), 1, NA_real_),
      .names = "{.col}_denominador"
    )
  )

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# média de questoes multiplas
# -==-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# ============================
# Helpers (versões vetorizadas)
# ============================

# (1) Razão com DUPLICIDADES: (∑num)/(∑den)
#     -> NA quando ∑den == 0 OU quando todos os den do bloco são NA
ratio_with_dups <- function(df, bases, new_name) {
  num_cols_full <- paste0(bases, "_ajustada_numerador")
  den_cols_full <- paste0(bases, "_ajustada_denominador")
  df_base <- as.data.frame(df)
  
  # Mantém ordem e duplicidades; remove apenas nomes inexistentes
  num_cols <- num_cols_full[num_cols_full %in% names(df_base)]
  den_cols <- den_cols_full[den_cols_full %in% names(df_base)]
  
  num_sum <- if (length(num_cols)) rowSums(df_base[, num_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, nrow(df_base))
  if (length(den_cols)) {
    den_mat <- df_base[, den_cols, drop = FALSE]
    den_sum <- rowSums(den_mat, na.rm = TRUE)
    all_na  <- rowSums(!is.na(den_mat)) == 0
    out <- ifelse(den_sum != 0 & !all_na, num_sum / den_sum, NA_real_)
  } else {
    out <- rep(NA_real_, nrow(df_base))
  }
  df[[new_name]] <- out
  df
}

# (2) "Marcou alguma" COM pré-checagem do primeiro item e ELSE = 0
#     - Se o primeiro item do bloco estiver NA -> NA
#     - Caso contrário: se soma(num) <> 0 (ou > 0) -> 1; senão 0
any_marked_first_else0 <- function(df, first_base, sum_bases, new_name, op = c("<>", ">")) {
  op <- match.arg(op)
  df_base <- as.data.frame(df)
  
  first_col <- paste0(first_base, "_ajustada_numerador")
  
  # Se a coluna do primeiro item não existe, devolve NA para todas as linhas
  if (!first_col %in% names(df_base)) {
    df[[new_name]] <- rep(NA_real_, nrow(df_base))
    return(df)
  }
  
  first_num <- df_base[[first_col]]
  
  sum_cols_full <- paste0(sum_bases, "_ajustada_numerador")
  sum_cols <- sum_cols_full[sum_cols_full %in% names(df_base)]
  
  s <- if (length(sum_cols)) rowSums(df_base[, sum_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, nrow(df_base))
  cond <- ifelse(op == "<>", s != 0, s > 0)
  
  out <- ifelse(is.na(first_num), NA_real_, ifelse(cond, 1.0, 0.0))
  df[[new_name]] <- out
  df
}

# (3) "Marcou alguma" SEM pré-checagem e ELSE = NA (igual ao bloco V29 no SQL)
#     - Se soma(num) > 0 -> 1; senão -> NA
any_marked_else_na <- function(df, sum_bases, new_name) {
  df_base <- as.data.frame(df)
  sum_cols_full <- paste0(sum_bases, "_ajustada_numerador")
  sum_cols <- sum_cols_full[sum_cols_full %in% names(df_base)]
  
  s <- if (length(sum_cols)) rowSums(df_base[, sum_cols, drop = FALSE], na.rm = TRUE) else rep(NA_real_, nrow(df_base))
  out <- ifelse(s > 0, 1.0, NA_real_)
  df[[new_name]] <- out
  df
}

# ============================
# Aplicação (EXATA ao SQL)
# ============================

tec_medias <- tec_nd

# --- "Marcou alguma" com pré-checagem do primeiro item (ELSE=0) ---
# V07_1_media: WHEN V07_1_num IS NULL -> NULL; WHEN V07_1_num <> 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V07_1", c("V07_1"), "V07_1_media", op = "<>")

# V07_2_3_media: first=V07_2; (V07_2 + V07_3) <> 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V07_2", c("V07_2","V07_3"), "V07_2_3_media", op = "<>")

# V122_media: first=V122_1; soma V122_1..6 <> 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V122_1", paste0("V122_", 1:6), "V122_media", op = "<>")

# V124_media: first=V124_1; soma V124_1..6 > 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V124_1", paste0("V124_", 1:6), "V124_media", op = ">")

# V18_media: first=V18_1; soma (V18_1,2,3,5,6) > 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V18_1", c("V18_1","V18_2","V18_3","V18_5","V18_6"), "V18_media", op = ">")

# V23_media: first=V23_1; soma V23_1..5 <> 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V23_1", paste0("V23_", 1:5), "V23_media", op = "<>")

# V24_media: first=V24_1; soma V24_1..6 > 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V24_1", paste0("V24_", 1:6), "V24_media", op = ">")

# V38_media: first=V38_1; soma V38_1..4 > 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V38_1", paste0("V38_", 1:4), "V38_media", op = ">")

# V53_media: first=V53_1; soma V53_1..7 <> 0 -> 1 ELSE 0
tec_medias <- any_marked_first_else0(tec_medias, "V53_1", paste0("V53_", 1:7), "V53_media", op = "<>")

# --- "Marcou alguma" SEM pré-checagem, ELSE = NA ---
# V29_media: WHEN sum(V29_1..8) > 0 THEN 1 ELSE NULL
tec_medias <- any_marked_else_na(tec_medias, paste0("V29_", 1:8), "V29_media")

# --- Médias por razão (∑num/∑den) COM DUPLICIDADES EXATAS do SQL ---

# V13
tec_medias <- ratio_with_dups(tec_medias, c("V13_1","V13_2","V13_6","V13_8","V13_9"), "V13_1_2_6_8_9_media")
tec_medias <- ratio_with_dups(tec_medias, c("V13_4"),                              "V13_4_media")
tec_medias <- ratio_with_dups(tec_medias, c("V13_7"),                              "V13_7_media")

# V16
tec_medias <- ratio_with_dups(tec_medias, paste0("V16_", 1:8),                     "V16_1_2_3_4_5_6_7_8_media")

# V21
tec_medias <- ratio_with_dups(tec_medias, paste0("V21_", 1:6),                     "V21_1_2_3_4_5_6_media")

# V26
tec_medias <- ratio_with_dups(tec_medias, c("V26_1"),                              "V26_1_media")
tec_medias <- ratio_with_dups(tec_medias, paste0("V26_", 1:5),                     "V26_1_2_3_4_5_media")
tec_medias <- ratio_with_dups(tec_medias, c("V26_2"),                              "V26_2_media")
tec_medias <- ratio_with_dups(tec_medias, c("V26_2","V26_4"),                      "V26_2_4_media")
tec_medias <- ratio_with_dups(tec_medias, c("V26_3","V26_5","V26_3","V26_5"),      "V26_3_5_media")  # duplicado no SQL

# V28
tec_medias <- ratio_with_dups(tec_medias, paste0("V28_", 1:8),                     "V28_1_2_3_4_5_6_7_8_media")

# V30
tec_medias <- ratio_with_dups(tec_medias, paste0("V30_", 1:8),                     "V30_1_2_3_4_5_6_7_8_media")

# V40
tec_medias <- ratio_with_dups(tec_medias, c("V40_1","V40_2","V40_7"),              "V40_1_2_7_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_10","V40_11","V40_11","V40_8"),   "V40_10_11_11_8_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_12","V40_13"),                    "V40_12_13_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_14","V40_16"),                    "V40_14_16_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_15"),                             "V40_15_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_19"),                             "V40_19_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_3"),                              "V40_3_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_4","V40_4"),                      "V40_4_media")      # duplicado no SQL
tec_medias <- ratio_with_dups(tec_medias, c("V40_5","V40_9"),                      "V40_5_9_media")
tec_medias <- ratio_with_dups(tec_medias, c("V40_8","V40_9","V40_8","V40_9"),      "V40_8_9_media")    # duplicado no SQL

# V41
tec_medias <- ratio_with_dups(tec_medias, c("V41_1","V41_2","V41_3","V41_7"),      "V41_1_2_3_7_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_10","V41_11","V41_11","V41_8","V41_9"), "V41_10_11_11_9_9_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_12","V41_13"),                    "V41_12_13_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_14","V41_16"),                    "V41_14_16_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_15"),                             "V41_15_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_19"),                             "V41_19_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_4","V41_4"),                      "V41_4_media")      # duplicado no SQL
tec_medias <- ratio_with_dups(tec_medias, c("V41_5"),                              "V41_5_media")
tec_medias <- ratio_with_dups(tec_medias, c("V41_8","V41_9","V41_8","V41_9"),      "V41_8_9_media")    # duplicado no SQL

# V64 / V91 (itens isolados)
tec_medias <- ratio_with_dups(tec_medias, c("V64_1"),                              "V64_1_media")
tec_medias <- ratio_with_dups(tec_medias, c("V91_2"),                              "V91_2_media")
tec_medias <- ratio_with_dups(tec_medias, c("V91_3"),                              "V91_3_media")
tec_medias <- ratio_with_dups(tec_medias, c("V91_4"),                              "V91_4_media")
tec_medias <- ratio_with_dups(tec_medias, c("V91_5"),                              "V91_5_media")
tec_medias <- ratio_with_dups(tec_medias, c("V91_6"),                              "V91_6_media")


# -=-=-=-=-=-=-=-=-=-=-=-=-=-
# agregação por item
# =-==-=-=-=-=-=-=-=-=-=-=-=-
# ----------------------------
# 0) Mapeamento entrada -> alias final (igual ao SQL)
#    (quando a entrada é ..._media, o alias vira ..._ajustada)
# ----------------------------
campos_tec_agregar <- c(
  # blocos simples já _ajustada
  "V01_ajustada"="V01_ajustada","V02_ajustada"="V02_ajustada","V03_ajustada"="V03_ajustada",
  "V04_ajustada"="V04_ajustada","V05_ajustada"="V05_ajustada","V06_ajustada"="V06_ajustada",
  "V08_ajustada"="V08_ajustada","V09_ajustada"="V09_ajustada","V10_ajustada"="V10_ajustada",
  "V11_ajustada"="V11_ajustada","V12_ajustada"="V12_ajustada",
  "V14_ajustada"="V14_ajustada","V17_ajustada"="V17_ajustada",
  "V19_ajustada"="V19_ajustada","V20_ajustada"="V20_ajustada",
  "V22_ajustada"="V22_ajustada","V25_ajustada"="V25_ajustada","V27_ajustada"="V27_ajustada",
  "V31_ajustada"="V31_ajustada","V32_ajustada"="V32_ajustada","V33_ajustada"="V33_ajustada",
  "V34_ajustada"="V34_ajustada","V35_ajustada"="V35_ajustada","V36_ajustada"="V36_ajustada",
  "V37_ajustada"="V37_ajustada","V39_ajustada"="V39_ajustada","V42_ajustada"="V42_ajustada",
  "V43_ajustada"="V43_ajustada","V44_ajustada"="V44_ajustada","V46_ajustada"="V46_ajustada",
  "V47_ajustada"="V47_ajustada","V48_ajustada"="V48_ajustada","V49_ajustada"="V49_ajustada",
  "V50_ajustada"="V50_ajustada","V51_ajustada"="V51_ajustada","V51_A_ajustada"="V51_A_ajustada",
  "V51_B_ajustada"="V51_B_ajustada","V54_ajustada"="V54_ajustada","V54_A_ajustada"="V54_A_ajustada",
  "V55_ajustada"="V55_ajustada","V56_ajustada"="V56_ajustada","V57_ajustada"="V57_ajustada",
  "V58_ajustada"="V58_ajustada","V59_ajustada"="V59_ajustada","V60_ajustada"="V60_ajustada",
  "V61_ajustada"="V61_ajustada","V61_A_ajustada"="V61_A_ajustada","V63_ajustada"="V63_ajustada",
  "V65_ajustada"="V65_ajustada","V66_ajustada"="V66_ajustada","V67_ajustada"="V67_ajustada",
  "V68_ajustada"="V68_ajustada","V69_ajustada"="V69_ajustada","V70_ajustada"="V70_ajustada",
  "V71_ajustada"="V71_ajustada","V72_ajustada"="V72_ajustada","V73_ajustada"="V73_ajustada",
  "V74_ajustada"="V74_ajustada","V75_ajustada"="V75_ajustada","V76_ajustada"="V76_ajustada",
  "V77_ajustada"="V77_ajustada","V78_ajustada"="V78_ajustada","V79_ajustada"="V79_ajustada",
  "V81_ajustada"="V81_ajustada","V83_ajustada"="V83_ajustada","V84_ajustada"="V84_ajustada",
  "V85_ajustada"="V85_ajustada","V86_ajustada"="V86_ajustada","V87_ajustada"="V87_ajustada",
  "V89_ajustada"="V89_ajustada","V93_ajustada"="V93_ajustada","V94_ajustada"="V94_ajustada",
  "V95_ajustada"="V95_ajustada","V97_ajustada"="V97_ajustada","V98_ajustada"="V98_ajustada",
  "V99_ajustada"="V99_ajustada","V100_ajustada"="V100_ajustada","V101_ajustada"="V101_ajustada",
  "V102_ajustada"="V102_ajustada","V103_ajustada"="V103_ajustada","V104_ajustada"="V104_ajustada",
  "V105_ajustada"="V105_ajustada","V106_ajustada"="V106_ajustada","V107_ajustada"="V107_ajustada",
  "V108_ajustada"="V108_ajustada","V110_ajustada"="V110_ajustada","V111_ajustada"="V111_ajustada",
  "V112_ajustada"="V112_ajustada","V112_A_ajustada"="V112_A_ajustada","V114_ajustada"="V114_ajustada",
  "V116_ajustada"="V116_ajustada","V118_ajustada"="V118_ajustada","V119_ajustada"="V119_ajustada",
  "V121_ajustada"="V121_ajustada","V125_ajustada"="V125_ajustada","V128_ajustada"="V128_ajustada",
  "V129_ajustada"="V129_ajustada",
  
  # entradas que são médias de múltiplas (_media) -> alias final _ajustada
  "V07_1_media"="V07_1_ajustada","V07_2_3_media"="V07_2_3_ajustada",
  "V13_1_2_6_8_9_media"="V13_1_2_6_8_9_ajustada","V13_4_media"="V13_4_ajustada","V13_7_media"="V13_7_ajustada",
  "V16_1_2_3_4_5_6_7_8_media"="V16_1_2_3_4_5_6_7_8_ajustada",
  "V18_media"="V18_ajustada",
  "V21_1_2_3_4_5_6_media"="V21_1_2_3_4_5_6_ajustada",
  "V23_media"="V23_ajustada","V24_media"="V24_ajustada",
  "V26_1_media"="V26_1_ajustada","V26_1_2_3_4_5_media"="V26_1_2_3_4_5_ajustada",
  "V26_2_media"="V26_2_ajustada","V26_2_4_media"="V26_2_4_ajustada","V26_3_5_media"="V26_3_5_ajustada",
  "V28_1_2_3_4_5_6_7_8_media"="V28_1_2_3_4_5_6_7_8_ajustada",
  "V29_media"="V29_ajustada",
  "V30_1_2_3_4_5_6_7_8_media"="V30_1_2_3_4_5_6_7_8_ajustada",
  "V38_media"="V38_ajustada",
  "V40_1_2_7_media"="V40_1_2_7_ajustada","V40_10_11_11_8_media"="V40_10_11_11_8_ajustada",
  "V40_12_13_media"="V40_12_13_ajustada","V40_14_16_media"="V40_14_16_ajustada","V40_15_media"="V40_15_ajustada",
  "V40_19_media"="V40_19_ajustada","V40_3_media"="V40_3_ajustada","V40_4_media"="V40_4_ajustada",
  "V40_5_9_media"="V40_5_9_ajustada","V40_8_9_media"="V40_8_9_ajustada",
  "V41_1_2_3_7_media"="V41_1_2_3_7_ajustada","V41_10_11_11_9_9_media"="V41_10_11_11_9_9_ajustada",
  "V41_12_13_media"="V41_12_13_ajustada","V41_14_16_media"="V41_14_16_ajustada","V41_15_media"="V41_15_ajustada",
  "V41_19_media"="V41_19_ajustada","V41_4_media"="V41_4_ajustada","V41_5_media"="V41_5_ajustada",
  "V41_8_9_media"="V41_8_9_ajustada",
  "V53_media"="V53_ajustada",
  "V64_1_media"="V64_1_ajustada",
  "V91_2_media"="V91_2_ajustada","V91_3_media"="V91_3_ajustada","V91_4_media"="V91_4_ajustada",
  "V91_5_media"="V91_5_ajustada","V91_6_media"="V91_6_ajustada",
  "V122_media"="V122_ajustada","V124_media"="V124_ajustada"
)

# ----------------------------
# 1) Quais colunas existem na base atual
# ----------------------------
cols_existentes <- intersect(names(campos_tec_agregar), names(tec_medias))

# ----------------------------
# 2) Agregar por SEE (AVG), fixando ATOR como no SQL
# ----------------------------
agregado_por_see_tecnico <- tec_medias %>%
  mutate(ATOR = "1. Técnico SEE") %>%        # igual ao SELECT '1. Técnico SEE' AS ATOR
  select(-any_of("SEE")) %>%                 # evita colisão caso SEE já exista
  rename(SEE = SEE_rotulo) %>%
  group_by(ATOR, SEE) %>%
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ----------------------------
# 3) Renomear colunas agregadas para os aliases finais (por posição)
# ----------------------------
alias_vec <- unname(campos_tec_agregar[cols_existentes])

# sanidade: não pode colidir com as chaves
stopifnot(length(intersect(alias_vec, c("ATOR","SEE"))) == 0)
# aplicar por posição
posicoes <- match(cols_existentes, names(agregado_por_see_tecnico))
names(agregado_por_see_tecnico)[posicoes] <- alias_vec
# garantir nomes únicos
stopifnot(anyDuplicated(names(agregado_por_see_tecnico)) == 0)

# ----------------------------
# 4) Amostra rápida
# ----------------------------
agregado_por_see_tecnico %>%
  select(ATOR, SEE,
         V07_1_ajustada, V13_1_2_6_8_9_ajustada, V21_1_2_3_4_5_6_ajustada,
         V40_10_11_11_8_ajustada, V41_10_11_11_9_9_ajustada) %>%
  head()

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# criar numerador e denominador
# -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# 1) Todas as colunas *_ajustada no agregado por SEE
ajustadas_cols_see <- grep("_ajustada$", names(agregado_por_see_tecnico), value = TRUE)

# (opcional) se por algum motivo já existirem pares antigos, remova antes
agregado_por_see_tecnico <- agregado_por_see_tecnico %>%
  select(-matches("_ajustada_(numerador|denominador)$"))

# 2) Criar numeradores e denominadores EXATAMENTE como no SQL (ELSE NULL):
#    - numerador = valor quando não-NA; NA caso contrário
#    - denominador = 1 quando não-NA; NA caso contrário
agregado_por_see_tecnico_nd <- agregado_por_see_tecnico %>%
  mutate(
    across(
      all_of(ajustadas_cols_see),
      ~ ifelse(!is.na(.x), as.numeric(.x), NA_real_),
      .names = "{.col}_numerador"
    ),
    across(
      all_of(ajustadas_cols_see),
      ~ ifelse(!is.na(.x), 1, NA_real_),
      .names = "{.col}_denominador"
    )
  )

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# calcula o descritor
# =-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-

# ---------------------------
# Helper: (∑ num) / (∑ den); 0.0 quando ∑den == 0
# Mantém DUPLICIDADES exatamente como no SQL (se um item aparece 2x, conta 2x).
# Se alguma coluna não existir, ela é simplesmente ignorada (equivalente a não contribuir).
add_descritor_ratio <- function(df, bases_ajustada, new_name) {
  df_base <- as.data.frame(df)
  
  # Vetores completos com duplicidades preservadas
  num_cols_full <- paste0(bases_ajustada, "_numerador")
  den_cols_full <- paste0(bases_ajustada, "_denominador")
  
  # Mantém a ordem e duplicidades; remove apenas nomes inexistentes
  num_cols <- num_cols_full[num_cols_full %in% names(df_base)]
  den_cols <- den_cols_full[den_cols_full %in% names(df_base)]
  
  # Somatórios (se o conjunto ficar vazio, devolvemos 0 no final)
  num_sum <- if (length(num_cols)) rowSums(df_base[, num_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  den_sum <- if (length(den_cols)) rowSums(df_base[, den_cols, drop = FALSE], na.rm = TRUE) else rep(0, nrow(df_base))
  
  out <- ifelse(den_sum != 0, num_sum / den_sum, 0.0)
  df[[new_name]] <- out
  df
}

# ---------------------------
# Aplicação literal do seu SQL (descritor -> bases)
# ---------------------------

descritores_see_tecnico <- agregado_por_see_tecnico_nd %>%
  mutate(SRE = "Agregado SEE")  # igual ao SELECT ... 'Agregado SEE' AS SRE
# [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20descritor.sql)

# --- A1* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V103_ajustada"),                     "A1D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V108_ajustada"),                     "A1D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V94_ajustada"),                      "A1D1_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V95_ajustada"),                      "A1D1_4")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V101_ajustada","V107_ajustada"),     "A1D1_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V97_ajustada"),                      "A1D2_6")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V104_ajustada"),                     "A1D2_7")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V99_ajustada"),                      "A1D2_8")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V98_ajustada"),                      "A1D2_9")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V100_ajustada"),                     "A1D3_10")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V106_ajustada"),                     "A1D3_12")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V102_ajustada"),                     "A1D3_13")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V93_ajustada"),                      "A1D3_14")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V105_ajustada"),                     "A1D5_15")

# --- A2* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V110_ajustada"),                     "A2D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V116_ajustada"),                     "A2D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V111_ajustada"),                     "A2D2_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V112_ajustada","V112_A_ajustada"),   "A2D2_8")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V118_ajustada"),                     "A2D3_6")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V119_ajustada"),                     "A2D3_7")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V114_ajustada"),                     "A2D3_9")

# --- A3* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V122_ajustada"),                     "A3D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V124_ajustada"),                     "A3D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V125_ajustada"),                     "A3D2_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V128_ajustada"),                     "A3D3_6")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V121_ajustada"),                     "A3D3_7")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V129_ajustada"),                     "A3D4_8")

# --- A4* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V12_ajustada","V14_ajustada"),       "A4D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V56_ajustada"),                      "A4D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V81_ajustada"),                      "A4D1_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V84_ajustada"),                      "A4D1_4")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V60_ajustada"),                      "A4D1_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V83_ajustada"),                      "A4D1_7")
# A4D3_10 no SQL soma V26_3_5_ajustada DUAS vezes (num e den) -> preservamos duplicidade
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V26_3_5_ajustada","V26_3_5_ajustada"), "A4D3_10")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V26_1_2_3_4_5_ajustada"),            "A4D3_13")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V26_2_ajustada"),                    "A4D3_15")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V25_ajustada"),                      "A4D3_16")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_4_ajustada","V41_4_ajustada"),   "A4D4_18")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_8_9_ajustada"),                  "A4D4_22")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V24_ajustada"),                      "A4D5_23")

# --- E1* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V01_ajustada"),                      "E1D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V04_ajustada"),                      "E1D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V78_ajustada","V79_ajustada"),       "E1D1_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V77_ajustada"),                      "E1D1_4")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V42_ajustada","V76_ajustada"),       "E1D1_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V54_A_ajustada"),                    "E1D2_6")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V05_ajustada"),                      "E1D2_9")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V02_ajustada"),                      "E1D3_11")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V03_ajustada"),                      "E1D3_12")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V06_ajustada"),                      "E1D3_13")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V07_1_ajustada"),                    "E1D5_16")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V07_2_3_ajustada"),                  "E1D5_17")

# --- E2* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V08_ajustada","V09_ajustada"),       "E2D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V13_1_2_6_8_9_ajustada"),            "E2D1_10")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V55_ajustada"),                      "E2D1_11")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V10_ajustada"),                      "E2D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V11_ajustada"),                      "E2D1_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V12_ajustada","V14_ajustada"),       "E2D1_4")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V56_ajustada"),                      "E2D1_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V81_ajustada"),                      "E2D1_6")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V13_7_ajustada"),                    "E2D1_7")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V54_ajustada"),                      "E2D1_8")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V16_1_2_3_4_5_6_7_8_ajustada"),      "E2D2_13")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V57_ajustada"),                      "E2D2_14")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_1_2_7_ajustada"),                "E2D3_17")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V67_ajustada","V68_ajustada","V69_ajustada"), "E2D3_18")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V46_ajustada"),                      "E2D3_19")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V70_ajustada"),                      "E2D3_20")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V73_ajustada","V75_ajustada"),       "E2D3_21")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V17_ajustada"),                      "E2D4_22")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_3_ajustada","V43_ajustada","V44_ajustada"), "E2D4_23")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V74_ajustada"),                      "E2D4_25")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V13_4_ajustada"),                    "E2D4_27")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V18_ajustada"),                      "E2D5_29")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V91_2_ajustada"),                    "E2D5_31")

# --- E3* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V19_ajustada"),                      "E3D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V84_ajustada"),                      "E3D1_10")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V58_ajustada"),                      "E3D1_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V21_1_2_3_4_5_6_ajustada","V22_ajustada","V23_ajustada"), "E3D1_4")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V59_ajustada"),                      "E3D1_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V60_ajustada"),                      "E3D1_8")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V83_ajustada"),                      "E3D1_9")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V20_ajustada"),                      "E3D2_11")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V61_A_ajustada"),                    "E3D2_18")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V75_ajustada"),                      "E3D3_23")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V72_ajustada"),                      "E3D3_24")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V39_ajustada"),                      "E3D3_25")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_5_9_ajustada","V41_5_ajustada"), "E3D4_26")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_4_ajustada"),                    "E3D4_29")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V24_ajustada"),                      "E3D5_32")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V91_3_ajustada"),                    "E3D5_36")

# --- E4* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V87_ajustada"),                      "E4D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V28_1_2_3_4_5_6_7_8_ajustada"),      "E4D1_3")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V27_ajustada"),                      "E4D2_7")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V61_ajustada"),                      "E4D2_8")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V26_3_5_ajustada"),                  "E4D3_11")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V26_2_4_ajustada"),                  "E4D3_12")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V26_1_ajustada"),                    "E4D3_4")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_8_9_ajustada"),                  "E4D4_21")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_10_11_11_8_ajustada","V41_10_11_11_9_9_ajustada"), "E4D4_22")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V29_ajustada"),                      "E4D5_23")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V91_4_ajustada"),                    "E4D5_25")

# --- E5* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V85_ajustada"),                      "E5D1_1")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V47_ajustada","V51_B_ajustada"),     "E5D1_3")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V71_ajustada"),                      "E5D1_4")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V63_ajustada"),                      "E5D2_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V51_A_ajustada"),                    "E5D2_7")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_19_ajustada","V41_19_ajustada","V50_ajustada"), "E5D3_13")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V48_ajustada","V86_ajustada"),       "E5D3_15")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V49_ajustada"),                      "E5D3_16")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V51_ajustada"),                      "E5D3_18")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_12_13_ajustada"),                "E5D4_20")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V53_ajustada"),                      "E5D5_21")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V91_5_ajustada"),                    "E5D5_23")

# --- E6* ---
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V64_1_ajustada"),                    "E6D1_2")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V32_ajustada"),                      "E6D1_3")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V33_ajustada"),                      "E6D2_10")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V34_ajustada","V35_ajustada"),       "E6D2_13")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V65_ajustada"),                      "E6D2_14")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_15_ajustada","V41_15_ajustada"), "E6D2_16")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V89_ajustada"),                      "E6D2_4")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V66_ajustada"),                      "E6D2_5")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V31_ajustada"),                      "E6D2_6")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V30_1_2_3_4_5_6_7_8_ajustada"),      "E6D2_7")

descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V40_14_16_ajustada"),                "E6D4_21")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V36_ajustada","V37_ajustada"),       "E6D4_23")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V38_ajustada"),                      "E6D5_25")
descritores_see_tecnico <- add_descritor_ratio(descritores_see_tecnico, c("V91_6_ajustada"),                    "E6D5_27")

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# pivotagem
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Lista de descritores, exatamente como no UNPIVOT do seu SQL
descritores_sql <- c(
  "A1D1_1","A1D1_2","A1D1_3","A1D1_4","A1D1_5",
  "A1D2_6","A1D2_7","A1D2_8","A1D2_9",
  "A1D3_10","A1D3_12","A1D3_13","A1D3_14","A1D5_15",
  "A2D1_1","A2D1_2","A2D2_3","A2D2_8","A2D3_6","A2D3_7","A2D3_9",
  "A3D1_1","A3D1_2","A3D2_3","A3D3_6","A3D3_7","A3D4_8",
  "A4D1_1","A4D1_2","A4D1_3","A4D1_4","A4D1_5","A4D1_7",
  "A4D3_10","A4D3_13","A4D3_15","A4D3_16",
  "A4D4_18","A4D4_22","A4D5_23",
  "E1D1_1","E1D1_2","E1D1_3","E1D1_4","E1D1_5",
  "E1D2_6","E1D2_9","E1D3_11","E1D3_12","E1D3_13",
  "E1D5_16","E1D5_17",
  "E2D1_1","E2D1_10","E2D1_11","E2D1_2","E2D1_3","E2D1_4","E2D1_5","E2D1_6","E2D1_7","E2D1_8",
  "E2D2_13","E2D2_14",
  "E2D3_17","E2D3_18","E2D3_19","E2D3_20","E2D3_21",
  "E2D4_22","E2D4_23","E2D4_25","E2D4_27",
  "E2D5_29","E2D5_31",
  "E3D1_1","E3D1_10","E3D1_3","E3D1_4","E3D1_5","E3D1_8","E3D1_9",
  "E3D2_11","E3D2_18",
  "E3D3_23","E3D3_24","E3D3_25",
  "E3D4_26","E3D4_29",
  "E3D5_32","E3D5_36",
  "E4D1_1","E4D1_3",
  "E4D2_7","E4D2_8",
  "E4D3_11","E4D3_12","E4D3_4",
  "E4D4_21","E4D4_22",
  "E4D5_23","E4D5_25",
  "E5D1_1","E5D1_3","E5D1_4",
  "E5D2_5","E5D2_7",
  "E5D3_13","E5D3_15","E5D3_16","E5D3_18",
  "E5D4_20",
  "E5D5_21","E5D5_23",
  "E6D1_2","E6D1_3",
  "E6D2_10","E6D2_13","E6D2_14","E6D2_16","E6D2_4","E6D2_5","E6D2_6","E6D2_7",
  "E6D4_21","E6D4_23",
  "E6D5_25","E6D5_27"
)

# Apenas os descritores que realmente existem na base (evita erro caso falte algum)
descritores_exist <- intersect(descritores_sql, names(descritores_see_tecnico))

# Pivot (wide -> long), reproduzindo o UNPIVOT do SQL
descritores_long_tecnico_see <- descritores_see_tecnico %>%
  pivot_longer(
    cols      = all_of(descritores_exist),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  arrange(ATOR, SEE, SRE, descritor)


























