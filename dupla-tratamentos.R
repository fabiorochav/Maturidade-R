
# Pacotes
library(tidyverse)
library(readxl)
library(rlang)
library(tidyr)


dupla <- read_excel("Dupla Gestora.xlsx")

# 1) Aplicar os dois filtros do SQL:
#    - SubmissionDate preenchido (não NA, não string vazia)
#    - KEY != 'uuid:'
dg_exclusao <- dupla %>%
  filter(!is.na(SubmissionDate) & SubmissionDate != "") %>%
  filter(KEY != "uuid:")

#=============================================
# Aplicação do de para
#=============================================

# 0/1 -> 0/1
rec_01 <- function(x) case_when(
  x == 0 ~ 0.00,
  x == 1 ~ 1.00,
  TRUE   ~ NA_real_
)

# Flags "_77" -> 0 somente quando for 0 ou 1; outros valores ficam NA (como no SQL)
rec_flag77_to_zero <- function(x) case_when(
  x %in% c(0, 1) ~ 0.00,
  TRUE           ~ NA_real_
)

# Likert 1..4 -> 0, .33, .66, 1
rec_1to4 <- function(x) case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.33,
  x == 3 ~ 0.66,
  x == 4 ~ 1.00,
  TRUE   ~ NA_real_
)

# Likert 1..4 com 77->0
rec_1to4_77zero <- function(x) case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.33,
  x == 3 ~ 0.66,
  x == 4 ~ 1.00,
  x == 77 ~ 0.00,
  TRUE    ~ NA_real_
)

# Likert 1..4 com 88->0
rec_1to4_88zero <- function(x) case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.33,
  x == 3 ~ 0.66,
  x == 4 ~ 1.00,
  x == 88 ~ 0.00,
  TRUE    ~ NA_real_
)

# Tri-nível 1/2/3 -> 0/0.5/1
rec_1to3_0_05_1 <- function(x) case_when(
  x == 1 ~ 0.00,
  x == 2 ~ 0.50,
  x == 3 ~ 1.00,
  TRUE   ~ NA_real_
)

# ---------------------------
# Listas de variáveis por padrão (do seu SQL DG)
# ---------------------------

# Famílias 0/1 -> 0/1
fam_01 <- c(
  paste0("V06_", c(1:10)),
  paste0("V07_", c(1:7)),
  paste0("V08_", c(1:7)),
  paste0("V11_", c(1:3)),
  paste0("V21_", c(1:5)),
  paste0("V23_", c(1:6)),
  paste0("V24_", c(1:7)),
  paste0("V26_", c(1:7)),
  paste0("V27_", c(1:6)),
  paste0("V39_", c(1:7)),
  paste0("V45_", c(1:17)),
  paste0("V46_", c(1:17)),
  paste0("V60_", c(1:5)),
  paste0("V63_", c(1:5)),
  paste0("V66_", c(1:5)),
  paste0("V68_", c(1:5))
)

# Flags "_77" -> 0
flags_77 <- c("V06_77","V11_77","V21_77","V24_77","V26_77","V27_77","V45_77",
              "V46_77","V60_77","V63_77","V66_77","V68_77")

# Likert "padrão" 1..4 -> 0..1 (sem 77/88)
likert_plain <- c("V10","V12","V14","V15","V16","V17","V18","V19","V20",
                  "V25","V28","V31","V32","V33","V34","V35","V36","V37",
                  "V41","V42","V57")

# Likert com 77->0
likert_77zero <- c("V43","V44","V47","V48","V50","V52","V53","V54","V55","V56","V64")

# Likert com 88->0
likert_88zero <- c("V59","V61","V69","V70","V71","V72")

# Tri 1/2/3 -> 0/.5/1
tri_1to3 <- c("V09","V13")

# ---------------------------
# Aplicar de-para DG
# ---------------------------

dg_depara <- dg_exclusao %>%
  # SEE_rotulo
  mutate(
    SEE_rotulo = case_when(
      SEE == 1 ~ "1. SEE Ceará",
      SEE == 2 ~ "2. SEE Espírito Santo",
      SEE == 3 ~ "3. SEE Goiás",
      SEE == 4 ~ "4. SEE Piauí",
      TRUE     ~ NA_character_
    )
  ) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Famílias 0/1 -> 0/1
  mutate(across(any_of(fam_01), rec_01, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Flags _77 -> 0
  mutate(across(any_of(flags_77), rec_flag77_to_zero, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Likert padrão 1..4
  mutate(across(any_of(likert_plain), rec_1to4, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Likert 1..4 com 77->0
  mutate(across(any_of(likert_77zero), rec_1to4_77zero, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Likert 1..4 com 88->0
  mutate(across(any_of(likert_88zero), rec_1to4_88zero, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Tri 1/2/3
  mutate(across(any_of(tri_1to3), rec_1to3_0_05_1, .names = "{.col}_ajustada")) %>%  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)
  
  # Casos especiais 1: V01, V03, V04, V05, V22, V29, V30, V49, V51, V58, V65, V67
  mutate(
    V01_ajustada = case_when(V01 == 1 ~ 1.00, V01 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V03_ajustada = case_when(V03 == 1 ~ 1.00, V03 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V04_ajustada = case_when(V04 == 1 ~ 1.00, V04 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V05_ajustada = case_when(V05 == 1 ~ 1.00, V05 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V22_ajustada = case_when(V22 == 1 ~ 1.00, V22 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V29_ajustada = case_when(V29 == 1 ~ 1.00, V29 == 2 ~ 0.00, V29 == 88 ~ 0.00,            TRUE ~ NA_real_), # 88 -> 0
    V30_ajustada = case_when(V30 == 1 ~ 1.00, V30 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V49_ajustada = case_when(V49 == 1 ~ 1.00, V49 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V51_ajustada = case_when(V51 == 1 ~ 1.00, V51 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V58_ajustada = case_when(V58 == 1 ~ 1.00, V58 == 2 ~ 0.00, V58 == 88 ~ 0.00,            TRUE ~ NA_real_), # 88 -> 0
    V65_ajustada = case_when(V65 == 1 ~ 1.00, V65 == 2 ~ 0.00,                              TRUE ~ NA_real_),
    V67_ajustada = case_when(V67 == 1 ~ 1.00, V67 == 2 ~ 0.00, V67 == 77 ~ 0.00,            TRUE ~ NA_real_)  # 77 -> 0
  ) %>%
  
  # Casos especiais 2: escalas com ordem diferente
  mutate(
    V38_ajustada = case_when(V38 == 1 ~ 0.33, V38 == 2 ~ 0.66, V38 == 3 ~ 1.00,             TRUE ~ NA_real_),
    V40_ajustada = case_when(V40 == 1 ~ 1.00, V40 == 2 ~ 0.33, V40 == 3 ~ 0.66, V40 == 4 ~ 0.00, TRUE ~ NA_real_),
    V62_ajustada = case_when(V62 == 1 ~ 1.00, V62 == 2 ~ 0.66, V62 == 3 ~ 0.33, V62 == 4 ~ 0.00, TRUE ~ NA_real_)
  )        # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20aplica%20depara.sql)


# ================================================
# calcula media por questoes multiplas
# ================================================

# 1) Identificar todas as colunas *_ajustada
ajustadas_cols <- grep("_ajustada$", names(dg_depara), value = TRUE)

# 2) Criar numeradores e denominadores
dg_nd <- dg_depara %>%
  mutate(
    # Numeradores: se não for NA, usa o valor; se NA, 0
    across(
      all_of(ajustadas_cols),
      ~ dplyr::coalesce(as.numeric(.x), 0),
      .names = "{.col}_numerador"
    ),
    
    # Denominadores: se não for NA → 1; se NA → 0
    across(
      all_of(ajustadas_cols),
      ~ ifelse(is.na(.x), 0, 1),
      .names = "{.col}_denominador"
    )
  )


# ---------------------------
# Função auxiliar: média segura
# Recebe df, um vetor de "bases" (sem os sufixos _ajustada_*), 
# e o nome da nova coluna (ex.: "V06_1_3_6_8_9_media").
# Calcula (∑numeradores)/(∑denominadores), devolvendo 0 se den==0.
add_media <- function(df, bases, new_name) {
  num_cols <- paste0(bases, "_ajustada_numerador")
  den_cols <- paste0(bases, "_ajustada_denominador")
  num_cols <- intersect(num_cols, names(df))
  den_cols <- intersect(den_cols, names(df))
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

# ---------------------------
# Mapeamento dos grupos (direto do SQL)
# ---------------------------
grupos <- list(
  # V06
  "V06_1_3_6_8_9_media" = c("V06_1","V06_3","V06_6","V06_8","V06_9"),
  "V06_5_media"         = "V06_5",
  "V06_7_media"         = "V06_7",
  
  # V08
  "V08_1_2_3_4_5_6_7_media" = paste0("V08_", 1:7),
  
  # V11
  "V11_1_2_3_media" = paste0("V11_", 1:3),
  
  # V21
  "V21_1_2_3_4_5_media" = paste0("V21_", 1:5),
  
  # V26
  "V26_1_2_3_4_5_6_7_media" = paste0("V26_", 1:7),
  
  # V27
  "V27_1_2_3_media" = paste0("V27_", 1:3),
  
  # V39
  "V39_1_2_3_4_5_6_7_media" = paste0("V39_", 1:7),
  
  # V45 (subgrupos definidos no SQL)
  "V45_1_3_media"       = c("V45_1","V45_3"),
  "V45_10_media"        = "V45_10",
  "V45_11_12_media"     = c("V45_11","V45_12"),
  "V45_13_14_15_media"  = c("V45_13","V45_14","V45_15"),
  "V45_2_media"         = "V45_2",
  "V45_4_media"         = "V45_4",  # no SQL ele repete, aqui mantemos 1x
  "V45_5_media"         = "V45_5",
  "V45_6_media"         = "V45_6",
  "V45_7_8_media"       = c("V45_7","V45_8"),  # no SQL ele repete, aqui 1x
  "V45_9_media"         = "V45_9",
  
  # V46
  "V46_1_3_media"       = c("V46_1","V46_3"),
  "V46_10_media"        = "V46_10",
  "V46_11_12_media"     = c("V46_11","V46_12"),
  "V46_13_14_15_media"  = c("V46_13","V46_14","V46_15"),
  "V46_16_media"        = "V46_16",
  "V46_2_media"         = "V46_2",
  "V46_4_media"         = "V46_4",  # no SQL ele repete, aqui 1x
  "V46_5_media"         = "V46_5",
  "V46_6_media"         = "V46_6",
  "V46_7_8_media"       = c("V46_7","V46_8"),  # no SQL ele repete, aqui 1x
  "V46_9_media"         = "V46_9"
)
# (Todos esses agrupamentos vêm do arquivo SQL desta etapa.)  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20media%20de%20questoes%20multiplas.sql)

# ---------------------------
# Aplicar os grupos no df
# ---------------------------
dg_medias <- dg_nd
for (nm in names(grupos)) {
  bases <- unique(grupos[[nm]])
  dg_medias <- add_media(dg_medias, bases, nm)
}

# ---------------------------
# Casos especiais (flags/indicadores) conforme o SQL
# ---------------------------

# V23_media: 1 se SOMA dos numeradores V23_1..6 != 0; caso contrário 0
v23_nums <- intersect(paste0("V23_", 1:6, "_ajustada_numerador"), names(dg_medias))
dg_medias <- dg_medias %>%
  rowwise() %>%
  mutate(
    V23_media = as.numeric(if (length(v23_nums) == 0) FALSE else
      sum(c_across(all_of(v23_nums)), na.rm = TRUE) != 0)
  ) %>% 
  ungroup()  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20media%20de%20questoes%20multiplas.sql)

# V24_media: 1 se V24_77_ajustada_numerador == 0; senão 0
dg_medias <- dg_medias %>%
  mutate(
    V24_media = as.numeric(ifelse(
      "V24_77_ajustada_numerador" %in% names(dg_medias) &
        !is.na(.data[["V24_77_ajustada_numerador"]]) &
        .data[["V24_77_ajustada_numerador"]] == 0,
      TRUE, FALSE
    ))
  )  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20media%20de%20questoes%20multiplas.sql)

# V60_media: 1 se V60_77_ajustada_numerador == 0; senão 0
# V63_media: 1 se V63_77_ajustada_numerador == 0; senão 0
dg_medias <- dg_medias %>%
  mutate(
    V60_media = as.numeric(ifelse(
      "V60_77_ajustada_numerador" %in% names(dg_medias) &
        !is.na(.data[["V60_77_ajustada_numerador"]]) &
        .data[["V60_77_ajustada_numerador"]] == 0,
      TRUE, FALSE
    )),
    V63_media = as.numeric(ifelse(
      "V63_77_ajustada_numerador" %in% names(dg_medias) &
        !is.na(.data[["V63_77_ajustada_numerador"]]) &
        .data[["V63_77_ajustada_numerador"]] == 0,
      TRUE, FALSE
    ))
  )  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20media%20de%20questoes%20multiplas.sql)


# 1) Mapeamento "coluna de entrada" -> "alias de saída" (igual ao seu SQL)
campos_dg_agregar <- c(
  "V01_ajustada" = "V01_ajustada",
  "V02_ajustada" = "V02_ajustada",
  "V03_ajustada" = "V03_ajustada",
  "V04_ajustada" = "V04_ajustada",
  "V05_ajustada" = "V05_ajustada",
  "V06_1_3_6_8_9_media" = "V06_1_3_6_8_9_ajustada",
  "V06_5_media"         = "V06_5_ajustada",
  "V06_7_media"         = "V06_7_ajustada",
  "V08_1_2_3_4_5_6_7_media" = "V08_1_2_3_4_5_6_7_ajustada",
  "V09_ajustada" = "V09_ajustada",
  "V10_ajustada" = "V10_ajustada",
  "V11_1_2_3_media" = "V11_1_2_3_ajustada",
  "V12_ajustada" = "V12_ajustada",
  "V13_ajustada" = "V13_ajustada",
  "V14_ajustada" = "V14_ajustada",
  "V15_ajustada" = "V15_ajustada",
  "V16_ajustada" = "V16_ajustada",
  "V17_ajustada" = "V17_ajustada",
  "V18_ajustada" = "V18_ajustada",
  "V19_ajustada" = "V19_ajustada",
  "V20_ajustada" = "V20_ajustada",
  "V21_1_2_3_4_5_media" = "V21_1_2_3_4_5_ajustada",
  "V22_ajustada" = "V22_ajustada",
  "V23_media"    = "V23_ajustada",
  "V24_media"    = "V24_ajustada",
  "V25_ajustada" = "V25_ajustada",
  "V26_1_2_3_4_5_6_7_media" = "V26_1_2_3_4_5_6_7_ajustada",
  "V27_1_2_3_media" = "V27_1_2_3_ajustada",
  "V28_ajustada" = "V28_ajustada",
  "V29_ajustada" = "V29_ajustada",
  "V30_ajustada" = "V30_ajustada",
  "V31_ajustada" = "V31_ajustada",
  "V32_ajustada" = "V32_ajustada",
  "V33_ajustada" = "V33_ajustada",
  "V34_ajustada" = "V34_ajustada",
  "V35_ajustada" = "V35_ajustada",
  "V36_ajustada" = "V36_ajustada",
  "V37_ajustada" = "V37_ajustada",
  "V38_ajustada" = "V38_ajustada",
  "V39_1_2_3_4_5_6_7_media" = "V39_1_2_3_4_5_6_7_ajustada",
  "V40_ajustada" = "V40_ajustada",
  "V41_ajustada" = "V41_ajustada",
  "V42_ajustada" = "V42_ajustada",
  "V44_ajustada" = "V44_ajustada",
  "V45_1_3_media"      = "V45_1_3_ajustada",
  "V45_10_media"       = "V45_10_ajustada",
  "V45_11_12_media"    = "V45_11_12_ajustada",
  "V45_13_14_15_media" = "V45_13_14_15_ajustada",
  "V45_2_media"        = "V45_2_ajustada",
  "V45_4_media"        = "V45_4_ajustada",
  "V45_5_media"        = "V45_5_ajustada",
  "V45_6_media"        = "V45_6_ajustada",
  "V45_7_8_media"      = "V45_7_8_ajustada",
  "V45_9_media"        = "V45_9_ajustada",
  "V46_1_3_media"      = "V46_1_3_ajustada",
  "V46_10_media"       = "V46_10_ajustada",
  "V46_11_12_media"    = "V46_11_12_ajustada",
  "V46_13_14_15_media" = "V46_13_14_15_ajustada",
  "V46_16_media"       = "V46_16_ajustada",
  "V46_2_media"        = "V46_2_ajustada",
  "V46_4_media"        = "V46_4_ajustada",
  "V46_5_media"        = "V46_5_ajustada",
  "V46_6_media"        = "V46_6_ajustada",
  "V46_7_8_media"      = "V46_7_8_ajustada",
  "V46_9_media"        = "V46_9_ajustada",
  "V47_ajustada" = "V47_ajustada",
  "V48_ajustada" = "V48_ajustada",
  "V49_ajustada" = "V49_ajustada",
  "V51_ajustada" = "V51_ajustada",
  "V52_ajustada" = "V52_ajustada",
  "V53_ajustada" = "V53_ajustada",
  "V55_ajustada" = "V55_ajustada",
  "V56_ajustada" = "V56_ajustada",
  "V58_ajustada" = "V58_ajustada",
  "V59_ajustada" = "V59_ajustada",
  "V60_media"    = "V60_ajustada",
  "V63_media"    = "V63_ajustada"
)  # (mapeamento retirado literalmente do seu SQL de agregação DG)  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/1%20agrega%20calculando%20item.sql)



# ============================================
# AGREGAÇÃO FINAL POR SEE  (terminar esta etapa)
# ============================================

cols_existentes <- intersect(names(campos_dg_agregar), names(dg_medias))

# 1) Agregação por SEE (AVG de todas as variáveis)
agregado_por_see_dg <- dg_medias %>%
  select(-SEE) %>%                  # ✅ remove SEE original
  mutate(ATOR = "4. Dupla Gestora") %>%
  rename(SEE = SEE_rotulo) %>%      # agora OK
  group_by(ATOR, SEE) %>%
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# 2) Renomeação SEGURA por posição (evita colisão com SEE/ATOR)
alias_vec <- unname(campos_dg_agregar[cols_existentes])

# Verificar se algum alias colide com ATOR ou SEE
if (length(intersect(alias_vec, c("ATOR", "SEE"))) > 0) {
  stop("Algum alias colide com ATOR/SEE — revise o mapeamento.")
}

# Confirmar que não há NA no mapeamento
if (any(is.na(alias_vec))) {
  faltantes <- cols_existentes[is.na(alias_vec)]
  stop(sprintf("Algumas colunas não têm alias: %s", paste(faltantes, collapse = ", ")))
}

# Aplicar renomeação por posição
posicoes <- match(cols_existentes, names(agregado_por_see_dg))
names(agregado_por_see_dg)[posicoes] <- alias_vec

# Garantir unicidade
stopifnot(anyDuplicated(names(agregado_por_see_dg)) == 0)

# ==============================================
# cria denominadores
# ==============================================

# 1) Identificar todas as colunas *_ajustada na base agregada por SEE
ajustadas_cols_see <- grep("_ajustada$", names(agregado_por_see_dg), value = TRUE)

# 2) Criar numeradores e denominadores conforme o SQL
agregado_por_see_nd <- agregado_por_see_dg %>%
  mutate(
    # Numeradores: se não for NA, usa o valor (coalesce para 0), exatamente como o CASE WHEN do SQL
    across(
      all_of(ajustadas_cols_see),
      ~ dplyr::coalesce(as.numeric(.x), 0),
      .names = "{.col}_numerador"
    ),
    # Denominadores: 1 quando não-NA; 0 quando NA (igual ao SQL)
    across(
      all_of(ajustadas_cols_see),
      ~ ifelse(is.na(.x), 0, 1),
      .names = "{.col}_denominador"
    )
  )
#================================================
# calculo descritor
#================================================

# 1) Função auxiliar: calcula um descritor como (∑num)/(∑den); 0 se den=0
add_descritor <- function(df, bases_ajustadas, new_name) {
  # bases_ajustadas são nomes TERMINANDO EM "_ajustada" (ex.: "V58_ajustada")
  num_cols <- paste0(bases_ajustadas, "_numerador")
  den_cols <- paste0(bases_ajustadas, "_denominador")
  
  num_cols <- intersect(num_cols, names(df))
  den_cols <- intersect(den_cols, names(df))
  
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

# 2) Mapeamento descritor -> bases (EXATAMENTE o que o seu SQL usa)
#    (Aqui as bases já vêm com o sufixo "_ajustada" porque estamos no agregado por SEE.)
descritores_dg_see <- list(
  # A2*
  A2D3_6  = c("V58_ajustada"),
  A2D3_7  = c("V59_ajustada"),
  
  # A3*
  A3D1_1  = c("V60_ajustada"),
  A3D1_2  = c("V63_ajustada"),
  A3D2_4  = c("V05_ajustada"),
  A3D4_9  = c("V46_16_ajustada","V56_ajustada"),
  
  # A4*
  A4D1_3  = c("V09_ajustada"),
  A4D1_4  = c("V17_ajustada"),
  A4D1_7  = c("V11_1_2_3_ajustada","V12_ajustada","V13_ajustada","V14_ajustada"),
  A4D2_9  = c("V10_ajustada","V15_ajustada","V16_ajustada","V20_ajustada"),
  A4D3_11 = c("V28_ajustada"),
  A4D4_19 = c("V45_4_ajustada","V46_4_ajustada"),
  A4D4_21 = c("V45_7_8_ajustada","V46_7_8_ajustada"),
  
  # E1*
  E1D1_5  = c("V01_ajustada"),
  E1D2_7  = c("V02_ajustada"),
  E1D2_8  = c("V06_5_ajustada"),
  
  # E2*
  E2D1_12 = c("V06_1_3_6_8_9_ajustada"),
  E2D1_3  = c("V03_ajustada"),
  E2D1_6  = c("V09_ajustada"),
  E2D1_9  = c("V06_7_ajustada"),
  E2D2_15 = c("V08_1_2_3_4_5_6_7_ajustada"),
  E2D4_22 = c("V04_ajustada"),
  E2D4_24 = c("V44_ajustada","V45_1_3_ajustada","V46_1_3_ajustada"),
  E2D4_26 = c("V47_ajustada","V48_ajustada"),
  
  # E3*
  E3D1_10 = c("V17_ajustada"),
  E3D1_2  = c("V18_ajustada"),
  E3D1_6  = c("V21_1_2_3_4_5_ajustada","V22_ajustada","V23_ajustada"),
  E3D1_9  = c("V11_1_2_3_ajustada","V12_ajustada","V13_ajustada","V14_ajustada"),
  E3D2_13 = c("V19_ajustada"),
  E3D2_20 = c("V24_ajustada"),
  E3D2_21 = c("V49_ajustada"),
  E3D4_27 = c("V45_5_ajustada","V46_5_ajustada"),
  E3D4_28 = c("V45_2_ajustada","V46_2_ajustada"),
  E3D4_30 = c("V45_4_ajustada","V46_4_ajustada"),
  E3D4_31 = c("V45_6_ajustada","V46_6_ajustada"),
  
  # E4*
  E4D1_1  = c("V26_1_2_3_4_5_6_7_ajustada"),
  E4D2_6  = c("V52_ajustada"),
  E4D2_9  = c("V27_1_2_3_ajustada"),
  E4D3_15 = c("V28_ajustada"),
  E4D4_18 = c("V45_10_ajustada","V46_10_ajustada","V51_ajustada"),
  E4D4_19 = c("V45_7_8_ajustada","V46_7_8_ajustada"),
  E4D4_20 = c("V45_9_ajustada","V46_9_ajustada","V53_ajustada"),
  
  # E5*
  E5D1_4  = c("V31_ajustada"),
  E5D2_11 = c("V30_ajustada"),
  E5D2_6  = c("V32_ajustada"),
  E5D2_9  = c("V35_ajustada"),
  E5D3_14 = c("V33_ajustada","V34_ajustada"),
  E5D3_15 = c("V29_ajustada"),
  E5D4_19 = c("V45_11_12_ajustada","V46_11_12_ajustada"),
  
  # E6*
  E6D1_1  = c("V36_ajustada","V37_ajustada"),
  E6D2_12 = c("V40_ajustada"),
  E6D2_15 = c("V41_ajustada","V42_ajustada"),
  E6D2_4  = c("V38_ajustada"),
  E6D2_9  = c("V39_1_2_3_4_5_6_7_ajustada"),
  E6D4_22 = c("V45_13_14_15_ajustada","V46_13_14_15_ajustada","V55_ajustada")
)
# (todas as combinações acima foram retiradas do seu arquivo "3 calcula descritor.sql")  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20descritor.sql)

# 3) Aplicar aos dados SEE agregados com num/den criados
descritores_por_see_dg <- agregado_por_see_nd %>%
  mutate(SRE = "Agregado SEE")  # como no SQL

for (nm in names(descritores_dg_see)) {
  bases <- unique(descritores_dg_see[[nm]])
  descritores_por_see_dg <- add_descritor(descritores_por_see_dg, bases, nm)
}

# ======================================
# pivotagem
# ======================================

descritor_cols_sql <- c(
  "A2D3_6","A2D3_7","A3D1_1","A3D1_2","A3D2_4","A3D4_9",
  "A4D1_3","A4D1_4","A4D1_7","A4D2_9","A4D3_11","A4D4_19","A4D4_21",
  "E1D1_5","E1D2_7","E1D2_8","E2D1_12","E2D1_3","E2D1_6","E2D1_9",
  "E2D2_15","E2D4_22","E2D4_24","E2D4_26",
  "E3D1_10","E3D1_2","E3D1_6","E3D1_9","E3D2_13","E3D2_20","E3D2_21",
  "E3D4_27","E3D4_28","E3D4_30","E3D4_31",
  "E4D1_1","E4D2_6","E4D2_9","E4D3_15","E4D4_18","E4D4_19","E4D4_20",
  "E5D1_4","E5D2_11","E5D2_6","E5D2_9","E5D3_14","E5D3_15","E5D4_19",
  "E6D1_1","E6D2_12","E6D2_15","E6D2_4","E6D2_9","E6D4_22"
)

# Usa só as colunas que de fato existem (robustez)
descritor_cols_exist <- intersect(descritor_cols_sql, names(descritores_por_see_dg))

descritores_long_see_dg <- descritores_por_see_dg %>%
  pivot_longer(
    cols      = all_of(descritor_cols_exist),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  mutate(valor = as.numeric(valor)) %>%
  arrange(ATOR, SEE, SRE, descritor)

# ======================================
# ator sre see
# ======================================

# ----------------------------
# 0) Escolher a coluna de SRE
# ----------------------------
# O SQL usa SRE_corresp AS SRE (prioridade).
chave_sre <- if ("SRE_corresp" %in% names(dg_medias)) {
  "SRE_corresp"
} else if ("SRE" %in% names(dg_medias)) {
  "SRE"
} else if ("sre" %in% names(dg_medias)) {
  "sre"
} else {
  stop("Não encontrei a coluna de SRE. Esperado: 'SRE_corresp' (ou 'SRE'/'sre').")
}

# ----------------------------
# 1) Quais colunas vamos agregar?
#     (as mesmas do nível SEE; entradas podem ser *_ajustada ou *_media)
# ----------------------------
cols_existentes <- intersect(names(campos_dg_agregar), names(dg_medias))

# ----------------------------
# 2) Agregar por ATOR, SEE, SRE
# ----------------------------
agregado_por_sre_dg <- dg_medias %>%
  mutate(ATOR = "4. Dupla Gestora") %>%   # constante, como no SQL
  # Evitar colisão com um SEE ou SRE já presentes
  select(-any_of(c("SEE", "SRE"))) %>%    
  rename(SEE = SEE_rotulo) %>%
  group_by(ATOR, SEE, across(all_of(chave_sre))) %>%
  summarise(
    across(all_of(cols_existentes), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  )

# ----------------------------
# 3) Padronizar o nome da coluna SRE (se necessário)
# ----------------------------
# Renomeia a coluna-chave usada (se não for "SRE") para "SRE"
if (chave_sre != "SRE") {
  names(agregado_por_sre_dg)[names(agregado_por_sre_dg) == chave_sre] <- "SRE"
}

# ----------------------------
# 4) Renomeação SEGURA por posição (entrada -> alias do SQL)
# ----------------------------
alias_vec <- unname(campos_dg_agregar[cols_existentes])

# Sanity checks de renomeação
if (length(intersect(alias_vec, c("ATOR","SEE","SRE"))) > 0) {
  stop("Algum alias colide com ATOR/SEE/SRE. Revise o mapeamento.")
}
if (any(is.na(alias_vec))) {
  faltantes <- cols_existentes[is.na(alias_vec)]
  stop(sprintf("Há colunas sem alias no mapeamento: %s", paste(faltantes, collapse=", ")))
}

posicoes <- match(cols_existentes, names(agregado_por_sre_dg))
names(agregado_por_sre_dg)[posicoes] <- alias_vec

# Garantir unicidade dos nomes
stopifnot(anyDuplicated(names(agregado_por_sre_dg)) == 0)

#=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# cria numeradores e denominadores
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# 1) Todas as colunas *_ajustada no agregado por SRE
ajustadas_cols_sre <- grep("_ajustada$", names(agregado_por_sre_dg), value = TRUE)

# 2) Criar numeradores e denominadores (regra do SQL)
agregado_por_sre_nd <- agregado_por_sre_dg %>%
  mutate(
    # Numeradores: valor quando não-NA; NA -> 0
    across(
      all_of(ajustadas_cols_sre),
      ~ dplyr::coalesce(as.numeric(.x), 0),
      .names = "{.col}_numerador"
    ),
    # Denominadores: 1 quando não-NA; NA -> 0
    across(
      all_of(ajustadas_cols_sre),
      ~ ifelse(is.na(.x), 0, 1),
      .names = "{.col}_denominador"
    )
  )

#=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# calcula descritor
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# 1) Função: (∑ num) / (∑ den); 0 se den=0
add_descritor <- function(df, bases_ajustadas, new_name) {
  # bases_ajustadas devem vir com "_ajustada" (ex.: "V58_ajustada")
  num_cols <- paste0(bases_ajustadas, "_numerador")
  den_cols <- paste0(bases_ajustadas, "_denominador")
  
  num_cols <- intersect(num_cols, names(df))
  den_cols <- intersect(den_cols, names(df))
  
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

# 2) Mapeamento descritor -> itens (copiado do seu SQL "3 calcula descritor.sql")
descritores_dg_sre <- list(
  # A2*
  A2D3_6  = c("V58_ajustada"),
  A2D3_7  = c("V59_ajustada"),
  
  # A3*
  A3D1_1  = c("V60_ajustada"),
  A3D1_2  = c("V63_ajustada"),
  A3D2_4  = c("V05_ajustada"),
  A3D4_9  = c("V46_16_ajustada","V56_ajustada"),
  
  # A4*
  A4D1_3  = c("V09_ajustada"),
  A4D1_4  = c("V17_ajustada"),
  A4D1_7  = c("V11_1_2_3_ajustada","V12_ajustada","V13_ajustada","V14_ajustada"),
  A4D2_9  = c("V10_ajustada","V15_ajustada","V16_ajustada","V20_ajustada"),
  A4D3_11 = c("V28_ajustada"),
  A4D4_19 = c("V45_4_ajustada","V46_4_ajustada"),
  A4D4_21 = c("V45_7_8_ajustada","V46_7_8_ajustada"),
  
  # E1*
  E1D1_5  = c("V01_ajustada"),
  E1D2_7  = c("V02_ajustada"),
  E1D2_8  = c("V06_5_ajustada"),
  
  # E2*
  E2D1_12 = c("V06_1_3_6_8_9_ajustada"),
  E2D1_3  = c("V03_ajustada"),
  E2D1_6  = c("V09_ajustada"),
  E2D1_9  = c("V06_7_ajustada"),
  E2D2_15 = c("V08_1_2_3_4_5_6_7_ajustada"),
  E2D4_22 = c("V04_ajustada"),
  E2D4_24 = c("V44_ajustada","V45_1_3_ajustada","V46_1_3_ajustada"),
  E2D4_26 = c("V47_ajustada","V48_ajustada"),
  
  # E3*
  E3D1_10 = c("V17_ajustada"),
  E3D1_2  = c("V18_ajustada"),
  E3D1_6  = c("V21_1_2_3_4_5_ajustada","V22_ajustada","V23_ajustada"),
  E3D1_9  = c("V11_1_2_3_ajustada","V12_ajustada","V13_ajustada","V14_ajustada"),
  E3D2_13 = c("V19_ajustada"),
  E3D2_20 = c("V24_ajustada"),
  E3D2_21 = c("V49_ajustada"),
  E3D4_27 = c("V45_5_ajustada","V46_5_ajustada"),
  E3D4_28 = c("V45_2_ajustada","V46_2_ajustada"),
  E3D4_30 = c("V45_4_ajustada","V46_4_ajustada"),
  E3D4_31 = c("V45_6_ajustada","V46_6_ajustada"),
  
  # E4*
  E4D1_1  = c("V26_1_2_3_4_5_6_7_ajustada"),
  E4D2_6  = c("V52_ajustada"),
  E4D2_9  = c("V27_1_2_3_ajustada"),
  E4D3_15 = c("V28_ajustada"),
  E4D4_18 = c("V45_10_ajustada","V46_10_ajustada","V51_ajustada"),
  E4D4_19 = c("V45_7_8_ajustada","V46_7_8_ajustada"),
  E4D4_20 = c("V45_9_ajustada","V46_9_ajustada","V53_ajustada"),
  
  # E5*
  E5D1_4  = c("V31_ajustada"),
  E5D2_11 = c("V30_ajustada"),
  E5D2_6  = c("V32_ajustada"),
  E5D2_9  = c("V35_ajustada"),
  E5D3_14 = c("V33_ajustada","V34_ajustada"),
  E5D3_15 = c("V29_ajustada"),
  E5D4_19 = c("V45_11_12_ajustada","V46_11_12_ajustada"),
  
  # E6*
  E6D1_1  = c("V36_ajustada","V37_ajustada"),
  E6D2_12 = c("V40_ajustada"),
  E6D2_15 = c("V41_ajustada","V42_ajustada"),
  E6D2_4  = c("V38_ajustada"),
  E6D2_9  = c("V39_1_2_3_4_5_6_7_ajustada"),
  E6D4_22 = c("V45_13_14_15_ajustada","V46_13_14_15_ajustada","V55_ajustada")
)
# (combinações copiadas do seu arquivo DG "3 calcula descritor.sql")  # [1](https://sgpiu-my.sharepoint.com/personal/fabio_vianna_institutounibanco_org_br/Documents/Arquivos%20de%20Microsoft%20Copilot%20Chat/3%20calcula%20descritor.sql)

# 3) Aplicar aos dados SRE (não fixamos SRE; mantemos o valor real da coluna SRE)
descritores_por_sre_dg <- agregado_por_sre_nd

for (nm in names(descritores_dg_sre)) {
  bases <- unique(descritores_dg_sre[[nm]])
  descritores_por_sre_dg <- add_descritor(descritores_por_sre_dg, bases, nm)
}

# Resultado: ATOR, SEE, SRE e todos os descritores A* e E* calculados no nível SRE

#=-=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# pivotagem
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

descritor_cols_sql <- c(
  "A2D3_6","A2D3_7","A3D1_1","A3D1_2","A3D2_4","A3D4_9",
  "A4D1_3","A4D1_4","A4D1_7","A4D2_9","A4D3_11","A4D4_19","A4D4_21",
  "E1D1_5","E1D2_7","E1D2_8","E2D1_12","E2D1_3","E2D1_6","E2D1_9",
  "E2D2_15","E2D4_22","E2D4_24","E2D4_26",
  "E3D1_10","E3D1_2","E3D1_6","E3D1_9","E3D2_13","E3D2_20","E3D2_21",
  "E3D4_27","E3D4_28","E3D4_30","E3D4_31",
  "E4D1_1","E4D2_6","E4D2_9","E4D3_15","E4D4_18","E4D4_19","E4D4_20",
  "E5D1_4","E5D2_11","E5D2_6","E5D2_9","E5D3_14","E5D3_15","E5D4_19",
  "E6D1_1","E6D2_12","E6D2_15","E6D2_4","E6D2_9","E6D4_22"
)

descritor_cols_exist <- intersect(descritor_cols_sql, names(descritores_por_sre_dg))

descritores_long_sre_dg <- descritores_por_sre_dg %>%
  pivot_longer(
    cols = all_of(descritor_cols_exist),
    names_to  = "descritor",
    values_to = "valor"
  ) %>%
  mutate(valor = as.numeric(valor)) %>%
  arrange(ATOR, SEE, SRE, descritor)


























