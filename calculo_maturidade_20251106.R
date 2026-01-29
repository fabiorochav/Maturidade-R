
# Pacotes
library(tidyverse)
library(readxl)

# Carregamento dos dicionarios -------------------------------------------------
acg <- read_excel("ACG.xlsx")
dupla <- read_excel("Dupla Gestora.xlsx")
tec.see <- read_excel("Tecnico SEE.xlsx")
tec.sre <- read_excel("Tecnico SRE.xlsx")
# ------------------------------------------------------------------------------

# Carregamento dos dados -------------------------------------------------------
dic.acg  <- read_excel("20231109 Versão Final Questionário do ACG.xlsx", sheet = "Questionário")
dic.dupla <- read_excel("20230926 Versão Final_Questionário da Dupla Gestora.xlsx", sheet = "Questionário")
dic.tec.see <- read_excel("20231025 Versão Final_Questionário do Técnico da SEE.xlsx", sheet = "Questionário")
dic.tec.sre<- read_excel("20231107 Versão Final_Questionário do Técnico da SRE.xlsx", sheet = "Questionário")
# ------------------------------------------------------------------------------

# Carregamento De para ---------------------------------------------------------
depara.dupla <- read_excel("20231207 Depara_Dupla Gestora.xlsx") %>% 
  select(variavel, valor_item, pontuacao)

depara.acg <- read_excel("20231208 Depara_ACG.xlsx")%>% 
  select(variavel, valor_item, pontuacao)%>% 
  # Ajusta completar dados faltantes com fill
  fill(variavel)

depara.tec.see <- read_excel("20231201 Depara_SEE_revisado20240111.xlsx")%>% 
  select(variavel, valor_item, pontuacao)

depara.tec.sre <- read_excel("20231208 Depara_SRE.xlsx")%>% 
  select(variavel, valor_item, pontuacao) %>% 
  # Ajusta completar dados faltantes com fill
  fill(variavel)

# Padronizar as questões binárias para uma escala 0-1
# Padronizar as questões likert (4 pontos) para uma escala 0-1, onde 1=0; 2=0,33; 3=0,66; 4=1
# Quando a questão for múltipla, há duas possibilidades: tratar cada item como uma questão (pontuando 1 ou 0 para cada item) ou tratar a marcação de qualquer item (exceto 77) como pontuação total
# Os itens 77 - "Prefiro não responder (Não sei)" serão pontuados como 0, exceto para o ator Técnico da SEE


# ---------------------------------------------------------
# 0) De/para: padronizar, colapsar N2/N3 e garantir unicidade
# ---------------------------------------------------------
# Se houver colunas de descrição no depara e você quiser preservá-las,
# adicione seus nomes em desc_cols abaixo (opcional).
desc_cols <- intersect(names(depara.tec.see), c("rotulo","texto","opcao","descricao","label"))

depara_clean <- depara.tec.see %>%
  select(variavel, valor_item, pontuacao, all_of(desc_cols)) %>%
  mutate(
    variavel   = as.character(variavel),
    valor_item = suppressWarnings(as.numeric(valor_item)),
    pontuacao  = suppressWarnings(as.numeric(pontuacao))
  ) %>%
  filter(!is.na(variavel)) %>%
  arrange(variavel, valor_item) %>%
  # 🔽 Colapsa duplicatas (ex.: V40_11 e V41_11 com N2/N3) mantendo uma única pontuação por chave
  group_by(variavel, valor_item) %>%
  summarise(
    pontuacao = {
      p <- unique(na.omit(pontuacao))
      if (length(p) == 0) NA_real_ else p[1]    # mesma pontuação para N2/N3 → fica uma só
    },
    .groups = "drop"
  )

# Sanidade: garantir que não há duplicatas
stopifnot(!anyDuplicated(depara_clean[c("variavel", "valor_item")]))

# ---------------------------------------------------------
# 1) Detectar variáveis de itens com prefixo "V"
#    (apenas as que aparecem no de/para serão pontuadas)
# ---------------------------------------------------------
vars_depara <- unique(depara_clean$variavel)
vars_itens  <- names(tec.see)[str_detect(names(tec.see), "^V")]
vars_usar   <- intersect(vars_itens, vars_depara)

if (length(vars_usar) == 0) {
  stop("Nenhuma variável de item encontrada em tec.see que conste no depara. Verifique nomes.")
}

# ---------------------------------------------------------
# 2) Forçar tipo CHAR nas variáveis de item usadas
# ---------------------------------------------------------
tec.see.char <- tec.see %>%
  mutate(across(all_of(vars_usar), as.character))

# ---------------------------------------------------------
# 3) Pivotar para formato longo (somente variáveis mapeáveis)
# ---------------------------------------------------------
tec_long <- tec.see.char %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(
    cols = all_of(vars_usar),
    names_to = "variavel",
    values_to = "valor_item_chr"
  )

# ---------------------------------------------------------
# 4) Converter para numérico quando possível
#    (múltipla escolha original "1 2 3" vira NA e é ignorada;
#     dummies V40_11, V41_11 etc. (0/1) seguem normalmente)
# ---------------------------------------------------------
tec_long <- tec_long %>%
  mutate(
    valor_item_num = suppressWarnings(as.numeric(valor_item_chr))
  )

# ---------------------------------------------------------
# 5) Join com o de/para usando o valor numérico (many-to-one)
# ---------------------------------------------------------
tec_score_joined <- tec_long %>%
  left_join(
    depara_clean,
    by = c("variavel" = "variavel", "valor_item_num" = "valor_item"),
    relationship = "many-to-one"  # agora seguro
  )

# ---------------------------------------------------------
# 6) Regras para a base tec.see:
#    - 77 -> NA na pontuação (não entra no cálculo), sem excluir respondente
# ---------------------------------------------------------
tec_score_joined <- tec_score_joined %>%
  mutate(
    pontuacao = ifelse(valor_item_num == 77, NA_real_, pontuacao)
  )

# ---------------------------------------------------------
# 7) Agregar para garantir UM valor por (row_id, variavel)
#    (evita list-columns no pivot_wider)
# ---------------------------------------------------------
tec_score_long <- tec_score_joined %>%
  mutate(pontuacao = suppressWarnings(as.numeric(pontuacao))) %>%
  group_by(row_id, variavel) %>%
  summarise(
    pontuacao = {
      vals <- unique(na.omit(pontuacao))
      if (length(vals) == 0) NA_real_ else vals[1]  # primeira não-NA
    },
    .groups = "drop"
  )

# ---------------------------------------------------------
# 8) Voltar ao formato wide: criar colunas _score
# ---------------------------------------------------------
tec_score_wide <- tec_score_long %>%
  mutate(col_score = paste0(variavel, "_score")) %>%
  select(row_id, col_score, pontuacao) %>%
  pivot_wider(names_from = col_score, values_from = pontuacao)

# ---------------------------------------------------------
# 9) Juntar com a base original (sem sobrescrever)
# ---------------------------------------------------------
tec.see.score <- tec.see %>%
  mutate(row_id = row_number()) %>%
  left_join(tec_score_wide, by = "row_id") %>%
  select(-row_id)

# ---------------------------------------------------------
# 10) Checagens rápidas (opcional)
# ---------------------------------------------------------
vars_score <- grep("_score$", names(tec.see.score), value = TRUE)
message("Total de variáveis pontuadas: ", length(vars_score))

# Itens com muitos NA (pode indicar múltipla escolha original ou ausência no de/para)
na_por_var <- sapply(tec.see.score[vars_score], function(x) sum(is.na(x)))
na_por_var <- sort(na_por_var, decreasing = TRUE)
print(na_por_var[1:min(10, length(na_por_var))])

# Conferência específica das dummies polêmicas
if ("V40_11_score" %in% names(tec.see.score)) {
  print(table(tec.see.score$V40_11_score, useNA = "ifany"))
}



