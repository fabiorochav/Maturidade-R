
#########################################################################
# Empilhamento
#########################################################################

# Acg
descritores_long
descritores_long_sre

# Dupla
descritores_long_see_dg
descritores_long_sre_dg

# Tenico SEE
descritores_long_tecnico_see

# Tecnico SRE
sre_descritores_long_see
sre_descritores_long


# Lista com os nomes EXATOS que você passou
obj_names <- c(
  # ACG
  "descritores_long", "descritores_long_sre",
  # Dupla Gestora
  "descritores_long_see_dg", "descritores_long_sre_dg",
  # Técnico SEE
  "descritores_long_tecnico_see",
  # Técnico SRE
  "sre_descritores_long_see", "sre_descritores_long"
)

# Filtra só os objetos que existem no ambiente
present <- obj_names[sapply(obj_names, exists)]
if (length(present) == 0) stop("Nenhuma das bases informadas foi encontrada no ambiente.")

# Função para padronizar o esquema e os tipos
norm_one <- function(df) {
  # Garante coluna SRE quando o nível é SEE (fica NA)
  if (!"SRE" %in% names(df)) df$SRE <- NA_character_
  # Tipos canônicos e ordem de colunas
  df %>%
    mutate(
      ATOR     = as.character(ATOR),
      SEE      = as.character(SEE),
      SRE      = as.character(SRE),
      descritor= as.character(descritor),
      valor    = as.numeric(valor)
    ) %>%
    select(ATOR, SEE, SRE, descritor, valor, everything())
}

# Carrega, normaliza e empilha (UNION ALL)
dfs_norm <- lapply(mget(present), norm_one)

all_atores_long <- bind_rows(dfs_norm) %>%
  # ordem canônica de colunas (se algum df tinha extras, eles vão para o final)
  select(ATOR, SEE, SRE, descritor, valor, everything())

# # ---- Checks rápidos ----
# # 1) Resumo por ator e presença de SRE (TRUE=tem SRE; FALSE=não tem)
# all_atores_long %>%
#   mutate(tem_SRE = !is.na(SRE)) %>%
#   count(ATOR, tem_SRE) %>%
#   arrange(ATOR, desc(tem_SRE)) %>%
#   print(n = 99)
# 
# # 2) Amostra de descritores-chave
# all_atores_long %>%
#   filter(descritor %in% c("A2D3_7","E2D4_25","E6D2_17")) %>%
#   arrange(ATOR, SEE, SRE, descritor) %>%
#   head(30) %>%
#   print(n = 30)


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# Inserção de descritor e etapa ação
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

# Entrada: sua base empilhada dos atores já em long
# (colunas: ATOR, SEE, SRE (opcional), descritor, valor)
empilhada <- all_atores_long

# Criar etapa_acao (A1..A4, E1..E6) e dimensao (D1..D5) a partir do 'descritor'
empilhada_com_grupos <- empilhada %>%
  mutate(
    etapa_acao = str_extract(descritor, "^[AE]\\d+"),
    dimensao   = str_extract(descritor, "D\\d+")
  )



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Calcula a média entre atores
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# Base de entrada: saída da etapa "2 insere descritor e etapa_acao"
entrada_media_atores <- empilhada_com_grupos

# Média entre atores (replica o AVG(valor) do SQL)
media_entre_atores <- entrada_media_atores %>%
  group_by(SEE, SRE, descritor, etapa_acao, dimensao) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  )

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# tira "d5" e SEE null
# =-=--=-=-=-=-=-=-=-=-=-=-=-=-=-
media_entre_atores = media_entre_atores %>%
  filter(!is.na(SEE), !is.na(etapa_acao), 
         dimensao != "D5")


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# média dos descritores - etapa/ação - dimensão
# =-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

et_ac_dim_g = media_entre_atores %>% 
  group_by(SRE, etapa_acao, dimensao) %>% 
  filter(SRE == "Agregado SEE") %>% 
  summarise(valor = mean(valor, na.rm=TRUE))

et_ac_dim_g = et_ac_dim_g %>% 
  pivot_wider(names_from = etapa_acao, values_from = valor) %>%
  # Reorganziar as colunas pra ficar igual ao compendio
  select(SRE, dimensao, E1, E2, E3, E4, E5, E6, A1, A2, A3, A4)




