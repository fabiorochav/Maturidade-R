
#########################################################################
# Empilhamento
#########################################################################

#Pacote necessário
library(tidyverse)
library(readxl)

# Dupla --------------------------------------------------
dupla = readRDS("final.descritores.dupla.R")

# Tratamentos complementares antes do empilhamento
dupla = dupla %>% 
  drop_na(SEE)

# Desconsiderar as ações avaliadas pela dimensão "D4. Apoio entre instâncias", por isso:
# Aplicar filtro que exclua as ações (variável: cod_etapa_acao) 
# "A1, A2, A3, A4" relacionadas a dimensão (variável: cod_dimensao) "D4".

dupla = dupla %>% 
  mutate(
    etapa_acao = str_extract(descritor, "^[AE][0-9]"),
    dimensao   = str_extract(descritor, "D[1-5]")
  ) %>% 
  filter(!(dimensao == "D4" & etapa_acao %in% c("A1","A2","A3","A4")),
         !dimensao %in% "D5") #REMOVER A DIMENSAO 5 - CUSTOMISAÇÃO

# Acg -----------------------------------------------------
acg = readRDS("final.descritores.acg.R")

# Tratamentos complementares antes do empilhamento
acg = acg %>% 
  drop_na(SEE)

acg = acg %>% 
  mutate(
    etapa_acao = str_extract(descritor, "^[AE][0-9]"),
    dimensao   = str_extract(descritor, "D[1-5]")
  ) %>% 
  filter(!(dimensao == "D4" & etapa_acao %in% c("A1","A2","A3","A4")),
         !dimensao %in% "D5") #REMOVER A DIMENSAO 5 - CUSTOMISAÇÃO


# Tenico SEE
tec.sre = readRDS("final.descritores.sre.R")

tec.sre = tec.sre %>% 
  drop_na(SEE)

tec.sre = tec.sre %>% 
  mutate(
    etapa_acao = str_extract(descritor, "^[AE][0-9]"),
    dimensao   = str_extract(descritor, "D[1-5]")
  ) %>% 
  filter(!(dimensao == "D4" & etapa_acao %in% c("A1","A2","A3","A4")),
         !dimensao %in% "D5") #REMOVER A DIMENSAO 5 - CUSTOMISAÇÃO


# Tecnico SRE
tec.see = readRDS("final.descritores.see.R")

tec.see = tec.see %>% 
  drop_na(SEE)

tec.see = tec.see %>% 
  mutate(
    etapa_acao = str_extract(descritor, "^[AE][0-9]"),
    dimensao   = str_extract(descritor, "D[1-5]")
  ) %>% 
  filter(!(dimensao == "D4" & etapa_acao %in% c("A1","A2","A3","A4")),
         !dimensao %in% "D5") #REMOVER A DIMENSAO 5 - CUSTOMISAÇÃO

# MonitoraSEE# Monitoramento SRE
monit.sre = readRDS("monitoramento.sre.R")

monit.sre = monit.sre %>% 
  mutate(
    etapa_acao = str_extract(descritor, "^[AE][0-9]"),
    dimensao   = str_extract(descritor, "D[1-5]")
  ) %>% 
  filter(!(dimensao == "D4" & etapa_acao %in% c("A1","A2","A3","A4")),
         !dimensao %in% "D5") #REMOVER A DIMENSAO 5 - CUSTOMISAÇÃO

# Monitoramento SEE
monit.see = readRDS("monitoramento.see.R")

monit.see = monit.see %>% 
  mutate(
    etapa_acao = str_extract(descritor, "^[AE][0-9]"),
    dimensao   = str_extract(descritor, "D[1-5]")
  ) %>% 
  filter(!(dimensao == "D4" & etapa_acao %in% c("A1","A2","A3","A4")),
         !dimensao %in% "D5") #REMOVER A DIMENSAO 5 - CUSTOMISAÇÃO

# Empilhamento filnal
base_final <- bind_rows(
  tec.see,
  tec.sre,
  monit.see,
  monit.sre,
  acg,
  dupla
)

# passo - Triangulação dos atores: o primeiro passo é agregar a base no nível dos 
# descritores calculando a média entre eles (triangulação), para as redes e regionais. 
# A base "1_agregado_nivel_descritor" apresenta os resultados para este passo.

agreg.descritor = base_final %>% 
  group_by(SEE, SRE, descritor) %>% 
  summarise(valor = mean(valor, na.rm=TRUE), .groups = "drop") %>% 
  rename(see = SEE, 
         sre = SRE) %>% 
  mutate(
    sre = str_replace_all(sre, "-", " "),
    sre = str_squish(sre),
    sre = str_to_upper(sre))

agreg.descritor.oppen = read_excel("1_agregado_nivel_descritor.xlsx")

agreg.descritor.oppen = agreg.descritor.oppen %>% 
  mutate(
  sre = str_replace_all(sre, "-", ""),
  sre = str_squish(sre),
  sre = str_to_upper(sre),
  sre = ifelse(sre == "CREDE 03 ARACAÚ", "CREDE 03 ACARAÚ", sre))

# Chegcagem com base Oppen
comparacao.nivel.descritor <- agreg.descritor %>% 
  rename(valor_iu = valor) %>% 
  full_join(
    agreg.descritor.oppen %>%
      select(see, sre, cod_descritor, valor) %>%
      rename(
        descritor = cod_descritor,
        valor_oppen = valor
      ),
    by = c("see","sre","descritor")
  )

# passo - Agregação - etapa e ação e dimensão: 
# o segundo passo consiste em agregar a base no nível das etapas, 
# ações disparadoras e dimensões críticas, por meio do cálculo da média 
# dos descritores, para as redes e regionais. 
# A base "2_agregado_nivel_etapa_acao_dimensao" apresenta os resultados para este passo.

agreg.descritor = agreg.descritor %>% 
mutate(
  etapa_acao = str_extract(descritor, "^[AE][0-9]"),
  dimensao   = str_extract(descritor, "D[1-5]")
)

agreg.et.ac.di = agreg.descritor %>% 
  group_by(see, sre, etapa_acao, dimensao) %>% 
  summarise(valor = mean(valor, na.rm=TRUE), .groups = "drop") %>% 
  mutate(
    sre = str_replace_all(sre, "-", " "),
    sre = str_squish(sre),
    sre = str_to_upper(sre))

agreg.et.ac.di.oppen = read_excel("2_agregado_nivel_etapa_acao_dimensao.xlsx")

agreg.et.ac.di.oppen = agreg.et.ac.di.oppen %>% 
  mutate(
  sre = str_replace_all(sre, "-", ""),
  sre = str_squish(sre),
  sre = str_to_upper(sre),
  sre = ifelse(sre == "CREDE 03 ARACAÚ", "CREDE 03 ACARAÚ", sre))

# Chegcagem com base Oppen
comparacao.nivel.et.ac.di <- agreg.et.ac.di %>% 
  rename(valor_iu = valor) %>% 
  full_join(
    agreg.et.ac.di.oppen %>%
      select(see, sre, cod_etapa_acao, cod_dimensao, valor) %>%
      rename(
        etapa_acao = cod_etapa_acao,
        dimensao = cod_dimensao,
        valor_oppen = valor
      ),
    by = c("see","sre","etapa_acao", "dimensao")
  )

# passo - Agregação - etapa e ação: o terceiro passo consiste em agregar a base 
# no nível das etapas e das ações, por meio do cálculo da média das dimensões, 
# para as redes e regionais. A base "3_agregado_nivel_etapa_acao" apresenta 
# os resultados para este passo.

agreg.et.ac = agreg.et.ac.di %>% 
  group_by(see, sre, etapa_acao) %>% 
  summarise(valor = mean(valor, na.rm=TRUE), .groups = "drop") %>% 
  mutate(
  sre = str_replace_all(sre, "-", " "),
  sre = str_squish(sre),
  sre = str_to_upper(sre))

agreg.et.ac.oppen = read_excel("3_agregado_nivel_etapa_acao-v2.xlsx")

agreg.et.ac.oppen = agreg.et.ac.oppen %>% 
  mutate(
    sre = str_replace_all(sre, "-", ""),
    sre = str_squish(sre),
    sre = str_to_upper(sre),
    sre = ifelse(sre == "CREDE 03 ARACAÚ", "CREDE 03 ACARAÚ", sre))

# Chegcagem com base Oppen
comparacao.nivel.et.ac <- agreg.et.ac %>% 
  rename(valor_iu = valor) %>% 
  full_join(
    agreg.et.ac.oppen %>%
      select(see, sre, cod_etapa_acao, valor) %>%
      rename(
        etapa_acao = cod_etapa_acao,
        valor_oppen = valor
      ),
    by = c("see","sre","etapa_acao")
  )

# passo - Agregação - resultado final: o quarto e último passo consiste em agregar 
# a base no nível geral, sem discriminação por etapa, ação ou dimensão, por meio 
# do cálculo da média das etapas e ações, para as redes e regionais. 
# A base "4_agregado_nivel_resultado_final" apresenta os resultados para este passo.

agreg.final = agreg.et.ac %>% 
  group_by(see, sre) %>% 
  summarise(valor = mean(valor, na.rm=TRUE), .groups = "drop") %>% 
  mutate(
  sre = str_replace_all(sre, "-", " "),
  sre = str_squish(sre),
  sre = str_to_upper(sre))

agreg.final.oppen = read_excel("4_agregado_nivel_resultado_final-v2.xlsx")

agreg.final.oppen = agreg.final.oppen %>% 
  mutate(
    sre = str_replace_all(sre, "-", ""),
    sre = str_squish(sre),
    sre = str_to_upper(sre),
    sre = ifelse(sre == "CREDE 03 ARACAÚ", "CREDE 03 ACARAÚ", sre))

# Chegcagem com base Oppen
comparacao.nivel.final <- agreg.final %>% 
  rename(valor_iu = valor) %>% 
  full_join(
    agreg.final.oppen %>%
      select(see, sre, valor) %>%
      rename(
        valor_oppen = valor
      ),
    by = c("see","sre")
  )

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# média dos descritores - etapa/ação - dimensão
# =-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# et_ac_dim_g = et_ac_dim_g %>% 
#   pivot_wider(names_from = etapa_acao, values_from = valor) %>%
#   # Reorganziar as colunas pra ficar igual ao compendio
#   select(dimensao, E1, E2, E3, E4, E5, E6, A1, A2, A3, A4)
# 
# et_ac_dim_g = et_ac_dim_g %>% 
#   mutate(across(where(is.numeric), ~ round(.x, 2)))





