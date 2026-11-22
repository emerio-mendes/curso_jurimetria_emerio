# 02_analise.R -------------------------------------------------------------
# Processa data/hc_tjal_fev2026.rds e calcula, para cada HC, o tempo entre o
# ajuizamento e a primeira decisão sobre o pedido liminar. Salva a base
# tratada em data/hc_tjal_tratado.rds.
#
# Códigos TPU (Movimentos) considerados decisão liminar — hierarquia
# conferida na API pública da TPU/CNJ:
#   339   Decisão > Concessão > Liminar               -> concedida
#   892   Decisão > Concessão em parte > Liminar      -> concedida em parte
#   792   Decisão > Não-Concessão > Liminar           -> denegada
#   Códigos genéricos de decisão, usados pelo TJAL em alguns HCs no lugar dos
#   específicos. Só contam se lançados ANTES do primeiro julgamento:
#   12444 Decisão > Deferimento                       -> concedida
#   15086 Decisão > Deferimento em Parte              -> concedida em parte
#   15223 Decisão > Prejudicado o Pedido              -> prejudicada
# NÃO são liminares (ficam de fora):
#   12459 Julgamento > Sem resolução de mérito > Prejudicado (a AÇÃO prejudicada)
#   1059  Recurso > Sem efeito suspensivo (lançado na remessa de recurso
#         após o julgamento do HC)
#
# Datas: dataAjuizamento vem como "AAAAMMDDhhmmss" e os movimentos como ISO
# com sufixo "Z". Ambos estão no mesmo relógio (horário local do TJAL), já
# que a distribuição ocorre minutos após o ajuizamento. Por isso os dois são
# lidos com o mesmo fuso (UTC nominal) e a diferença entre eles é válida.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

entrada <- "data/hc_tjal_fev2026.rds"
saida   <- "data/hc_tjal_tratado.rds"

hc_bruto <- readRDS(entrada)

# Dicionário de códigos ----------------------------------------------------
cod_liminar <- tribble(
  ~codigo, ~resultado_liminar,   ~codigo_generico,
  339L,    "concedida",          FALSE,
  892L,    "concedida em parte", FALSE,
  792L,    "denegada",           FALSE,
  12444L,  "concedida",          TRUE,
  15086L,  "concedida em parte", TRUE,
  15223L,  "prejudicada",        TRUE
)

# Movimentos que marcam julgamento/extinção do HC (limite para os genéricos)
cod_julgamento <- c(
  12199L,                      # Julgado (sessão)
  443L, 447L, 451L,            # Julgamento com mérito > Habeas corpus
  12459L, 454L, 459L, 460L,    # Julgamento sem resolução de mérito
  461L, 463L, 235L
)

# Movimentos em formato longo ----------------------------------------------
movimentos <- map2(hc_bruto$numeroProcesso, hc_bruto$movimentos, \(np, ms) {
  tibble(
    numeroProcesso = np,
    codigo   = map_int(ms, \(m) m$codigo %||% NA_integer_),
    nome     = map_chr(ms, \(m) m$nome %||% NA_character_),
    dataHora = ymd_hms(map_chr(ms, \(m) m$dataHora %||% NA_character_),
                       tz = "UTC", quiet = TRUE)
  )
}) |>
  list_rbind() |>
  arrange(numeroProcesso, dataHora)

# Base de processos --------------------------------------------------------
processos <- hc_bruto |>
  transmute(
    numeroProcesso,
    orgao_julgador   = map_chr(orgaoJulgador, \(o) o$nome %||% NA_character_),
    origem_cnj       = substr(numeroProcesso, 17, 20),
    data_ajuizamento = ymd_hms(dataAjuizamento, tz = "UTC")
  )

datas_ref <- movimentos |>
  group_by(numeroProcesso) |>
  summarise(
    data_distribuicao = suppressWarnings(min(dataHora[codigo == 26L], na.rm = TRUE)),
    data_julgamento   = suppressWarnings(min(dataHora[codigo %in% cod_julgamento], na.rm = TRUE)),
    n_movimentos      = n(),
    .groups = "drop"
  ) |>
  mutate(across(c(data_distribuicao, data_julgamento),
                \(x) if_else(is.infinite(x), as.POSIXct(NA, tz = "UTC"), x)))

eventos_liminar <- movimentos |>
  inner_join(cod_liminar, by = "codigo") |>
  left_join(datas_ref |> select(numeroProcesso, data_julgamento),
            by = "numeroProcesso") |>
  mutate(valido = !codigo_generico | is.na(data_julgamento) |
                    dataHora < data_julgamento)

primeira_liminar <- eventos_liminar |>
  filter(valido) |>
  slice_min(dataHora, n = 1, by = numeroProcesso, with_ties = FALSE) |>
  select(numeroProcesso, data_liminar = dataHora, codigo_liminar = codigo,
         resultado_liminar, codigo_generico)

hc <- processos |>
  left_join(datas_ref, by = "numeroProcesso") |>
  left_join(primeira_liminar, by = "numeroProcesso") |>
  mutate(
    teve_liminar  = !is.na(data_liminar),
    tempo_horas   = as.numeric(difftime(data_liminar, data_ajuizamento, units = "hours")),
    tempo_dias    = tempo_horas / 24,
    dias_corridos = as.integer(as_date(data_liminar) - as_date(data_ajuizamento))
  ) |>
  left_join(movimentos |> nest(movimentos = -numeroProcesso),
            by = "numeroProcesso")

# Validações e anomalias -----------------------------------------------------
stopifnot(
  "Processos perdidos no processamento" = nrow(hc) == nrow(hc_bruto),
  "numeroProcesso duplicado"            = !anyDuplicated(hc$numeroProcesso),
  "dataAjuizamento ausente"             = !anyNA(hc$data_ajuizamento),
  "Tempo negativo até a liminar"        = all(hc$tempo_horas >= 0, na.rm = TRUE)
)

relata <- function(titulo, x) {
  message("\n== ", titulo)
  print(x, n = Inf)
}

relata("Resultado da primeira decisão liminar",
       hc |> count(teve_liminar, resultado_liminar, codigo_liminar, codigo_generico))

relata("Tempo até a liminar (dias)",
       hc |> filter(teve_liminar) |>
         summarise(n = n(), min = min(tempo_dias), q1 = quantile(tempo_dias, .25),
                   mediana = median(tempo_dias), media = mean(tempo_dias),
                   q3 = quantile(tempo_dias, .75), max = max(tempo_dias)))

relata("Anomalia: movimentos sem código TPU",
       movimentos |> summarise(sem_codigo = sum(is.na(codigo)),
                               total = n(), proporcao = mean(is.na(codigo))))

relata("Anomalia: códigos com nome vazio",
       movimentos |> filter(!is.na(codigo), is.na(nome) | nome == "") |>
         count(codigo))

relata("Anomalia: código 1059 (recurso) lançado em HC",
       movimentos |> filter(codigo == 1059L) |>
         summarise(movimentos = n(), processos = n_distinct(numeroProcesso)))

relata("Códigos de liminar descartados (genérico após julgamento)",
       eventos_liminar |> filter(!valido) |>
         select(numeroProcesso, codigo, dataHora, data_julgamento))

relata("Processos com mais de uma decisão liminar",
       eventos_liminar |> filter(valido) |> count(numeroProcesso) |>
         filter(n > 1))

relata("Movimento anterior ao ajuizamento (horas de antecedência)",
       movimentos |> left_join(processos, by = "numeroProcesso") |>
         filter(dataHora < data_ajuizamento) |>
         mutate(horas = as.numeric(difftime(data_ajuizamento, dataHora,
                                            units = "hours"))) |>
         select(numeroProcesso, codigo, nome, dataHora, horas))

relata("Distribuição anterior ao ajuizamento",
       hc |> filter(data_distribuicao < data_ajuizamento) |>
         select(numeroProcesso, data_ajuizamento, data_distribuicao))

relata("Liminar no mesmo minuto do ajuizamento (possível erro de registro)",
       hc |> filter(tempo_horas < 1 / 60) |>
         select(numeroProcesso, data_ajuizamento, data_liminar))

relata("Processos sem decisão liminar, por órgão e desfecho",
       hc |> filter(!teve_liminar) |>
         mutate(julgado = !is.na(data_julgamento)) |>
         count(orgao_julgador, julgado))

saveRDS(hc, saida)
message(sprintf("\nBase tratada salva em %s (%d processos, %.2f MB)",
                saida, nrow(hc), file.size(saida) / 1e6))
