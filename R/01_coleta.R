# 01_coleta.R --------------------------------------------------------------
# Coleta todos os Habeas Corpus (classe TPU 307) da 2ª instância (G2) do TJAL
# ajuizados entre 2026-02-01 e 2026-02-28 e salva o resultado bruto em
# data/hc_tjal_fev2026.rds.
#
# Estratégia: a paginação por from/size do DataJud repete e pula documentos,
# então usamos datajud::search_slice(), que baixa cada fatia em uma única
# requisição e confere se a resposta veio completa. Se uma fatia de datas
# tiver mais documentos que `max_size`, ela é dividida ao meio recursivamente.

suppressPackageStartupMessages({
  library(tidyverse)
  library(datajud)
})

# Chave: DATAJUD_API_KEY (no ~/.Renviron) ou a chave pública do pacote
if (nzchar(Sys.getenv("DATAJUD_API_KEY"))) {
  set_api_key()
} else {
  set_api_key(get_default_api_key())
}

tribunal <- "tjal"
inicio   <- as.Date("2026-02-01")
fim      <- as.Date("2026-02-28")
max_size <- 1000  # a resposta inclui movimentos; fatias menores falham menos
arquivo  <- "data/hc_tjal_fev2026.rds"

monta_query <- function(start, end) {
  list(bool = list(must = list(
    list(match = list(classe.codigo = 307)),
    list(match = list(grau = "G2")),
    date_range_query("dataAjuizamento", format(start), format(end))
  )))
}

coleta_fatia <- function(start, end) {
  message(sprintf("Fatia %s a %s ...", start, end))
  tryCatch(
    {
      res <- search_slice(tribunal, monta_query(start, end),
                          max_size = max_size)
      message(sprintf("  %d processos", nrow(res)))
      res
    },
    datajud_slice_too_large = function(e) {
      if (start == end) stop("Um único dia excede max_size: ", start)
      message(sprintf("  %d documentos > max_size; dividindo", e$total))
      partes <- split_date_range(start, end, parts = 2)
      bind_rows(map2(partes$start, partes$end, coleta_fatia))
    }
  )
}

hc <- coleta_fatia(inicio, fim)

# Validações ---------------------------------------------------------------
stopifnot(
  "API não retornou processos" = nrow(hc) > 0,
  "Grau diferente de G2"       = all(hc$grau == "G2"),
  "Classe diferente de 307"    = all(map_lgl(hc$classe, \(x) x$codigo == 307))
)

datas <- as.Date(substr(hc$dataAjuizamento, 1, 8), format = "%Y%m%d")
stopifnot("Data de ajuizamento fora de fevereiro/2026" =
            all(!is.na(datas) & datas >= inicio & datas <= fim))

# Duplicatas por numeroProcesso. O DataJud pode ter mais de um documento para
# o mesmo número (ex.: reautuação, órgãos julgadores diferentes). Mantemos o
# bruto intacto e apenas reportamos; a deduplicação é decidida na análise.
dup <- hc |> count(numeroProcesso) |> filter(n > 1)
message(sprintf("Total de documentos: %d | processos únicos: %d | números duplicados: %d",
                nrow(hc), n_distinct(hc$numeroProcesso), nrow(dup)))
if (nrow(dup) > 0) print(dup)

dir.create("data", showWarnings = FALSE)
saveRDS(hc, arquivo)
message(sprintf("Salvo em %s (%.1f MB)", arquivo, file.size(arquivo) / 1e6))
