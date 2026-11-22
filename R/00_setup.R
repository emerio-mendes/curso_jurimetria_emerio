# 00_setup.R ---------------------------------------------------------------
# Instala (se necessário) e carrega os pacotes do projeto.

options(repos = c(CRAN = "https://cloud.r-project.org"))

pacotes_cran <- c("remotes", "tidyverse", "lubridate", "officer", "ragg")

faltantes <- pacotes_cran[!vapply(pacotes_cran, requireNamespace,
                                  logical(1), quietly = TRUE)]
if (length(faltantes) > 0) {
  message("Instalando do CRAN: ", paste(faltantes, collapse = ", "))
  install.packages(faltantes)
}

if (!requireNamespace("datajud", quietly = TRUE)) {
  message("Instalando datajud do GitHub (jjesusfilho/datajud)")
  remotes::install_github("jjesusfilho/datajud", upgrade = "never")
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(datajud)
})

# Chave da API -------------------------------------------------------------
# datajud::set_api_key() lê a variável de ambiente DATAJUD_API_KEY (defina-a
# no ~/.Renviron, FORA da pasta do projeto). Se ela não existir, usa a chave
# pública do CNJ embutida no pacote (get_default_api_key()). Nenhuma chave é
# gravada nos arquivos do projeto.
if (nzchar(Sys.getenv("DATAJUD_API_KEY"))) {
  set_api_key()
} else {
  set_api_key(get_default_api_key())
}

# Smoke test: 5 Habeas Corpus (classe 307) no TJAL, 2º grau ---------------
smoke <- search_processes("tjal", classe_codigo = 307, grau = "G2", size = 5)

stopifnot(
  "API não retornou um data.frame" = is.data.frame(smoke),
  "API não retornou 5 processos"   = nrow(smoke) == 5,
  "Grau diferente de G2"           = all(smoke$grau == "G2"),
  "Classe diferente de 307"        = all(vapply(smoke$classe,
                                                \(x) x$codigo == 307,
                                                logical(1)))
)

print(dplyr::select(smoke, numeroProcesso, grau, dataAjuizamento))
message("Setup concluído: pacotes carregados e API DataJud respondendo.")
