# 03_apresentacao.R --------------------------------------------------------
# Gera apresentacao.pptx (10 slides, 16:9) a partir de
# data/hc_tjal_tratado.rds, com as mesmas figuras do relatório.

suppressPackageStartupMessages({
  library(tidyverse)
  library(survival)
  library(officer)
})
source("R/graficos.R")

saida <- "apresentacao.pptx"
dir_fig <- file.path(tempdir(), "figs_apresentacao")
dir.create(dir_fig, showWarnings = FALSE)

# Dados e números --------------------------------------------------------
hc      <- prepara_desfechos(readRDS("data/hc_tjal_tratado.rds"))
com_lim <- filter(hc, teve_liminar)
t       <- com_lim$tempo_dias
mov     <- bind_rows(hc$movimentos)
cif     <- calcula_cif(hc)
cif_lim <- filter(cif, desfecho == "liminar")
km      <- survfit(Surv(t_evento, desfecho == "liminar") ~ 1, data = hc)

n <- list(
  total     = nrow(hc),
  lim       = nrow(com_lim),
  sem       = sum(!hc$teve_liminar),
  julg      = sum(hc$desfecho == "julgamento sem liminar"),
  cens      = sum(hc$desfecho == "censurado"),
  deneg     = sum(com_lim$resultado_liminar == "denegada"),
  conced    = sum(str_detect(com_lim$resultado_liminar, "concedida")),
  mov       = nrow(mov),
  sem_cod   = mean(is.na(mov$codigo)),
  sem_dist  = sum(is.na(hc$data_distribuicao)),
  ate7      = mean(com_lim$dias_corridos <= 7),
  p7        = max(cif_lim$prob[cif_lim$tempo <= 7]),
  p14       = max(cif_lim$prob[cif_lim$tempo <= 14]),
  km7       = 1 - summary(km, times = 7)$surv,
  med_den   = median(t[com_lim$resultado_liminar == "denegada"]),
  med_con   = median(t[com_lim$resultado_liminar != "denegada"])
)
stopifnot(sum(hc$tempo_horas < 0, na.rm = TRUE) == 0)

# Figuras (PNG em alta resolução) ----------------------------------------
salva <- function(plot, nome, w, h) {
  f <- file.path(dir_fig, nome)
  ggsave(f, plot, width = w, height = h, dpi = 200, device = ragg::agg_png,
         bg = fundo)
  f
}
f_hist <- salva(fig_histograma(com_lim, 15), "hist.png", 8.4, 4.9)
f_res  <- salva(fig_resultado(com_lim, 15), "resultado.png", 8.4, 4.3)
f_cif  <- salva(fig_cif(cif, n$total, 15), "cif.png", 8.4, 4.9)

# Estilos -----------------------------------------------------------------
W <- 13.333; H <- 7.5; M <- 0.6   # slide 16:9 e margem lateral (polegadas)
fonte <- "Calibri"
tx <- function(size, color = ink_1, bold = FALSE)
  fp_text(font.size = size, color = color, bold = bold, font.family = fonte)
par_esq <- fp_par(text.align = "left", padding.bottom = 6)

texto <- function(pres, linhas, left, top, width, height, size = 18,
                  color = ink_1, bold = FALSE, esp = 6) {
  pars <- map(linhas, \(l) fpar(ftext(l, tx(size, color, bold)),
                                fp_p = fp_par(padding.bottom = esp)))
  ph_with(pres, do.call(block_list, pars),
          location = ph_location(left = left, top = top, width = width,
                                 height = height))
}

# Lista com marcador; cada item pode ter um trecho inicial em negrito
lista <- function(pres, itens, left, top, width, height, size = 18) {
  pars <- map(itens, \(it) {
    partes <- str_split_1(it, "\\|")
    if (length(partes) == 2) {
      fpar(ftext("•  ", tx(size, cor_liminar, TRUE)),
           ftext(partes[1], tx(size, ink_1, TRUE)),
           ftext(partes[2], tx(size, ink_2)), fp_p = fp_par(padding.bottom = 10))
    } else {
      fpar(ftext("•  ", tx(size, cor_liminar, TRUE)),
           ftext(it, tx(size, ink_1)), fp_p = fp_par(padding.bottom = 10))
    }
  })
  ph_with(pres, do.call(block_list, pars),
          location = ph_location(left = left, top = top, width = width,
                                 height = height))
}

caixa <- function(pres, left, top, width, height, cor = "#f1f0ec") {
  ph_with(pres, block_list(fpar(ftext(""))),
          location = ph_location(left = left, top = top, width = width,
                                 height = height, bg = cor))
}

novo_slide <- function(pres, titulo, num_pag) {
  pres <- add_slide(pres, layout = "Blank", master = "Office Theme")
  pres <- caixa(pres, 0, 0, 0.18, H, cor_liminar)
  pres <- texto(pres, titulo, M, 0.35, W - 2 * M, 1.0, size = 28, bold = TRUE)
  texto(pres, paste0(num_pag, " / 10"), W - 1.6, H - 0.55, 1.1, 0.4,
        size = 11, color = muted)
}

imagem <- function(pres, arq, left, top, width, height) {
  ph_with(pres, external_img(arq, width = width, height = height),
          location = ph_location(left = left, top = top, width = width,
                                 height = height))
}

# Slides --------------------------------------------------------------------
pres <- read_pptx()

# 1. Capa
pres <- add_slide(pres, layout = "Blank", master = "Office Theme")
pres <- caixa(pres, 0, 0, W, H, cor_liminar)
pres <- texto(pres, "Quanto tempo o TJAL leva para decidir a liminar em Habeas Corpus?",
              M + 0.2, 2.0, W - 2.5, 2.0, size = 38, color = "#ffffff", bold = TRUE)
pres <- texto(pres, c(
  paste0(n$total, " HCs da 2ª instância ajuizados em fevereiro de 2026"),
  "Dados públicos do DataJud (CNJ)"),
  M + 0.2, 4.2, W - 2.5, 1.2, size = 20, color = "#ffffff")
pres <- texto(pres, "Curso de Jurimetria — USP",
              M + 0.2, H - 1.3, 8, 0.5, size = 16, color = "#dbe8f8")

# 2. Pergunta e recorte
pres <- novo_slide(pres, "A pergunta e o recorte", 2)
pres <- lista(pres, c(
  "Pergunta: |quanto tempo passa entre o ajuizamento de um HC e a primeira decisão sobre o pedido liminar?",
  "Tribunal: |Tribunal de Justiça de Alagoas (TJAL), 2ª instância (grau G2)",
  "Classe: |Habeas Corpus, código TPU 307",
  "Período: |HCs ajuizados de 01/02/2026 a 28/02/2026",
  "Fonte: |API Pública do DataJud (CNJ), pelo pacote R jjesusfilho/datajud"
), M, 1.6, W - 2 * M, 5.2, size = 20)

# 3. Dados
pres <- novo_slide(pres, paste0("Dados: ", n$total, " HCs e ",
                                num(n$mov, 0), " movimentos"), 3)
pres <- lista(pres, c(
  "Coleta completa: |a paginação comum da API repete e pula processos; usamos search_slice(), que baixa tudo de uma vez e confere se veio completo",
  "Conferência: |sem o filtro de grau, a API traz os mesmos 150 HCs no G2 (e 3 no G1)",
  "Validações: |nenhum número duplicado, nenhum tempo negativo, nenhum movimento antes do ajuizamento",
  "Reprodutível: |o pipeline rodou do zero, a partir de um clone novo, e chegou aos mesmos resultados"
), M, 1.6, W - 2 * M, 5.2, size = 20)

# 4. Medida do tempo
pres <- novo_slide(pres, "Como o tempo foi medido", 4)
pres <- lista(pres, c(
  paste0("Início: |data de ajuizamento (a distribuição falta em ",
         n$sem_dist, " processos)"),
  "Fim: |primeiro movimento de decisão liminar"
), M, 1.5, W - 2 * M, 1.3, size = 20)
cod <- tribble(
  ~c, ~h, ~r,
  "339",   "Decisão › Concessão › Liminar",          "concedida",
  "892",   "Decisão › Concessão em parte › Liminar", "concedida em parte",
  "792",   "Decisão › Não-Concessão › Liminar",      "denegada",
  "12444 · 15086 · 15223", "Deferimento, deferimento em parte, prejudicado o pedido", "só antes do julgamento"
)
pres <- caixa(pres, M, 3.0, W - 2 * M, 3.2)
pres <- texto(pres, "Código TPU", M + 0.3, 3.15, 3.2, 0.4, 14, muted, TRUE)
pres <- texto(pres, "Hierarquia na tabela do CNJ", M + 3.6, 3.15, 5.4, 0.4, 14, muted, TRUE)
pres <- texto(pres, "Resultado", M + 9.2, 3.15, 2.8, 0.4, 14, muted, TRUE)
for (i in seq_len(nrow(cod))) {
  y <- 3.6 + (i - 1) * 0.55
  pres <- texto(pres, cod$c[i], M + 0.3, y, 3.2, 0.5, 17, ink_1, TRUE)
  pres <- texto(pres, cod$h[i], M + 3.6, y, 5.5, 0.5, 17, ink_2)
  pres <- texto(pres, cod$r[i], M + 9.2, y, 2.9, 0.5, 17, ink_1)
}
pres <- texto(pres, "O TJAL às vezes lança a liminar com códigos genéricos de decisão; eles só contam se vierem antes do julgamento.",
              M, 6.35, W - 2 * M, 0.5, 15, ink_2)

# 5. Censura e risco competitivo
pres <- novo_slide(pres, paste0(n$sem, " HCs não tiveram liminar registrada"), 5)
bloco <- function(pres, left, numero, rotulo, desc, cor) {
  pres <- caixa(pres, left, 1.6, 5.7, 3.0)
  pres <- caixa(pres, left, 1.6, 0.1, 3.0, cor)
  pres <- texto(pres, numero, left + 0.4, 1.8, 5, 0.9, 44, ink_1, TRUE)
  pres <- texto(pres, rotulo, left + 0.4, 2.75, 5, 0.5, 20, ink_1, TRUE)
  texto(pres, desc, left + 0.4, 3.3, 5.1, 1.3, 16, ink_2)
}
pres <- bloco(pres, M, n$julg, "Julgados sem liminar → risco competitivo",
              "Depois do julgamento a liminar não pode mais acontecer. Não é uma simples espera.",
              cor_julgamento)
pres <- bloco(pres, M + 6.1, n$cens, "Ainda pendente → censura à direita",
              "Sem liminar e sem julgamento na data de corte: o tempo observado é um limite inferior.",
              muted)
pres <- lista(pres, c(
  "Método: |incidência acumulada de Aalen-Johansen, que trata os dois casos corretamente",
  paste0("Por que importa: |um Kaplan-Meier comum estimaria ", pct(n$km7),
         " de liminares em 7 dias, contra ", pct(n$p7), " pelo método correto")
), M, 4.95, W - 2 * M, 1.8, size = 18)

# 6. Números principais
pres <- novo_slide(pres, "Quando há liminar, ela sai em cerca de 4 dias", 6)
kpi <- tribble(
  ~valor, ~rotulo, ~nota,
  paste(num(median(t)), "dias"), "tempo mediano", paste0("média de ", num(mean(t)), " dias"),
  paste(num(quantile(t, .25)), "a", num(quantile(t, .75))), "metade dos casos (dias)",
    paste0("IQR de ", num(IQR(t)), " dias"),
  pct(n$lim / n$total, 0), "dos HCs tiveram liminar", paste0(n$lim, " de ", n$total),
  pct(n$deneg / n$lim, 0), "das liminares foram denegadas", paste0(n$deneg, " de ", n$lim)
)
lk <- (W - 2 * M - 3 * 0.3) / 4
for (i in seq_len(nrow(kpi))) {
  x <- M + (i - 1) * (lk + 0.3)
  pres <- caixa(pres, x, 2.0, lk, 3.2)
  pres <- texto(pres, kpi$valor[i], x + 0.25, 2.4, lk - 0.4, 1.0, 34, cor_liminar, TRUE)
  pres <- texto(pres, kpi$rotulo[i], x + 0.25, 3.5, lk - 0.4, 0.9, 18, ink_1, TRUE)
  pres <- texto(pres, kpi$nota[i], x + 0.25, 4.4, lk - 0.4, 0.6, 15, ink_2)
}
pres <- texto(pres, paste0("Tempo do ajuizamento à primeira decisão liminar, ",
                           n$lim, " HCs com liminar."),
              M, 5.6, W - 2 * M, 0.5, 15, muted)

# 7. Histograma
pres <- novo_slide(pres, "A maioria das liminares sai na primeira semana", 7)
pres <- imagem(pres, f_hist, M, 1.45, 8.4, 4.9)
pres <- texto(pres, c(
  pct(n$ate7, 0),
  "das liminares saíram em até 7 dias corridos"),
  M + 8.9, 2.2, 3.3, 1.6, 20, ink_1, TRUE)
pres <- texto(pres, paste0("A distribuição é assimétrica: alguns casos chegam a ",
                           num(max(t)), " dias e puxam a média (", num(mean(t)),
                           ") para cima da mediana (", num(median(t)), ")."),
              M + 8.9, 4.0, 3.3, 2.2, 16, ink_2)

# 8. Por resultado
pres <- novo_slide(pres, "Concedidas e denegadas levam tempos parecidos", 8)
pres <- imagem(pres, f_res, M, 1.6, 8.4, 4.3)
pres <- texto(pres, c(
  paste0("Mediana: ", num(n$med_den), " dias (denegadas) e ",
         num(n$med_con), " dias (concedidas)."),
  paste0("Só ", n$conced, " liminares foram concedidas: a diferença é descritiva, não permite inferência.")),
  M + 8.9, 2.2, 3.3, 3.5, 17, ink_2, esp = 14)

# 9. Incidência acumulada
pres <- novo_slide(pres, paste0(pct(n$p7, 0), " dos HCs têm liminar em até 7 dias"), 9)
pres <- imagem(pres, f_cif, M, 1.45, 8.4, 4.9)
pres <- texto(pres, c(
  paste0("Todos os ", n$total, " HCs, pelo método de Aalen-Johansen."),
  paste0("Em 14 dias: ", pct(n$p14, 0), ". Depois disso a curva quase não sobe."),
  "Os HCs restantes foram julgados sem liminar registrada."),
  M + 8.9, 2.0, 3.3, 4.0, 17, ink_2, esp = 14)

# 10. Limitações
pres <- novo_slide(pres, "Limitações e próximos passos", 10)
pres <- texto(pres, "Limitações", M, 1.5, 5.8, 0.5, 20, cor_liminar, TRUE)
pres <- lista(pres, c(
  "Pedido liminar: |o DataJud não informa se houve pedido",
  paste0("Movimentos sem código: |", pct(n$sem_cod, 0),
         " dos movimentos do TJAL não têm código TPU"),
  "Codificação local: |códigos genéricos e o código 1059 (recurso) em HC",
  paste0("Amostra: |um mês e só ", n$conced, " liminares concedidas")
), M, 2.05, 5.9, 4.6, size = 17)
pres <- texto(pres, "Próximos passos", M + 6.4, 1.5, 5.8, 0.5, 20, cor_liminar, TRUE)
pres <- lista(pres, c(
  "Dias úteis: |separar o efeito de fins de semana",
  "Relatores e órgãos: |a variação vem de quem decide?",
  "Mais meses: |para comparar grupos com segurança",
  "Movimentos sem código: |conferir uma amostra no site do TJAL"
), M + 6.4, 2.05, 5.9, 4.6, size = 17)

# Salva e ajusta o tamanho do slide para 16:9 --------------------------------
tmp <- tempfile(fileext = ".pptx")
print(pres, target = tmp)

dir_x <- tempfile("pptx_")
utils::unzip(tmp, exdir = dir_x)
pxml <- file.path(dir_x, "ppt", "presentation.xml")
xml <- readLines(pxml, warn = FALSE, encoding = "UTF-8")
xml <- sub('<p:sldSz[^>]*/>', '<p:sldSz cx="12192000" cy="6858000"/>', xml)
writeLines(xml, pxml, useBytes = TRUE)

# O officer cria cada caixa como placeholder genérico (<p:ph/>) sem
# geometria própria, e o PowerPoint não pinta o fundo dessas formas. Aqui
# elas viram formas comuns com geometria retangular explícita.
for (f in list.files(file.path(dir_x, "ppt", "slides"), "\\.xml$",
                     full.names = TRUE)) {
  s <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  s <- gsub("<p:ph/>", "", s, fixed = TRUE)
  s <- gsub("(?s)(<p:spPr>\\s*<a:xfrm>.*?</a:xfrm>)(?!\\s*<a:prstGeom)",
            "\\1<a:prstGeom prst=\"rect\"><a:avLst/></a:prstGeom>", s,
            perl = TRUE)
  writeLines(s, f, useBytes = TRUE)
}
if (file.exists(saida)) file.remove(saida)
arquivos <- list.files(dir_x, recursive = TRUE, all.files = TRUE)
# [Content_Types].xml precisa ser a primeira entrada do pacote
arquivos <- c("[Content_Types].xml", setdiff(arquivos, "[Content_Types].xml"))
zip::zip(normalizePath(saida, mustWork = FALSE), files = arquivos, root = dir_x)

stopifnot(length(pres) == 10)
message(sprintf("Apresentação salva em %s (%d slides, %.1f MB)",
                saida, length(pres), file.size(saida) / 1e6))
