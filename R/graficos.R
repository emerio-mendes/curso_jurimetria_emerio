# graficos.R ---------------------------------------------------------------
# Funções compartilhadas por relatorio.qmd e R/03_apresentacao.R:
# paleta, tema, formatação, preparo dos desfechos, incidência acumulada e as
# três figuras. Requer tidyverse e survival carregados.

# Paleta (slots 1 e 2 da paleta categórica de referência) e tokens de texto
cor_liminar    <- "#2a78d6"
cor_julgamento <- "#eb6834"
ink_1  <- "#0b0b0b"
ink_2  <- "#52514e"
muted  <- "#898781"
grid_c <- "#e1e0d9"
base_c <- "#c3c2b7"
fundo  <- "#fcfcfb"

tema_hc <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background      = element_rect(fill = fundo, colour = NA),
      panel.grid.major.x   = element_blank(),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_line(colour = grid_c, linewidth = 0.3),
      axis.line.x          = element_line(colour = base_c, linewidth = 0.4),
      axis.text            = element_text(colour = muted),
      axis.title           = element_text(colour = ink_2),
      plot.title           = element_text(face = "bold"),
      plot.subtitle        = element_text(colour = ink_2),
      plot.caption         = element_text(colour = muted, hjust = 0),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank()
    )
}

num <- function(x, d = 1) format(round(x, d), nsmall = d, decimal.mark = ",",
                                  big.mark = ".")
pct <- function(x, d = 1) paste0(num(100 * x, d), "%")

# Data de corte = último movimento da base. Desfecho para riscos competitivos:
# liminar (evento), julgamento sem liminar (competitivo) ou censurado.
prepara_desfechos <- function(hc) {
  data_corte <- max(map_vec(hc$movimentos, \(m) max(m$dataHora)))
  hc |>
    mutate(
      desfecho = case_when(
        teve_liminar            ~ "liminar",
        !is.na(data_julgamento) ~ "julgamento sem liminar",
        TRUE                    ~ "censurado"
      ),
      t_evento = case_when(
        desfecho == "liminar" ~ tempo_dias,
        desfecho == "julgamento sem liminar" ~
          as.numeric(difftime(data_julgamento, data_ajuizamento, units = "days")),
        TRUE ~ as.numeric(difftime(data_corte, data_ajuizamento, units = "days"))
      )
    ) |>
    structure(data_corte = data_corte)
}

# Incidência acumulada de Aalen-Johansen dos dois desfechos competitivos
calcula_cif <- function(hc) {
  dados <- hc |>
    mutate(estado = factor(desfecho, levels = c("censurado", "liminar",
                                                "julgamento sem liminar")))
  aj <- survfit(Surv(t_evento, estado) ~ 1, data = dados)
  tibble(
    tempo = c(0, aj$time),
    liminar = c(0, aj$pstate[, which(aj$states == "liminar")]),
    `julgamento sem liminar` =
      c(0, aj$pstate[, which(aj$states == "julgamento sem liminar")])
  ) |>
    pivot_longer(-tempo, names_to = "desfecho", values_to = "prob") |>
    mutate(desfecho = factor(desfecho,
                             levels = c("liminar", "julgamento sem liminar")))
}

fig_histograma <- function(com_lim, base_size = 11) {
  med <- median(com_lim$tempo_dias)
  ggplot(com_lim, aes(tempo_dias)) +
    geom_histogram(binwidth = 1, boundary = 0, fill = cor_liminar,
                   colour = fundo, linewidth = 0.6) +
    geom_vline(xintercept = med, linetype = "dashed", colour = ink_2,
               linewidth = 0.5) +
    annotate("label", x = med, y = Inf, vjust = 1.3, hjust = -0.05,
             label = paste0("Mediana: ", num(med), " dias"),
             size = base_size * 0.31, colour = ink_2, fill = fundo,
             linewidth = 0) +
    scale_x_continuous(breaks = seq(0, 16, 2), expand = expansion(c(0, .02))) +
    scale_y_continuous(expand = expansion(c(0, .1))) +
    labs(x = "Dias desde o ajuizamento", y = "Número de HCs",
         caption = paste0("N = ", nrow(com_lim), " HCs. Faixas de 1 dia.")) +
    tema_hc(base_size)
}

fig_resultado <- function(com_lim, base_size = 11) {
  com_lim |>
    mutate(grupo = if_else(resultado_liminar == "denegada", "Denegada",
                           "Concedida\n(total ou parcial)")) |>
    add_count(grupo) |>
    mutate(grupo = paste0(grupo, "\nn = ", n),
           grupo = fct_reorder(grupo, n)) |>
    ggplot(aes(tempo_dias, grupo)) +
    geom_boxplot(width = 0.5, outlier.shape = NA, colour = ink_2,
                 fill = NA, linewidth = 0.4) +
    geom_point(position = position_jitter(height = 0.12, width = 0, seed = 1),
               size = 2.4, alpha = 0.75, colour = cor_liminar) +
    scale_x_continuous(breaks = seq(0, 16, 2), limits = c(0, 15.5)) +
    labs(x = "Dias desde o ajuizamento", y = NULL) +
    tema_hc(base_size) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = grid_c, linewidth = 0.3))
}

fig_cif <- function(cif, n_total, base_size = 11, ate = 30) {
  cif <- filter(cif, tempo <= ate)
  rot <- cif |>
    group_by(desfecho) |>
    slice_max(tempo, n = 1, with_ties = FALSE) |>
    mutate(rotulo = paste0(str_to_sentence(desfecho), ": ", pct(prob, 0)))

  ggplot(cif, aes(tempo, prob, colour = desfecho, linetype = desfecho)) +
    geom_step(linewidth = 0.8) +
    geom_text(data = rot, aes(label = rotulo), hjust = 1, vjust = -0.6,
              size = base_size * 0.31, colour = ink_2, show.legend = FALSE) +
    scale_colour_manual(values = c(liminar = cor_liminar,
                                   `julgamento sem liminar` = cor_julgamento),
                        labels = str_to_sentence) +
    scale_linetype_manual(values = c(liminar = "solid",
                                     `julgamento sem liminar` = "22"),
                          labels = str_to_sentence) +
    scale_y_continuous(labels = \(x) pct(x, 0), limits = c(0, 1),
                       breaks = seq(0, 1, .25)) +
    scale_x_continuous(breaks = seq(0, ate, 5), expand = expansion(c(0, .02))) +
    labs(x = "Dias desde o ajuizamento", y = "Probabilidade acumulada",
         caption = paste0("N = ", n_total, " HCs. Eixo cortado em ", ate,
                          " dias; o julgamento sem liminar continua subindo depois disso.")) +
    tema_hc(base_size)
}
