# Tempo até a liminar em Habeas Corpus no TJAL

Quanto tempo o Tribunal de Justiça de Alagoas leva para decidir o pedido
liminar em um Habeas Corpus? Este projeto mede o intervalo entre o ajuizamento
e a primeira decisão liminar nos HCs (classe TPU 307) da 2ª instância
ajuizados em **fevereiro de 2026**, com dados públicos do DataJud (CNJ).

Exercício do curso de Jurimetria da USP.

## Principais resultados

| | |
|---|---|
| HCs ajuizados no período | 150 |
| HCs com decisão liminar registrada | 107 (71,3%) |
| Tempo mediano até a liminar | **3,8 dias** |
| Tempo médio | 4,3 dias |
| Intervalo interquartil | 2,1 a 6,0 dias |
| Probabilidade de liminar em até 7 dias (todos os HCs) | 60,7% |
| Liminares denegadas | 93 de 107 |

Os 43 HCs sem decisão liminar foram, em quase todos os casos, julgados sem
liminar registrada. O relatório os trata como risco competitivo, e não como
simples censura (detalhes abaixo).

O relatório completo, com metodologia, tabelas e gráficos, está em
[`relatorio.html`](relatorio.html). O GitHub não exibe HTML: baixe o arquivo
e abra no navegador.

## Como reproduzir

Requisitos: R (testado na versão 4.6.2) e [Quarto](https://quarto.org)
(testado na versão 1.11).

```bash
Rscript R/00_setup.R     # instala pacotes e testa a conexão com a API
Rscript R/01_coleta.R    # coleta os HCs e salva data/hc_tjal_fev2026.rds
Rscript R/02_analise.R   # calcula os tempos e salva data/hc_tjal_tratado.rds
quarto render relatorio.qmd
Rscript R/03_apresentacao.R   # gera apresentacao.pptx (10 slides)
```

Os dados já coletados estão versionados em `data/`. Para só gerar o
relatório, basta rodar o `00_setup.R` e o `quarto render`.

**Chave da API:** a API pública do DataJud exige uma chave. O pacote
`datajud` já traz a chave pública do CNJ, usada por padrão. Para usar outra,
defina a variável `DATAJUD_API_KEY` no seu `~/.Renviron`, fora da pasta do
projeto. Nenhuma chave fica gravada no repositório.

**Reprodutibilidade:** o pipeline completo foi executado do zero, em sessões
R limpas e a partir de um clone novo, e reproduziu os mesmos 150 processos e
os mesmos tempos. Como o DataJud é atualizado continuamente, coletas futuras
podem trazer movimentos novos.

## Estrutura

```
R/
  00_setup.R         instalação de pacotes e teste da API
  01_coleta.R        coleta via API do DataJud
  02_analise.R       extração dos movimentos e cálculo dos tempos
  03_apresentacao.R  geração da apresentação em PowerPoint
  graficos.R         tema e gráficos usados no relatório e na apresentação
data/
  hc_tjal_fev2026.rds   dados brutos da API
  hc_tjal_tratado.rds   uma linha por processo, com tempos e desfecho
relatorio.qmd        relatório (fonte)
relatorio.html       relatório renderizado
apresentacao.pptx    apresentação (10 slides)
```

## Metodologia em resumo

- **Fonte:** API Pública do DataJud, pelo pacote
  [`jjesusfilho/datajud`](https://github.com/jjesusfilho/datajud). A coleta
  usa `search_slice()` porque a paginação comum dessa API repete e pula
  processos.
- **Início da contagem:** data de ajuizamento. A data de distribuição falta em
  22 processos.
- **Decisão liminar:** primeiro movimento com código TPU 339 (concedida), 892
  (concedida em parte) ou 792 (denegada). Os códigos genéricos 12444, 15086 e
  15223, que o TJAL às vezes usa no lugar dos específicos, só contam se
  lançados antes do julgamento.
- **HCs sem liminar:** o julgamento sem liminar é tratado como risco
  competitivo e o processo ainda pendente como censura à direita. A
  probabilidade acumulada de liminar é estimada pelo método de
  Aalen-Johansen.

## Limitações

- O DataJud não informa se houve pedido liminar. Os HCs julgados sem liminar
  podem simplesmente não ter tido pedido.
- 15% dos movimentos do TJAL vêm sem código TPU. Se algum deles for uma
  liminar, ela não é contada.
- Com um só mês e 14 liminares concedidas, as comparações entre grupos são
  apenas descritivas.
