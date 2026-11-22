# Projeto: Tempo para Liminar em Habeas Corpus no TJAL (2ª Instância)

## Objetivo
Estimar o tempo entre o protocolo/distribuição e a primeira decisão liminar em Habeas Corpus (classe TPU 307) na 2ª instância (`G2`) do Tribunal de Justiça de Alagoas (TJAL), considerando pedidos ajuizados em fevereiro de 2026 (`2026-02-01` a `2026-02-28`).

## Ferramentas e Ambiente
- Linguagem: R (versão instalada no Positron).
- Fonte de dados: API Pública do DataJud (CNJ) via pacote `jjesusfilho/datajud`.
- Relatório final: Quarto (`.qmd`).
- Repositório GitHub já criado e autenticado via `gitcreds` (`origin/main`).

## Regras de Execução e Git
1. **Segurança:** Nunca crie arquivos com chaves, tokens ou senhas dentro da pasta do projeto. Adicione `data/*.rds` e `data/*.csv` ao `.gitignore` caso os arquivos ultrapassem 10 MB.
2. **Estrutura de pastas:** Organize o projeto em `R/` (scripts numerados `00_setup.R`, `01_coleta.R`, `02_analise.R`), `data/` (dados coletados) e `relatorio.qmd` na raiz.
3. **Conventional Commits:** Para cada tarefa lógica concluída e testada, faça `git add`, `git commit` seguindo o padrão (`chore:`, `feat:`, `fix:`, `docs:`, `test:`) e `git push` para `origin/main`.
4. **Validação obrigatória:** Antes de avançar de etapa, execute testes de consistência (verificar se a API respondeu, checar ausência de tempos negativos e validar reprodutibilidade em sessão limpa).

