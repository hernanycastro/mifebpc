--- editor_options: markdown: wrap: 72 ---

# mifebpc: Modelo Atuarial e Projeção do BPC 📊🏛️

[![R-CMD-check](https://github.com/hernanycastro/mifebpc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hernanycastro/mifebpc) [![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/hernanycastro/mifebpc/releases/tag/v0.1.0) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

O **`mifebpc`** é um pacote em R desenvolvido para o cálculo atuarial, projeção da fila de espera, análise macroeconómica e decomposição de impacto financeiro do **Benefício de Prestação Continuada (BPC)**. A ferramenta integra rigor metodológico, padrões de desenvolvimento robustos e um módulo seguro de auditoria e consulta de parâmetros.

------------------------------------------------------------------------

\## 🚀 Principais Funcionalidades

O pacote estrutura-se num pipeline modular completo:

\* **Fase 0 (Consulta e Auditoria):** Módulo seguro para inspeção de parâmetros, tabelas e histórico, mascarando caminhos absolutos e garantindo rastreabilidade.

\* **Fase Macro:** Obtenção e tratamento de variáveis macroeconómicas para as projeções atuariais.

\* **Fase Fila:** Modelação e projeção da fila de requerimentos e dinâmica de concessões do BPC.

\* **Fase Atuarial:** Motor de cálculo e projeção populacional e atuarial de longo prazo.

\* **Fase Financeira:** Decomposição de preço versus volume e impactos financeiros orçamentários.

\* **Fase Relatório:** Geração automatizada de relatórios gerenciais e técnicos integrados.

------------------------------------------------------------------------

\## 📦 Instalação

Atualmente, o pacote pode ser instalado diretamente do GitHub utilizando o pacote `remotes`:

\`\`\`r \# Instalar o pacote remotes se necessário if (!requireNamespace("remotes", quietly = TRUE)) { install.packages("remotes") }

# 💡 Exemplo Rápido de Utilização

Abaixo encontra-se um exemplo básico de como iniciar a inspeção e execução do pipeline atuarial:

library(mifebpc)

**1. Consultar metadados e parâmetros iniciais (Módulo de Consulta - Fase 0)**

listar_metadados_pacote()

**2. Executar o motor atuarial principal**

(Certifique-se de carregar os dados de entrada necessários do seu ambiente)

resultado_atuarial \<- executar_motor_atuarial(dados = meus_dados)

Para uma introdução detalhada a todas as fases e fluxos de trabalho, consulte a vignette integrada no pacote:

vignette("mifebpc-introducao", package = "mifebpc")

## 🧪 Qualidade e Confiabilidade

O `mifebpc` segue rigorosos padrões de desenvolvimento em R, contando com:

- **0 Erros, 0 Avisos e 0 Notas** no `devtools::check()`.

- Cobertura de testes unitários automatizados via `testthat`.

- Documentação completa de todas as funções e fluxos via `roxygen2`.

## 👥 Autoria e Colaboração

- **Autor / Desenvolvedor:** Hernany Gomes de Castro

- Contribuições, sugestões e reporte de problemas são bem-vindos através das [*Issues* do repositório](https://www.google.com/search?q=https://github.com/hernanycastro/mifebpc/issues).
