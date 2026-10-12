# =====================================================================
# SCRIPT DE EXEMPLO DO PACOTE MIFEBPC
# =====================================================================
# Descrição: Este script demonstra o fluxo operacional do pacote mifebpc,
# englobando a consulta prévia a dados históricos e parâmetros nativos,
# a coleta macroeconómica, projeção da fila do BPC, processamento atuarial,
# cálculo de impactos financeiros com decomposição (efeito preço e volume)
# e geração de relatórios em Excel.
# =====================================================================

# ---------------------------------------------------------------------
# FASE 0: CONSULTA E AUDITORIA PRÉVIA DE DADOS E PARÂMETROS NATIVOS
# ---------------------------------------------------------------------
# Inspeciona os metadados gerais e os caminhos dos arquivos base do pacote
metadados_gerais <- listar_metadados_pacote()
print(metadados_gerais)

# Consulta o histórico de fila processada (ex: filtrando apenas a espécie PCD)
df_hist_fila <- consultar_historico_fila(especie = "87", data_inicio = "2024-01-01")
head(df_hist_fila)

# Consulta o histórico agregado de quadrantes
df_quadrantes <- consultar_historico_quadrantes()
head(df_quadrantes)

# Inspeciona os parâmetros de expectativa de revisão cadastral
df_revisoes <- consultar_parametros_revisao()
head(df_revisoes)

# Inspeciona os dados da tábua biométrica anualizada
df_tabua <- consultar_parametros_tabua(ano_filtro = 2025)
head(df_tabua)

# Consulta os valores financeiros executados históricos para comparação
df_executados <- consultar_valores_executados(data_inicio = "2026-01-01")
head(df_executados)

# ---------------------------------------------------------------------
# CONFIGURAÇÕES DO CENÁRIO DE PROJEÇÃO
# ---------------------------------------------------------------------
# HORIZONTE_ESCOLHIDO: Define a quantidade de meses futuros a projetar.
#   - Exemplo: 6L (curto prazo / teste rápido) ou 60L / 84L (médio/longo prazo).
HORIZONTE_ESCOLHIDO <- 60L

# DATA_CORTE_INICIO: Permite opcionalmente fixar uma data de corte para o
#   início da projeção (ex: "2027-01-01"). Se definido como NULL, o pacote
#   assume o mês imediatamente posterior ao último mês da base nativa.
DATA_CORTE_INICIO <- NULL

# ---------------------------------------------------------------------
# FASE 1: OBTENÇÃO DE INDICADORES MACROECONÓMICOS
# ---------------------------------------------------------------------
# Recolhe os dados mais recentes de IPCA, taxa de desemprego, população idosa
# e taxa de informalidade através das APIs públicas suportadas pelo pacote.
dt_macro <- obter_dados_macro()

# Validação rápida das últimas linhas obtidas (garantindo ausência de falhas na série)
tail(dt_macro)

# ---------------------------------------------------------------------
# FASE 2 & 3: MODELAGEM E PROJEÇÃO DA FILA DO BPC (ADMINISTRATIVA E JUDICIAL)
# ---------------------------------------------------------------------
# Utiliza modelos de Random Forest e extrapolação temporal para projetar
# as concessões e os Tempos Médios de Concessão (TMC) conforme o horizonte.
df_fila <- projetar_fila_bpc(
  dt_macro = dt_macro,
  horizonte_meses = HORIZONTE_ESCOLHIDO,
  data_inicio_projecao = DATA_CORTE_INICIO
)

# Validação estrita: assegura que o número de linhas geradas corresponde
# exatamente ao horizonte desejado e que não existem valores omissos (NA).
stopifnot(nrow(df_fila) == HORIZONTE_ESCOLHIDO, !anyNA(df_fila))

# Resumo estatístico das concessões projetadas (PCD e Idoso)
summary(df_fila[, c("Con_Adm_PCD", "Con_Adm_IDOSO",
                    "Con_Jud_PCD", "Con_Jud_IDOSO")])

# ---------------------------------------------------------------------
# FASE 4: PROCESSAMENTO DO MOTOR ATUARIAL
# ---------------------------------------------------------------------
# Simula a evolução do estoque ativo, saídas por óbito, revisões cadastrais
# e saídas residuais ao longo do horizonte temporal projetado.
df_atuarial <- executar_motor_atuarial(df_final_engine = df_fila)

# Verifica o intervalo de variação do estoque ativo por espécie (IDOSO e PCD)
tapply(df_atuarial$Estoque_Ativo, df_atuarial$especie, range)

# ---------------------------------------------------------------------
# FASE 5: CONFIGURAÇÃO DE PARÂMETROS FINANCEIROS (SALÁRIO MÍNIMO)
# ---------------------------------------------------------------------
# Define a tabela de salário-mínimo vigente para abranger todo o horizonte
# da projeção (estendida até 2033, permitindo customizações por ano).
tabela_sm_vigente <- tibble::tibble(
  Ano = 2026:2033,
  SM = c(1621, 1741, 1854, 1956, 2066, 2180, 2300, 2420)
)

# ---------------------------------------------------------------------
# FASE 6: CÁLCULO DE IMPACTOS FINANCEIRO E DECOMPOSIÇÃO DE VARIAÇÕES
# ---------------------------------------------------------------------
# Calcula o custo total, valores retroativos, juros e isola os efeitos
# de preço (variação do salário mínimo) e volume (variação física do estoque)
# em base anualizada de 12 meses (corrigindo distorções de anos parciais).
resultado_financeiro <- calcular_impactos_financeiros(
  df_atuarial,
  tabela_sm = tabela_sm_vigente
)

# Inspeciona o resumo mensal consolidado e a tabela de impactos e decomposições
head(resultado_financeiro$resumo_mensal)
resultado_financeiro$impactos_var

# ---------------------------------------------------------------------
# FASE 7: GERAÇÃO DO RELATÓRIO ATUARIAL EM EXCEL (.XLSX)
# ---------------------------------------------------------------------
# Consolida todas as abas técnicas, físicas, financeiras e de sensibilidade
# num único livro de trabalho em formato Excel.
rel <- gerar_relatorio_atuarial(
  df_atuarial,
  tabela_sm = tabela_sm_vigente,
  caminho_xlsx = "relatorio.xlsx"
)

# ---------------------------------------------------------------------
# FASE 8: VALIDAÇÃO RÁPIDA DAS ABAS DO RELATÓRIO GERADO
# ---------------------------------------------------------------------
rel$resumo_fisico_financeiro     # Resumo consolidado anual físico e financeiro
rel$resumo_entradas_saidas       # Dinâmica de entradas e saídas por ano
head(rel$resumo_detalhes)        # Detalhamento analítico por componente
head(rel$planilha1)              # Projeção analítica detalhada mês a mês
head(rel$estoque_mensal_detalhado) # Estoque ativo e fluxos segregados

# Verificação de avisos operacionais acumulados durante o ciclo
warnings()
