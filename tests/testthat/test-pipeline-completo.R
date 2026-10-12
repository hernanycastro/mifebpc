# =====================================================================
# TESTES UNITÁRIOS E DE INTEGRAÇÃO - MIFEBPC
# =====================================================================

library(testthat)

test_that("A projecao da fila do BPC respeita o horizonte e retorna formato valido", {
  dt_macro <- tryCatch({
    obter_dados_macro()
  }, error = function(e) {
    skip("API do Banco Central indisponível no momento do teste.")
  })

  horizonte_teste <- 6L
  df_fila <- projetar_fila_bpc(
    dt_macro = dt_macro,
    horizonte_meses = horizonte_teste,
    data_inicio_projecao = NULL
  )

  # Verificações estruturais
  expect_true(is.data.frame(df_fila))
  expect_equal(nrow(df_fila), horizonte_teste)
  expect_false(anyNA(df_fila))

  colunas_esperadas <- c(
    "Data", "Con_Adm_PCD", "Con_Adm_IDOSO", "Con_Jud_PCD", "Con_Jud_IDOSO",
    "Proj_Con_Adm", "Proj_Con_Jud", "TMC_Adm_PCD", "TMC_Adm_IDOSO",
    "TMC_Jud_PCD", "TMC_Jud_IDOSO"
  )
  expect_true(all(colunas_esperadas %in% names(df_fila)))
})

test_that("O motor atuarial processa corretamente a evolucao do estoque e saidas", {
  dt_macro <- tryCatch({
    obter_dados_macro()
  }, error = function(e) {
    skip("API do Banco Central indisponível no momento do teste.")
  })

  df_fila <- projetar_fila_bpc(dt_macro = dt_macro, horizonte_meses = 6L)

  df_atuarial <- executar_motor_atuarial(df_final_engine = df_fila)

  expect_true(is.data.frame(df_atuarial))
  expect_true("Estoque_Ativo" %in% names(df_atuarial))
  expect_false(anyNA(df_atuarial$Estoque_Ativo))

  expect_true(all(c("PCD", "IDOSO") %in% unique(df_atuarial$especie)))
})

test_that("O calculo de impactos financeiros e decomposicao respeita o rigor matematico", {
  dt_macro <- tryCatch({
    obter_dados_macro()
  }, error = function(e) {
    skip("API do Banco Central indisponível no momento do teste.")
  })

  df_fila <- projetar_fila_bpc(dt_macro = dt_macro, horizonte_meses = 6L)
  df_atuarial <- executar_motor_atuarial(df_final_engine = df_fila)

  tabela_sm_teste <- tibble::tibble(
    Ano = 2026:2027,
    SM = c(1621, 1741)
  )

  resultado <- calcular_impactos_financeiros(
    df_fluxo_detalhado = df_atuarial,
    tabela_sm = tabela_sm_teste
  )

  expect_type(resultado, "list")
  expect_named(resultado, c("resumo_mensal", "impactos_var"))

  df_imp <- resultado$impactos_var
  expect_true(is.data.frame(df_imp))

  expect_true(is.na(df_imp$Impacto_Variacao_Fisico[1]))
  expect_true(is.na(df_imp$Impacto_Variacao_SM[1]))

  if (nrow(df_imp) > 1L) {
    expect_false(is.na(df_imp$Impacto_Variacao_Fisico[2]))
    expect_false(is.na(df_imp$Impacto_Variacao_SM[2]))
  }
})

test_that("As funcoes do modulo de consulta retornam dados estruturados corretamente", {
  # 1. Histórico de Fila Processada
  df_hf <- consultar_historico_fila()
  expect_true(is.data.frame(df_hf))
  expect_gt(nrow(df_hf), 0L)

  # 2. Histórico de Quadrantes
  df_q <- consultar_historico_quadrantes()
  expect_true(is.data.frame(df_q))
  expect_gt(nrow(df_q), 0L)

  # 3. Parâmetros de Revisão
  df_rev <- consultar_parametros_revisao()
  expect_true(is.data.frame(df_rev))
  expect_gt(nrow(df_rev), 0L)

  # 4. Parâmetros da Tábua Atuarial
  df_tab <- consultar_parametros_tabua()
  expect_true(is.data.frame(df_tab))
  expect_gt(nrow(df_tab), 0L)

  # 5. Valores Executados Históricos
  df_exec <- consultar_valores_executados()
  expect_true(is.data.frame(df_exec))
  expect_gt(nrow(df_exec), 0L)

  # 6. Metadados do Pacote
  meta <- listar_metadados_pacote()
  expect_type(meta, "list")
  expect_named(meta, c("versao_pacote", "bases_disponiveis", "descricao"))
})
