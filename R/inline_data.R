
# Código para criar objetos com dados p/ uso no corpo do relatório



# Funções Definidas pelo Usuário ------------------------------------------

# Diferenciação (créditos: tsibble)
difference <- function(x, lag = 1, differences = 1, default = NA) {
  diff_x <- diff(x, lag = lag, differences = differences)
  c(rep(default, lag * differences), diff_x)
}



# Seção headline ----------------------------------------------------------


# 1º destaque: valor, texto, data
headline_variacao <- dados$ipca_cheio |>
  dplyr::filter(variavel == "Var. % mensal", data == max(data)) |>
  dplyr::mutate(
    texto = dplyr::case_when(
      valor > 0 ~ "acelera",
      valor == 0 ~ "estabiliza em",
      valor < 0 ~ "desacelera"
      )
    )


# 1º destaque: ritmo de aceleração
headline_ritmo_mensal <- dados$ipca_cheio |>
  dplyr::filter(variavel == "Var. % mensal", !is.na(valor)) |>
  dplyr::mutate(
    sinal = sign(valor),
    contagem = data.table::rowid(data.table::rleid(sinal)),
    texto = dplyr::case_when(
      sinal > 0 & contagem == 1 ~ paste0("sendo este o ", contagem, "º aumento no período recente"),
      sinal > 0 & contagem > 1 ~ paste0("sendo este o ", contagem, "º aumento consecutivo"),
      sinal == 0 & contagem == 1 ~ paste0("sendo esta a ", contagem, "ª estabilização no período recente"),
      sinal == 0 & contagem > 1 ~ paste0("sendo esta a ", contagem, "ª estabilização consecutiva"),
      sinal < 0 & contagem == 1 ~ paste0("sendo esta a ", contagem, "ª desaceleração no período recente"),
      sinal < 0 & contagem > 1 ~ paste0("sendo esta a ", contagem, "ª desaceleração consecutiva")
      )
    ) |>
  dplyr::slice_tail(n = 1)


# 2º destaque: texto e valor
headline_ritmo_12m <- dados$ipca_cheio |>
  dplyr::filter(variavel == "Var. % acum. 12m", !is.na(valor))|>
  dplyr::mutate(
    sinal = sign(difference(valor)),
    contagem = data.table::rowid(data.table::rleid(sinal)),
    texto = dplyr::case_when(
      sinal > 0 & contagem == 1 ~ paste0("inicia uma tendência de alta em relação ao período anterior, registrando"),
      sinal > 0 & contagem > 1 ~ paste0("segue em tendência de alta, registrando"),
      sinal == 0 & contagem >= 1  ~ paste0("se estabiliza em "),
      sinal < 0 & contagem == 1 ~ paste0("inicia uma tendência de queda em relação ao período anterior, registrando"),
      sinal < 0 & contagem > 1 ~ paste0("segue em tendência de queda, registrando")
      )
    ) |>
  dplyr::slice_tail(n = 1)


# 3º destaque: valor positivo/negativo e texto
headline_subitem <- dados$ipca_subitens |>
  tidyr::pivot_wider(
    id_cols     = c("data", "subitem", "codigo"),
    names_from  = "variavel",
    values_from = "valor"
    ) |>
  dplyr::filter(`Var. % mensal` %in% c(max(`Var. % mensal`), min(`Var. % mensal`))) |>
  dplyr::mutate(subitem = stringr::str_to_lower(subitem, locale = "pt_BR")) |>
  dplyr::arrange(dplyr::desc(`Var. % mensal`))


# 4º destaque: valor e texto
headline_meta <- dados$meta |>
  dplyr::filter(
    lubridate::year(data) == dados$expectativas$DataReferencia,
    lubridate::month(data) == max(lubridate::month(data))
    ) |>
  dplyr::mutate(
    texto = dplyr::case_when(
      dados$expectativas$Mediana > meta ~ "acima da",
      dados$expectativas$Mediana == meta ~ "igual a",
      dados$expectativas$Mediana < meta ~ "abaixo da"
      )
    )


