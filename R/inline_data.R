
# Código para criar objetos com dados p/ uso no corpo do relatório



# Funções Definidas pelo Usuário ------------------------------------------

# Diferenciação (créditos: tsibble)
difference <- function(x, lag = 1, differences = 1, default = NA) {
  diff_x <- diff(x, lag = lag, differences = differences)
  c(rep(default, lag * differences), diff_x)
}



# Principais destaques ----------------------------------------------------

# 1º destaque: valor, texto, data
headline_variacao <- dados$ipca_cheio |>
  dplyr::arrange(data) |>
  dplyr::filter(variavel == "Var. % mensal") |>
  dplyr::mutate(
    texto = dplyr::case_when(
      valor > 0 ~ "acelera",
      valor == 0 ~ "estabiliza em",
      valor < 0 ~ "desacelera"
      )
    ) |>
  dplyr::slice_tail(n = 2)


# 1º destaque: ritmo de aceleração
headline_ritmo_mensal <- dados$ipca_cheio |>
  dplyr::filter(variavel == "Var. % mensal", !is.na(valor)) |>
  dplyr::mutate(
    sinal = sign(valor),
    contagem = data.table::rowid(data.table::rleid(sinal)),
    texto = dplyr::case_when(
      sinal > 0 & contagem == 1 ~ paste0("Este é o ", contagem, "º aumento no período recente"),
      sinal > 0 & contagem > 1 ~ paste0("Este é o ", contagem, "º aumento consecutivo"),
      sinal == 0 & contagem == 1 ~ paste0("Esta é a ", contagem, "ª estabilização no período recente"),
      sinal == 0 & contagem > 1 ~ paste0("Esta é a ", contagem, "ª estabilização consecutiva"),
      sinal < 0 & contagem == 1 ~ paste0("Esta é a ", contagem, "ª desaceleração no período recente"),
      sinal < 0 & contagem > 1 ~ paste0("Esta é a ", contagem, "ª desaceleração consecutiva")
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


# 3º destaque: valor e texto
headline_nucleos <- dados$nucleos |>
  dplyr::arrange(data) |>
  dplyr::filter(variavel == "Média dos núcleos") |>
  dplyr::slice_tail(n = 2) |>
  dplyr::mutate(
    texto = dplyr::case_when(
      valor > 0 ~ "acelerou",
      valor == 0 ~ "se estabilizou em",
      valor < 0 ~ "desacelerou"
      )
    )


# 4º destaque: valor positivo/negativo e texto
headline_subitem <- dados$ipca_subitens |>
  tidyr::pivot_wider(
    id_cols     = c("data", "subitem", "codigo"),
    names_from  = "variavel",
    values_from = "valor"
    ) |>
  dplyr::filter(`Var. % mensal` %in% c(max(`Var. % mensal`), min(`Var. % mensal`))) |>
  dplyr::mutate(subitem = stringr::str_to_lower(subitem, locale = "pt_BR")) |>
  dplyr::arrange(dplyr::desc(`Var. % mensal`))


# 5º destaque: valor e texto
headline_difusao <- dados$difusao |>
  dplyr::mutate(
    media = mean(x = difusao, na.rm = TRUE),
    sinal = sign(difference(difusao)),
    texto1 = dplyr::case_when(
      sinal == 1 ~ "avançou para",
      sinal == 0 ~ "está estável em",
      sinal == -1 ~ "retrocedeu para"
      ),
    texto2 = dplyr::case_when(
      difusao > media ~ "acima da",
      difusao == media ~ "em linha com a",
      difusao < media ~ "abaixo da"
      )
    ) |>
  dplyr::arrange(data) |>
  dplyr::slice_tail(n = 2)


# 6º destaque: valor e texto
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



# Grupos ------------------------------------------------------------------

# Variação mensal última observação
grupos_mensal <- dados$ipca_grupos |>
  dplyr::filter(data == max(data), variavel == "Var. % mensal") |>
  dplyr::arrange(valor, grupo) |>
  dplyr::mutate(
    grupo = forcats::as_factor(grupo),
    cor   = dplyr::if_else(valor > 0, colors_42[1], colors_42[2])
    )


# Variação mais positiva/negativa
grupos_max <- grupos_mensal |>
  dplyr::filter(valor %in% c(max(valor), min(valor))) |>
  dplyr::arrange(valor) |>
  dplyr::mutate(
    grupo = stringr::str_to_lower(grupo, locale = "pt"),
    sinal = sign(valor),
    texto = dplyr::case_when(
      sinal == 1 ~ "subindo",
      sinal == -1 ~ "caindo",
      sinal == 0 ~ "de"
      )
    )


# Contagem de grupos por sinal de variação mensal
grupos_sinal <- grupos_mensal |>
  dplyr::count(sinal = sign(valor)) |>
  dplyr::arrange(dplyr::desc(sinal)) |>
  dplyr::mutate(
    texto = dplyr::case_when(
      n > 1 & n != dplyr::n_distinct(grupos_mensal$grupo) ~ paste0(
        n, " grupos tiveram variação mensal positiva em ",
        formatar_data(grupos_max$data[1]),
        " com destaque para o grupo ",
        grupos_max$grupo[2], " que subiu ",
        formatar_num(grupos_max$valor[2]), "%"
        ),
      n == dplyr::n_distinct(grupos_mensal$grupo) ~ paste0(
        "todos tiveram variação mensal positiva em ",
        formatar_data(grupos_mensal$data[1]),
        " com destaque para o grupo ", formatar_num(grupos_max$valor[2]),
        grupos_max$grupo[2], " que subiu ",
        formatar_num(grupos_max$valor[2]), "%"
        ),
      n == 1 ~ paste0(
        "apenas ", n, " grupo teve variação mensal positiva em ",
        formatar_data(grupos_mensal$data[1]),
        " com aumento de ", formatar_num(grupos_max$valor[2]),
        "% (", grupos_max$grupo[2], ")"
        ),
      n == 0 ~ paste0(
        "nenhum grupo teve variação mensal positiva em ",
        formatar_data(grupos_max$data[1])
        )
      )
    )


# Variação acum. 12 meses
grupos_12m <- dados$ipca_grupos |>
  dplyr::filter(variavel == "Var. % acum. 12m", data == max(data)) |>
  dplyr::filter(valor %in% c(max(valor), min(valor))) |>
  dplyr::arrange(valor) |>
  dplyr::mutate(grupo = stringr::str_to_lower(grupo, locale = "pt"))


# Pressão de subitem (peso)
subitem_peso <- dados$ipca_subitens |>
  tidyr::pivot_wider(
    id_cols     = c("subitem", "codigo"),
    names_from  = "variavel",
    values_from = "valor"
    ) |>
  dplyr::filter(Peso == max(Peso)) |>
  dplyr::mutate(subitem = stringr::str_to_lower(subitem)) |>
  dplyr::rename("var_mensal" = "Var. % mensal")



# Núcleos -----------------------------------------------------------------

# Contagem de nucleos por sinal de variação mensal
nucleos_sinal <- dados$nucleos |>
  dplyr::filter(data == max(data), variavel != "Média dos núcleos") |>
  dplyr::count(sinal = sign(valor)) |>
  dplyr::arrange(dplyr::desc(sinal)) |>
  dplyr::mutate(
    texto = dplyr::case_when(
      n == 5 ~ "todas tiveram",
      n == 0 ~ "nenhuma teve",
      n == 1 ~ paste0(n, " teve"),
      TRUE ~ paste0(n, " tiveram")
      )
    )


# Variações mensais e acumuladas em 12m
nucleos_variacoes <- dados$nucleos |>
  dplyr::filter(variavel == "Média dos núcleos") |>
  dplyr::arrange(data) |>
  dplyr::mutate(
    var_acum_12m = (
      RcppRoll::roll_prodr(
        x = valor / 100 + 1,
        n = 12,
        na.rm = TRUE
        ) - 1
      ) * 100,
    texto1 = dplyr::case_when(
      valor > 0 ~ "acelerou",
      valor == 0 ~ "se estabilizou em",
      valor < 0 ~ "desacelerou"
      ),
    sinal = sign(difference(var_acum_12m)),
    contagem = data.table::rowid(data.table::rleid(sinal)),
    texto2 = dplyr::case_when(
      sinal > 0 & contagem == 1 ~ paste0("iniciando uma tendência de alta em relação ao período anterior"),
      sinal > 0 & contagem > 1 ~ paste0("seguindo em tendência de alta"),
      sinal == 0 & contagem == 1  ~ paste0("iniciando uma tendência de estabilidade"),
      sinal == 0 & contagem > 1  ~ paste0("seguindo com tendência de estabilidade"),
      sinal < 0 & contagem == 1 ~ paste0("iniciando uma tendência de queda em relação ao período anterior"),
      sinal < 0 & contagem > 1 ~ paste0("seguindo em tendência de queda")
      )
    ) |>
  dplyr::slice_tail(n = 1)



# Difusão -----------------------------------------------------------------

# Sinal, valor e tendência
difusao_tendencia <- dados$difusao |>
  dplyr::arrange(data) |>
  dplyr::mutate(
    sinal = sign(difference(difusao)),
    contagem = data.table::rowid(data.table::rleid(sinal)),
    texto = dplyr::case_when(
      sinal > 0 & contagem == 1 ~ paste0("iniciando uma tendência de alta em relação ao período anterior"),
      sinal > 0 & contagem > 1 ~ paste0("em tendência de alta"),
      sinal == 0 & contagem == 1  ~ paste0("iniciando uma tendência de estabilidade"),
      sinal == 0 & contagem > 1  ~ paste0("em tendência de estabilidade"),
      sinal < 0 & contagem == 1 ~ paste0("iniciando uma tendência de queda em relação ao período anterior"),
      sinal < 0 & contagem > 1 ~ paste0("em tendência de queda")
      )
    ) |>
  dplyr::slice_tail(n = 1)



# Classificações ----------------------------------------------------------

# Variações e texto
classificacao_variacoes <- dados$classificacoes |>
  dplyr::filter(variavel %in% c("Livres", "Monitorados")) |>
  dplyr::group_by(variavel) |>
  dplyr::arrange(data) |>
  dplyr::mutate(
    var_acum_12m = (
      RcppRoll::roll_prodr(
        x = valor / 100 + 1,
        n = 12,
        na.rm = TRUE
        ) - 1
      ) * 100,
    texto = dplyr::case_when(
      valor > 0 ~ "acelerou",
      valor == 0 ~ "se estabilizou em",
      valor < 0 ~ "desacelerou"
      )
    ) |>
  dplyr::ungroup() |>
  dplyr::slice_tail(n = 2)
