
# Código de Extração, Tratamento e Carregamento de dados


# Pacotes -----------------------------------------------------------------

# Carregar pacotes
library(purrr)
library(sidrar)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)



# Extração de dados -------------------------------------------------------

# IPCA subitens (% a.m. e peso, IBGE)
url_sidra <- paste0(
  "https://apisidra.ibge.gov.br/values/t/7060/n1/all/v/63,66/p/all/c315/7173/",
  "d/v63%202,v66%204"
  )
dados_sidra <- httr2::request(url_sidra) |>
  httr2::req_perform() |>
  httr2::resp_body_json(simplifyDataFrame = TRUE)
dados_brutos_ipca_subitens <- purrr::map(
  .x = c("IPCA - Variacao mensal (%)" = 63, "IPCA - Peso mensal" = 66),
  .f = ~sidrar::get_sidra(
    x        = 7060,
    variable = .x,
    period   = "all"
    )
  )



# Tratamento de dados -----------------------------------------------------

# IPCA subitens (% a.m. e peso, IBGE)
dados_ipca_subitens <- dados_brutos_ipca_subitens |>
  dplyr::bind_rows(.id = "variavel") |>
  dplyr::mutate(
    data = lubridate::ym(`Mês (Código)`),
    variavel = dplyr::case_when(
      variavel == "IPCA - Variacao mensal (%)" ~ "Var. % mensal",
      variavel == "IPCA - Peso mensal" ~ "Peso",
      TRUE ~ NA_character_
      ),
    descricao = `Geral, grupo, subgrupo, item e subitem`,
    snipc = stringr::str_count(`Geral, grupo, subgrupo, item e subitem`, "\\d"),
    grupo = dplyr::case_when(
      snipc == 0 ~ "Geral",
      snipc == 1 ~ "Grupo",
      snipc == 2 ~ "Subgrupo",
      snipc == 4 ~ "Item",
      snipc == 7 ~ "Subitem"
      ),
    subitem = stringr::str_sub(string = descricao, start = 9, end = -1),
    valor = Valor,
    .keep = "none"
    ) |>
  dplyr::filter(grupo == "Subitem", data == max(data)) |>
  dplyr::as_tibble()



# Salvar dados ------------------------------------------------------------

# Salvar como arquivo CSV
if (!dir.exists("data")) {dir.create("data")}
readr::write_csv(x = dados_ipca_subitens, file = "data/ipca_subitens.csv")
