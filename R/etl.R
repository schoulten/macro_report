
# Código de Extração, Tratamento e Carregamento de dados


# Pacotes -----------------------------------------------------------------

# Carregar pacotes
library(nucleos) # https://analisemacropro.github.io/nucleos/
library(dplyr)
library(tidyr)
library(stringr)
library(readr)



# Extração de dados -------------------------------------------------------

# IPCA subitens (% a.m. e peso, IBGE)
dados_brutos_ipca_subitens <- nucleos::get_ipca(table = 7060)



# Tratamento de dados -----------------------------------------------------

# IPCA subitens (% a.m. e peso, IBGE)
dados_ipca_subitens <- dados_brutos_ipca_subitens |>
  nucleos::group_desc() |>
  dplyr::filter(group == "Subitem", date == max(date)) |>
  tidyr::pivot_longer(
    cols      = c("pct_change", "weight"),
    names_to  = "var",
    values_to = "val"
    ) |>
  dplyr::mutate(
    data = date,
    variavel = dplyr::case_when(
      var == "pct_change" ~ "Var. % mensal",
      var == "weight" ~ "Peso",
      TRUE ~ NA_character_
      ),
    subitem = stringr::str_sub(string = desc, start = 9, end = -1),
    subitem_codigo = code,
    valor = val,
    .keep = "none"
    )



# Salvar dados ------------------------------------------------------------

# Salvar como arquivo CSV
if (!dir.exists("data")) {dir.create("data")}
readr::write_csv(x = dados_ipca_subitens, file = "data/ipca_subitens.csv")
