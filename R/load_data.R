
# Código para carregar dados em CSV

#  Lista de arquivos
arquivos_csv <- list.files(path = "data", full.names = TRUE)

# Loop para ler CSVs
dados <- purrr::map(
  .x = arquivos_csv,
  .f = readr::read_csv
  ) |>
  purrr::set_names(
    nm = stringr::str_remove_all(
      string = arquivos_csv,
      pattern = "data/|\\.csv"
      )
    )
