
# Código para Funções Definidas pelo Usuário



# Formatar números para impressão
formatar_num <- function(x, ...) {
  format(
    x            = x,
    digits       = 2,
    nsmal        = 2,
    scientific   = FALSE,
    big.mark     = ".",
    decimal.mark = ",",
    ...
    )
}


# Formatar datas para impressão
formatar_data <- function(x, fmt = "%B/%Y", ...) {
  format(x, format = fmt, ...)
}
