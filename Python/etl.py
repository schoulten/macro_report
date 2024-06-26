
"""# Código de Extração, Tratamento e Carregamento de dados"""


# Importa bibliotecas
import pandas as pd
import numpy as np
import pathlib
import sidrapy as sidra



"""# Funções"""


# Coleta dados da API do Banco Central (SGS)
def coleta_bcb_sgs(
  codigo,
  nome, 
  data_inicio = "01/01/2000", 
  data_fim = (pd.to_datetime("today") + pd.offsets.DateOffset(months = 36)).strftime("%d/%m/%Y")
  ):

  url = (
    f"https://api.bcb.gov.br/dados/serie/bcdata.sgs.{codigo}/dados?" +
    f"formato=csv&dataInicial={data_inicio}&dataFinal={data_fim}"
    )

  try:
    print(f"Coletando a série {codigo} ({nome})")
    resposta = pd.read_csv(filepath_or_buffer = url, sep = ";", decimal = ",")
  except:
    raise Exception(f"Falha na coleta da série {codigo} ({nome})")
  else:
    return (
        resposta
        .rename(columns = {"valor": nome})
        .assign(data = lambda x: pd.to_datetime(x.data, format = "%d/%m/%Y"))
        .set_index("data")
    )



"""# Extração de dados"""


# IPCA cheio (% a.m. / acum. 12m, IBGE)
dados_brutos_ipca_cheio = sidra.get_table(
    table_code = "1737", 
    territorial_level = "1", 
    ibge_territorial_code = "all", 
    variable = "63,2265", 
    period = "all"
    )


# IPCA grupos (% a.m. / acum. 12m, IBGE)
dados_brutos_ipca_grupos = sidra.get_table(
    table_code = "7060", 
    territorial_level = "1", 
    ibge_territorial_code = "all", 
    variable = "63,2265", 
    period = "all",
    classifications = {
        "315": "7170,7445,7486,7558,7625,7660,7712,7766,7786"
        }
    )


# IPCA subitens (% a.m. e peso, IBGE)
dados_brutos_ipca_subitens = pd.concat(
  list(
    map(
      lambda x: (
        sidra.get_table(
          table_code = "7060", 
          territorial_level = "1", 
          ibge_territorial_code = "all", 
          variable = x, 
          period = "last",
          classifications = {"315": "allxt"}
          )
        ),
      ["63", "66"]
      )
    )
)


# IPCA núcleos (% a.m., BCB)
dados_brutos_nucleos = []
codigos = {
    "EX0": 11427,
    "EX3": 27839,
    "MS": 4466,
    "DP": 16122,
    "P55": 28750
}
for serie in codigos.items():
  dados_brutos_nucleos.append(coleta_bcb_sgs(serie[1], serie[0]))


# IPCA difusão (%, BCB)
dados_brutos_difusao = coleta_bcb_sgs(21379, "difusao")


# IPCA classificações (% a.m., BCB)
dados_brutos_classificacoes = []
codigos = {
    "Livres": 11428,
    "Alimentos": 27864,
    "Serviços": 10844,
    "Industriais": 27863,
    "Duráveis": 10843,
    "Semi-duráveis": 10842,
    "Não-duráveis": 10841,
    "Monitorados": 4449,
    "Comercializáveis": 4447,
    "Não comercializáveis": 4448
    }
for serie in codigos.items():
  dados_brutos_classificacoes.append(coleta_bcb_sgs(serie[1], serie[0]))


# Meta de inflação (% anual, CMN)
dados_brutos_meta = coleta_bcb_sgs(13521, "meta")


# Expectativas anuais de inflação (IPCA, % no ano, Focus/BCB)
dados_brutos_expectativas = pd.read_csv(
    "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/" +
    "ExpectativasMercadoAnuais?$filter=Indicador%20eq%20'IPCA'%20a" +
    "nd%20Data%20ge%20'2022-01-01'%20and%20baseCalculo%20eq%200&$format=text/csv",
    decimal = ","
    )



"""# Tratamento de dados"""


# IPCA cheio (% a.m / acum. 12m, IBGE)
dados_ipca_cheio = (
    dados_brutos_ipca_cheio
    .rename(columns = dados_brutos_ipca_cheio.iloc[0])
    .rename(columns = {
        "Mês (Código)": "data", 
        "Valor": "valor",
        "Variável": "variavel"
        }
      )
    .filter(items = ["data", "variavel", "valor"], axis = "columns")
    .query("valor not in ['Valor', '...']")
    .replace(
        to_replace = {
            "variavel": {
                "IPCA - Variação mensal": "Var. % mensal",
                "IPCA - Variação acumulada em 12 meses": "Var. % acum. 12m"
                }
            }
          )
    .assign(
        data = lambda x: pd.to_datetime(x.data, format = "%Y%m"),
        valor = lambda x: x.valor.astype(float)
      )
    .sort_values(["variavel", "data"])
)


# IPCA grupos (% a.m., IBGE)
dados_ipca_grupos = (
    dados_brutos_ipca_grupos
    .rename(columns = dados_brutos_ipca_grupos.iloc[0])
    .rename(columns = {
        "Mês (Código)": "data", 
        "Valor": "valor",
        "Variável": "variavel",
        "Geral, grupo, subgrupo, item e subitem": "grupo"
        }
      )
    .filter(items = ["data", "variavel", "grupo", "valor"], axis = "columns")
    .query("valor not in ['Valor', '...']")
    .replace(
        to_replace = {
            "variavel": {
                "IPCA - Variação mensal": "Var. % mensal",
                "IPCA - Variação acumulada em 12 meses": "Var. % acum. 12m"
                },
             "grupo": {r"^(\d){1}(\.)": ""}
             },
        regex = True
      )    
    .assign(
        data = lambda x: pd.to_datetime(x.data, format = "%Y%m"),
        valor = lambda x: x.valor.astype(float)
      )
    .sort_values(["data", "variavel", "grupo"])
)


# IPCA subitens (% a.m. e peso, IBGE)
dados_ipca_subitens = (
  dados_brutos_ipca_subitens
  .rename(columns = dados_brutos_ipca_subitens.iloc[0])
  .rename(columns = {
      "Mês (Código)": "data", 
      "Valor": "valor",
      "Variável": "variavel",
      "Geral, grupo, subgrupo, item e subitem": "descricao"
      }
    )
  .filter(items = ["data", "variavel", "descricao", "valor"], axis = "columns")
  .query("valor not in ['Valor', '...']")
  .replace(
      to_replace = {
          "variavel": {
              "IPCA - Variação mensal": "Var. % mensal",
              "IPCA - Peso mensal": "Peso"
              }
            }
    )
  .assign(
      data = lambda x: pd.to_datetime(x.data, format = "%Y%m"),
      snipc = lambda x: x.descricao.str.count(r"\d"),
      grupo = lambda x: np.where(x.snipc == 7, "Subitem", "outro"),
      codigo = lambda x: x.descricao.str.slice(stop = 7),
      subitem = lambda x: x.descricao.str.slice(start = 8),
      valor = lambda x: x.valor.astype(float)
    )
  .query("grupo == 'Subitem'")
  .filter(items = ["data", "variavel", "subitem", "codigo", "valor"])
)



# IPCA núcleos (% a.m., BCB)
dados_nucleos = (
    dados_brutos_nucleos[0]
    .join(other = dados_brutos_nucleos[1:], how = "outer")
    .assign(**{"Média dos núcleos": lambda x: x.mean(axis = 1, skipna = True)})
    .reset_index()
    .melt(id_vars = "data", var_name = "variavel", value_name = "valor")
    .sort_values(["data", "variavel"])
)


# IPCA difusão (%, BCB)
dados_difusao = dados_brutos_difusao.reset_index()


# IPCA classificações (% a.m., BCB)
dados_classificacoes = (
    dados_brutos_classificacoes[0]
    .join(other = dados_brutos_classificacoes[1:], how = "outer")
    .reset_index()
    .melt(id_vars = "data", var_name = "variavel", value_name = "valor")
    .sort_values(["data", "variavel"])
    .dropna()
)


# Meta de inflação (% anual, CMN)
dados_meta = (
    pd.concat([dados_brutos_meta] * 12)
    .sort_index()
    .reset_index()
    .assign(
        data = lambda x: pd.date_range(
            start = x.data.min(), 
            end = x.data.max() + pd.offsets.MonthBegin(11), 
            freq = "MS"
            )
        )
)


# Expectativas anuais de inflação (IPCA, % no ano, Focus/BCB)
dados_expectativas = (
    dados_brutos_expectativas
    .query("DataReferencia == @dados_ipca_cheio.data.dt.year.max()")
    .query("Data == Data.max()")
)



"""# Salvar dados"""


# Dicionário de tabelas tratadas
dados = {
  "classificacoes": dados_classificacoes,
  "difusao": dados_difusao,
  "ipca_cheio": dados_ipca_cheio,
  "ipca_grupos": dados_ipca_grupos,
  "ipca_subitens": dados_ipca_subitens,
  "meta": dados_meta,
  "nucleos": dados_nucleos,
  "expectativas": dados_expectativas
}


# Loop para salvar arquivos CSV
pathlib.Path("data").mkdir(exist_ok = True)
for tabela in list(dados.keys()):
  dados[tabela].to_csv(path_or_buf = "data/" + tabela + ".csv", index = False)
