# Rodando o pacote --------------------------------------------------------
install.packages('basedosdados')

library(basedosdados)

# Vou setar meu billing-id, que é o id do projeto:
set_billing_id("<Projeto-ID>")

# Procurar uma base de interesse ------------------------------------------

# Vamos no site escolher uma base:
# https://basedosdados.org/

# Download de direto ------------------------------------------------------

# Crio uma query SQL simples: AND OR
query <- "
  SELECT * FROM `basedosdados.xxxxxxx` 
  "

esgotos_acre <- read_sql(query)

download(query, path = "dados_esgoto.csv")

# Na primeira vez que rodar é esperado que o R ative a interface com o google
# e faça meu login. Se não funcionar:
bigrquery::bq_auth()


# Carregar o tidyverse
library(tidyverse)

# escolher a base
nome_base <- "br_me_rais.microdados_vinculos"

base_remota <- basedosdados::bdplyr(nome_base)

base_remota

# filtar a base

base_remota_preparada <- base_remota  |>  
  filter(id_municipio == '4104808' & ano == 2020) |> 
  head(100)

# posso também verificar o que está sendo feito com o comando show_query do
# {dplyr}
dplyr::show_query(base_remota_preparada)

# Coletar os dados ou salvar em disco -------------------------------------

# Depois de realizar as operações que quiser, posso "coletar" os dados, ou seja
# mandar baixar do servidor para a memória do meu computador:
dados <- bd_collect(base_remota_preparada)

# Posso também salvar diretamente em disco:
bd_write_rds(base_remota_preparada, path = "dados/dados.rds")

# Ou usar qualquer outra função de escrita:
# exemplo salvado em .xlsx
bd_write(
  base_remota_preparada,
  .write_fn = writexl::write_xlsx,
  path = "dados/base_salva.xlsx"
)


## Referências ------------------------------------------------------------

#Apresentação
# (https://www.youtube.com/watch?v=d2mPTI6-KmE&ab_channel=LatinR)

#Canal do base dos dados
# (https://www.youtube.com/@BasedosDados)








# PASSO 1: criar usuário e projeto no BigQuery

# PASSO 2: criar arquivo de credenciais e salvar numa pasta
# https://console.cloud.google.com/apis/credentials/serviceaccountkey?project=<project_id>
# service account name: admin
# role: project owner
