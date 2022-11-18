################################################################################
#                                                                              #
#                               FERRAMENTAS GRÁFICAS                           #
#                                                                              #
################################################################################

#Pacotes e serem utilizados:

install.packages(c("plotly","sf", "brazilmaps", "esquisse"))

################################################################################
#                                GRÁFICOS DE BARRAS                            #
################################################################################
library(tidyverse)

#Carregando a base de dados
load("perfil_investidor.RData")

#Observando os dados
head(perfil_investidor)
glimpse(perfil_investidor)

#Aplicando a sintaxe básica do ggplot2 à base de dados
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil))

#Note que a nomenclatura dos perfis foi ordenada de forma alfabética. Para
#alterar a situação, podemos utilizar a função factor():

perfil_investidor <- perfil_investidor |> 
  mutate(perfil = factor(perfil, levels = c("Conservador", "Moderado", "Agressivo")))

#Utilizando, mais uma vez, a sintaxe básica do ggplot2 à base de dados:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil))


#Adicionando informações ao nosso gráfico:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil)) +
  labs(title = "Perfil dos Investidores do Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade")

#Adicionando detalhamentos ao gráfico:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil)) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

#Adicionando cores à plotagem:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), fill = "darkorchid") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

#Para verificar as cores possíveis de serem declaradas com a linguagem básica
#R, basta:
colours()
demo("colors")

#Alterando as cores:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), fill = "orange") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

#Adicionando bordas:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "darkorchid", fill = "orange") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020")

#Modificando o fundo dos gráficos:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "darkorchid", fill = "orange") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  theme_light()

#Adicionando labels:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "darkorchid", fill = "orange") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count") +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  theme_light()

#Reposicionando as labels:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "darkorchid", fill = "orange") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", vjust = -1) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  theme_light()

#Girando um gráfico:
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = perfil), color = "darkorchid", fill = "orange") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", hjust = -1) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  coord_flip() +
  theme_light()

#Invertendo as ordens das categorias (fct_rev()):
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = fct_rev(perfil)), color = "darkorchid", fill = "orange") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", hjust = -1) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  coord_flip() +
  theme_light()

#colocar em ordem crescente de frequencia - fct_infreq()
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = fct_infreq(perfil)), color = "darkorchid", fill = "orange") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", hjust = -1) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  coord_flip() +
  theme_light()

#colocar em ordem decrescente de frequencia  - fct_inorder()
ggplot(data = perfil_investidor) +
  geom_bar(aes(x = fct_inorder(perfil)), color = "darkorchid", fill = "orange") +
  geom_text(aes(x = perfil, label = ..count..), stat = "count", hjust = -1) +
  labs(title = "Perfil dos Investidores",
       subtitle = "Banco X",
       x = "Perfil do Investidor",
       y = "Quantidade",
       caption = "Período: 2020") +
  coord_flip() +
  theme_light()

################################################################################
#                                    HISTOGRAMA                                #
################################################################################

#Carregando os dados:
#load("dados_sp.RData")


#Observando os dados:
glimpse(dados_sp)

#Aplicando a sintaxe básica do ggplot2:
ggplot(data = dados_sp) +
  geom_histogram(aes(x = idh))

#Colorindo o gráfico:
ggplot(data = dados_sp) +
  geom_histogram(aes(x = idh), fill = "darkorchid")

#Alterando o plano de fundo:
ggplot(data = dados_sp) +
  geom_histogram(aes(x = idh), fill = "darkorchid") +
  theme_minimal()

#Adicionando contornos e nomes dos eixos:
ggplot(data = dados_sp) +
  geom_histogram(aes(x = idh), fill = "darkorchid", color = "black") +
  labs(x = "IDH dos Municípios de SP",
       y = "Frequência") +
  theme_minimal()

#Alterando a quantidade de caixas do histograma:
ggplot(data = dados_sp) +
  geom_histogram(aes(x = pib), fill = "darkorchid", color = "black", bins = 100) +
  labs(x = "PIB dos Municípios de SP",
       y = "Frequência") +
  theme_minimal()

################################################################################
#                                GRÁFICOS DE PONTOS                            #
################################################################################

#Carregando os dados
#load("atlas_ambiental.RData")

#Observando os dados:
glimpse(atlas_ambiental)

#Aplicando a sintaxe básica do ggplot2:
ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade))

#Estratificando informações - o uso do argumento size:
ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, size = idade))

#Estratificando informações - o uso do argumento color:
ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, size = idade, color = favel < 6))

#Estratificando informações - o uso do argumento shape:
ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, 
                 size = idade, color = favel < 6, 
                 shape = mortalidade > 18)) +
  labs(title = "Indicadores dos Distritos do Município de São Paulo",
       x = "Renda",
       y = "Escolaridade") +
  theme_bw()

#Traçando uma linha de fitted values e IC:
ggplot(atlas_ambiental) +
  geom_point(aes(x = renda, y = escolaridade, 
                 size = idade, color = favel < 6, 
                 shape = mortalidade > 18)) +
  geom_smooth(aes(x = renda, y = escolaridade), method = "loess", se = FALSE) +
  labs(title = "Indicadores dos Distritos do Município de São Paulo",
       x = "Renda",
       y = "Escolaridade") +
  theme_bw()

################################################################################
#                                GRÁFICOS DE LINHAS                            #
################################################################################

#Carregando a base de dados
#load("covid_23072020.RData")

#Observado os dados
glimpse(dados_covid)

#Aplicando a sintaxe básica do ggplot2:
ggplot(dados_covid) +
  geom_line(aes(x = t, y = cumulative_cases))

#Note que não informamos para o ggplot2 que cada linha deveria representar um
#país. Assim:
ggplot(dados_covid) +
  geom_line(aes(x = t, y = cumulative_cases, color = country))

#Adicionando informações e deixando o gráfico mais elegante:
ggplot(dados_covid) +
  geom_line(aes(x = t, y = cumulative_cases, color = country)) +
  geom_point(aes(x = t, y = cumulative_cases, color = country)) +
  labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
       y = "Casos Cumulativos",
       color = "País") +
  theme_bw()

#O gráfico anterior poderia ser mais informativo, visto que, por exemplo, 
#compara países com tamanhos populacionais distintos. Portanto, a magnitude das
#infecções também são distintas. Uma possibilidade de suavização da situação
#seria a padronização da variável 'cumulative_cases'. No gráfico a seguir,
#padronizamos os casos cumulativos de cada país numa escala log10:

dados_covid <- dados_covid |> 
  mutate(log_ccases = log(cumulative_cases))

#O gráfico resultante é o seguinte:
ggplot(dados_covid) +
  geom_line(aes(x = t, y = log_ccases, color = country)) +
  geom_point(aes(x = t, y = log_ccases, color = country)) +
  labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
       y = "Log10 Casos Cumulativos",
       color = "País") +
  theme_bw()

#Por mais que dê para se extrair alguns insights interessantes do gráfico 
#anterior, alguém poderia dizer que seria melhor, por exemplo, utilizar uma
#proporção da população infectada em razão do tempo passado:

dados_covid <- dados_covid |> 
  mutate(pop_ccases = cumulative_cases/pop)

#O resultado visual vem abaixo:
ggplot(dados_covid) +
  geom_line(aes(x = t, y = pop_ccases, color = country)) +
  geom_point(aes(x = t, y = pop_ccases, color = country)) +
  labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
       y = "Casos Cumulativos",
       color = "País") +
  theme_bw()

#Ainda assim, os gráficos não trazem uma informação importante: os valores dos
#casos cumulativos por dia e por país. Em regra, no ggplot, podemos utilizar
#a geometria 'text' para isso:
ggplot(dados_covid) +
  geom_line(aes(x = t, y = cumulative_cases, color = country)) +
  geom_point(aes(x = t, y = cumulative_cases, color = country)) +
  geom_text(aes(x = t, y = cumulative_cases, label = cumulative_cases), 
            color = "black", size = 2) +
  labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
       y = "Casos Cumulativos",
       color = "País") +
  theme_bw()

#E se deslocássemos as labels dos valores dos casos cumulativos?
ggplot(dados_covid) +
  geom_line(aes(x = t, y = cumulative_cases, color = country)) +
  geom_point(aes(x = t, y = cumulative_cases, color = country)) +
  geom_text(aes(x = t, y = cumulative_cases, label = cumulative_cases), 
            color = "black", size = 2, vjust = -1) +
  labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
       y = "Casos Cumulativos",
       color = "País") +
  theme_bw()

#Ainda está caótico, certo? E se alterássemos o ângulo de exibição das labels?
ggplot(dados_covid) +
  geom_line(aes(x = t, y = cumulative_cases, color = country)) +
  geom_point(aes(x = t, y = cumulative_cases, color = country)) +
  geom_text(aes(x = t, y = cumulative_cases, label = cumulative_cases), 
            color = "black", size = 2, vjust = -1, angle = 45) +
  labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
       y = "Casos Cumulativos",
       color = "País") +
  theme_bw()

#Dá até vontade de desistir, a gente sabe! Em casos extremos, como no exemplo
#apresentado, talvez seja melhor omitir as labels e deixar o gráfico interativo
#com o usuário da informação. Podemos fazer isso com a função ggplotly() do
#pacote 'plotly':
ggplotly(
  ggplot(dados_covid) +
    geom_line(aes(x = t, y = cumulative_cases, color = country)) +
    geom_point(aes(x = t, y = cumulative_cases, color = country)) +
    labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
         y = "Casos Cumulativos",
         color = "País") +
    theme_bw()
)

#Agora passa passar o mouse pelo gráfico para ter acesso às informações 
#necessárias!

#Caso quiséssemos fazer uma análise diária, bastaria mudar a variável de 
#interesse:

ggplotly(
  ggplot(dados_covid) +
    geom_line(aes(x = t, y = daily_cases, color = country)) +
    geom_point(aes(x = t, y = daily_cases, color = country)) +
    labs(x = "Tempo em dias desde o primeiro caso oficial do Sars-Cov-2 reportado",
         y = "Casos Diários",
         color = "País") +
    theme_bw()
)

################################################################################
#                                      BOXPLOT                                 #
################################################################################

#No R, para construir alguns boxplots, é melhor que utilizemos nossas bases de
#dados no formato long. Basta utilizarmos a função melt() do pacote 'reshape2':

atlas_long <- atlas_ambiental |> 
  select(2:11) |> 
  pivot_longer(!distritos, values_to = 'value')

head(atlas_long)
tail(atlas_long)

#Agora sim, podemos plotar nossos dados:
ggplot(atlas_long) +
  geom_boxplot(aes(x = name , y = value, fill = name))

#A visualização ficou diferente do esperado, não é? Você consegue dizer a
#razão disso?

#O problema está na comparação de variáveis com tipos distintos de magnitude.
#Podemos resolver a situação padronizando as variáveis com o procedimento
#zscores, por exemplo:

atlas_padronizado <- atlas_ambiental |> 
  mutate(across(c(renda:denspop), scale))


atlas_padronizado[, 3:11] <- scale(atlas_padronizado[, 3:11])

head(atlas_padronizado)
tail(atlas_padronizado)

#Como já visto, devemos transformar nossa base de dados em formato long:
atlas_padronizado_long <- atlas_padronizado |> 
  select(2:11) |> 
  pivot_longer(!distritos, values_to = 'value')

head(atlas_padronizado_long)
tail(atlas_padronizado_long)

#Vamos tentar, mais uma vez, utilizar a sintaxe básica do ggplot2 para a 
#construção de boxplots:
ggplot(atlas_padronizado_long) +
  geom_boxplot(aes(x = name, y = value, fill = name))

#Podemos deixar o gráfico mais elegante nomeando os eixos e alterando o plano
#de fundo:
ggplot(atlas_padronizado_long) +
  geom_boxplot(aes(x = name, y = value, fill = name)) +
  labs(x = "Variáveis",
       y = "Valores") +
  theme_bw()

#Com objetivos puramente didáticos, podemos visualizar cada observação de cada
#variável padronizada da seguinte maneira:
ggplot(atlas_padronizado_long) +
  geom_boxplot(aes(x = name, y = value, fill = name)) +
  geom_point(aes(x = name, y = value), alpha = 0.1) +
  labs(x = "Variáveis",
       y = "Valores") +
  theme_bw()

#E sim! Podemos deixas nossos boxplots interativos com ajuda do pacote 'plotly'
ggplotly(
  ggplot(atlas_padronizado_long) +
    geom_boxplot(aes(x = name, y = value, fill = name)) +
    labs(x = "Variáveis",
         y = "Valores") +
    theme_bw()
)
################################################################################
#                                       MAPAS                                  #
################################################################################
library(brazilmaps)
library(sf)
  
#obtendo o mapa das cidades de SP
map_sp <- get_brmap(geo = "City",
                    geo.filter = list(State = 35),
                    class = "sf")

# join dados e shapes das cidades
dados_sp_map <- dados_sp |> 
  left_join(map_sp, by = c("codigo" = "City"))


# plotando o mapa
dados_sp_map |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = idh), colour = "black", size = 0.3) +
  theme(legend.position = c(.85,.8)) +
  theme_void(14) +
  labs(fill = "IDH")
  
################################################################################
#                               FERRAMENTA ESQUISSER                           #
################################################################################
library(esquisse)

esquisser(atlas_ambiental)
