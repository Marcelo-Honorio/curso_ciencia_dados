library(tidyverse)
#1
imdb_prejuizo <- imdb |> 
  mutate(prejuizo = receita - orcamento) |> 
  filter(prejuizo < 0 & !is.na(prejuizo)) |> 
  arrange(desc(prejuizo))

#2
imdb |> 
  group_by(direcao) |> 
  summarise(sum_duracao = sum(duracao),
            titulo_max = titulo[which.max(duracao)]) |> 
  arrange(desc(sum_duracao)) |> 
  select(direcao, titulo_max) |> 
  head(1)

#3
imdb |> 
  group_by(producao) |> 
  summarise(sum_titulo = length(titulo),
            titulo_imdb = titulo[which.max(nota_imdb)],
            nota_imdb = nota_imdb[which.max(nota_imdb)]) |> 
  filter(sum_titulo >= 100) |> 
  arrange(desc(sum_titulo))

#4
imdb |> 
  mutate(num_ator = map_int(str_split(elenco, ','), length)) |> 
  group_by(ano) |> 
  summarise(media_elenco = mean(num_ator, na.rm = T))
  
#5
imdb |> 
  group_by(direcao) |> 
  summarise(diferenca_nota = max(nota_imdb) - min(nota_imdb),
            nota_max = max(nota_imdb),
            nota_min = min(nota_imdb)) |> 
  arrange(desc(diferenca_nota)) |> 
  head(1)

#6
imdb |> 
  mutate(num_idiomas = map_int(str_split(idioma, ','), length)) |> 
  filter(num_idiomas > 1) |> 
  nrow()

#7
imdb |> 
  separate(data_lancamento, c('ano', 'mes', 'dia'), sep = '-') |> 
  group_by(mes) |> 
  summarise(media_lancamento = mean(length(titulo))) |> 
  arrange(desc(media_lancamento))

#8
dir_rot_imdb <- imdb |> 
  filter(direcao == roteiro)

#9
imdb |> 
  separate(data_lancamento, c('ano', 'mes', 'dia'), sep = '-') |> 
  group_by(dia) |> 
  summarise(media_lancamento = mean(length(titulo))) |> 
  arrange(desc(media_lancamento))

imdb |> 
  separate(data_lancamento, c('ano', 'mes', 'dia'), sep = '-') |> 
  group_by(dia) |> 
  summarise(nota_mediana = median(nota_imdb)) |> 
  arrange(nota_mediana)

#10
imdb |> 
  filter(is.na(orcamento) | is.na(receita)) |> 
  nrow()

#11
imdb |> 
  filter(!is.na(orcamento) & !is.na(receita)) |> 
  arrange(as.Date(data_lancamento)) |> 
  select(data_lancamento, titulo) |> 
  head(1)

