#################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
library(tidyverse)
library(cluster)
library(dendextend)
library(factoextra) 
library(gridExtra)
################################################################################
#                                                                              #
#                       CLUSTER HIERARQUICO -  alunos                          #
#                                                                              #
################################################################################

#LEITURA DOS DADOS
alunos <- readxl::read_excel("alunos_pap.xlsx")

#EXPLORANDO OS DADOS
glimpse(alunos)

#COLOCAR A IDENTIFICACAO NAS LINHAS
alunos <- alunos |> 
  column_to_rownames(var='Aluno')

#Estabelecendo a matriz de dissimilaridades
d <- dist(alunos, method = "euclidean")
d

#DEFININDO O CLUSTER A PARTIR DO METODO ESCOLHIDO
#metodos disponiveis "average", "single" e "complete"
hc1 <- hclust(d, method = "single" ) #menor
hc2 <- hclust(d, method = "complete" ) #maior
hc3 <- hclust(d, method = "average" ) #media


#DESENHANDO O DENDOGRAMA
plot(hc1, cex = 0.8, hang = -1)
plot(hc2, cex = 0.8, hang = -1)
plot(hc3, cex = 0.8, hang = -1)


#BRINCANDO COM O DENDOGRAMA PARA 2 GRUPOS
plot(hc1, cex = 0.8, hang = -1)
rect.hclust(hc1, k = 3)

#COMPARANDO DENDOGRAMAS
#comparando o metodo complete e average
dend2 <- as.dendrogram(hc2)
dend3 <- as.dendrogram(hc3)
dend_list <- dendlist(dend2, dend3) 

#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend3, dend4, main = paste("Emaranhado =", round(entanglement(dend_list),2)))

#agora comparando o metodo single com complete
dend1 <- as.dendrogram(hc1)
dend2 <- as.dendrogram(hc2)
dend_list2 <- dendlist(dend1, dend2) 

#EMARANHADO, quanto menor, mais iguais os dendogramas sao
tanglegram(dend1, dend2, main = paste("Emaranhado =", round(entanglement(dend_list2),2)))

################################################################################
#                                                                              #
#                       CLUSTER HIERARQUICO - MCDonald                         #
#                                                                              #
################################################################################

#Carregar base de dados: 
mcdonalds <- read.table("MCDONALDS.csv", sep = ";", dec = ",", header = T)

#transformar o nome dos lanches em linhas
mcdonalds <- mcdonalds |> 
  column_to_rownames(var = 'Lanche')

#Padronizar variaveis
mcdonalds.padronizado <- scale(mcdonalds)

#calcular as distancias da matriz utilizando a distancia euclidiana
distancia <- dist(mcdonalds.padronizado, method = "euclidean")

#Calcular o Cluster: metodos disponiveis "average", "single", "complete" e "ward.D"
cluster.hierarquico <- hclust(distancia, method = "single" )

# Dendrograma
plot(cluster.hierarquico, cex = 0.6, hang = -1)

# Aqui foi utilizado o pacote factoextra e sua função
# fviz_dend para a criação do dendograma
fviz_dend(cluster.hierarquico, cex = 0.5)

## QUANTOS GRUPOS VOCES VEM?

#Criar o grafico e destacar os grupos
plot(cluster.hierarquico, cex = 0.6, hang = -1)
rect.hclust(cluster.hierarquico, k = 4)

#VERIFICANDO ELBOW E SILHOUTTE (para definir o número de clusters)
#COM A FUNCÃO fviz_nbclust() DO PACOTE factoextra
fviz_nbclust(mcdonalds.padronizado, FUN = hcut, method = "wss") #ELBOW
fviz_nbclust(mcdonalds.padronizado, FUN = hcut, method = "silhouette") #SILHOUTT

#criando 4 grupos de lanches
grupo_lanches4 <- cutree(cluster.hierarquico, k = 4)
table(grupo_lanches4)

#adicionando uma variável que informe os respectivos cluster
mcdonalds_final <- mcdonalds |> 
  mutate(cluster_hcut = cutree(cluster.hierarquico, k = 4))

################################################################################
#                                                                              #
#                           DENDROGRAMAS PERSONALIZADOS                        #
#                                                                              #
################################################################################
#Visualizando os clusters formados
fviz_dend(cluster.hierarquico, 
          k = 4,
          k_colors = c("orange", "darkorchid", "bisque4"),
          color_labels_by_k = F,
          rect = T, 
          rect_fill = T,
          lwd = 1,
          ggtheme = theme_bw())

dend <- as.dendrogram(cluster.hierarquico)

# gráfico (left)
dend  |>  
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) |> 
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) |> 
  plot(horiz=TRUE, axes=FALSE)
abline(v = 350, lty = 2)

# Highlight a cluster with rectangle
par(mar=c(9,1,1,1))
dend |> 
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) |> 
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) |> 
  plot(axes=FALSE)
rect.dendrogram(dend, k=3, lty = 5, lwd = 0, x=1, col=rgb(0.1, 0.2, 0.4, 0.1) ) 

################################################################################
#                                                                              #
#                   CLUSTER NAO HIERARQUICO - MCDonald                         #
#                                                                              #
################################################################################

#Rodar o modelo
mcdonalds.k2 <- kmeans(mcdonalds.padronizado, centers = 2)

#Visualizar os clusters
fviz_cluster(mcdonalds.k2, data = mcdonalds.padronizado, main = "Cluster K2")

#Criar clusters
mcdonalds.k3 <- kmeans(mcdonalds.padronizado, centers = 3)
mcdonalds.k4 <- kmeans(mcdonalds.padronizado, centers = 4)
mcdonalds.k5 <- kmeans(mcdonalds.padronizado, centers = 5)

#Criar graficos
G1 <- fviz_cluster(mcdonalds.k2, geom = "point", data = mcdonalds.padronizado) + ggtitle("k = 2")
G2 <- fviz_cluster(mcdonalds.k3, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 3")
G3 <- fviz_cluster(mcdonalds.k4, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 4")
G4 <- fviz_cluster(mcdonalds.k5, geom = "point",  data = mcdonalds.padronizado) + ggtitle("k = 5")

#Imprimir graficos na mesma tela
grid.arrange(G1, G2, G3, G4, nrow = 2)

#VERIFICANDO ELBOW E SILHOUTTE
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "wss")
fviz_nbclust(mcdonalds.padronizado, kmeans, method = "silhouette")

#adicionando uma variável que informa os respectivos cluster
mcdonalds_final <- mcdonalds |> 
  mutate(cluster_kmeans = mcdonalds.k2$cluster)

# Cálculo da média utilizando a função aggregate
aggregate(mcdonalds, by=list(cluster=mcdonalds.k2$cluster), mean)

#plot k-means personalizados
fviz_cluster(mcdonalds.k2, data=mcdonalds_final,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal())

################################################################################
#                                                                              #
#                 CLUSTER NAO HIERARQUICO - MUNICIPIOS                         #
#                                                                              #
################################################################################

#carregar base municipio
municipios <- read_delim("municipios.csv", delim = ';', 
                         locale=locale(encoding="latin1", decimal_mark = ","))


################################################################################
#                                                                              #
#                               k-means FOTO                                   #
#                                                                              #
################################################################################
library(jpeg)

# Neste exemplo iremos utilizar o K-médias para fazer um agrupamento de pixels 
# de uma imagem. O agrupamento será feito de acordo com as cores do pixel. Cada 
# pixel passará então a ser representado pelo centróide do cluster. Assim, 
# o número de clusters será o número de cores de cada imagem.

## carregando JPEG - a imagem é um array 588x880x3 
imagem <- readJPEG('foto.jpg')

# As duas primeira dimensões indicam o tamanho da imagem e a última dimensão
# indica as camadas de cor (red/green/blue)

## organizando a imagem em data frame
imagemRGB <- tibble(
  x = rep(1:dim(imagem)[2], each = dim(imagem)[1]),
  y = rep(dim(imagem)[1]:1, dim(imagem)[2]),
  R = as.vector(imagem[, , 1]),
  G = as.vector(imagem[, , 2]),
  B = as.vector(imagem[, , 3])
)

k <- 10 # especificando o valor de k

kMeans <- kmeans(imagemRGB[, c("R", "G", "B")], centers = k)

# visualizando
imagemRGB |> 
  mutate(kColours = rgb(kMeans$centers[kMeans$cluster, ])) |> 
  ggplot(aes(x = x, y = y, color = I(kColours))) +
  geom_point(show.legend = F) +
  labs(title = paste("K-Means (k=", k , "cores)")) +
  theme_minimal()