##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
install.packages('PerformanceAnalytics')

################################################################################
#                                                                              #
#                          ESTIMATIVAS DE LOCALIZAÇÃO                          #
#                                                                              #
################################################################################



#média

#média aparada

#média ponderada

#mediana

################################################################################
#                                                                              #
#                         ESTIMATIVAS DE VARIABILIDADE                         #
#                                                                              #
################################################################################

#desvio absoluto médio

#variância

#desvio-padrão

#desvio absoluto mediano

#IQR

#BOX-PLOTS
ggplot(diamante) +
  geom_boxplot(aes(x=corte, y = preco, fill = corte), alpha = 0.3, 
               outlier.colour="red",
               outlier.fill="red",
               outlier.size=1) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="BuPu") +
  xlab("")

#whiskers do box-plot


#tabela de frequência


#histograma com linha
ggplot(diamante) +
  geom_histogram(aes(x=preco), color="#e9ecef", fill="#69b3a2", alpha=0.6, position = 'identity') +
  geom_vline(aes(xintercept = mean(preco)),linewidth = 1.2) +
  geom_vline(aes(xintercept = median(preco)),linewidth = 1.2, color = "red") +
  theme_bw()

#frequencia de variaveis categorias

################################################################################
#                                                                              #
#                  CORRELAÇÃO E COMPARTIMENTACAO HEXAGONAL                     #
#                                                                              #
################################################################################


#correlação

#matriz de correlação

#gráficos correlacionais com a função chart.Correlation() do pacote 
#'PerformanceAnalytics":
chart.Correlation(mtcarros, histogram = T)

#gráficos correlacionais com a função corrplot() do pacote 
#'corrplot":
corrplot()

# compartimentação hexagonal com geom_hex()
ggplot(diamante) +
  geom_hex(aes(x=quilate, y=preco)) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()


################################################################################
#                                                                              #
#                        OUTLIERS e VALORES AUSENTES                           #
#                                                                              #
################################################################################

#calculo do Zscore

#plot dos autliers encontrados (Zscore > 3)

#calculo do Zscore ROBUSTO

##plot dos autliers encontrados (Zscore_R > 3)

#eliminando todos os NA com drop_na()

#utilizar a função summary() para apresentar resumo por variável 

#substituindo os NA pelo valor médio

#determinar o valor que NA vai assumir com replace_na()

################################################################################
#                                                                              #
#                             DADOS DESBALANCEADOS                             #
#                                                                              #
################################################################################

#preparando o dataset
df_desbalanceado <-  pessoas |> 
  select(peso, altura, rebatedor) |> 
  filter(rebatedor != "B") |> 
  drop_na() |> 
  mutate(across(1:2, as.numeric))


#Excluir aleatoriamente de algumas instâncias de classe majoritária com a
# função ovun.sample do pacote ROSE
under <- ovun.sample(rebatedor~., data=df_desbalanceado, method = "under")$data


#criar sinteticamente novas observações da classe minoritária com a
# função ovun.sample do pacote ROSE
over <- ovun.sample(rebatedor~., data=df_desbalanceado, method = "over")$data

#criar observações intermediárias entre dados parecidos com a
# função SMOTE do pacote smotefamily
SMOTE <- SMOTE(pessoas[, -3], pessoas$rebatedor, K = 7)$data


################################################################################
#                                                                              #
#                             VER TAMBÉM                                       #
#                                                                              #
################################################################################

#DataExplorer (https://boxuancui.github.io/DataExplorer/)
install.packages('DataExplorer')


