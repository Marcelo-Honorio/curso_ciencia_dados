##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines", "reshape2", "PerformanceAnalytics", "nortest", "rgl",
             "car", "olsrr", "jtools", "ggstance", "GGally")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

library(tidyverse)
library(plotly)
library(car)
library(fastDummies)
library(GGally)
library(PerformanceAnalytics)
library(jtools)
library(nortest)
library(pROC)
library(rpart)
library(rpart.plot)
library(rattle)
library(olsrr)
library(caret)
##################################################################################
#                               REGRESSÃO LINEAR SIMPLES                         #
#                       EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS               #
##################################################################################
#Listar os arquivos do nosso project
list.files()

#Carregando a base de dados
load(file = "tempodist.RData")

#################################################################################
#                    OBSERVANDO OS DADOS CARREGADOS DA BASE tempodist           #
#################################################################################
glimpse(tempodist) #Visualização das observações e das especificações referentes
#às variáveis da base de dados

#Estatísticas univariadas
summary(tempodist)

#################################################################################
#                       PLOTANDO AS OBSERVAÇÕES DA BASE tempodist               #
#################################################################################
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "darkorchid") +
    xlab("Distância") +
    ylab("Tempo") +
    theme_classic()
)

#################################################################################
#   ELABORANDO UMA MATRIZ DE CORRELAÇÕES PARA AS VARIÁVEIS DA BASE tempodist    #
#################################################################################
#Elaborando uma Matriz de Correlações
cor(tempodist)

#################################################################################
#            MODELAGEM DE UMA REGRESSÃO LINEAR SIMPLES PARA O EXEMPLO 01        #
#################################################################################
#Estimando o modelo
modelo_simples <- lm(formula = tempo ~ distancia,
                       data = tempodist)

#Observando os parâmetros do modelo_simples
summary(modelo_simples)

#Calculando os intervalos de confiança

confint(modelo_simples, level = 0.90) # siginificância 10%
confint(modelo_simples, level = 0.95) # siginificância 5%
confint(modelo_simples, level = 0.99) # siginificância 1%

#Plotando o Intervalo de Confiança de 90%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "orange") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.90) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    theme_bw()
)

#Plotando o Intervalo de Confiança de 95%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "orange") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    theme_bw()
)

#Plotando o Intervalo de Confiança de 99%
ggplotly(
  ggplot(tempodist, aes(x = distancia, y = tempo)) +
    geom_point(color = "orange") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.99) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    theme_bw()
)

#Fazendo predições em modelos OLS - e.g.: qual seria o tempo gasto, em média, para
#percorrer a distância de 20km?
predict(object = modelo_simples,
        data.frame(distancia = 20))

#Caso se queira as predições com os IC:
predict(object = modelo_simples,
        data.frame(distancia = 20),
        interval = "confidence", level = 0.95)

#####################################################################################
#                             Analisando o Modelo                                   #
#####################################################################################

#Analisando o modelo
tempodist <- tempodist |> 
  mutate(fit.values = modelo_simples$fitted.values,
         erro = modelo_simples$residuals)

#Soma do erro quadrado
sum(modelo_simples$residuals^2)

#pacote Metrics
sse(tempodist$tempo, modelo_simples$fitted.values)

rmse(tempodist$tempo, modelo_simples$fitted.values) #Erro quadratico médio

#####################################################################################
#     NOVA MODELAGEM PARA O EXEMPLO 01, COM NOVO DATASET QUE CONTÉM REPLICAÇÕES     #
#####################################################################################

#replicar o dataset
tempodistnovo <- rbind(tempodist, tempodist, tempodist)

# Reestimando o modelo
modelo_simplesnovo <- lm(formula = tempo ~ distancia,
                           data = tempodistnovo)

#Observando os parâmetros do modelo_simplesnovo
summary(modelo_simplesnovo)

#Calculando os novos intervalos de confiança
confint(modelo_simplesnovo, level = 0.95) # siginificância 5%


#Plotando o Novo Gráfico com Intervalo de Confiança de 95%
ggplotly(
  ggplot(tempodistnovo, aes(x = distancia, y = tempo)) +
    geom_point(color = "orange") +
    geom_smooth(aes(color = "Fitted Values"),
                method = "lm", 
                level = 0.95) +
    labs(x = "Distância",
         y = "Tempo") +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    theme_bw()
)

#Analisando modelo
sse(tempodistnovo$tempo, modelo_simplesnovo$fitted.values)

rmse(tempodistnovo$tempo, modelo_simplesnovo$fitted.values) #Erro quadratico médio

##################################################################################
#                           AVALIANDO OS TERMOS DE ERRO                          #
#                                   EXEMPLO 1                                    #
##################################################################################
summary(tempodist)

#Plot erro vs. fitted values
plot(tempodist$erro ~ tempodist$fit.values)

#add horizontal line from 0
abline(h = 0, lty = 2)

# Linearidade dos termos de erro (pacote car)
residualPlots(modelo_simples)

#################################################################################
#            MODELAGEM DE UMA REGRESSÃO LINEAR MULTIPLA PARA O EXEMPLO 01       #
#################################################################################

# adicionando colunas
estudante <- c('Gabriela', 'Dalila', 'Gustavo', 'Letícia', 'Luiz', 
               'Leonor', 'Ana', 'Antônio', 'Júlia', 'Mariana')
semafaro <- c(0, 1, 0, 1, 2, 1, 0, 3, 1, 1)

tempodist <- cbind(estudante, tempodist, semafaro)

#Estimando o modelo com mais variáveis
modelo_multiplo <- lm(tempo ~ distancia + semafaro, data = tempodist)

#Observando os parâmetros do modelo_multiplo
summary(modelo_multiplo)

#Calculando os intervalos de confiança
confint(modelo_multiplo, level = 0.90) # siginificância 10%
confint(modelo_multiplo, level = 0.95) # siginificância 5%
confint(modelo_multiplo, level = 0.99) # siginificância 1%

#comparando os modelo com R-ajustado
summary(modelo_simples)$adj.r.squared
summary(modelo_multiplo)$adj.r.squared

#Analisando modelo (soma do erro quadrado e rmse)
sse(tempodist$tempo, modelo_simples$fitted.values)
sse(tempodist$tempo, modelo_multiplo$fitted.values)

rmse(tempodist$tempo, modelo_simples$fitted.values)
rmse(tempodist$tempo, modelo_multiplo$fitted.values)

# adicionando colunas
periodo <- c('manha', 'manha', 'manha', 'tarde', 'tarde', 'manha', 
             'manha', 'tarde', 'manha', 'manha')
perfil <- c('calmo', 'moderado', 'moderado', 'agressivo', 'agressivo', 
            'moderado', 'calmo', 'calmo', 'moderado', 'moderado')

tempodist <- cbind(tempodist, periodo, perfil)

# transformando variaveis qualitativas em dummies
df_b <- dummy_cols(tempodist, select_columns = c('periodo', 'perfil'), remove_first_dummy = TRUE)

#removendo a coluna transformada
df_b <- dummy_cols(tempodist, 
                   select_columns = c('periodo', 'perfil'), 
                   remove_first_dummy = TRUE,
                   remove_selected_columns = TRUE)

modelo_multiplo2 <- lm(tempo ~ . -estudante, data = df_b)

#Observando os parâmetros do modelo_multiplo
summary(modelo_multiplo2)

#Calculando os intervalos de confiança
confint(modelo_multiplo2, level = 0.90) # siginificância 10%
confint(modelo_multiplo2, level = 0.95) # siginificância 5%
confint(modelo_multiplo2, level = 0.99) # siginificância 1%

#comparando os modelos multiplos com R-ajustado
summary(modelo_multiplo)$adj.r.squared
summary(modelo_multiplo2)$adj.r.squared

#Analisando os modelos multiplos (soma do erro quadrado e rmse)
sse(tempodist$tempo, modelo_multiplo$fitted.values)
sse(tempodist$tempo, modelo_multiplo2$fitted.values)

rmse(tempodist$tempo, modelo_multiplo$fitted.values)
rmse(tempodist$tempo, modelo_multiplo2$fitted.values)

##################################################################################
#                                 PROCEDIMENTO STEPWISE                          #
##################################################################################
load(file = "empresas.RData")

glimpse(empresas) #Visualização das observações e das especificações referentes
#às variáveis da base de dados

#Estatísticas univariadas
summary(empresas)

#A função pairs() cria scatters para todas as variáveis métricas
pairs(empresas[, 2:6], pch = 19, col = "darkorchid")

#A função chart.Correlation() apresenta as distribuições das variáveis,
#scatters, valores das correlações e suas respectivas significâncias
chart.Correlation((empresas[2:6]), histogram = TRUE)

#Estimando a Regressão Múltipla
modelo_empresas <- lm(formula = retorno ~ . - empresa,
                      data = empresas)

#Parâmetros do modelo
summary(modelo_empresas)
#Note que o parâmetro da variável 'endividamento' não é estatisticamente
#significante ao nível de significância de 5% (nível de confiança de 95%)


#O R não elimina os parâmetros não estatisticamente significantes se não definirmos
#a abscissa da distribuição qui-quadrado correspondente  ao nível de significância
#desejado #(comumente 5%) e 1 grau de liberdade. Veja:
step_empresas <- step(modelo_empresas)

summary(step_empresas) #Note que o R não conseguiu remover a variável 'endividamento'

#qui quadrado critico de 5% e 1 grau de liberdade
qchisq(0.95, 1)

#Aplicando o procedimento Stepwise corrigido, temos, portanto, o seguinte código:
step_multiplo2 <- step(modelo_empresas, k = 3.841459)

summary(step_empresas)

#Agora sim, o R conseguiu remover a variável 'endividamento'. Note que a variável
#'disclosure' também acabou sendo excluída após o procedimento Stepwise, nesta
#forma funcional linear!
export_summs(step_empresas, scale = F, digits = 5)

#Parâmetros reais do modelo com procedimento Stepwise
plot_summs(step_empresas, colors = "darkorchid") #função plot_summs do pacote ggstance
confint(step_empresas, level = 0.95) # siginificância 5%

#Parâmetros padronizados
plot_summs(step_empresas, scale = TRUE, colors = "darkorchid")

#Adicionando a caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "darkorchid")

#Comparando os ICs dos betas dos modelos sem e com procedimento Stepwise
plot_summs(modelo_empresas, step_empresas, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95)

#intervalo de conficiente
confint(modelo_multiplo2)

##################################################################################
#            TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE        #
##################################################################################
#Shapiro-Wilk: n <= 30 do pacote nortest
## shapiro.test(modelo$residuals)

#Shapiro-Francia: n > 30 do pacote nortest - Objetivo é (p > 0,5)
sf.test(step_empresas$residuals)

#Plotando os resíduos do modelo step_empresas
ggplotly(
  empresas %>%
    mutate(residuos = step_empresas$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(color = "bisque4", 
                   fill = "orange", 
                   bins = 30,
                   alpha = 0.6) +
    labs(x = "Resíduos",
         y = "Frequência") + 
    theme_bw()
)

#Acrescentando uma curva normal teórica para comparação entre as distribuições
ggplotly(
  empresas %>%
    mutate(residuos = step_empresas$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(aes(y = after_stat(density)), 
                   color = "bisque4", 
                   fill = "orange", 
                   bins = 30,
                   alpha = 0.6) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(step_empresas$residuals),
                              sd = sd(step_empresas$residuals)),
                  aes(color = "Curva Normal Teórica"),
                  linewidth = 1) +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    labs(x = "Resíduos",
         y = "Frequência") +
    ylim(0, 0.10) +
    theme_bw()
)

##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(empresas$retorno) #função powerTransform pertence ao pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
empresas["bcretorno"] <- (((empresas$retorno ^ lambda_BC$lambda) - 1) / 
                            lambda_BC$lambda)

#Estimando um novo modelo múltiplo
modelo_bc <- lm(formula = bcretorno ~ . -empresa -retorno, 
                data = empresas)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_modelo_bc <- step(modelo_bc, k = 3.841459)

summary(step_modelo_bc)
#Note que a variável 'disclosure' acaba voltando ao modelo na forma funcional não linear!

#Verificando a normalidade dos resíduos do modelo step_modelo_bc
sf.test(step_modelo_bc$residuals)

#Plotando os novos resíduos do step_modelo_bc
ggplotly(
  empresas  |> 
    mutate(residuos = step_modelo_bc$residuals) %>%
    ggplot(aes(x = residuos)) +
    geom_histogram(aes(y = ..density..), 
                   color = "bisque4", 
                   fill = "orange", 
                   bins = 30,
                   alpha = 0.6) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(step_modelo_bc$residuals),
                              sd = sd(step_modelo_bc$residuals)),
                  aes(color = "Curva Normal Teórica"),
                  size = 1) +
    scale_color_manual("Legenda:",
                       values = "darkorchid") +
    labs(x = "Resíduos",
         y = "Frequência") +
    theme_bw()
)

# Linearidade dos termos de erro nos dois modelos (step_empresas e step_modelo_bc)
residualPlots(step_empresas)
residualPlots(step_modelo_bc)

#Resumo dos dois modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
summary(step_empresas)
summary(step_modelo_bc)

export_summs(step_empresas, step_modelo_bc, scale = F, digits = 6)
#função export_summs do pacote jtools

#Parâmetros reais do modelo com procedimento Stepwise e Box-Cox
plot_summs(step_modelo_bc, colors = "darkorchid") #função plot_summs do pacote ggstance
confint(step_modelo_bc, level = 0.95) # siginificância 5%

#Parâmetros padronizados
plot_summs(step_modelo_bc, scale = TRUE, colors = "darkorchid")

#Adicionando caracterização da distribição normal no IC de cada parâmetro beta
plot_summs(step_modelo_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "darkorchid")

#Comparando os ICs do betas dos modelos sem e com Transformação de Box-Cox
plot_summs(step_empresas, step_modelo_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95)


#Fazendo predições no step_modelo_bc, e.g.: qual é o valor do retorno, em
#média, para disclosure igual a 50, liquidez igual a 14 e ativo igual a 4000,
#ceteris paribus?
predict(object = step_modelo_bc, 
        data.frame(disclosure = 50, 
                   liquidez = 14, 
                   ativos = 4000),
        interval = "confidence", level = 0.95)

#Resposta
(((3.702015 * -0.02256414) + 1)) ^ (1 / -0.02256414)


## Plotando um erro
empresas["fitted_step"] <- step_modelo_bc$fitted.values

empresas["residuos_step"] <- step_modelo_bc$residuals

empresas  |> 
  ggplot() +
  geom_point(aes(x = fitted_step, y = residuos_step),
             color = "orange") +
  labs(x = "Fitted Values of stepwise",
       y = "Residuals of stepwise") +
  theme_bw()

##################################################################################
#           DIAGNÓSTICO DE HETEROCEDASTICIDADE EM MODELOS DE REGRESSÃO           #
#                   EXEMPLO 05 - CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################

#Sistema de Avaliação da Educação Básica (Saeb)
load(file = "saeb_rend.RData")

##################################################################################
#               OBSERVANDO OS DADOS CARREGADOS DA BASE saeb_rend                 #
##################################################################################
#rendimento = Taxa de aprovação em cada ano ou série dos anos iniciais

#Estatísticas univariadas
summary(saeb_rend)

#Tabela de frequências absolutas das variáveis 'uf' e rede'
table(saeb_rend$uf)
table(saeb_rend$rede)

#Plotando saeb em função de rendimento, com linear fit
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb)) +
    geom_point(size = 1, color = "deepskyblue") +
    geom_smooth(method = "lm", 
                color = "dodgerblue4", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

#Plotando saeb em função de rendimento, com cores por rede escolar 
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

#Plotando saeb em função de rendimento, com cores e linear fit por rede escolar 
ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = rede, shape = rede)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

##################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO E                       #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################

modelosaeb <- lm(formula = saeb ~ rendimento,
                 data = saeb_rend)

summary(modelosaeb)

saeb_rend$residuo <- modelosaeb$residuals

ols_test_breusch_pagan(modelosaeb)
#Presença de heterocedasticidade -> omissão de variável(is) preditora(s) relevante(s)

#################################################################################
#              PROCEDIMENTO N-1 DUMMIES PARA UNIDADES FEDERATIVAS               #
#################################################################################
saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)

##################################################################################
#             ESTIMAÇÃO DO MODELO DE REGRESSÃO MÚLTIPLA COM DUMMIES E            #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #
##################################################################################
modelosaeb_dummies_uf <- lm(formula = saeb ~ rendimento + uf,
                            data = saeb_rend)

modelosaeb_dummies_uf <- lm(formula = saeb ~ . -municipio -codigo -escola -rede,
                            data = saeb_rend_dummies_uf)

summary(modelosaeb_dummies_uf)

#teste breusch_pagan
ols_test_breusch_pagan(modelosaeb_dummies_uf)

ggplotly(
  ggplot(saeb_rend, aes(x = rendimento, y = saeb, color = uf, shape = uf)) +
    geom_point(size = 1) +
    geom_smooth(method = "lm", se = F) +
    xlab("rendimento") +
    ylab("saeb") +
    theme_classic()
)

##############################################################################
#                   REGRESSÃO LOGÍSTICA BINÁRIA - PARTE CONCEITUAL           #
##############################################################################
#Estabelecendo uma função para a probabilidade de ocorrência de um evento
curva_evento <- function(x){
  y = 1 / (1 + exp(-x))
}

#Plotando a curva sigmóide teórica de ocorrência de um evento para um range
#do logito entre -5 e +5
data.frame(x = -5:5) %>% 
  ggplot() +
  stat_function(aes(x = x, color = "Evento"), 
                fun = curva_evento,
                size = 1.2) +
  geom_hline(yintercept = 0.5, linetype = "dotted") +
  scale_color_manual("Legenda:",
                     values = c("orange", "darkorchid")) +
  labs(x = "Logito",
       y = "Probabilidade") +
  theme_bw()

##############################################################################
#                       REGRESSÃO LOGÍSTICA BINÁRIA                          #                  
#                EXEMPLO 01 - CARREGAMENTO DA BASE DE DADOS                  #
##############################################################################
atrasado <- readRDS("atrasado.rds")

#Estatísticas descritivas univariadas da base de dados
summary(atrasado)

#Tabela de frequências absolutas da variável 'atrasado'
table(atrasado$atrasado) 

# Estimação de modelo Logístico Binário
modelo_atrasos <- glm(formula = atrasado ~ dist + sem, 
                      data = atrasado, 
                      family = "binomial")

#Parâmetros do modelo_atrasos
summary(modelo_atrasos) #Note que não há a explicitação do estatística geral
#do modelo, nem do valor de LL e nem dos intervalos de confiança.

#Outro modo de apresentar os outputs do modelo
summ(modelo_atrasos, confint = T, digits = 3, ci.width = .95) #função summ do pacote jtools
export_summs(modelo_atrasos, scale = F, digits = 6) #função export_summs do pacote jtools

#Extração do valor do LL
logLik(modelo_atrasos)

#Fazendo predições para o modelo_atrasos. Exemplo: qual a probabilidade média
#de se chegar atrasado quando o trajeto tem 7 km e passa-se por 10 semáforos no percurso?
predict(object = modelo_atrasos, 
        data.frame(dist = 7, sem = 10), 
        type = "response")

##############################################################################
#               EXEMPLO 01 - CONSTRUÇÃO DE UMA MATRIZ DE CONFUSÃO            #
##############################################################################
# Adicionando os valores previstos de probabilidade da base de dados
atrasado$probfit <- modelo_atrasos$fitted.values

#Matriz de confusão
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.5, 
                      atrasado$atrasado == 1)[2:1, 2:1]) 
#função confusionMatrix do pacote caret#

#Matriz de confusão testando outro cutoff
confusionMatrix(table(predict(modelo_atrasos, type = "response") >= 0.4, 
                      atrasado$atrasado == 1)[2:1, 2:1]) 

##############################################################################
#                       EXEMPLO 01 - CONSTRUÇÃO DA CURVA ROC                 #
##############################################################################
ROC <- roc(response = atrasado$atrasado, 
           predictor = modelo_atrasos$fitted.values)

ggplotly(
  ggroc(ROC, color = "darkorchid", size = 1) +
    geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), 
                 color="orange", 
                 size = 0.2)+
    labs(x = "1 - Especificidade",
         y = "Sensitividade",
         title = paste("Área abaixo da curva:", 
                       round(ROC$auc, 3))) +
    theme_bw()
    )


################################################################################
#                               Arvore de Decisao                              #                              
#                                                                              #
#                                 EXEMPLO 01 - SPAM                            #
#                          VARIÁVEL DEPENDENTE CATEGÓRICA                      #
#                         VARIÁVEIS PREDITORAS CATEGÓRICAS                     #
################################################################################

#Carregando a base de dados:
load("spam.RData")

#Estabelecendo nossa primeira CART:
CART_emails <- rpart(formula = classificacao ~ . -id,
                     data = spam,
                     control = rpart.control(minsplit = 1),
                     parms = list(split = "gini"),
                     method = "class")

#Plotando nossa primeira CART:
rpart.plot(x = CART_emails,
           type = 5,
           main = "Classificação de E-mails", 
           box.palette = "-RdYlGn", 
           extra = 2)

################################################################################
#                               EXEMPLO 02 - ATRASOS                           #
#                 APROFUNDAMENTO NO PASSO A PASSO DO ALGORITMO                 #
################################################################################

#carregando os dados
atrasado <- readRDS("atrasado.rds")

#Observando o dataset:
atrasado  |>  
  ggplot() +
  geom_point(aes(x = dist, y = sem, 
                 color = factor(atrasado), shape = factor(atrasado)), size = 2) +
  labs(x = "Distância",
       y = "Quantidade de Semáforos",
       color = "Houve atraso?",
       shape = "Houve atraso?") +
  theme_bw()

#transformando em factor a variávle atrasado
atrasado <- atrasado |> 
  mutate(atrasado = factor(atrasado))

#Estatísticas univariadas descritivas e tabelas de frequências das variáveis:
summary(atrasado)

#Estimando a CART
CART_atrasos <- rpart(formula = atrasado ~ . -estudante, 
                      data = atrasado,
                      control = rpart.control(minsplit = 1),
                      method = "class")

#Plotando a CART: (pacote rattl)e
fancyRpartPlot(CART_atrasos,
               type = 3,
               sub = "Classificação de Atrasos")

rpart.plot(x = CART_atrasos,
           type = 1,
           main = "Classificação de Atrasos")

#Salvando os valores preditos do modelo CART_atrasos na base de dados:
atrasado["fitted_CART_01"] <- predict(object = CART_atrasos,
                                                newdata = atrasado,
                                      type = "class")

confusionMatrix(predict(object = CART_atrasos,
                        newdata = atrasado,
                        type = "class"),
                atrasado$atrasado)