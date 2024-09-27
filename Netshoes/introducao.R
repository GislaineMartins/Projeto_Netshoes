# Entendimento dos dados
# Especificação das colunas
# STATE = estado
# ENROLL = Associação de Outono
# NAME = nome da escola
# YRDATA = data por ano 
# TOTALREV = Receita total do ensino fundamental secundario
# TFEDREV = Receita total de fontes federais
# TSTREV = Receita total de fontes estaduais
# TLOCREV = Receita total de fontes locais
# TOTALEXP = DESPESAS TOTAIS DO ENSINO FUNDAMENTAL E SECUNDÁRIO
# TCURINST = Despesas totais atuais com instrução
# TCURSSVC = Despesas correntes totais para serviços de suporte
# TCURONON = Outras despesas correntes
# TCAPOUT = Despesa total de desembolso de capital

# Perguntas para serem respondidas.
# Estimativa de Receita Total com Base em Fontes de Receita
# Pergunta: Como posso estimar a TOTALREV (Receita total) de um distrito escolar com base nas receitas federais (TFEDREV), estaduais (TSTREV) e locais (TLOCREV)?
# Modelo: Regressão Linear Múltipla.
# Formulação: TOTALREV como variável dependente e TFEDREV, TSTREV, TLOCREV como variáveis independentes.


# Instalação de bibliotecas
install.packages("ggplot2")
install.packages("dplyr")
install.packages("caTools")
install.packages("Metrics")
install.packages("caret")
install.packages('GGally')
install.packages("corrplot")

#utilizando as bibliotecas
library(ggplot2)
library(scales)
library(dplyr)
library(caTools) # biblioteca usada para dividir o conjunto de dados
library(Metrics) # biblioteca para calcular as metricas
library(caret) # biblioteca para calcular as predições
library(corrplot) # Para calcular matriz correlação

# Passo 1: Carregar os dados
dados <- read.csv("districts.csv")

# Passo 2: Explorar os dados
summary(dados)

# Verificando a estrutura dos dados (serve para verificar os tipos dos dados)
str(dados)

# Visualizar as primeiras linhas do dataset
head(dados)

# Verificar valores ausentes
sum(is.na(dados$TOTALREV))
sum(is.na(dados$TFEDREV))
sum(is.na(dados$TSTREV))
sum(is.na(dados$TLOCREV))


# Padronizando as variáveis dependentes e independentes.
dados$TOTALREV <- scale(dados$TOTALREV)
dados$TFEDREV <- scale(dados$TFEDREV)
dados$TSTREV <- scale(dados$TSTREV)
dados$TLOCREV <- scale(dados$TLOCREV)

# Fazer matriz correlação entre as variaveis
variaveis <- dados[, c("TFEDREV", "TSTREV", "TLOCREV", "TOTALREV")]
cor_matrix <- cor(variaveis, use = "complete.obs")
print(cor_matrix)


# Definir uma semente para reprodutibilidade
set.seed(42)

# Dividir os dados em treino (70%) e teste (30%)
split <- sample.split(dados$TOTALREV, SplitRatio = 0.7)
train <- subset(dados, split == TRUE)
test <- subset(dados, split == FALSE)

# Treinar o modelo de regressão linear
modelo <- lm(TOTALREV ~ TFEDREV + TSTREV + TLOCREV, data = train)

# Resumo do modelo para visualizar os coeficientes e a significância
summary(modelo)

# Predizer os valores para o conjunto de teste
predicoes <- predict(modelo, newdata = test)

# Calcular métricas de avaliação
# Calcular RMSE
rmse_val <- rmse(test$TOTALREV, predicoes)

# Calcular MAE
mae_val <- mae(test$TOTALREV, predicoes)

# Calcular R²
r2_val <- summary(modelo)$r.squared

# Exibir os resultados
cat("R²: ", r2_val, "\n")
cat("RMSE: ", rmse_val, "\n")
cat("MAE: ", mae_val, "\n")

# Interpretação dos resultados
# R2 = 1: Isso geralmente é um sinal de que o modelo se ajustou muito bem aos dados. No entanto, também pode indicar um possível overfitting, especialmente se o modelo foi treinado e avaliado no mesmo conjunto de dados ou se há alta colinearidade entre as variáveis independentes.
# RMSE: Apresenta um valor extrematente baixo.sugere que o modelo tem erros de predição quase insignificantes. Isso, junto com o R² igual a 1, reforça a possibilidade de overfitting
# MAE: O MAE é a média das diferenças absolutas entre os valores previstos e os valores reais. Assim como o RMSE, um valor de MAE extremamente baixo indica que as predições do modelo são quase idênticas aos valores reais

# Considerando que o modelo foI muito bem ajustado. Podemos dizer que pode ter acontecido um Overfitting,
# ou seja, o modelo pode ter capturado ruído nos dados e isso é comum se modelo foi treinado e testado nos mesmos dados ou se há muitas variáveis independentes.
# sendo assim, para tentar adquirir melhor resultado será feito uma validação cruzada

# Definir a semente para reprodutibilidade
set.seed(42)

# Realizar K-fold cross-validation com K=10
train_control <- trainControl(method = "cv", k = 10)
modelo_cv <- train(TOTALREV ~ TFEDREV + TSTREV + TLOCREV, data = dados, method = "lm", trControl = train_control)

# Resultados da validação cruzada
print(modelo_cv)

# Após fazer a validação cruzada e observar os resultados é possível perceber que 

# Graficos de residuos

# Gráfico dos resíduos
residuos <- residuals(modelo_cv)
fitted <- fitted(modelo_cv)


ggplot(data = NULL, aes(x = fitted, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Gráfico de Resíduos", x = "Valores Ajustados", y = "Resíduos")

# Gráfico de Predição vs Realidade

# Previsões do modelo
previsoes <- predict(modelo_cv, newdata = dados)

# Gráfico de Predição vs Realidade
ggplot(dados, aes(x = TOTALREV, y = previsoes)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Predição vs Realidade", x = "TOTALREV Real", y = "TOTALREV Previsto")


# *********************************************************************


# PARTE SOMENTE DE VISUALIZAÇÕES

# Se a distribuição dos dados for muito assimétrica ou tiver valores extremos, considere transformar os dados (por exemplo, usando uma transformação logarítmica) antes de criar o histograma

# Histograma da variável dependente TOTALREV
# Adicionando uma pequena constante para evitar log(0)
dados$TOTALREV_log <- log(dados$TOTALREV + 1)

# Criando histograma com transformação logarítmica
ggplot(dados, aes(x = dados$TOTALREV_log)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histograma de TOTALREV", x = "TOTALREV", y = "Frequência")

# Verifique a presença de outliers no log-transformado
boxplot(dados$TOTALREV_log, main="Boxplot de TOTALREV")


# Histograma das variáveis independentes
# Aplicando a transformação logaritima
dados$TFEDREV_log <- log(dados$TFEDREV + 1)

# Criando o histograma com a transformação logaritima
ggplot(dados, aes(x = TFEDREV_log)) +
  geom_histogram(binwidth = 0.1, fill = "red", color = "black") +
  labs(title = "Histograma de TFEDREV", x = "TFEDREV", y = "Frequência")

# Verifique a presença de outliers no log-transformado
boxplot(dados$TFEDREV_log, main="Boxplot de TFEDREV")


# Adicionando uma pequena constante para evitar log(0)
dados$TSTREV_log <- log(dados$TSTREV + 1)

ggplot(dados, aes(x = TSTREV_log)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Histograma de TSTREV", x = "TSTREV", y = "Frequência")

# Verifique a presença de outliers no log-transformado
boxplot(dados$TSTREV_log, main="Boxplot de TSTREV")


# Adicionando uma pequena constante para evitar log(0)
dados$TLOCREV_log <- log(dados$TLOCREV + 1)

ggplot(dados, aes(x = TLOCREV_log)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black") +
  labs(title = "Histograma de TLOCREV", x = "TLOCREV", y = "Frequência")

# Verifique a presença de outliers no log-transformado
boxplot(dados$TLOCREV_log, main="Boxplot de TLOCREV")


# GRAFICO DE MATRIZ DE DISPERSÃO ENTRE AS VARIAVEIS

# Gráfico de dispersão de TOTALREV versus TFEDREV
ggplot(dados, aes(x = TFEDREV, y = TOTALREV)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Dispersão entre TFEDREV e TOTALREV", x = "TFEDREV", y = "TOTALREV")

# Gráfico de dispersão de TOTALREV versus TSTREV
ggplot(dados, aes(x = TSTREV, y = TOTALREV)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Dispersão entre TSTREV e TOTALREV", x = "TSTREV", y = "TOTALREV")

# Gráfico de dispersão de TOTALREV versus TLOCREV
ggplot(dados, aes(x = TLOCREV, y = TOTALREV)) +
  geom_point(color = "purple") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Dispersão entre TLOCREV e TOTALREV", x = "TLOCREV", y = "TOTALREV")

# Criar o gráfico da matriz de correlação
par(mar = c(3, 1, 1, 1))  # Ajusta as margens para evitar corte do título
corrplot(cor_matrix, method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45)























 








