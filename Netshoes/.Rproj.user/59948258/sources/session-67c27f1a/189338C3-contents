#> Instalação das bibliotecas necessárias
install.packages("tidyverse")
install.packages("stringr")
install.packages("readxl")
install.packages("factoextra") # utilizado para modelo de cluster



#> Uso das bibliotecas necessarias
library(tidyverse)
library(stringr)
library(readxl)
library(factoextra)


#>1) Carregar os dados
dados <- read_excel("netshoes_dados_import.xlsx")

#>2) Limpeza os dados
#> Para limpeza de dados foi necessario criar scripts em python para excluir linhas com dados sujos por exemplo: na coluna nome veio linha com 
#> aspas duplas, tbm veio nomes de forma de pagamentos da descrição do produto. Sendo assim, foi necessario excluir essas linhas

#>3) Analise exploratoria dos dados
#> Para entender melhor os dados a primeira função a ser usada será: str(dados) para verificar os tipos dos dados. 
str(dados)
#>(nome dessa imagem é str)

#>Como essa função é possível verificar que os dados são: numericos e caracteres
#>
#> 3.1) Visualização das distribuições
#> Como os dados são continuos podemos utilizar um histograma para a verificação da distribuição dos dados

ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Valor), binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Histograma dos Valores dos Tênis", x = "Preço (R$)", y = "Contagem") +
  theme_minimal()
#>(nome dessa imagem é histograma)

#> Através do grafico é possível perceber que a maioria dos tenis estão e uma faixa de ZERO a 500 reais. 

#>  Outra alternativa para exibir a distribuição de uma variável contínua é usando o boxplot 

ggplot(data = dados, mapping = aes(y = Valor)) +
  geom_boxplot()

#>(nome dessa imagem é boxplot)
#>
#>É possível observar que existe uma grande quantidade de valores zero.
#> A quantidade de valores zero observados nos dados pode ser atribuída a duas causas 
#> principais. Primeiramente, durante o processo de raspagem de dados utilizando a técnica de 
#> web scraping, o script pode não ter concedido tempo suficiente para que a barra de rolagem carregasse todos os produtos na página. Isso pode resultar na captura de informações incompletas, levando a registros com valores nulos ou zero. Em segundo lugar, algumas descrições de tênis podem simplesmente não ter
#> informações de preço disponíveis, o que também contribuiria para a presença de zeros nos dados.
#> Esses fatores destacam a importância de garantir uma coleta de dados robusta e a necessidade de verificar a integridade das informações obtidas durante o processo de raspagem.
#> Como o objetivo do trabalho é fazer uma segmentação utilizando os preços, vamos excluir as informações 
#> que contém prços com valor ZERO

# Excluindo linhas com valores ZERO
dados <- dados %>%
  filter(Valor != 0)

#> 4) Preparação para Clustering
#> 4.1) Seleção de variáveis
#> Como vamos focar apenas no preço, selecionamos essa variável para o clustering.

# Selecionar a variável de preço
# Selecionar a variável de preço
dados_num <- dados %>%
  select(Valor)

#> 4.2) Normalização dos Dados
#> A normalização é importante para que a variável de preço contribua de maneira adequada para o clustering.

# Normalizar os dados
dados_norm <- scale(dados_num)

#> 4.3) Determinação do Número de Clusters
#> Foi utilizadado os métodos como o Elbow Method ou a Silhouette Analysis para determinar o número ideal de clusters.
#> 


# Calcular o número de clusters usando o Elbow Method
fviz_nbclust(dados_norm, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Ajuste conforme o gráfico
  labs(title = "Elbow Method para Determinação do Número de Clusters", x = "Número de Cluster", y= "Total dentro do quadro")

#>(nome dessa imagem é Elbow_Method)
#>O Elbow Method sugere que 3 é o número ideal de clusters porque após esse ponto, a diminuição 
#>na WSS começa a se estabilizar (formando um "cotovelo" no gráfico). A linha vertical 
#>tracejada ajuda a visualizar exatamente onde esse ponto ocorre.
#>


# Calcular o número de clusters usando Silhouette Analysis
fviz_nbclust(dados_norm, kmeans, method = "silhouette") +
  labs(title = "Silhouette Analysis para Determinação do Número de Clusters", x = "Numero de cluster", y = "Largura média da silhueta")

#>(nome dessa imagem é Silhouette_Analysis)
#>
#> O número ideal de clusters geralmente corresponde ao valor de k que maximiza a média do coeficiente 
#> de silhueta. Este valor indica o agrupamento que, em média, tem os pontos mais bem ajustados
#>ao seu próprio cluster e mais mal ajustados aos outros clusters
#>

#> 5) Aplicação do Algoritmo de Clustering
#> 
#> Vamos definir o número de clusters como 3. 

# Definir o número de clusters 
set.seed(123)  # Para reprodutibilidade
kmeans_result <- kmeans(dados_norm, centers = 3, nstart = 25)

# Adicionar os clusters aos dados originais
dados$Cluster <- kmeans_result$cluster

#> 6) Análise e Interpretação dos Clusters

# 6.1) Resumo dos clusters
dados %>%
  group_by(Cluster) %>%
  summarise(
    Quantidade = n(),
    Media_Preco = mean(Valor),
    Min_Preco = min(Valor),
    Max_Preco = max(Valor)
  )

# (nome dessa imagem é Resumo_clusters) 

# 6.2) Visualização dos Clusters

# Visualização dos clusters nos dados
# (nome dessa imagem é tabela_cluster) 

# Visualização dos clusters com Boxplot
ggplot(dados, aes(x = factor(Cluster), y = Valor, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(title = "Boxplot dos Clusters de Preço dos Tênis",
       x = "Cluster",
       y = "Preço (R$)") +
  theme_minimal()
# (nome dessa imagem é Visualizacao_clusters) 

# histogramas separados para cada cluster
ggplot(data = dados, aes(x = Valor)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  facet_wrap(~ Cluster, ncol = 1) +  # Cria uma coluna de histogramas, um para cada cluster
  labs(title = "Distribuição de Preços dos Tênis por Cluster",
       x = "Preço (R$)",
       y = "Contagem") +
  theme_minimal()

# (nome dessa imagem é histograma_por_cluster)

#> 7) Segmentação dos Produtos
#> Com os cluters identificados foi possível segmentar os produtos conforme suas caractristicas de preços

#> 7.1) Segmentação dos prosutos
#> Baseado nas estatísticas dos clusters, pode-se definir:
#> Cluster 1: Preços mais baixos (Acessível)
#> Cluster 2: Preços intermediários
#> Cluster 3: Preços mais altos (Premium)


#> 7.2) Exportar os Resultados
#> Exportação dos dados segmentados para análises futuras
#> 
# Exportar para um arquivo CSV
write.csv(dados, "clustered_tenis.csv", row.names = FALSE)
































