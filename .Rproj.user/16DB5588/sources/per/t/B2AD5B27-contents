# CÓDIGOS R – BOLETIM ESTATÍSTICO-COMERCIAL 2024
# VARIÁVEIS: ATRACAÇÕES E MOVIMENTAÇÕES (ÚLTIMOS 7 ANOS)
# Carregamento de Pacotes
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(readxl))install.packages("readxl")
library(readxl)
##
 
# Dados
MEP24_full<- read_excel("MEP_24_full.xlsx", sheet = "base2") 
MEP24_full
str(MEP24_full)

# Filtra as linhas onde SMov é diferente de 0
MEP_km <- MEP24_full %>% 
  filter(Origem != "F. NORONHA", Destino != "F. NORONHA") %>% 
  group_by(Destino) %>% 
  summarise(Atrac = n(), SMov = sum(Mov, na.rm = TRUE)) %>% 
  filter(SMov != 0)  # Exclui valores iguais a 0 (Agentes sem movimentação)

# Exibe o resultado
print(MEP_km, n = Inf)

#plota o gr�fico coluna vs coluna
plot(MEP_km[, 2:3])

#mostra a distribui��o dos gastos e da renda dos clientes - histograma
hist(MEP_km$Atrac, col = "red4")
hist(MEP_km$SMov, col = "blue4")

# Faz um gr�fico de Spend vs INCOME. A escala de cores mostra as diferentes regi�es
plot(MEP_km$Atrac, MEP_km$SMov, pch = 21, col = "green4")

#separa apenas as colunas 2 a 7 para analisar os dados
preprocessed <- MEP_km[,2:3]
preprocessed
k <- 3 # especifica o n�mero m�ximo de clusters que se deseja separar

# roda o modelo k-means
# nstart = n�mero de inicializa��es aleat�rias; a melhor ser� a usada
output <- kmeans(preprocessed, centers = k, nstart = 20)
output

# Cria o nome da coluna de cluster dinamicamente
Nome_coluna <- paste("cluster", k, sep = "_")

# Adiciona o número do cluster ao dataset
MEP_km[[Nome_coluna]] <- factor(output$cluster, levels = 1:k)

# Gráfico de clusters corrigido
cluster_graph <- ggplot(MEP_km, aes(x = Atrac, y = SMov, colour = .data[[Nome_coluna]])) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cluster Group",
    values = c('red', 'orange', 'green3', 'deepskyblue', 'blue', 'darkorchid4', 'violet', 'pink1', 'tan3', 'black')[1:k]
  ) +
  xlab("Atracações") +
  ylab("Movimentação (x mil t)") +
  scale_y_continuous(labels=scales::number_format(big.mark = ".", decimal.mark = ",",
                                                  scale = 1e-3))+
  ggtitle(paste("k-means com", k, "Clusters")) +
  theme_bw()

# Exibe o gráfico
print(cluster_graph)

# Gráfico de clusters com rótulos
cluster_graph <- ggplot(MEP_km, aes(x = Atrac, y = SMov, colour = MEP_km[[Nome_coluna]])) +
  geom_point(size = 3) +
  geom_text(aes(label = Destino), vjust = -0.5, size = 3) +  # Adiciona os rótulos com base na coluna 'Destino'
  scale_colour_manual(name = "Cluster Group", values = c('red', 'orange', 'green3', 'deepskyblue', 'blue', 'darkorchid4', 'violet', 'pink1', 'tan3', 'black')) +
  xlab("Atracações") +
  ylab("Movimentação (x mil t)") +
  scale_y_continuous(labels=scales::number_format(big.mark = ".", decimal.mark = ",",
                                                  scale = 1e-3))+
  ggtitle(paste("k-means com", k, "Clusters")) +
  theme_bw()

# Exibe o gráfico
print(cluster_graph)

# Gráfico de clusters com rótulos, cor e shape
cluster_graph <- ggplot(MEP_km, aes(x = Atrac, y = SMov, colour = .data[[Nome_coluna]], shape = .data[[Nome_coluna]])) +
  geom_point(size = 3) +  # Pontos com cor e forma baseados no cluster
  geom_text(aes(label = Destino), vjust = -0.5, size = 3) +  # Rótulos com base na coluna 'Berco'
  scale_colour_manual(
    name = "Cluster Group",
    values = c('red', 'orange', 'green3', 'deepskyblue', 'blue', 'darkorchid4', 'violet', 'pink1', 'tan3', 'black')[1:k]
  ) +
  scale_shape_manual(
    name = "Cluster Group",
    values = c(16, 17, 18, 19, 15, 4, 8, 3, 6, 7)[1:k]  # Define formas específicas para cada cluster
  ) +
  xlab("Atracações") +
  ylab("Movimentação (x mil t)") +
  scale_y_continuous(
    labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)
  ) +
  ggtitle(paste("k-means com", k, "Clusters")) +
  theme_bw()

# Exibe o gráfico
print(cluster_graph)

##
##

# CÓDIGOS R – BOLETIM ESTATÍSTICO-COMERCIAL 2024
# VARIÁVEIS: ATRACAÇÕES, MOVIMENTAÇÕES, ANO, COMPRIMENTO E CALADO DOS NAVIOS
# Carregamento de Pacotes
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(readxl))install.packages("readxl")
library(readxl)
##

# Dados
MEP24_full<- read_excel("MEP_24_full.xlsx", sheet = "base2") 
MEP24_full
str(MEP24_full)

# Filtra as linhas onde SMov é diferente de 0
MEP_km <- MEP24_full %>% 
  filter(Origem != "F. NORONHA", Destino != "F. NORONHA") %>% 
  group_by(Origem) %>% 
  summarise(
    Atrac = n(),
    SMov = sum(Mov, na.rm = TRUE),
    Ano = first(Ano),
    Compr = first(Compr),
    Calado = first(Calado.m)
  ) %>% 
  filter(SMov != 0) # Exclui valores iguais a 0 (Agentes sem movimentação)

# Exibe o resultado
print(MEP_km, n = Inf)

#plota o gr�fico coluna vs coluna
plot(MEP_km[, c(2:6)])

#mostra a distribui��o dos gastos e da renda dos clientes - histograma
hist(MEP_km$Atrac, col = "red4")
hist(MEP_km$SMov, col = "blue4")
hist(MEP_km$Compr, col = "green4")
hist(MEP_km$Calado, col = "orange4")

# Faz um gr�fico de Spend vs INCOME. A escala de cores mostra as diferentes regi�es
plot(MEP_km$Calado, MEP_km$Atrac, pch = 21, col = "green4")

#separa apenas as colunas 2 a 7 para analisar os dados
preprocessed <- MEP_km[,2:6]
preprocessed
k <- 3 # especifica o n�mero m�ximo de clusters que se deseja separar

# roda o modelo k-means
# nstart = n�mero de inicializa��es aleat�rias; a melhor ser� a usada
output <- kmeans(preprocessed, centers = k, nstart = 20)
output

# Cria o nome da coluna de cluster dinamicamente
Nome_coluna <- paste("cluster", k, sep = "_")

# Adiciona o número do cluster ao dataset
MEP_km[[Nome_coluna]] <- factor(output$cluster, levels = 1:k)

# Gráfico de clusters corrigido
cluster_graph <- ggplot(MEP_km, aes(x = Calado, y = Atrac, colour = .data[[Nome_coluna]])) +
  geom_point(size = 3) +
  scale_colour_manual(
    name = "Cluster Group",
    values = c('red', 'orange', 'green3', 'deepskyblue', 'blue', 'darkorchid4', 'violet', 'pink1', 'tan3', 'black')[1:k]
  ) +
  xlab("Calado (m)") +
  ylab("Nº de Atracações") +
  scale_y_continuous(labels=scales::number_format(big.mark = ".", decimal.mark = ",",
                                                  scale = 1e-3))+
  ggtitle(paste("k-means com", k, "Clusters")) +
  theme_bw()

# Exibe o gráfico
print(cluster_graph)

# Gráfico de clusters com rótulos
cluster_graph <- ggplot(MEP_km, aes(x = Calado, y = Atrac, colour = MEP_km[[Nome_coluna]])) +
  geom_point(size = 3) +
  geom_text(aes(label = Origem), vjust = -0.5, size = 3) +  # Adiciona os rótulos com base na coluna 'Destino'
  scale_colour_manual(name = "Cluster Group", values = c('red', 'orange', 'green3', 'deepskyblue', 'blue', 'darkorchid4', 'violet', 'pink1', 'tan3', 'black')) +
  xlab("Caladao (m)") +
  ylab("Nº de Atracações") +
  scale_y_continuous(labels=scales::number_format(big.mark = ".", decimal.mark = ",",
                                                  scale = 1e-3))+
  ggtitle(paste("k-means com", k, "Clusters")) +
  theme_bw()

# Exibe o gráfico
print(cluster_graph)

# Gráfico de clusters com rótulos, cor e shape
cluster_graph <- ggplot(MEP_km, aes(x = Calado, y = Atrac, colour = .data[[Nome_coluna]], shape = .data[[Nome_coluna]])) +
  geom_point(size = 3) +  # Pontos com cor e forma baseados no cluster
  geom_text(aes(label = Origem), vjust = -0.5, size = 3) +  # Rótulos com base na coluna 'Berco'
  scale_colour_manual(
    name = "Cluster Group",
    values = c('red', 'orange', 'green3', 'deepskyblue', 'blue', 'darkorchid4', 'violet', 'pink1', 'tan3', 'black')[1:k]
  ) +
  scale_shape_manual(
    name = "Cluster Group",
    values = c(16, 17, 18, 19, 15, 4, 8, 3, 6, 7)[1:k]  # Define formas específicas para cada cluster
  ) +
  xlab("Calado (m)") +
  ylab("Nº de Atracações") +
  scale_y_continuous(
    labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)
  ) +
  ggtitle(paste("k-means com", k, "Clusters")) +
  theme_bw()

# Exibe o gráfico
print(cluster_graph)

















