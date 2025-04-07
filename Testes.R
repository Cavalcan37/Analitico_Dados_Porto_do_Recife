# HISTOGRAMA DE PROBABILIDADE
# EXEMPLO 1: MODELO P(X < 65000) = ?
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 76708.79, sd = 15000))

# Valor de referência para destaque
valor_referencia <- 65000
media_valor <- mean(dados$movimentacao) # Calcular a média antes de usar

# Calcular a probabilidade
probabilidade <- pnorm(valor_referencia, mean = 76708.79, sd = 15000)
probabilidade

# Gerar o gráfico ajustado
grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  #geom_density(color = "blue", size = 1) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  stat_function(
    fun = dnorm, 
    args = list(mean = 76708.79, sd = 15000), 
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 76708.79, sd = 15000),
    fill = "red", alpha = 0.5,
    xlim = c(min(dados$movimentacao), valor_referencia)
  ) +
  labs(
    #title = "Distribuição da Movimentação com Região Destacada",
    x = "Movimentação Simulada GS (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 45000, y = 0.000018, 
    label = paste0("P(X < 75.000) = ", round(probabilidade * 100, 2), "%"), 
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = media_valor + 20000, y = 0.000025, 
    label = paste0("Média = ", round(media_valor, 2)), 
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

# Exibir o gráfico
print(grafico)

# EXEMPLO 2: MODELO P(X > 65000) = ?
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 76708.79, sd = 15000))

# Valor de referência para destaque
valor_referencia <- 65000
media_valor <- mean(dados$movimentacao) # Calcular a média antes de usar

# Calcular a probabilidade
probabilidade <- 1-pnorm(valor_referencia, mean = 76708.79, sd = 15000)
probabilidade

# Gerar o gráfico ajustado
grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  #geom_density(color = "blue", size = 1) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  stat_function(
    fun = dnorm, 
    args = list(mean = 76708.79, sd = 15000), 
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 76708.79, sd = 15000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    #title = "Distribuição da Movimentação com Região Destacada",
    x = "Movimentação Simulada GS (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 110000, y = 0.000018, 
    label = paste0("P(X < 75.000) = ", round(probabilidade * 100, 2), "%"), 
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = media_valor + 20000, y = 0.000025, 
    label = paste0("Média = ", round(media_valor, 2)), 
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

# Exibir o gráfico
print(grafico)

# O PORTO DO RECIFE
# GS Mês 1: P(X > 102119) = ? (aqui muda o sentido da prob. em relação ao modelo acima)
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 99892, sd = 17553))

# Valor de referência para destaque
valor_referencia <- 110208
media_valor <- mean(dados$movimentacao) # Calcular a média antes de usar

# Calcular a probabilidade de ultrapassar o valor de referência
probabilidade <- 1 - pnorm(valor_referencia, mean = 99892, sd = 17553)
probabilidade

# Gerar o gráfico ajustado
G1 <- grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    scale = 1e-3)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 99892, sd = 17553), 
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 99892, sd = 17553),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Simulação Mês 1 - GS (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 145000, y = 0.00001, 
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"), 
    color = "red", size = 4.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 65000, y = 0.00002, 
    label = paste0("Média = ", round(media_valor, 2)), 
    color = "black", size = 4.5, fontface = "bold"
  ) +
  theme_grey()

G1

# Exibir o gráfico
print(grafico)

# GS Mês 2: P(X > ) = ? (aqui muda o sentido da prob. em relação ao modelo acima)
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 90727, sd = 25255))

# Valor de referência para destaque
valor_referencia <- 88742
media_valor <- mean(dados$movimentacao) # Calcular a média antes de usar

# Calcular a probabilidade de ultrapassar o valor de referência
probabilidade <- 1 - pnorm(valor_referencia, mean = 90727, sd = 25255)
probabilidade

# Gerar o gráfico ajustado
G2 <- grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    scale = 1e-3)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 90727, sd = 25255), 
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 90727, sd = 25255),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Simulação Mês 2 - GS (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 140000, y = 0.00001, 
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"), 
    color = "red", size = 4.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 50000, y = 0.000015, 
    label = paste0("Média = ", round(media_valor, 2)), 
    color = "black", size = 4.5, fontface = "bold"
  ) +
  theme_grey()

G2

# GS Mês 3: P(X > 69112) = ? (aqui muda o sentido da prob. em relação ao modelo acima)
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 84425, sd = 29563))

# Valor de referência para destaque
valor_referencia <- 121326
media_valor <- mean(dados$movimentacao) # Calcular a média antes de usar

# Calcular a probabilidade de ultrapassar o valor de referência
probabilidade <- 1 - pnorm(valor_referencia, mean = 84425, sd = 29563)
probabilidade

# Gerar o gráfico ajustado
G3 <- grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    scale = 1e-3)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 84425, sd = 29563), 
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 84425, sd = 29563),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Simulação Mês 3 - GS (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 155000, y = 0.000005, 
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"), 
    color = "red", size = 4.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 13000, y = 0.00001, 
    label = paste0("Média = ", round(media_valor, 2)), 
    color = "black", size = 4.5, fontface = "bold"
  ) +
  theme_grey()

G3

# GS TRIM: P(X > 69112) = ? (aqui muda o sentido da prob. em relação ao modelo acima)
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 275045, sd = 42271))

# Valor de referência para destaque
valor_referencia <- 320276
media_valor <- mean(dados$movimentacao) # Calcular a média antes de usar

# Calcular a probabilidade de ultrapassar o valor de referência
probabilidade <- 1 - pnorm(valor_referencia, mean = 275045, sd = 42271)
probabilidade

# Gerar o gráfico ajustado
G4 <- grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 15000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    scale = 1e-3)) +
  stat_function(
    fun = dnorm, 
    args = list(mean = 275045, sd = 42271), 
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 275045, sd = 42271),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Simulação 1º Trim - GS (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 380000, y = 0.000005, 
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"), 
    color = "red", size = 4.5, fontface = "bold"
  ) +
  annotate(
    "text", x = 180000, y = 0.0000078, 
    label = paste0("Média = ", round(media_valor, 2)), 
    color = "black", size = 4.5, fontface = "bold"
  ) +
  theme_grey()

G4

(G1+G2)/(G3+G4)







