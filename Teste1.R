# HISTOGRAMA DE PROBABILIDADE
# EXEMPLO 1: MODELO P(X < 65000) = ?
# Carregar pacotes necessários
library(ggplot2)
library(ggthemes)
library(patchwork)
library(magrittr)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = 36361 + rnorm(5000, mean = 0, sd = 20000))

# Valor de referência para destaque
valor_referencia <- 28000
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- pnorm(valor_referencia, mean = 36361, sd = 20000)

# Gerar o gráfico ajustado
grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 36361, sd = 20000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 36361, sd = 20000),
    fill = "red", alpha = 0.5,
    xlim = c(min(dados$movimentacao), valor_referencia)
  ) +
  labs(
    x = "Movimentação Simulada CG (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 20000, y = 0.000018,
    label = paste0("P(X < 28.000) = ", round(probabilidade * 100, 2), "%"),
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

###
###
###

library(ggplot2)

# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = 36361 + rnorm(5000, mean = 0, sd = 20000))

# Valor de referência para destaque
valor_referencia <- 28000
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 36361, sd = 20000)

# Gerar o gráfico ajustado
grafico <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 36361, sd = 20000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 36361, sd = 20000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação Simulada CG (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 45000, y = 0.000018,
    label = paste0("P(X > 28.000) = ", round(probabilidade * 100, 2), "%"),
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





# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = 36361 + rnorm(5000, mean = 0, sd = 20000))

# Valor de referência para destaque
valor_referencia <- 38553
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 36361, sd = 20000)

# Gerar o gráfico ajustado
GC_G1 <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 36361, sd = 20000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 36361, sd = 20000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada Mês 1 (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 86000, y = 0.00001,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 5000, y = 0.00002,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_G1



# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = 52694 + rnorm(5000, mean = 0, sd = 20000))

# Valor de referência para destaque
valor_referencia <- 71749
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 52694, sd = 20000)

# Gerar o gráfico ajustado
GC_G2 <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 52694, sd = 20000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 52694, sd = 20000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada Mês 2 (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 95000, y = 0.00001,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 14000, y = 0.000015,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_G2



# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = 36930 + rnorm(5000, mean = 0, sd = 20000))

# Valor de referência para destaque
valor_referencia <- 58118
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 36930, sd = 20000)

# Gerar o gráfico ajustado
CG_G3 <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 6000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 36930, sd = 20000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 36930, sd = 20000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada Mês 3 (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 93000, y = 0.000005,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 2000, y = 0.000015,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

CG_G3



# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = 125985 + rnorm(5000, mean = 0, sd = 54000))

# Valor de referência para destaque
valor_referencia <- 168420
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 125985, sd = 54000)

# Gerar o gráfico ajustado
GC_1TRI <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 16000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 125985, sd = 54000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 125985, sd = 54000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada 1º Trim. (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 270000, y = 0.000002,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 20000, y = 0.000005,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_1TRI

(GC_G1 + GC_G2)/(CG_G3 + GC_1TRI)













