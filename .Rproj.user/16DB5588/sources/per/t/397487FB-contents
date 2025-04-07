# SARIMA GL (atualizado 2024)
# Pacotes
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(readxl))install.packages("readxl")
library(readxl)
if(!require(forecast))install.packages("forecast")
library(forecast)
if(!require(lmtest))install.packages("lmtest")
library(lmtest)
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)

# Carregamento de Dados
MEP24_full<- read_excel("MEP_24_full.xlsx", sheet = "base2") 
MEP24_full
str(MEP24_full)
MEP24_full$Ano <- as.character(MEP24_full$Ano)
str(MEP24)

# GL
GL <- MEP24_full %>% 
  filter(Perfil == "GL") %>% 
  group_by(Ano, Mes) %>% 
  summarise(GL_mes = sum(Mov, rm.na = T))
GL
print(GL, n = Inf) # Visualizar toda a tabela (data frame)

## AQUI COMEÇA O MODELO SARIMA
# 1. Dados
GL_mes <- read_excel("MEP_24_full.xlsx", sheet = "GL")
GL_mes
print(GL_mes, n = Inf)
str(GL_mes)

# 2. Converter em ST
GL_mes <- ts(GL_mes,frequency = 12, start = c(2018,1))
GL_mes
str(GL_mes)

ts.plot(GL_mes)
decomp.GL_mes<-decompose(GL_mes)
plot(decomp.GL_mes)
ggtsdisplay(GL_mes)

# 3. Aferição Gráfica de Sazonalidade
seasonplot(GL_mes, col = rainbow(7), year.labels = T, type = "o", pch = 22)

# 4. Transformação Box-Cox 
L <- BoxCox.lambda(GL_mes)
L # para diminuir a amplitude de volatilidade
GL_mes.L <- BoxCox(GL_mes, lambda = L)
GL_mes.L

ts.plot(GL_mes.L)

decomp.GL_mes.L<-decompose(GL_mes.L)
plot(decomp.GL_mes.L)

ggtsdisplay(GL_mes.L)

# 5. Nº de Integrações (Diferenciações)
ndiffs(GL_mes.L) # Aqui deu 1 diferenciação"

GL_mes.L_DIFF <- diff(GL_mes.L,1)
ts.plot(GL_mes.L_DIFF)

decomp.GL_mes.L_DIFF<-decompose(GL_mes.L_DIFF)
plot(GL_mes.L_DIFF)

ggtsdisplay(GL_mes.L_DIFF) # "contar" "p" e "q"

# 6. Nº de Diferenciações Sazonais # usando a série original
nsdiffs(GL_mes) # Acho que tenho que fazer isso com a série original!!!

GL_mes.L_DIFF2 <- diff(GL_mes,12)
seasonplot(GL_mes.L_DIFF2, col = rainbow(7), year.labels = T, type = "o", pch = 22)

ggtsdisplay(GL_mes.L_DIFF2) # "contar" "P" e "Q"

# 7. Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
GL_mes_train <- ts(GL_mes[1:60], frequency = 12)
GL_mes_train

GL_mes_test <- ts(GL_mes[61:84], frequency = 12)
GL_mes_test

FIT <- Arima(y = GL_mes_train, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# Plotar a Série Treino e Acurácia
plot(GL_mes_train)

lines(FIT$fitted, col = "green")

accuracy(GL_mes_train, FIT$fitted)

# 8. Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((GL_mes_train - mean(GL_mes_train))^2)
ss_residual <- sum((GL_mes_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# 9. Realizando Agora Forecast para 24 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 24)
predi
plot(predi)

# 10. Plotar as Previsões com a Base de Teste
plot(as.numeric(GL_mes_test), type = "l")
lines(as.numeric(predi$mean), col = "green")

accuracy(as.numeric(GL_mes_test), as.numeric(predi$mean))

# 11. Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# 12. Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = GL_mes, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full
plot(forecast_full, col="darkgreen")
lines(forecast_full$mean, col = "darkgreen", type = "o", pch = 16)

# HISTOGRAMAS DE PROBABILIDADE (GL)
# 13. Probabilidades Trimestrais (GL)
# 13.1. 1º Trimestre de 2025 (GL)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 3113, sd = 500))
dados

# Valor de referência para destaque
valor_referencia <- 2653
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 3113, sd = 500)

# Gerar o gráfico ajustado
GL_1trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 3113, sd = 500),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 3113, sd = 500),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GL Simulada 1º Trim. (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 4300, y = 5e-04,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 4000, y = 7e-04,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GL_1trim

# 13.2. 2º Trimestre de 2025 (GL)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 2814, sd = 365))
dados

# Valor de referência para destaque
valor_referencia <- 2552
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 2814, sd = 365)

# Gerar o gráfico ajustado
GL_2trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 2814, sd = 365),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 2814, sd = 365),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GL Simulada 2º Trim. (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 3700, y = 5e-04,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 3600, y = 7e-04,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GL_2trim

# 13.3. 3º Trimestre de 2025 (GL)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 2825, sd = 623))
dados

# Valor de referência para destaque
valor_referencia <- 3003
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 2825, sd = 623)

# Gerar o gráfico ajustado
GL_3trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 2825, sd = 623),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 2825, sd = 623),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GL Simulada 3º Trim. (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 3800, y = 5e-04,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 3800, y = 6e-04,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GL_3trim

# 13.4. 4º Trimestre de 2025 (GL)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 3058, sd = 534))
dados

# Valor de referência para destaque
valor_referencia <- 2806
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 3058, sd = 534)

# Gerar o gráfico ajustado
GL_4trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 200, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 3058, sd = 534),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 3058, sd = 534),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GL Simulada 4º Trim. (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 4000, y = 5e-04,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 4000, y = 6e-04,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GL_4trim

library(patchwork)
library(magrittr)

(GL_1trim + GL_2trim)/(GL_3trim + GL_4trim)

# 13.5. Ano de 2025 (CG)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 11811, sd = 1770))
dados

# Valor de referência para destaque
valor_referencia <- 11014
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 11811, sd = 1770)

# Gerar o gráfico ajustado
GL_ano <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 600, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 11811, sd = 1770),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 11811, sd = 1770),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GL Simulada Ano 2025 (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 15000, y = 1.5e-4,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 15000, y = 2e-04,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GL_ano






