# SARIMA GS (atualizado 2024)
# Pacotes
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)
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

# CG
CG <- MEP24_full %>% 
  filter(Perfil == "CG") %>% 
  group_by(Ano, Mes) %>% 
  summarise(CG_mes = sum(Mov, rm.na = T))
CG
print(CG, n = Inf) # Visualizar toda a tabela (data frame)

## AQUI COMEÇA O MODELO SARIMA
# 1. Dados
CG_mes <- read_excel("MEP_24_full.xlsx", sheet = "CG")
CG_mes
print(CG_mes, n = Inf)

str(CG_mes)

# 2. Converter em ST
CG_mes <- ts(CG_mes,frequency = 12, start = c(2018,1))
CG_mes
str(CG_mes)

ts.plot(CG_mes)
decomp.CG_mes<-decompose(CG_mes)
plot(decomp.CG_mes)
ggtsdisplay(CG_mes)

# 3. Aferição Gráfica de Sazonalidade
seasonplot(CG_mes, col = rainbow(7), year.labels = T, type = "o", pch = 22)

# 4. Transformação Box-Cox 
L <- BoxCox.lambda(CG_mes)
L # para diminuir a amplitude de volatilidade
CG_mes.L <- BoxCox(CG_mes, lambda = L)
CG_mes.L

ts.plot(CG_mes.L)

decomp.CG_mes.L<-decompose(CG_mes.L)
plot(decomp.CG_mes.L)

ggtsdisplay(CG_mes.L)

# 5. Nº de Integrações (Diferenciações)
ndiffs(CG_mes.L) # Aqui deu 1 diferenciação"

CG_mes.L_DIFF <- diff(CG_mes.L,1)
ts.plot(CG_mes.L_DIFF)

decomp.CG_mes.L_DIFF<-decompose(CG_mes.L_DIFF)
plot(CG_mes.L_DIFF)

ggtsdisplay(CG_mes.L_DIFF) # "contar" "p" e "q"

# 6. Nº de Diferenciações Sazonais # usando a série original
nsdiffs(CG_mes) # Acho que tenho que fazer isso com a série original!!!

CG_mes.L_DIFF2 <- diff(CG_mes,12)
seasonplot(CG_mes.L_DIFF2, col = rainbow(7), year.labels = T, type = "o", pch = 22)

ggtsdisplay(CG_mes.L_DIFF2) # "contar" "P" e "Q"

# 7. Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
CG_mes_train <- ts(CG_mes[1:60], frequency = 12)
CG_mes_train

CG_mes_test <- ts(CG_mes[61:84], frequency = 12)
CG_mes_test

FIT <- Arima(y = CG_mes_train, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# Plotar a Série Treino e Acurácia
plot(CG_mes_train)

lines(FIT$fitted, col = "blue")

accuracy(CG_mes_train, FIT$fitted)

# 8. Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((CG_mes_train - mean(CG_mes_train))^2)
ss_residual <- sum((CG_mes_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# 9. Realizando Agora Forecast para 24 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 24)
predi
plot(predi)

# 10. Plotar as Previsões com a Base de Teste
plot(as.numeric(CG_mes_test), type = "l")
lines(as.numeric(predi$mean), col = "blue")

accuracy(as.numeric(CG_mes_test), as.numeric(predi$mean))

# 11. Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "blue")

# 12. Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = CG_mes, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full
plot(forecast_full, col="blue")
lines(forecast_full$mean, col = "blue", type = "o", pch = 16)

# HISTOGRAMAS DE PROBABILIDADE (CG)
# 13. Probabilidades Trimestrais (CG)
# 13.1. 1º Trimestre de 2025 (CG)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 125985, sd = 54000))
dados

# Valor de referência para destaque
valor_referencia <- 168420
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 125985, sd = 54000)

# Gerar o gráfico ajustado
GC_1trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 18000, fill = "blue", color = "black", alpha = 0.7) +
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
    "text", x = 250000, y = 2.8e-06,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 45000, y = 0.000007,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_1trim

# 13.2. 2º Trimestre de 2025 (CG)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 48299, sd = 17500))
dados

# Valor de referência para destaque
valor_referencia <- 45852
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 48299, sd = 17500)

# Gerar o gráfico ajustado
GC_2trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 48299, sd = 17500),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 48299, sd = 17500),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada 2º Trim (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 90000, y = 1e-5,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 18000, y = 2e-5,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_2trim

# 13.3. 3º Trimestre de 2025 (CG)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 58243, sd = 24400))
dados

# Valor de referência para destaque
valor_referencia <- 44813
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 58243, sd = 24400)

# Gerar o gráfico ajustado
GC_3trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 10000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 58243, sd = 24400),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 58243, sd = 24400),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada 3º Trim (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 110000, y = 1e-5,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 90000, y = 1.6e-5,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_3trim

# 13.4. 4º Trimestre de 2025 (CG)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 179238, sd = 60000))
dados

# Valor de referência para destaque
valor_referencia <- 120205
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 179238, sd = 60000)

# Gerar o gráfico ajustado
GC_4trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 22000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 179238, sd = 60000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 179238, sd = 60000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada 4º Trim (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x =300000, y = 4e-6,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 270000, y = 6e-6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_4trim

library(patchwork)
library(magrittr)

(GC_1trim + GC_2trim)/(GC_3trim + GC_4trim)

# 13.5. Ano de 2025 (CG)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 411765, sd = 101520))
dados

# Valor de referência para destaque
valor_referencia <- 379290
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 411765, sd = 101520)

# Gerar o gráfico ajustado
GC_ano <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 30000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 411765, sd = 101520),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 411765, sd = 101520),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação CG Simulada Ano 2025 (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x =600000, y = 2.8e-6,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 550000, y = 4e-6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GC_ano











