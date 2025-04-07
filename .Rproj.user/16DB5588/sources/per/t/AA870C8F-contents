# SARIMA GS (atualizado 2024)
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

# GS
GS <- MEP24_full %>% 
  filter(Perfil == "GS") %>% 
  group_by(Ano, Mes) %>% 
  summarise(GS_mes = sum(Mov, rm.na = T))
GS
print(GS, n = Inf) # Visualizar toda a tabela (data frame)

## AQUI COMEÇA O MODELO SARIMA
# 1. Dados
GS_mes <- read_excel("MEP_24_full.xlsx", sheet = "GS")
GS_mes
print(GS_mes, n = Inf)

str(GS_mes)

# 2. Converter em ST
GS_mes <- ts(GS_mes,frequency = 12, start = c(2018,1))
GS_mes
str(GS_mes)

ts.plot(GS_mes)
decomp.GS_mes<-decompose(GS_mes)
plot(decomp.GS_mes)
ggtsdisplay(GS_mes)

# 3. Aferição Gráfica de Sazonalidade
seasonplot(GS_mes, col = rainbow(7), year.labels = T, type = "o", pch = 22)

# 4. Transformação Box-Cox
L <- BoxCox.lambda(GS_mes)
L
GS_mes.L <- BoxCox(GS_mes, lambda = L)
GS_mes.L

ts.plot(GS_mes.L)

decomp.GS_mes.L<-decompose(GS_mes.L)
plot(decomp.GS_mes.L)

ggtsdisplay(GS_mes.L)

# 5. Nº de Integrações (Diferenciações)
ndiffs(GS_mes.L) # Na verdade, o "ndiffs(GS_mes.L): 0"

GS_mes.L_DIFF <- diff(GS_mes.L,1)
ts.plot(GS_mes.L_DIFF)

decomp.GS_mes.L_DIFF<-decompose(GS_mes.L_DIFF)
plot(GS_mes.L_DIFF)

ggtsdisplay(GS_mes.L_DIFF) # "contar" "p" e "q"

# 6. Nº de Diferenciações Sazonais # usando a série original
nsdiffs(GS_mes) # Acho que tenho que fazer isso com a série original!!!

GS_mes.L_DIFF2 <- diff(GS_mes,12)
seasonplot(GS_mes.L_DIFF2, col = rainbow(7), year.labels = T, type = "o", pch = 22)

ggtsdisplay(GS_mes.L_DIFF2) # "contar" "P" e "Q"

# 7. Ajustando ARIMA/SARIMA
# Separando os Dados em Treino (train) e Teste (test)
GS_mes_train <- ts(GS_mes[1:60], frequency = 12)
GS_mes_train

GS_mes_test <- ts(GS_mes[61:84], frequency = 12)
GS_mes_test

FIT <- Arima(y = GS_mes_train, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
summary(FIT)

coeftest(FIT)

# Plotar a Série Treino e Acurácia
plot(GS_mes_train)

lines(FIT$fitted, col = "red")

accuracy(GS_mes_train, FIT$fitted)

# 8. Calcular o coeficiente de determinação (R²)
# Fazer previsões no conjunto de treino
fitted_values <- FIT$fitted
ss_total <- sum((GS_mes_train - mean(GS_mes_train))^2)
ss_residual <- sum((GS_mes_train - fitted_values)^2)
r_squared <- 1 - (ss_residual / ss_total)
r_squared

# 9. Realizando Agora Forecast para 24 Previsões para Compara com o Teste (test)
predi <- forecast(FIT, h = 24)
predi
plot(predi)

# 10. Plotar as Previsões com a Base de Teste
plot(as.numeric(GS_mes_test), type = "l")
lines(as.numeric(predi$mean), col = "red")

accuracy(as.numeric(GS_mes_test), as.numeric(predi$mean))

# 11. Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# 12. Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = GS_mes, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 12)
forecast_full
plot(forecast_full, col="red")
lines(forecast_full$mean, col = "red", type = "o", pch = 16)

# HISTOGRAMAS DE PROBABILIDADE (GS)
# 13. Probabilidades Trimestrais (GS)
# 13.1. 1º Trimestre de 2025 (GS)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 296050, sd = 42000))
dados

# Valor de referência para destaque
valor_referencia <- 320276
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 296050, sd = 42000)

# Gerar o gráfico ajustado
GS_1trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 18000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 296050, sd = 42000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 296050, sd = 42000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GS Simulada 1º Trim. (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 380000, y = 5e-06,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 250000, y = 0.000007,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GS_1trim

# 13.2. 2º Trimestre de 2025 (GS)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 285310, sd = 54000))
dados

# Valor de referência para destaque
valor_referencia <- 290952
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 285310, sd = 54000)

# Gerar o gráfico ajustado
GS_2trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 15000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 285310, sd = 54000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 285310, sd = 54000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GS Simulada 2º Trim (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 380000, y = 5e-6,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 200000, y = 6e-6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GS_2trim

# 13.3. 3º Trimestre de 2025 (GS)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 258370, sd = 42000))
dados

# Valor de referência para destaque
valor_referencia <- 289370
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 258370, sd = 42000)

# Gerar o gráfico ajustado
GS_3trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 15000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 258370, sd = 42000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 258370, sd = 42000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GS Simulada 3º Trim (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x = 370000, y = 3e-6,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 200000, y = 5.8e-6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GS_3trim

# 13.4. 4º Trimestre de 2025 (GS)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 348651, sd = 70000))
dados

# Valor de referência para destaque
valor_referencia <- 349104
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 348651, sd = 70000)

# Gerar o gráfico ajustado
GS_4trim <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 22000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 348651, sd = 70000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 348651, sd = 70000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GS Simulada 4º Trim (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x =470000, y = 4e-6,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 230000, y = 5e-6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GS_4trim

library(patchwork)
library(magrittr)

(GS_1trim + GS_2trim)/(GS_3trim + GS_4trim)

# 13.5. Ano de 2025 (GS)
# Geração dos dados simulados
set.seed(123) # Para reprodutibilidade
dados <- data.frame(movimentacao = rnorm(5000, mean = 1188380, sd = 200000))
dados

# Valor de referência para destaque
valor_referencia <- 1250008
media_valor <- mean(dados$movimentacao) # Calcular a média dos dados

# Calcular a probabilidade
probabilidade <- 1 - pnorm(valor_referencia, mean = 1188380, sd = 200000)

# Gerar o gráfico ajustado
GS_ano <- ggplot(dados, aes(x = movimentacao)) +
  geom_histogram(aes(y = ..density..), binwidth = 50000, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = valor_referencia, linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = media_valor, linetype = "dashed", color = "black", size = 1) +
  scale_x_continuous(labels = scales::number_format(big.mark = ".", decimal.mark = ",", scale = 1e-3)) +
  stat_function(
    fun = dnorm,
    args = list(mean = 1188380, sd = 200000),
    color = "red", linetype = "solid", size = 1
  ) +
  geom_area(
    stat = "function",
    fun = dnorm,
    args = list(mean = 1188380, sd = 200000),
    fill = "red", alpha = 0.5,
    xlim = c(valor_referencia, max(dados$movimentacao))
  ) +
  labs(
    x = "Movimentação GS Simulada Ano 2025 (x Mil t)",
    y = "Densidade"
  ) +
  annotate(
    "text", x =1600000, y = 1e-6,
    label = paste0("Prob = ", round(probabilidade * 100, 2), "%"),
    color = "red", size = 3.9, fontface = "bold"
  ) +
  annotate(
    "text", x = 800000, y = 1.5e-6,
    label = paste0("Média = ", round(media_valor, 2)),
    color = "black", size = 3.9, fontface = "bold"
  ) +
  theme_grey()

GS_ano




