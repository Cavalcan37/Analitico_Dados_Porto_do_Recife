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
lines(as.numeric(predi$mean), col = "red")

accuracy(as.numeric(CG_mes_test), as.numeric(predi$mean))

# 11. Diagnóstico dos Resíduos do Modelo
tsdiag(FIT)

qqnorm(FIT$residuals)
qqline(FIT$residuals, col = "red")

# 12. Fazer 12 previsões além do banco de dados
FIT_full <- Arima(y = CG_mes, order = c(0,1,1), seasonal = c(0,1,1), lambda = L)
forecast_full <- forecast(FIT_full, h = 13)
forecast_full
plot(forecast_full, col="blue")
lines(forecast_full$mean, col = "red", type = "o", pch = 16)

##
##

# 13. Carregar a variável câmbio
cambio <- read_excel("MEP_24_full.xlsx", sheet = "cambio")$Cambio
cambio <- ts(cambio, frequency = 12, start = c(2018, 1)) # Ajustar frequência e início
cambio

# 14. Separar em treino e teste
cambio_train <- cambio[1:60]
cambio_test <- cambio[61:84]

# 15. Ajustar o modelo SARIMAX
FIT_SARIMAX <- Arima(
  y = CG_mes_train, 
  order = c(0, 1, 1), 
  seasonal = c(0, 1, 1), 
  lambda = L,
  xreg = cambio_train
)
summary(FIT_SARIMAX)

# Cálculo do R² no conjunto de treino
fitted_values_train <- FIT_SARIMAX$fitted # Valores ajustados pelo modelo no treino
ss_total_train <- sum((CG_mes_train - mean(CG_mes_train))^2) # Soma total dos quadrados no treino
ss_residual_train <- sum((CG_mes_train - fitted_values_train)^2) # Soma dos resíduos ao quadrado no treino
r_squared_train <- 1 - (ss_residual_train / ss_total_train) # R² para o conjunto de treino
cat("R² no conjunto de treino (SARIMAX): ", r_squared_train, "\n")

# 16. Fazer previsões no conjunto de teste
forecast_SARIMAX <- forecast(FIT_SARIMAX, h = 24, xreg = cambio_test)
forecast_SARIMAX
plot(forecast_SARIMAX)

# 17. Avaliar o modelo no conjunto de teste
accuracy(as.numeric(CG_mes_test), as.numeric(forecast_SARIMAX$mean))

# 18. Diagnóstico dos resíduos
tsdiag(FIT_SARIMAX)

qqnorm(FIT_SARIMAX$residuals)
qqline(FIT_SARIMAX$residuals, col = "red")

# 19. Fazer previsões além do banco de dados
cambio_future <- c(cambio_test, rep(mean(cambio_test), 12)) # Exemplo de previsão futura do câmbio
forecast_full_SARIMAX <- forecast(FIT_SARIMAX, h = 12, xreg = cambio_future)
forecast_full_SARIMAX
plot(forecast_full_SARIMAX)
lines(forecast_full_SARIMAX$mean, col = "red", type = "o", pch = 16)








# 13. HISTOGRAMAS SEM IC 95%
# 13.1. HISTOGRAMA PARA AS PREVISÕES DOS PRÓXIMOS 12 MESES
# Resíduos do modelo ajustado
residuals <- residuals(forecast_full_SARIMAX)
residuals

# Distribuição dos resíduos
residual_mean <- mean(residuals)
residual_sd <- sd(residuals)
# Função para simular erros de previsão
simulate_errors <- function(n, mean, sd) {
  rnorm(n, mean = mean, sd = sd)
}
# Simular 500.000 erros para cada um dos 3 períodos de previsão
set.seed(123)
num_simulations <- 500000
errors_simulated <- matrix(nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  errors_simulated[, i] <- simulate_errors(num_simulations, residual_mean, residual_sd)
}
# Gerar cenários de previsão com erros simulados
scenarios <- matrix(NA, nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  scenarios[, i] <- forecast_full_SARIMAX$mean[i] + errors_simulated[, i]
}
# Plotar histogramas para cada uma das 3 previsões com curvas de densidade e linhas tracejadas indicando a média
par(mfrow = c(3,4)) # Configurar a janela gráfica para 3 colunas
for (i in 1:12) {
  hist(scenarios[, i], main = paste("Mês", i),
       xlab = "Movimentação CG (t)", col = "blue", border = "black", breaks = 20,
       prob = TRUE) # Ajustar probabilidade a TRUE para a densidade
  dens <- density(scenarios[, i])
  lines(dens, col = "red") # Adicionar a curva de densidade
  media <- mean(scenarios[, i])
  abline(v = media, col = "red", lty = 2) # Adicionar a linha vermelha tracejada da média
  text(x = media + 0.3 * diff(range(scenarios[, i])), y = max(dens$y) * 0.9,
       labels = paste("Média:", round(media, 2)), col = "black") # Adicionar o valor da média em preto
}

# Retornar ao gráfico único na tela
par(mfrow = c(1,1))

# 13.2. HISTOGRAMA APENAS PARA O MÊS 6
# Configurar a janela gráfica para exibir apenas um gráfico
par(mfrow = c(1,1)) 

# Selecionar os dados referentes ao mês 6
hist(scenarios[, 6], 
     main = "Porto do Recife - Mês 6", 
     xlab = "Movimentação CG (t)", 
     col = "blue", border = "black", 
     breaks = 20, prob = TRUE) 

# Adicionar a curva de densidade
dens <- density(scenarios[, 6])
lines(dens, col = "red") 

# Calcular e adicionar a média como linha tracejada
media <- mean(scenarios[, 6])
abline(v = media, col = "red", lty = 2) 

# Adicionar o valor da média no gráfico
text(x = media + 0.25 * diff(range(scenarios[, 6])), 
     y = max(dens$y) * 0.9, 
     labels = paste("Média:", round(media, 2)), 
     col = "black")

# 13.3.CALCULAR A PROB(CG < 12850) NO MÊS 6 (SÓ PARA O MÊS 6)
# Configurar a janela gráfica para exibir apenas um gráfico
par(mfrow = c(1,1))

# Selecionar os dados referentes ao mês 6
dados_mes6 <- scenarios[, 6]

# Calcular a probabilidade de demanda inferior a 75000
limite <- 12850
probabilidade <- mean(dados_mes6 < limite)

# Criar o histograma com densidade
histograma <- hist(dados_mes6, 
                   main = "Porto do Recife - Mês 6", 
                   xlab = "Movimentação CG (t)", 
                   col = "blue", border = "black", 
                   breaks = 20, prob = TRUE)

# Adicionar a curva de densidade
dens <- density(dados_mes6)
lines(dens, col = "red")

# Pintar de vermelho a área acima do limite
x_preenchimento <- dens$x[dens$x < limite]  # Valores de x menores que o limite
y_preenchimento <- dens$y[dens$x < limite]  # Densidades correspondentes

polygon(c(limite, x_preenchimento, max(x_preenchimento)), 
        c(0, y_preenchimento, 0), 
        col = rgb(1, 0, 0, 0.5), border = NA)  # Área vermelha semi-transparente

# Adicionar o valor da média como linha tracejada
media <- mean(dados_mes6)
abline(v = media, col = "black", lty = 2)

# Adicionar o limite como linha tracejada
abline(v = limite, col = "red", lty = 2)

# Adicionar o valor da probabilidade no gráfico
text(x = limite - 0.25 * diff(range(dados_mes6)), # Posição o texto da probabilidade
     y = max(dens$y) * 0.7, 
     labels = paste("P(X < 12850) =", round(probabilidade, 4)), 
     col = "red", font = 2)

# Adicionar o valor da média no gráfico
text(x = media + 0.25 * diff(range(dados_mes6)), # Posição do texto da média
     y = max(dens$y) * 0.9, 
     labels = paste("Média:", round(media, 2)), 
     col = "blue", font = 2)

# 14. HISTOGRAMA COM IC 95%
# 14.1. HISTOGRAMA PARA AS PREVISÕES DOS PRÓXIMOS 12 MESES COM IC 95%
# Resíduos do modelo ajustado
residuals <- residuals(FIT_full)
residuals

# Distribuição dos resíduos
residual_mean <- mean(residuals)
residual_sd <- sd(residuals)
# Função para simular erros de previsão
simulate_errors <- function(n, mean, sd) {
  rnorm(n, mean = mean, sd = sd)
}
# Simular 1.000 erros para cada um dos 12 períodos de previsão
set.seed(123)
num_simulations <- 1000
errors_simulated <- matrix(nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  errors_simulated[, i] <- simulate_errors(num_simulations, residual_mean, residual_sd)
}
# Gerar cenários de previsão com erros simulados
scenarios <- matrix(NA, nrow = num_simulations, ncol = 12)
for (i in 1:12) {
  scenarios[, i] <- forecast_full$mean[i] + errors_simulated[, i]
}
# Plotar histogramas para cada uma das 12 previsões com curvas de densidade e linhas tracejadas indicando a média
par(mfrow = c(3, 4)) # Configurar a janela gráfica para 3 linhas e 4 colunas
for (i in 1:12) {
  hist(scenarios[, i], main = paste("Mês", i),
       xlab = "Movimentações GS (t)", col = "blue", border = "black", breaks = 20,
       prob = TRUE) # Ajustar probabilidade a TRUE para a densidade
  dens <- density(scenarios[, i])
  lines(dens, col = "red") # Adicionar a curva de densidade
  media <- mean(scenarios[, i])
  abline(v = media, col = "red", lty = 2) # Adicionar a linha vermelha tracejada da média
  # Calcular os intervalos de confiança de 95%
  ci_lower <- quantile(scenarios[, i], probs = 0.025)
  ci_upper <- quantile(scenarios[, i], probs = 0.975)
  abline(v = ci_lower, col = "red", lty = 2) # Adicionar a linha verde tracejada para o limite inferior do IC
  abline(v = ci_upper, col = "red", lty = 2) # Adicionar a linha verde tracejada para o limite superior do IC
  # Adicionar os valores dos intervalos de confiança no histograma
  text(x = ci_lower, y = max(dens$y) * 0.6, labels = paste("IC 95% Inf:", round(ci_lower, 2)), col = "black", pos = 4)
  text(x = ci_upper, y = max(dens$y) * 0.8, labels = paste("IC 95% Sup:", round(ci_upper, 2)), col = "black", pos = 2)
  # Adicionar o valor da média em preto
  text(x = media, y = max(dens$y) * 0.9,
       labels = paste("Média:", round(media, 2)), col = "black", pos = 3)
}

# 14.2. HISTOGRAMA COM IC 95% PARA O MÊS 5
par(mfrow = c(1, 1))

# Selecionar os dados do mês 5
mes <- 5
dados_mes <- scenarios[, mes]

# Criar o histograma com densidade
hist(dados_mes, 
     main = paste("Mês", mes, "- Previsões com IC 95%"),
     xlab = "Movimentações CG (t)", 
     col = "blue", border = "black", 
     breaks = 20, prob = TRUE)

# Adicionar a curva de densidade
dens <- density(dados_mes)
lines(dens, col = "red")

# Calcular a média
media <- mean(dados_mes)
abline(v = media, col = "red", lty = 2)

# Calcular os intervalos de confiança de 95%
ci_lower <- quantile(dados_mes, probs = 0.025)
ci_upper <- quantile(dados_mes, probs = 0.975)
abline(v = ci_lower, col = "red", lty = 2)
abline(v = ci_upper, col = "red", lty = 2)

# Adicionar os valores dos intervalos de confiança no gráfico
text(x = ci_lower, y = max(dens$y) * 0.6, 
     labels = paste("IC 95% Inf:", round(ci_lower, 2)), 
     col = "black", pos = 4)

text(x = ci_upper, y = max(dens$y) * 0.8, 
     labels = paste("IC 95% Sup:", round(ci_upper, 2)), 
     col = "black", pos = 2)

# Adicionar o valor da média no gráfico
text(x = media, y = max(dens$y) * 0.9, 
     labels = paste("Média:", round(media, 2)), 
     col = "black", pos = 3)



