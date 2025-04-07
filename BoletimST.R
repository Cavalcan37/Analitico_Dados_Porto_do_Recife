# Carregar pacotes
library(ggplot2)
library(dplyr)
library(ggthemes)

# 1. Comparativo Anual (2023/2024) - Movimentação Total
# Data frame fornecido
dados <- data.frame(
  Ano = c(rep(2023, 12), rep(2024, 12)),
  Mes = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez",
          "jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
  Mov = c(142919, 106748, 129430, 102902, 51311, 51924, 104877, 86701, 96613, 176030, 200744, 176818,
          149887, 128436, 181267, 164706, 132169, 77043, 120406, 57596, 159749, 193403, 156691, 122015)
)


# Transformar 'Mes' em um fator ordenado
dados$Mes <- factor(dados$Mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun", 
                                          "jul", "ago", "set", "out", "nov", "dez"))

# Criar o gráfico de área
library(ggplot2)
library(ggthemes)

ggplot(dados, aes(x = Mes, y = Mov, fill = as.factor(Ano), group = Ano)) +
  geom_area(alpha = 0.6) +
  labs(#title = "Movimentação Total Mensal por Ano",
       x = "Mês",
       y = "Movimentação Total",
       fill = "Ano") +
  scale_fill_manual(values = c("2023" = "blue", "2024" = "green")) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", 
                                                    decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

# 2. Comparativo Anual (2023/2024) - Granéis Sólidos
# Data frame fornecido
dados_GS <- data.frame(
  Ano = c(rep("2023", 12), rep("2024", 12)),
  Mes = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez",
          "jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
  GS = c(104361, 69281, 53351, 80295, 35662, 47856, 74319, 76800, 57532, 119701, 148833, 101668,
         110168, 55208, 124523, 146129, 106501, 62793, 96173, 47963, 120420, 144379, 117581, 87142)
)

# Transformar 'Mes' em um fator ordenado
dados_GS$Mes <- factor(dados_GS$Mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun", 
                                                "jul", "ago", "set", "out", "nov", "dez"))

# Criar o gráfico de área
library(ggplot2)
library(ggthemes)

ggplot(dados_GS, aes(x = Mes, y = GS, fill = as.factor(Ano), group = Ano)) +
  geom_area(alpha = 0.6) +
  labs(#title = "Movimentação Total Mensal por Ano (GS)",
       x = "Mês",
       y = "Movimentação Total (GS)",
       fill = "Ano") +
  scale_fill_manual(values = c("2023" = "blue", "2024" = "green")) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", 
                                                    decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

# 3. Comparativo Anual (2023/2024) - Carga Geral
# Carregar pacotes
library(ggplot2)

# Data frame fornecido
dados_CG <- data.frame(
  Ano = c(rep("2023", 12), rep("2024", 12)),
  Mes = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez",
          "jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
  CG = c(36306, 33611, 75116, 21842, 14569, 3317, 29673, 9122, 38135, 53271, 50889, 74175,
         38871, 72421, 55576, 17779, 24625, 13457, 23413, 8795, 38212, 47974, 38010, 34219)
)

# Transformar 'Mes' em um fator ordenado
dados_CG$Mes <- factor(dados_CG$Mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun", 
                                                "jul", "ago", "set", "out", "nov", "dez"))

# Criar o gráfico de área
library(ggplot2)
library(ggthemes)

ggplot(dados_CG, aes(x = Mes, y = CG, fill = as.factor(Ano), group = Ano)) +
  geom_area(alpha = 0.6) +
  labs(#title = "Movimentação Total Mensal por Ano (CG)",
       x = "Mês",
       y = "Movimentação Total (CG)",
       fill = "Ano") +
  scale_fill_manual(values = c("2023" = "blue", "2024" = "green")) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", 
                                                    decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()

# 4. Comparativo Anual (2023/2024) - Granéis Líquidos
# Carregar pacotes
library(ggplot2)

# Data frame fornecido
dados_GL <- data.frame(
  Ano = c(rep("2023", 12), rep("2024", 12)),
  Mes = c("jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez",
          "jan", "fev", "mar", "abr", "mai", "jun", "jul", "ago", "set", "out", "nov", "dez"),
  GL = c(2252, 3856, 963, 765, 1080, 751, 884, 779, 945, 3058, 1022, 975,
         847, 807, 1169, 798, 1043, 793, 819, 838, 1118, 1050, 1100, 654)
)

# Transformar 'Mes' em um fator ordenado
dados_GL$Mes <- factor(dados_GL$Mes, levels = c("jan", "fev", "mar", "abr", "mai", "jun", 
                                                "jul", "ago", "set", "out", "nov", "dez"))

# Criar o gráfico de área
library(ggplot2)
library(ggthemes)

ggplot(dados_GL, aes(x = Mes, y = GL, fill = as.factor(Ano), group = Ano)) +
  geom_area(alpha = 0.6) +
  labs(title = "Movimentação Total Mensal por Ano (GL)",
       x = "Mês",
       y = "Movimentação Total (GL)",
       fill = "Ano") +
  scale_fill_manual(values = c("2023" = "blue", "2024" = "green")) +
  scale_y_continuous(labels = scales::number_format(big.mark = ".", 
                                                    decimal.mark = ",")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_bw()


