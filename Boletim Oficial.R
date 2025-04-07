# CÓDIGOS R – BOLETIM ESTATÍSTICO-COMERCIAL 2024

# Carregamento de Pacotes
if(!require(ggplot2))install.packages("ggplot2")
library(ggplot2)
if(!require(dplyr))install.packages("dplyr")
library(dplyr)
if(!require(corrplot))install.packages("corrplot")
library(corrplot)
if(!require(PerformanceAnalytics))install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
if(!require(readxl))install.packages("readxl")
library(readxl)
##
if(!require(plyr))install.packages("plyr")
library(plyr) # Pacote responsável pelo 'mu'
# Interfere no 'ggplot2' e 'dplyr' ('group_by' e 'sammarise')
# Desativá-lo para rodar o 

if(!require(ggthemes))install.packages("ggthemes")
library(ggthemes)

if(!require(ggpubr))install.packages("ggpubr")
library(ggpubr) # Equação da Reta

# 1. Mov/Atrac como função do Tipo de Operação por Ano
# Ajuste dos Dados
MEP24_full<- read_excel("MEP_24_full.xlsx", sheet = 1) 
MEP24_full
str(MEP24_full)
MEP24_full$Ano <- as.character(MEP24_full$Ano)
str(MEP24)
View(MEP24)

# Trabalhando Apenas com os dados dos anos 2023 e 2024
MEP23_24 <- MEP24_full %>% 
  filter(Ano %in% c(2023, 2024))
MEP23_24
str(MEP23_24)

# Preparando os dados para representá-los em barras
MEP23_24 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Operacao,Ano) %>% 
  summarise(Atrac = n(), Quant = sum(Mov))
MEP23_24

# Gráficos
# Pacotes Extras (para ladear gráficos ou um em baixo do outro)
if(!require(patchwork))install.packages("patchwork")
library(patchwork)
if(!require(magrittr))install.packages("magrittr")
library(magrittr)

g3 <- ggplot(MEP23_24, aes(x = Operacao, y = Quant, fill = Ano)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
  #geom_text(aes(label = scales::number(Quant/1000, big.mark = ".", decimal.mark = ",")), 
            #position = position_dodge(width = 0.9), # Alinhamento dos rótulos
            #vjust = 1.2, size = 3.5) + 
  scale_fill_manual(values = c("2023" = "steelblue", "2024" = "tomato")) +
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(x = "Operação", y = "Movimentação (x Mil t)", fill = "Ano") +
  theme_bw()
g3

g4 <- ggplot(MEP23_24, aes(x = Operacao, y = Atrac, fill = Ano)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
  #geom_text(aes(label = scales::number(Atrac, big.mark = ".", decimal.mark = ",")), 
            #position = position_dodge(width = 0.9), # Alinhamento dos rótulos
            #vjust = 1.2, size = 3.5) + 
  scale_fill_manual(values = c("2023" = "steelblue", "2024" = "tomato")) +
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  labs(x = "Operação", y = "Nº de Atracações", fill = "Ano") +
  theme_bw()
g4

g3/g4

# 2. Mov/Atrac como função do Perfil da Carga por Ano
# Ajustando os Dados
MEP24_full<- read_excel("MEP_24_full.xlsx", sheet = 1) 
MEP24_full
str(MEP24_full)
MEP24_full$Ano <- as.character(MEP24_full$Ano)
str(MEP24_full)
View(MEP24_full)

# Trabalhando Apenas com os dados dos anos 2023 e 2024
MEP23_24 <- MEP24_full %>% 
  filter(Ano %in% c(2023, 2024))
MEP23_24
str(MEP23_24)

# Preparando os dados para representá-los em barras
MEP23_24 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Perfil,Ano) %>% 
  summarise(Atrac = n(), Quant = sum(Mov))
MEP23_24

# Pacotes Extras (para ladear gráficos ou um em baixo do outro)
if(!require(patchwork))install.packages("patchwork")
library(patchwork)
if(!require(magrittr))install.packages("magrittr")
library(magrittr)

# Gráficos
g1 <- ggplot(MEP23_24, aes(x = Perfil, y = Quant, fill = Ano)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
  #geom_text(aes(label = scales::number(Quant/1000, big.mark = ".", decimal.mark = ",")), 
            #position = position_dodge(width = 0.9), # Alinhamento dos rótulos
            #vjust = -0.1, size = 3.5) + 
  scale_fill_manual(values = c("2023" = "steelblue", "2024" = "tomato")) +
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(x = "Perfil de Carga", y = "Movimentação (x Mil t)", fill = "Ano") +
  theme_bw()
g1

g2 <- ggplot(MEP23_24, aes(x = Perfil, y = Atrac, fill = Ano)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
  #geom_text(aes(label = scales::number(Atrac, big.mark = ".", decimal.mark = ",")), 
            #position = position_dodge(width = 0.9), # Alinhamento dos rótulos
            #vjust = -0.1, size = 3.5) + 
  scale_fill_manual(values = c("2023" = "steelblue", "2024" = "tomato")) +
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  labs(x = "Perfil de Carga", y = "Nº de Atracações", fill = "Ano") +
  theme_bw()
g2

g1/g2

# 3. Mov/Atrac como função do Tipo de Carga por Ano
# Ajuste dos Dados
MEP24_full<- read_excel("MEP_24_full.xlsx", sheet = 1) 
MEP24_full
str(MEP24_full)
MEP24_full$Ano <- as.character(MEP24_full$Ano)
str(MEP24_full)
View(MEP24_full)

# Trabalhando Apenas com os dados dos anos 2023 e 2024
MEP23_24 <- MEP24_full %>% 
  filter(Ano %in% c(2023, 2024))
MEP23_24
str(MEP23_24)

# Preparando os dados para representá-los em barras
MEP23_24 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  filter(!is.na(Carga)) %>%
  group_by(Carga,Ano) %>% 
  summarise(Atrac = n(), Quant = sum(Mov))
MEP23_24
print(MEP23_24, n = Inf) # Visualizar toda a tabela (data frame)

# Gráficos
g5 <- ggplot(MEP23_24, aes(x = Carga, y = Quant, fill = Ano)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
  #geom_text(aes(label = scales::number(Quant/1000, big.mark = ".", decimal.mark = ",")), 
            #position = position_dodge(width = 0.9), # Alinhamento dos rótulos
            #vjust = 0.8, size = 2.7) + 
  scale_fill_manual(values = c("2023" = "steelblue", "2024" = "tomato")) +
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(x = "Tipo de Carga", y = "Movimentação (x Mil t)", fill = "Ano") +
  theme_bw() 
g5

g6 <- ggplot(MEP23_24, aes(x = Carga, y = Atrac, fill = Ano)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
  #geom_text(aes(label = scales::number(Atrac, big.mark = ".", decimal.mark = ",")), 
            #position = position_dodge(width = 0.9), # Alinhamento dos rótulos
            #vjust = 0.8, size = 2.7) + 
  scale_fill_manual(values = c("2023" = "steelblue", "2024" = "tomato")) +
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  labs(x = "Tipo de Carga", y = "Nº de Atracações", fill = "Ano") +
  theme_bw()
g6

g5/g6

# 4. Movimentação em Função dos Berços por Perfil de Carga por Ano (2023/2024)
MEP23_24 <- MEP24_full %>% 
  filter(Ano %in% c(2023, 2024))
MEP23_24
str(MEP23_24)

View(MEP23_24 %>% 
  group_by(Ano,Perfil,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Perfil)))


print(MEP23_24 %>% 
        group_by(Ano,Perfil,Berco) %>% 
        summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
        filter(!is.na(Berco)) %>% 
        filter(!is.na(Perfil)), n = Inf)


MEP23_24 %>% 
  group_by(Ano,Perfil,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Perfil)) %>% 
  ggplot(aes(x=Berco,y=Quantidade,fill=Perfil))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Movimentação (Mil t)",fill="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw()

# 4. Atracações em Função dos Berços e por Perfil de Carga por Ano (2023/2024)
MEP23_24 %>% 
  group_by(Ano,Perfil,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Perfil)) %>% 
  ggplot(aes(x=Berco,y=Atracações,fill=Perfil))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Nº de Atracações",fill="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw()

# 5. Movimentação em Função dos Berços e por Tipo de Carga por Ano (2023/2024)
print(MEP23_24 %>% 
  group_by(Ano,Carga,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Carga)),n = Inf)

MEP23_24 %>% 
  group_by(Ano,Carga,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Carga)) %>% 
  ggplot(aes(x=Berco,y=Quantidade,fill=Carga))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Movimentações",fill="Carga")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw()

# 6. Atracações em Função dos Berços e por Tipo de Carga por Ano (2023/2024)
MEP23_24 %>% 
  group_by(Ano,Carga,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%   
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Carga)) %>% 
  ggplot(aes(x=Berco,y=Atracações,fill=Carga))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Nº de Atracações",fill="Carga")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw()

# 7. Movimentação em Função dos Berços e por Tipo de Operação por Ano (2023/2024)
View(MEP23_24 %>% 
  group_by(Ano,Operacao,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%     
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Operacao)))

MEP23_24 %>% 
  group_by(Ano,Operacao,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%     
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Operacao)) %>% 
  ggplot(aes(x=Berco,y=Quantidade,fill=Operacao))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Movimentação (Mil t)",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw()

# 8. Atracação em Função dos Berços e por Tipo de Operação por Ano (2023/2024)
MEP23_24 %>% 
  group_by(Ano,Operacao,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>%     
  filter(!is.na(Berco)) %>% 
  filter(!is.na(Operacao)) %>% 
  ggplot(aes(x=Berco,y=Atracações,fill=Operacao))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Nº de Atracações",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw()

# 9. Movimentação em Função do Tempo (Ano) por Perfil de Carga
MEP23_24 %>% 
  group_by(Ano,Perfil) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Perfil)) %>%   
  ggplot(aes(x=Ano,y=Quantidade,fill=Perfil))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Ano",y="Movimentação (Mil t)",fill="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  facet_wrap(~Perfil,ncol=2,scales = "free")+
  theme_bw()


# 10. TOP 10 - MAIORES MOVIMENTAÇÕES
# Ajustando os Dados
MEP24 <- MEP24_full %>% 
  filter(Ano == 2024)
MEP24
MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Berco) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

# 10.1. TOP 10 - Berços 2024
g1_berco <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Berco) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Berco,y=Movimentação,fill=Berco))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 BERÇOS (2024)",y="Movimentações (Mil t)",
       x="Berços de Atracação",fill="Berços")+
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none")
g1_berco

g2_berco <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Berco) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Berco,y=Atracações))+
  geom_point(aes(fill=Berco),shape=21, size = 5, color = "black")+
  labs(y="Atracações",x="",color="Berço")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none")
g2_berco

g1_berco + g2_berco

# 10.2. TOP 10 - Embarcações 2024
g1_embar <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Navio) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Navio,y=Movimentação,fill=Navio))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 NAVIOS (2024)",y="Movimentações (Mil t)",x="Navios",
       fill="Navio")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

g1_embar

g2_embar <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Navio) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Navio,y=Atracações))+
  geom_point(aes(fill=Navio),shape=21, size = 5, color = "black")+
  labs(y="Atracações",x="",color="Navio")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

g2_embar

g1_embar + g2_embar

# 10.3. TOP 10 - Nacionalidade 2024 
g1_nac <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Nacion) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Nacion,y=Movimentação,fill=Nacion))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 NACIONALIDADES (2024)",y="Movimentações (Mil t)",x="Nacionalidade",
       fill="Nacionalidade")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g1_nac

g2_nac <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Nacion) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Nacion,y=Atracações))+
  geom_point(aes(fill=Nacion),shape=21,size=7, color = "black")+
  labs(y="Atracações",x="",color="Nacionalidade")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")

g2_nac

g1_nac + g2_nac

# 10.4. TOP 10 - Agente de Navegação 2024
g1_agen <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Agente) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Agente,y=Movimentação,fill=Agente))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 AGENTE NAV. (2024)",y="Movimentações (Mil t)",x="Agente de Navegação",
       fill="Agente")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g1_agen

g2_agen <- MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Agente) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Agente,y=Atracações))+
  geom_point(aes(fill=Agente),shape=21,size=5, color = "black")+
  labs(y="Atracações",x="",color="Agente")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g2_agen

g1_agen + g2_agen

# 10.5. Top 10 - Operador Portuário 2024
g1_oper <- MEP24 %>% 
  filter(!is.na(Operador)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Operador) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Operador,y=Movimentação,fill=Operador))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 OPERADOR PORT. (2024)",y="Movimentações (Mil t)",x="Operador",
       fill="Operador")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g1_oper

g2_oper <- MEP24 %>% 
  filter(!is.na(Operador)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Operador) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)
g2_oper
  ggplot(aes(x=Operador,y=Atracações))+
  geom_point(aes(fill = Operador),shape=21,size=5, color = "black")+
  labs(y="Atracações",x="",color="Operador")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g2_oper

g1_oper + g2_oper

# 10.6. TOP 10 - Cargas 2024
g1_carga <- MEP24 %>% 
  filter(!is.na(Carga)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Carga) %>%  
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Carga,y=Movimentação,fill=Carga))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 Cargas (2024)",y="Movimentações (Mil t)",x="Carga",
       fill="Carga")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g1_carga

g2_carga <- MEP24 %>% 
  filter(!is.na(Carga)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Carga) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Carga,y=Atracações))+
  geom_point(aes(fill = Carga),shape=21,size=5, color = "black")+
  labs(y="Atracações",x="",color="Carga")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g2_carga

g1_carga + g2_carga

# 10.7. TOP 10 - Cidades/Portos de Origem
g1_ori <- MEP24 %>% 
  filter(!is.na(Origem)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Origem) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Origem,y=Movimentação,fill=Origem))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP 10 Origem (2024)",y="Movimentações (Mil t)",x="Porto de Origem",
       fill="Origem")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g1_ori

g2_ori <- MEP24 %>% 
  filter(!is.na(Origem)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Origem) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Origem,y=Atracações))+
  geom_point(aes(fill = Origem),shape=21,size=5, color ="black")+
  labs(y="Atracações",x="",color="Origem")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g2_ori

g1_ori + g2_ori

# 10.7. TOP 10 - Cidades/Portos Destinos
g1_dest <- MEP24 %>% 
  filter(!is.na(Destino)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Destino) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)  
g1_dest
  ggplot(aes(x=Destino,y=Movimentação,fill=Destino))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA - TOP DESTINOS (2024)",y="Movimentações (Mil t)",x="Porto de Destino",
       fill="Destino")+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g1_dest

g2_dest <- MEP24 %>% 
  filter(!is.na(Destino)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Destino) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Destino,y=Atracações))+
  geom_point(aes(fill=Destino),shape=21,size=5, color = "black")+
  labs(y="Atracações",x="",color="Destino")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
g2_dest

g1_dest + g2_dest





















































