# CÓDIGOS R – BOLETIM ESTATÍSTICO-COMERCIAL 2024

# 1. Carregamento de Pacotes
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

# 2. Carregamento de Dados
MEP24<- read_excel("MEP_24.1.xlsx", sheet = 1) 
MEP24
str(MEP24)
View(MEP24)

# Trabalhando Apenas com os dados dos anos 2023 e 2024
MEP23_24 <- MEP24 %>% 
  filter(Ano %in% c(2023, 2024))
MEP23_24
str(MEP23_24)
View(MEP23_24)

MEP24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Perfil,Ano) %>% 
  summarise(count = n(), Quant = sum(Mov))
  



# 3. Movimentações como Função dos Perfis por Ano
MEP23_24 %>% 
  group_by(Ano,Perfil) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Perfil)) %>% 
  ggplot(aes(x=Perfil,y=Quantidade,fill=Perfil))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Perfil de Carga",y="Movimentação (Mil t)",
       fill="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  facet_wrap(~Ano,ncol=2,scales = "free")+
  theme_bw()

# 4. Nº de Atracações como Função dos Perfis por Ano
MEP23_24 %>% 
  group_by(Ano,Perfil) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Perfil,y=Atracações,fill=Perfil))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Perfil de Carga",y="Nº de Atracações",
       fill="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=2,scales = "free")+
  theme_bw()

# 5. Movimentações como função das Operações por Ano
MEP23_24 %>% 
  group_by(Ano,Operacao) %>% 
  summarise(n=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Operacao)) %>% 
  ggplot(aes(x=Operacao,y=Quantidade,fill=Operacao))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Operação Portuária",
       y="Movimentação (Mil t)",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3)) +
  facet_wrap(~Ano,ncol=2,scales = "free")+
  theme_bw()

# 6. Nº de Atracações como função das Operações por Ano
MEP23_24 %>% 
  group_by(Ano,Operacao) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Operacao)) %>% 
  ggplot(aes(x=Operacao,y=Atracações,fill=Operacao))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Operação Portuária",
       y="Nº de Atracações",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=2,scales = "free")+
  theme_bw() 

# 6. Movimentações em Função dos Berços por Ano
MEP23_24 %>% 
  group_by(Ano,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Berco)) %>%  
  ggplot(aes(x=Berco,y=Quantidade,fill=Berco))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Movimentação (Mil t)",fill="Berços")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw() +
  theme(legend.position = "none")
  

# 8. Nº de Atracações em Função dos Berços por Ano
MEP23_24 %>% 
  group_by(Ano,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Berco)) %>% 
  ggplot(aes(x=Berco,y=Atracações,fill=Berco))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Berço de Atracação",
       y="Nº de Atracações",fill="Berços")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Ano,ncol=1,scales = "free")+
  theme_bw() +
  theme(legend.position = "none")

# 9. Movimentações em Função dos Berços e dos Perfis de Carga por Ano
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

# 10. Nº de Atracações em Função dos Berços e dos Perfis de Carga por Ano
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

# 11. Movimentações em Função dos Berços e por Tipo de Carga
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

# 12. Nº de Atracações em Função dos Berços e por Tipo de Carga
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

# 13. Movimentações em Função dos Berços e das Operações de Carga por Ano
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

# 14. Nº de Atracações em Função dos Berços e das Operações de Carga por Ano
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

# 15. Movimentação em Função do Tempo (Ano) por Perfil de Carga
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

# 16. Nº de Atracações em Função do Tempo (Ano) por Perfil de Carga
MEP23_24 %>% 
  group_by(Ano,Perfil) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Perfil)) %>% 
  ggplot(aes(x=Ano,y=Atracações,fill=Perfil))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Ano",y="Nº de Atracações",fill="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scales = "free")+
  theme_bw()


# 17. Movimentação em Função do Tempo (Ano) por Operação de Carga
MEP23_24 %>% 
  group_by(Ano,Operacao) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Operacao)) %>% 
  ggplot(aes(x=Ano,y=Quantidade,fill=Operacao))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Ano",y="Movimentação (Mil t)",
       fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  facet_wrap(~Operacao,ncol=2,scales = "free")+
  theme_bw()

# 18. Nº de Atracação em Função do Tempo (Ano) por Operação de Carga
MEP23_24 %>% 
  group_by(Ano,Operacao) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Ano,y=Atracações,fill=Operacao))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Ano",
       y="Nº de Atracações",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scales = "free")+
  theme_bw()

# 19. Movimentação como Função do Tempo (Ano) por Berço
MEP23_24 %>% 
  group_by(Ano,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Berco)) %>%  
  ggplot(aes(x=Ano,y=Quantidade,fill=Berco))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Ano",y="Movimentações (t)",
       fill="Berços")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Berco,ncol=2,scales = "free")+
  theme_bw() +
  theme(legend.position = "none")

# 20. Nº de Atracações como Função do Tempo (Ano) por Berço
MEP23_24 %>% 
  group_by(Ano,Berco) %>% 
  summarise(Atracações=n(),Quantidade=sum(Mov,na.rm=T)) %>% 
  filter(!is.na(Berco)) %>%  
  ggplot(aes(x=Ano,y=Atracações,fill=Berco))+
  geom_bar(stat="identity",position="dodge",alpha=1,color="black")+
  labs(title="Porto do Recife SA",x="Ano",y="Nº de Atracações",
       fill="Berços")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Berco,ncol=2,scales = "free")+
  theme_bw() +
  theme(legend.position = "none")

# 21. Histograma 
MEP24_2 <- MEP23_24 %>% 
  filter(!is.na(Perfil)) %>% 
  filter(!is.na(Mov)) 
MEP24_2

# Carregar o "plyr" para acionar o "ddplyr"
if(!require(plyr))install.packages("plyr")
library(plyr) # Pacote responsável pelo "mu"
# Interfere no "dplyr" ("group_by" e "sammarise")
# Desativá-lo para rodar o "ggplot"

mu1<-ddply(MEP24_2,"Perfil",summarise,grp.mean=mean(Mov))
mu1

ggplot(data = MEP24_2,aes(x=Mov))+
  geom_histogram(aes(y = ..density..,fill=Perfil),color="grey",
                 bins = 25)+
  stat_density(geom = "line",color="red",size=0.5) +
  scale_x_continuous(labels=scales::number_format(big.mark = ".",decimal.mark = ",",
  ))+
  scale_y_continuous(labels=scales::percent_format(big.mark = ".",decimal.mark = ","))+
  labs(title="Porto do Recife SA",y="Frequência Relativa",
       x="Movimentação em Mil t (Últimos 5 Anos)",
       fill="Perfil de Carga")+
  geom_vline(data=mu1,aes(xintercept=grp.mean),color="red",
             linetype="dashed", size=1)+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  theme_bw()

# Desabilitar o "plyr"
if ("package:plyr" %in% search()) {
  detach("package:plyr", unload = TRUE, character.only = TRUE)
}

# 22. Ajuste Linear: Movimentações em Função do Calado
# 22.1. Movimentações em Função do Calado e por Perfil 
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Perfil),size=2)+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Operação
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Operacao),size=2)+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 22.2. Movimentações em Função do Calado e por Perfil e Comprimento dos Navios
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Perfil, size = Compr))+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Perfil", size = "Comprimento")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Operação
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Operacao, size = Compr))+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Operação", size = "Comprimento")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 22.3. Movimentações em Função do Calado e por Perfil c/ Contorno dos Pontos
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Perfil), shape = 21, size = 2, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Operação
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Operacao), shape = 21, size = 2, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)", fill = "Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 22.4. Movimentações em Função do Calado e por Perfil e Comprimento dos Navios
# c/ Contorno dos Pontos
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Perfil, size = Compr), shape = 21, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Perfil", size = "Comp. (m)")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregação por Operação
MEP23_24 %>% 
  filter(!is.na(Perfil)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Operacao, size = Compr), shape = 21, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",fill ="Operação", size = "Comp. (m)")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Perfil,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 23. Ajuste Linear: Movimentações em Função do Calado
# 23.1. Movimentações em Função do Calado e por Operação 
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Operacao),size=2)+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Perfil
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Perfil),size=2)+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Perfil")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 23.2. Movimentações em Função do Calado e por Operação e Comprimento dos Navios
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Operacao, size = Compr))+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Operação", size = "Compr. (m)")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Perfil
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(shape=16,aes(color=Perfil, size = Compr))+
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Perfil", size = "Compr. (m)")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 22.3. Movimentações em Função do Calado e por Operação c/ Contorno dos Pontos
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Operacao),shape = 21, size = 2, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Perfil
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Perfil),shape = 21, size = 2, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",fill="Operação")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 23.4. Movimentações em Função do Calado e por Perfil e Comprimento dos Navios
# c/ Contorno dos Pontos
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Operacao, size = Compr),shape = 21, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",color="Operação Portuária")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# Segregando por Perfil
MEP23_24 %>% 
  filter(!is.na(Operacao)) %>%  
  ggplot(aes(x=Calado.m,y=Mov))+
  geom_point(aes(fill = Perfil, size = Compr),shape = 21, stroke = 0.5, colour = "black") +
  geom_smooth(method = "lm",se=F,size=0.7, color="blue")+
  labs(title="Porto do Recife SA",y="Movimentação (x Mil t)",
       x="Calado (m)",perfil = "Operação", size = "Compr. (m)")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  scale_x_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  facet_wrap(~Operacao,ncol=2,scale="free")+
  stat_regline_equation()+
  theme_bw()

# 24. TOP 10
# Pacotes Extras (para ladear gráficos ou um em baixo do outro)
if(!require(patchwork))install.packages("patchwork")
library(patchwork)
if(!require(magrittr))install.packages("magrittr")
library(magrittr)

# 24.1. Maiores Movimentações por Berço
MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Berco) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

g1 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Berco) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Berco,y=Movimentação,fill=Berco))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",
       x="Berços de Atracação",fill="Berços")+
  theme_bw() +
  theme(legend.position = "none")
g1

g2 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Berco) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Berco,y=Atracações))+
  geom_point(aes(color=Berco),shape=16,size=5)+
  labs(y="Atracações",x="Berços de Atracação",color="Berço")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")
g2

g1/g2

# 24.2. Maiores Movimentações por Embarcação
MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Navio) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>%  
  slice_max(Movimentação,n=10)


g3 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Navio) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Navio,y=Movimentação,fill=Navio))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Navio",
       fill="Navio")+
  theme_bw() +
  theme(legend.position = "none")

g3

g4 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Navio) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Navio,y=Atracações))+
  geom_point(aes(color=Navio),shape=16,size=7)+
  labs(y="Atracações",x="Navio",color="Navio")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")

g4
g3/g4

# 24.3. Maiores Movimentações por Nacionalidade
MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Nacion) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

g5 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Nacion) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Nacion,y=Movimentação,fill=Nacion))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Nacionalidade",
       fill="Nacionalidade")+
  theme_bw() +
  theme(legend.position = "none")
g5

g6 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Nacion) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Nacion,y=Atracações))+
  geom_point(aes(color=Nacion),shape=16,size=7)+
  labs(y="Atracações",x="Nacionalidade",color="Nacionalidade")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")

g6
g5/g6

# 24.4. Maiores Movimentações por Agente de Navegação
MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Agente) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

g7 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Agente) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>% 
  ggplot(aes(x=Agente,y=Movimentação,fill=Agente))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Agente",
       fill="Agente")+
  theme_bw() +
  theme(legend.position = "none")
g7

g8 <- MEP23_24 %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Agente) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Agente,y=Atracações))+
  geom_point(aes(color=Agente),shape=16,size=5)+
  labs(y="Atracações",x="Agente",color="Agente")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")
g8

g7/g8

# 24.5. Maiores Movimentações por Operador Portuário
MEP23_24 %>% 
  filter(!is.na(Operador)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Operador) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

g9 <- MEP23_24 %>% 
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
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Operador",
       fill="Operador")+
  theme_bw() +
  theme(legend.position = "none")
g9

g10 <- MEP23_24 %>% 
  filter(!is.na(Operador)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Operador) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Operador,y=Atracações))+
  geom_point(aes(color=Operador),shape=16,size=5)+
  labs(y="Atracações",x="Operador",color="Operador")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")
g10

g9/g10

# 24.6. Maiores Movimentações por Tipo de Carga
MEP23_24 %>% 
  filter(!is.na(Carga)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Carga) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

g11 <- MEP23_24 %>% 
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
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Carga",
       fill="Carga")+
  theme_bw() +
  theme(legend.position = "none")
g11

g12 <- MEP23_24 %>% 
  filter(!is.na(Carga)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Carga) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Carga,y=Atracações))+
  geom_point(aes(color=Carga),shape=16,size=5)+
  labs(y="Atracações",x="Carga",color="Carga")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")
g12

g11/g12

# 24.7. Maiores Movimentações por Porto/Cidade de Origem
MEP23_24 %>% 
  filter(!is.na(Origem)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Origem) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)

g13 <- MEP23_24 %>% 
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
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Porto de Origem",
       fill="Origem")+
  theme_bw() +
  theme(legend.position = "none")
g13

g14 <- MEP23_24 %>% 
  filter(!is.na(Origem)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Origem) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Origem,y=Atracações))+
  geom_point(aes(color=Origem),shape=16,size=7)+
  labs(y="Atracações",x="Porto de Origem",color="Origem")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")
g14

g13/g14

# 24.7. Maiores Movimentações por Porto/Cidade de Destino
MEP23_24 %>% 
  filter(!is.na(Destino)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Destino) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10)


g15 <- MEP23_24 %>% 
  filter(!is.na(Destino)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Destino) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%  
  ggplot(aes(x=Destino,y=Movimentação,fill=Destino))+
  geom_bar(stat="identity",alpha=1,color="black")+
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ",",
                                                  scale=1e-3))+
  labs(title="Porto do Recife SA",y="Movimentações (Mil t)",x="Porto de Destino",
       fill="Destino")+
  theme_bw() +
  theme(legend.position = "none")
g15

g16 <- MEP23_24 %>% 
  filter(!is.na(Destino)) %>% 
  filter(!is.na(Mov)) %>% 
  group_by(Destino) %>% 
  summarise(Atracações=n(),Movimentação=sum(Mov)) %>% 
  slice_max(Movimentação,n=10) %>%   
  ggplot(aes(x=Destino,y=Atracações))+
  geom_point(aes(color=Destino),shape=16,size=5)+
  labs(y="Atracações",x="Porto de Destino",color="Destino")+ 
  scale_y_continuous(labels=scales::number_format(big.mark = ".",
                                                  decimal.mark = ","))+
  theme_bw() +
  theme(legend.position = "none")
g16

g15/g16
