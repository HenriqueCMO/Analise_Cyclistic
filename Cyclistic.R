# Análise Cyclistyc 
# Data: janeiro/2022
# Criado por : Henrique

# Carregar bibliotecas
library(tidyverse)
library(lubridate)
library(janitor)
library(csvread)
library(skimr)
library(scales)
library(ggrepel)

# Exibir diretório de trabalho
setwd("C:/Users/Henrique/Desktop/Capstone/Dados/Arquivos_csv")

# Carregar conjunto de dados
q1<-read_csv("Q1.csv")
q2<-read_csv("Q2.csv")
q3<-read_csv("Q3.csv")
q4<-read_csv("Q4.csv")

# Exibir nomes das colunas
colnames(q1)
colnames(q2) # possui nomes de colunas divergentes com as demais
colnames(q3)
colnames(q4)

# Renomear nomes das colunas
q1 <- rename(q1
             ,id_passeio = trip_id
             ,tipo_bike = bikeid 
             ,inicio = start_time  
             ,final = end_time  
             ,nome_estacao_inicio = from_station_name 
             ,id_estacao_inicio = from_station_id 
             ,nome_estacao_final = to_station_name 
             ,id_estacao_final = to_station_id 
             ,tipo_membro = usertype
             ,genero = gender
             ,ano_nascimento = birthyear
)


q2 <- rename(q2
             ,id_passeio = "01 - Rental Details Rental ID"
             ,tipo_bike = "01 - Rental Details Bike ID"
             ,tripduration = "01 - Rental Details Duration In Seconds Uncapped"
             ,inicio = "01 - Rental Details Local Start Time"
             ,final = "01 - Rental Details Local End Time"
             ,nome_estacao_inicio = "03 - Rental Start Station Name"
             ,id_estacao_inicio = "03 - Rental Start Station ID"
             ,nome_estacao_final = "02 - Rental End Station Name"
             ,id_estacao_final = "02 - Rental End Station ID"
             ,tipo_membro = "User Type"
             ,genero = "Member Gender"
             ,ano_nascimento = "05 - Member Details Member Birthday Year"
)



q3 <- rename(q3
             ,id_passeio = trip_id
             ,tipo_bike = bikeid 
             ,inicio = start_time  
             ,final = end_time  
             ,nome_estacao_inicio = from_station_name 
             ,id_estacao_inicio = from_station_id 
             ,nome_estacao_final = to_station_name 
             ,id_estacao_final = to_station_id 
             ,tipo_membro = usertype
             ,genero = gender
             ,ano_nascimento = birthyear
)

q4 <- rename(q4
             ,id_passeio = trip_id
             ,tipo_bike = bikeid 
             ,inicio = start_time  
             ,final = end_time  
             ,nome_estacao_inicio = from_station_name 
             ,id_estacao_inicio = from_station_id 
             ,nome_estacao_final = to_station_name 
             ,id_estacao_final = to_station_id 
             ,tipo_membro = usertype
             ,genero = gender
             ,ano_nascimento = birthyear
)

# Exibir nomes das colunas renomeadas
colnames(q1)
colnames(q2) 
colnames(q3)
colnames(q4)


# Inspecionar tópicos e procurar divergências
str(q1)
str(q2)
str(q3)
str(q4)


# Converter colunas id_passeio e tipo_bike em 'character'
q1 <- mutate(q1,
             id_passeio = as.character(id_passeio)
             ,tipo_bike = as.character(tipo_bike)
)
q2 <- mutate(q2,
             id_passeio = as.character(id_passeio)
             ,tipo_bike = as.character(tipo_bike)
)
q3 <- mutate(q3,
             id_passeio = as.character(id_passeio)
             ,tipo_bike = as.character(tipo_bike)
)
q4 <- mutate(q4,
             id_passeio = as.character(id_passeio)
             ,tipo_bike = as.character(tipo_bike)
)

# Unir todos daframes em um único dataframe
viagens_totais <- bind_rows(q1,q2,q3,q4)


# Visualizar dataframe 'viagens_totais'
view(viagens_totais)

# Exibir estatística sumária do dataframe 'viagens_totais'
resumo <- skim(viagens_totais)
view(resumo)

# Eliminar colunas 
viagens_totais <- viagens_totais %>% 
  select(-c
         (ano_nascimento,
           tripduration,
           tipo_bike,
           genero) 
  )


# Exibir nomes das colunas após exclusão
colnames(viagens_totais)

# Exibir número de linhas do dataframe
nrow(viagens_totais)
ncol(viagens_totais)

# Exibir número de linhas e de colunas do dataframe
dim(viagens_totais)

# Retornar e visualizar as 6 primeiras linhas do dataframe
view(head(viagens_totais))

# Retornar e visualizar as 6 últimas linhas do dataframe
view(tail(viagens_totais))

# Retorna um compacto da estrutura do dataframe
str(viagens_totais)

# Retorna um resumo do dataframe
summary(viagens_totais)

# Renomear dados da coluna tipo_membro 
viagens_totais <-  viagens_totais %>% 
  mutate(tipo_membro = 
           recode(tipo_membro,
                  "Subscriber" = "membro",
                  "Customer" = "casual")
  )

# Exibir contagem do número de membros e do número de casuais
table(viagens_totais$tipo_membro)

# Adicionar colunas 'dia', 'mes', 'ano', 'data' com base na coluna 'início'
viagens_totais$data <-as.Date(viagens_totais$inicio)  # O formato padrão é yyyy-mm-dd 
viagens_totais$dia <- format(as.Date(viagens_totais$data), "%d")
viagens_totais$mes <- format(as.Date(viagens_totais$data), "%m")
viagens_totais$ano <- format(as.Date(viagens_totais$data), "%Y")
viagens_totais$dia_da_semana <- format(as.Date(viagens_totais$data), "%A")
#viagens_totais$data <-format(as.Date(viagens_totais$data),"%d %m %Y") # Altera o formata para dd-mm-yyyy

# Criar coluna duracao_passeio
viagens_totais$duracao_passeio <- difftime(viagens_totais$final,viagens_totais$inicio)

# Inspecionar tópicos para verificar tipo dos dados
str(viagens_totais)

# Conferir se é Fator
is.factor(viagens_totais$duracao_passeio)

# Converter para Numérico
viagens_totais$duracao_passeio <- as.numeric(as.character(viagens_totais$duracao_passeio))

# Conferir se é Numérico
is.numeric(viagens_totais$duracao_passeio)

# Verificar tempo máximo e mínimo de viagens
max(viagens_totais$duracao_passeio)
min(viagens_totais$duracao_passeio)

# verificar quantidade de viagens com tempo negativo
# Segundo roteiro viagem com tempo negativo siginifica que foram retiradas das docas para verificação
filtro <- filter(viagens_totais,duracao_passeio < 0)

# Remover dados de duracao_passeio < 0  carregando resultado em um novo dataframe
viagens_totais_v2 <- viagens_totais [! (viagens_totais$duracao_passeio < 0),]

# Exibirestatística sumária do dataframe 'viagens_totais_v2'
skim(viagens_totais_v2)


## explorando tabelas

# tabela número de viagens x tipo de membros
tabela_contagem <- viagens_totais_v2 %>% 
  group_by(tipo_membro) %>% 
  summarise(contagem = n())

# tabela tipo de membro x duração passeio
tabela_hora <- viagens_totais_v2 %>% 
  group_by(tipo_membro, duracao_passeio) %>% 
  summarise(contagem = n())

# tabela tipo de membro x dia da semana
tabela_dia <- viagens_totais_v2 %>% 
  group_by(tipo_membro, dia_da_semana) %>% 
  summarise(contagem = n())

# Reorganizar dias da semana 
tabela_dia$dia_da_semana <- ordered(tabela_dia$dia_da_semana,
                                           levels = c("segunda-feira",
                                                      "terça-feira",
                                                      "quarta-feira",
                                                      "quinta-feira",
                                                      "sexta-feira",
                                                      "sábado",
                                                      "domingo")
)


# tabela tipo de membro x mês
tabela_mes <- viagens_totais_v2 %>% 
  group_by(tipo_membro, mes) %>% 
  summarise(contagem = n())

# Converter mês do tipo numérico para tipo caracter
tabela_mes$mes <- month(as.numeric(tabela_mes$mes),label = TRUE) 














# Hour Viz
  
ggplot(data=tabela_hora,
         aes(x = duracao_passeio/60, y = contagem, fill = tipo_membro)) +
  facet_grid(~tipo_membro) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Casual and Member by Hour",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Número de viagens") +
  geom_text(aes(label=contagem), vjust=1.6, color="white",
            position = position_dodge(0.9), size=2,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(0,60,5))
  
  












#-------------------------------------------------

# Inicio análise descritiva

#-------------------------------------------------

# Retorna um resumo do dataframe e coluna selecionados
summary(viagens_totais_v2$duracao_passeio)


#tempo medio de viagem
mean(viagens_totais_v2$duracao_passeio)

# Tempo de viagem mediano na amostra
median(viagens_totais_v2$duracao_passeio)

# Viagem mais longa
max(viagens_totais_v2$duracao_passeio)

# Viagem mais curta
min(viagens_totais_v2$duracao_passeio)

# Exibir resumo dos tempos de viagem
summary(viagens_totais_v2$duracao_passeio) 


# Tabela compararativa dos tempos de viagens entre 'membros' vs 'casuais'

tabela_media <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = mean)
tabela_mediana <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = median)  
tabela_maximo <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = max)
tabela_minimo <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = min)

# verificar media de viagens entre 'membros' vs 'casuais' de acordo com os dias da semana
analise_semana <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro + 
          viagens_totais_v2$dia_da_semana, FUN = mean)

view(analise_semana)

# Reorganizar dias da semana 
viagens_totais_v2$dia_da_semana <- ordered(viagens_totais_v2$dia_da_semana,
                  levels = c("segunda-feira",
                             "terça-feira",
                             "quarta-feira",
                             "quinta-feira",
                             "sexta-feira",
                             "sábado",
                             "domingo")
                             )


# Verificar novamente a mesma media com os dias da semana ordenados
analise_semana_1 <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro + 
                               viagens_totais_v2$dia_da_semana, FUN = mean)

view(analise_semana_1)  

# Verificar número de viagens por dia da semana feito por cada tipo de membro
n_viagem <- viagens_totais_v2 %>%
  mutate(dia_semana = wday(inicio, label = TRUE)) %>% # Cria campo dia_semana
  group_by(tipo_membro,dia_semana) %>% # Agrupar por tipo_embro e dia_semana
  summarise(numero_de_viagens = n(),
  duracao_media =  mean(duracao_passeio)) %>%
  arrange(tipo_membro,dia_semana)

view(n_viagem)

# Verificar duração das viagens feito por cada tipo de membro a cada dia da semana

analise <- aggregate(viagens_totais_v2$duracao_passeio ~ 
                       viagens_totais_v2$tipo_membro + 
                       viagens_totais_v2$dia_da_semana, FUN = mean)


#-------------------------------------------------

# Plotagem dos gráficos

#-------------------------------------------------


# Plotar gráfico do 'número de viagens' vs 'tipo de membro' 
ggplot(data=tabela_contagem,
       aes(x = tipo_membro, y = contagem, fill = tipo_membro)) +
  geom_bar(stat="identity", width = 0.7) +
  geom_text(aes(label=contagem), vjust=3, color="black", size=4)+
  labs(title="Comparação do número de viagens realizadas",
       subtitle="",
       caption = "Ano: 2019",
       x = "Tipo de Membro",
       y = "Número de viagens") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 0), legend.position="none",
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)


# Plotar gráfico donuts comparando percentual de utilização
donuts <- data.frame(tabela_contagem$tipo_membro, 
                     as.numeric(tabela_contagem$contagem))
donuts$fraction <- donuts$as.numeric.tabela_contagem/sum(donuts$as.numeric.tabela_contagem)
donuts$ymax <- cumsum(donuts$fraction)
donuts$ymin <- c(0, head(donuts$ymax, n = -1))
donuts$labelposition <- (donuts$ymax + donuts$ymin) / 2
donuts$label <- paste0(donuts$tabela_contagem.tipo_membro,
                       "\n" , 
                       round(donuts$fraction*100, digits = 2),
                       "%")
ggplot(donuts, aes(ymax=ymax, ymin=ymin, 
                   xmax=4, xmin=3, 
                   fill = tabela_contagem.tipo_membro )) +
  geom_rect() +
  geom_label(x = 3.5, aes(y = labelposition, label = label), size = 4.5) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  labs(title="Comparação em porcentagem entre Membro vs. Casual",
       subtitle="",
       caption = "Ano: 2019") +
  theme(plot.title = element_text(hjust=0.5), legend.position="none",
        panel.background = element_rect(fill = "gray88" , color = "black"))


# Plotar gráfico de comparação diária entre tipo de membro 
ggplot(data=tabela_dia,
       aes(x = dia_da_semana, y = contagem, fill = tipo_membro)) +
  facet_grid(~tipo_membro) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Comparação do número de viagens realizadas semanalmente",
       subtitle="",
       caption = "Ano: 2019",
       fill=" Tipo de Membro",
       x = "",
       y = "Número de viagens") +
  geom_text(aes(label=contagem), vjust=5, color="black",
            position = position_dodge(0.9), size=3,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(hjust=0.5, angle = 90),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)


# Plotar gráfico de comparação mensal entre tipo de membro 
ggplot(data=tabela_mes,
       aes(x = factor(mes), 
           y = contagem, fill = tipo_membro)) +
  facet_grid(~tipo_membro) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Comparação do número de viagens realizadas mensalmente",
       caption = "Ano: 2019",
       subtitle="",
       fill=" Tipo de Membro",
       x = "",
       y = "Número de viagens") +
  geom_text(aes(label=contagem), vjust=2, color="black",
            position = position_dodge(0.9), size=2.5,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 90),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)


# Plotar gráfico de linhas do número de viagens por mês
ggplot(data=tabela_mes,
       aes(x = mes, y = contagem,
           color = tipo_membro,
           group = tipo_membro,)) +
  geom_line(size = 1.2) + 
  labs(title = "Comparação do número de viagens realizados a cada mês",
       caption = "Ano: 2019",
       x = "",
       y = "Número de viagens") +
  scale_y_continuous(labels = label_number()) +
  scale_color_manual(name = "Tipo de Membro", 
                     values = c("#F8766D", "#00BFC4")) + 
  geom_vline(xintercept = 6, linetype="dotted",color = "darkorchid2", size=1.5) +
  
  geom_vline(xintercept = 8, linetype="dotted",color = "darkorchid2", size=1.5) 
  


# Plotar gráfico do 'número de viagens' vs 'dias da semana' comparado por cada tipo de membro   
viagens_totais_v2 %>%
  mutate(dia_semana = wday(inicio, label = TRUE)) %>% # Cria campo dia_semana
  group_by(tipo_membro,dia_semana) %>% # Agrupar por tipo_embro e dia_semana
  summarise(numero_de_viagens = n(),
  duracao_media =  mean(duracao_passeio)) %>%
  arrange(tipo_membro,dia_semana) %>%
  ggplot(aes(x = dia_semana, y = numero_de_viagens, fill = tipo_membro)) +
  labs(x = "Dia da Semana",
       y = "Número de viagens",
       fill ="Tipo de membro",
       title = "Comparação do número de viagens em relação a cada tipo de membro ",
       caption = "Ano: 2019") +
  scale_y_continuous(labels = label_number()) +
  geom_col(position = "dodge") +
  geom_text(aes(label=numero_de_viagens), vjust=5, color="black",
            position = position_dodge(0.9), size=3.5, angle = 0) +
  theme(plot.title = element_text(hjust=0.5))


# Plotar gráfico duração média de viagens   
viagens_totais_v2 %>%
  mutate(dia_semana = wday(inicio, label = TRUE)) %>% # Cria campo dia_semana
  group_by(tipo_membro,dia_semana) %>% # Agrupar por tipo_embro e dia_semana
  summarise(numero_de_viagens = n(),
            duracao_media =  mean(duracao_passeio)) %>%
  arrange(tipo_membro,dia_semana) %>%
  ggplot(aes(x = dia_semana, y = duracao_media, fill = tipo_membro)) +
  labs(x = "Dia da semana",
       y = "Durração média (min.)",
       fill ="Tipo de membro",
       title = "Relação da duração de viagens comparado por tipo de membro",
       caption = "Ano: 2019") +
  scale_y_continuous(breaks = seq(0,60,10)) +
  geom_col(position = "dodge") +
  theme(plot.title = element_text(hjust=0.5)) +
  geom_text(aes(label=round(duracao_media, digits = 2)), vjust=5, color="black",
            position = position_dodge(0.9), size=3.5, angle = 0)


#-------------------------------------------------

# Exportar análises para arquivos csv

#-------------------------------------------------

write.csv(tabela_contagem,"C:\\Users\\Henrique\\Desktop\\Tabela Contagem.csv", row.names = FALSE)
write.csv(tabela_hora,"C:\\Users\\Henrique\\Desktop\\Tabela Hora.csv", row.names = FALSE)
write.csv(tabela_dia,"C:\\Users\\Henrique\\Desktop\\Tabela Dia.csv", row.names = FALSE)
write.csv(tabela_mes,"C:\\Users\\Henrique\\Desktop\\Tabela Mês.csv", row.names = FALSE)
write.csv(tabela_media,"C:\\Users\\Henrique\\Desktop\\Tabela Média.csv", row.names = FALSE)
write.csv(tabela_mediana,"C:\\Users\\Henrique\\Desktop\\Tabela Mediana.csv", row.names = FALSE)
write.csv(tabela_maximo,"C:\\Users\\Henrique\\Desktop\\Tabela Maximo.csv", row.names = FALSE)
write.csv(tabela_minimo,"C:\\Users\\Henrique\\Desktop\\Tabela Minimo.csv", row.names = FALSE)




write.csv(analise,"C:\\Users\\Henrique\\Desktop\\Análise.csv", row.names = FALSE)
write.csv(n_viagem,"C:\\Users\\Henrique\\Desktop\\Numero de viagens", row.names = FALSE)
write.csv(analise_semana_1,"C:\\Users\\Henrique\\Desktop\\Analise semana.csv", row.names = FALSE)

write.csv(viagens_totais_v2,"C:\\Users\\Henrique\\Desktop\\Cyclistic_analise.csv", row.names = FALSE)



tabela_contagem <- viagens_totais_v2 %>% 
  group_by(tipo_membro) %>% 
  summarise(mean = round(seconds_to_period(
    mean(duracao_passeio, na.rm = TRUE)), digits = 2),
    min = seconds_to_period(min(duracao_passeio, na.rm = TRUE)),
    max = seconds_to_period(max(duracao_passeio, na.rm = TRUE))
  )


#-------------------------------------------------

ggplot(data=tabela_contagem,
       aes(x = tipo_membro, 
           y = as.numeric(mean), 
           fill = tipo_membro)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(title="Casual and Member",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Duração da viagem em minutos") +
  geom_text(aes(label=mean), vjust=3, color="black",
            position = position_dodge(0.9), size=4.5,) +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 0), legend.position="none",
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)






ggplot(data=tabela_hora,
       aes(x = duracao_passeio, y = as.numeric(mean)/60, fill = tipo_membro)) +
  facet_grid(~tipo_membro) +
  geom_bar(stat="identity",
           position="dodge") +
  labs(title="Casual and Member by Hour",
       caption = "Data from jan/20 to oct/21",
       x = "",
       y = "Ride Length Minutes") +
  theme(plot.title = element_text(hjust=0.5), 
        axis.text.x = element_text(angle = 45),
        panel.background = element_rect(fill = "gray88")) +
  scale_y_continuous(labels = scales::comma)







