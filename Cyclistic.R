# Análise Cyclistyc 
# Data: janeiro/2022
# Criado por : Henrique

# Carregar bibliotecas
library(tidyverse)
library(lubridate)
library(janitor)
library(csvread)

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

# Unir todos daframes em um único
viagens_totais <- bind_rows(q1,q2,q3,q4)

# Visualizar dataframe 'viagens_totais'
view(viagens_totais)

# Eliminar colunas 
viagens_totais <- viagens_totais %>% 
  select(-c
         (ano_nascimento,
           genero,
           tripduration)
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

# Exibir contagem do número de membros e do número de casuais
table(viagens_totais$tipo_membro)

# Renomear dados da coluna tipo_membro 
viagens_totais <-  viagens_totais %>% 
  mutate(tipo_membro = 
           recode(tipo_membro,
                  "Subscriber" = "membro",
                  "Customer" = "casual")
  )

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

#-------------------------------------------------

# Inicio análise

#-------------------------------------------------

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


# Compararar dos tempos de viagens entre 'membros' vs 'casuais'
aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = mean)
aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = median)
aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = max)
aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro, FUN = min)

# verificar media de viagens entre 'membros' vs 'casuais' de acordo com os dias da semana
analise_semana <- aggregate(viagens_totais_v2$duracao_passeio ~ viagens_totais_v2$tipo_membro + 
          viagens_totais_v2$dia_da_semana, FUN = mean)
view(analise_semana)

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


# Plotar gráfico do 'número de viagens' vs 'dias da semana' comparado por cada tipo de membro   
viagens_totais_v2 %>%
  mutate(dia_semana = wday(inicio, label = TRUE)) %>% # Cria campo dia_semana
  group_by(tipo_membro,dia_semana) %>% # Agrupar por tipo_embro e dia_semana
  summarise(numero_de_viagens = n(),
  duracao_media =  mean(duracao_passeio)) %>%
  arrange(tipo_membro,dia_semana) %>%
  ggplot(aes(x = dia_semana, y = numero_de_viagens, fill = tipo_membro)) +
  labs(x = "Dia da semana",
       y = "Número de viagens",
       fill ="Tipo de membro",
       title = "Relação do número de viagens comparado por tipo de membro",
       caption = "Autor: Henrique Coelho") +
       scale_y_continuous(breaks = seq(0,4000000,200000)) +
       geom_col(position = "dodge")

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
       caption = "Autor: Henrique Coelho") +
  scale_y_continuous(breaks = seq(0,60,10)) +
  geom_col(position = "dodge")

analise <- aggregate(viagens_totais_v2$duracao_passeio ~ 
                     viagens_totais_v2$tipo_membro + 
                     viagens_totais_v2$dia_da_semana, FUN = mean)
view(analise)


view(n_viagem)
view(analise_semana)  


write.csv(analise,"C:\\Users\\Henrique\\Desktop\\Análise.csv", row.names = FALSE)
write.csv(n_viagem,"C:\\Users\\Henrique\\Desktop\\Numero de viagens", row.names = FALSE)
write.csv(analise_semana,"C:\\Users\\Henrique\\Desktop\\Analise semana.csv", row.names = FALSE)

           
