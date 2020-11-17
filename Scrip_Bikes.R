##Scrip 1
# Este código contém comandos para filtrar e plotar os dados de aluguel de bikes, dados que estão em nosso dataset
# Este código foi criado para executar tanto no Azure, quanto no RStudio.
# Para executar no Azure, altere o valor da variavel Azure para TRUE. Se o valor for FALSE, o código sera executado no RStudio  # Obs: Caso tenha problemas com a acentuação, consulte este link:
# https://support.rstudio.com/hc/en-us/articles/200532197-Character-Encoding
  
# Configurando o diretório de trabalho
# Coloque entre aspas o diretório de trabalho que você está usando no seu computador
# Não use diretórios com espaço no nome


##Coletando os Dados
library(readr)
source('Tools.R')
bikes <- read.csv('bikes.csv', sep = ',', header = T, stringsAsFactors = F)
bikes$dteday <- char.toPOSIXct(bikes)
str(bikes)
sapply(bikes, function(x) sum(is.na(x)))


require(dplyr)
print('Dimensões do DataFrame andtes das oprerações de transformação:')
dim(bikes)

# Filtrando o dataframe
bikes <- bikes %>% filter(cnt >100)

print('Dimensões do DataFrame andtes das oprerações de transformação:')
dim(bikes)

# ggplot2
#Quantidade de locação de bicicleta, turande um período de sempre sempre as 09 horas da manhã
require(ggplot2)
qplot(dteday, cnt, data = subset(bikes, hr == 9), geom = "line")+
  ylab("Numero de Bikes") +
  xlab("Linha do Tempo") +
  ggtitle("Demanda por Bikes as 09:00") +
  theme(text = element_text(size = 20))

## Transformação de carga
# Selecionar as variáveis que serão usadas (considerando as variáveis mais relevantes)

cols <- c("dteday", "mnth", "hr", "holiday",
          "workingday", "weathersit", "temp",
          "hum", "windspeed", "cnt")

# Criando um subset dos dados
bikes <- bikes[,cols]

# Transformar o objeto de data
bikes$dteday <- char.toPOSIXct(bikes)

# Esta linha acima gera dois valores NA
# Esta linha abaixo corrige

bikes <- na.omit(bikes)

# Normalizar as variaveis preditoras numericas 

cols <- c('temp', 'hum', 'windspeed')
bikes[,cols] <- scale(bikes[,cols])

str(bikes)
head(bikes)

# Criar uma nova variável para indicar dia da semana (workday)
bikes$isWorking <- ifelse(bikes$workingday & !bikes$holiday, 1, 0)

# Adicionar uma coluna com a quantidade de meses, o que vai ajudar a criar o modelo
bikes <- month.count(bikes)

# Criar um fator ordenado para o dia da semana, comecando por segunda-feira
# Neste fator eh convertido para ordenado numérico para ser compativel com os tipos de dados do Azure ML
bikes$dayWeek <- as.factor(weekdays(bikes$dteday))

head(bikes)
# podemos observar que na coluna dayWeek está recebendo os dias da semana

# Se o seu sistema operacional estiver em portugês, execute o comando abaixo.

bikes$dayWeek <- as.numeric(ordered(bikes$dayWeek, 
                                    levels = c("segunda-feira", 
                                               "terça-feira", 
                                               "quarta-feira", 
                                               "quinta-feira", 
                                               "sexta-feira", 
                                               "sábado", 
                                               "domingo")))

head(bikes)


str(bikes)
# podemos observar que criamos mais 03 variáveis (isWorking, month.count, dayWeek)

# Agora os dias da semana devem estar como valores numéricos

str(bikes$dayWeek)

## Engenaria de Atributos

#Adiciona uma variável com valores únicos para o horário do dia em dias de semana e dias de fim de semana
# Com isso diferenciamos as horas dos dias de semana, das horas em dias de fim de semana
bikes$workTime <- ifelse(bikes$isWorking, bikes$hr, bikes$hr + 24) 

# Transforma os valores de hora na madrugada, quando a demanda por bibicletas é praticamente nula 
bikes$xformHr <- ifelse(bikes$hr > 4, bikes$hr - 5, bikes$hr + 19)

# Adiciona uma variável com valores únicos para o horário do dia para dias de semana e dias de fim de semana
# Considerando horas da madrugada
bikes$xformWorkHr <- ifelse(bikes$isWorking, bikes$xformHr, bikes$xformHr + 24) 

str(bikes)

## Análise de Correlação 

cols <- c("mnth", "hr", "holiday", "workingday",
          "weathersit", "temp", "hum", "windspeed",
          "isWorking", "monthCount", "dayWeek", 
          "workTime", "xformHr", "cnt")

# Métodos de Correlação
# Pearson - coeficiente usado para medir o grau de relacionamento entre duas variáveis com relação linear
# Spearman - teste não paramétrico, para medir o grau de relacionamento entre duas variaveis
# Kendall - teste não paramétrico, para medir a força de dependência entre duas variaveis

# Vetor com os métodos de correlação
metodos <- c('pearson','spearman')

# Aplicando os métodos de correlação com a função cor()
cors <- lapply(metodos, function(method)
  (cor(bikes[,cols], method = method)))

head(cors)

# Preprando o plot
require(lattice)

plot.cors <- function(x, labs){
  diag(x) <- 0.0 
  plot( levelplot(x, 
                  main = paste("Plot de Correlação usando Método", labs),
                  scales = list(x = list(rot = 90), cex = 1.0)) )
}

# Mapa de Correlação
Map(plot.cors, cors, metodos)


## Análise de Série Temporal 

# Avaliando a demanda por aluguel de bikes ao longo do tempo
# Construindo um time series plot para alguns determinados horários 
# em dias úteis e dias de fim de semana.

times <- c(7, 9, 12, 15, 18, 20, 22) 

# Time Series Plot

tms.plot <- function(times){
  ggplot(bikes[bikes$workTime == times, ], aes(x = dteday, y = cnt)) +
    geom_line() +
    ylab('Número de Bikes') +
    xlab('Data dia') +
    labs(title = paste('Demanda de Bikes as ', as.character(times), ':00', sep =''))+
    theme(text = element_text(size = 20))
}

lapply(times, tms.plot)

## Analisando BoxPlots

# Convertendo a variável dayWeek para fator ordenado e plotando em ordem de tempo
bikes$dayWeek <- fact.conv(bikes$dayWeek)

# Demanda de bikes x potenciais variáveis preditoras
labels <- list("Boxplots - Demanda de Bikes por Hora",
               "Boxplots - Demanda de Bikes por Estação",
               "Boxplots - Demanda de Bikes por Dia Útil",
               "Boxplots - Demanda de Bikes por Dia da Semana")

xAxis <- list("hr", "weathersit", "isWorking", "dayWeek")

# Função para criar os boxplots
plot.boxes  <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt", group = X)) + 
    ylab('Número de Bikes alugadas') +
    xlab('Dia da Semana') +
    geom_boxplot( ) + 
    ggtitle(label) +
    theme(text = element_text(size = 18)) 
}

Map(plot.boxes, xAxis, labels)


labels1 <- list("Boxplots - Demanda de Bikes por Hora",
               "Boxplots - Demanda de Bikes por Estação",
               "Boxplots - Demanda de Bikes por Dia Útil",
               "Boxplots - Demanda de Bikes por Dia da Semana")

xAxis <- list("hr", "weathersit", "isWorking", "dayWeek")


#Analisanda o Boxplot - Demanda de bike por dias da semana:

# Podemos observar que temos valores extremos, ou seja outlier,em quase todos os dias,
# fugindo do nossos conjuntos de dados.
# Podemos observar que aos sábados e domingos possuimos valores maiores acima da mediana
# significando que posuimos mais alugeuis de bikes nos finais de semanas em relação a dias de semana.

# Analisando o Boxplot - Demanda de bike por dia Útil
# Colocamos o número '1', para simblizar os dias da semana e o dia '0' para indicar os finais de semana.
# Podemos observar neste gráfico que durante os finais de semana são realizado mais alugueis de bikes do que durante os dias durante semana.

# Analisando o Boxplot - Demanda de bike por Estação
# o gráfico representa as quatros estados do ano, primavera, verão, outono e invrno, sendo respecttivamente os códgo: 1, 2, 3 e 4.
# Podemos concluir nesse gráfico que a estação que vai se aluga bicicletas é a primavera e no verão, porém com um número um pouco menor.


# Analisando o Boxplot - Demanda de bike por hora
# podemos observar que existem a maior demanda por aluguel de biks durantes as 09 horas da manhã e as 18h00.
# Podemos supor que as pessoas aluguem bicicletas para irem trabalhar e após o horário de trabalho para retorno a suas residências, ou passear com a família, entre outras possibilidades. 
# Podemos apresentar este gráfico para a área de Marketing, para que possa ser realizado campanham diferentes de acordo com os perfils dos públicos, da parte da manhã, a tarde e a noite.
# Derrepente durante o horários após as 18h00, podemos fazer campanham vontada para as famílias, acreditando que as pessoas tenham saido do trabalho.

## Analisando Density Plots

# Visualizando o relacionamento entre as variáveis preditoras e demanda por bike
labels <- c("Demanda de Bikes vs Temperatura",
            "Demanda de Bikes vs Humidade",
            "Demanda de Bikes vs Velocidade do Vento",
            "Demanda de Bikes vs Hora")

xAxis <- c("temp", "hum", "windspeed", "hr")

# Função para os Density Plots
plot.scatter <- function(X, label){ 
  ggplot(bikes, aes_string(x = X, y = "cnt")) + 
    geom_point(aes_string(colour = "cnt"), alpha = 0.1) + 
    scale_colour_gradient(low = "green", high = "blue") + 
    geom_smooth(method = "loess") + 
    ggtitle(label) +
    theme(text = element_text(size = 20)) 
}

Map(plot.scatter, xAxis, labels)


# Explorando a interação entre tempo e dia, em dias da semana e fins de semana
labels <- list("Box plots - Demanda por Bikes as 09:00 para \n dias da semana e fins de semana",
               "Box plots - Demanda por Bikes as 18:00  para \n dias da semana e fins de semana")

Times <- list(9, 18)

plot.box2 <- function(time, label){ 
  ggplot(bikes[bikes$hr == time, ], aes(x = isWorking, y = cnt, group = isWorking)) + 
    geom_boxplot( ) + ggtitle(label) +
    theme(text = element_text(size = 18)) }

Map(plot.box2, Times, labels)

# podemos observar que não há correlação entre a quantidade de bicicletas alugado em relação a Velocidade do Vento.
# Porém existe correlação negativa em relação a quantidade de bicicletas alugas em relação a Humidade do ar, ou seja, 
# conforme aumenta a umidade do ar, aumenta a probabilidade de chuva, diminuindo a demanda por aluguel de bicicletas.   

## Feature Selection

# A seleção de Atributos, ou o Feature Selection tem como objetivo a simpleficação do modelo, para facilitar sua interpretação, dredução do tempo de treinamento do modelo e melhoria da generalização do modelo, evitando overfitting.
# Utilizaremos a tecnicas de feaure selection para automitizar a seleção de variáveis com maior potencialpara variáveis preditoras.
# Sendo uma espécia de filtro, que remove do seu dataset as variáveis qwue não serão úteis para a criação do modelo preditivo.
# Tem como principal objetivo a criação de um modelo preditoco com a maior precisão possível e que seja generalizável. 
# As técnicas de Feaure Selection basicamente calculam o nível de signifiância de cada variável e eliminam aquelas com significância mais baixa.


dim(bikes)
any()

# Criando um modelo para identificar os atributos com maior importância para o modelo preditivo

require(randomForest)

# Avalidando a importância de todas as variaveis
modelo <- randomForest(cnt ~.,
                       data = bikes,
                       ntree = 100,
                       nodesize = 10,
                       inportance = TRUE)

# Plotando as variáveis por grau de importância
varImpPlot(modelo)

# Removendo variáveis colineares
modelo <- randomForest(cnt ~ . - mnth
                       - hr
                       - workingday
                       - isWorking
                       - dayWeek
                       - xformHr
                       - workTime
                       - holiday
                       - windspeed
                       - monthCount
                       - weathersit, 
                       data = bikes, 
                       ntree = 100, 
                       nodesize = 10,
                       importance = TRUE)

# Removendo variáveis colineares
varImpPlot(modelo)

## Cria um modelo preditivo usando randomForest

# Função para tratar as datas
set.asPOSIXct <- function(inFrame) { 
  dteday <- as.POSIXct(
    as.integer(inFrame$dteday), 
    origin = "1970-01-01")
  
  as.POSIXct(strptime(
    paste(as.character(dteday), 
          " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S"))
}

char.toPOSIXct <-   function(inFrame) {
  as.POSIXct(strptime(
    paste(inFrame$dteday, " ", 
          as.character(inFrame$hr),
          ":00:00", 
          sep = ""), 
    "%Y-%m-%d %H:%M:%S")) }


# Treinamento do modelo

model <- randomForest(cnt ~ xformWorkHr + dteday + temp + hum, 
                      data = bikes,
                      ntree = 40, 
                      nodesize = 5)
print(model)


# Score do modelo preditivo com randomForest

# Testando o modelo

scores <- data.frame(actual = bikes$cnt,
                     prediction = predict(model, newdata = bikes))


## Avaliação do modelo

inFrame <- scores[, c("actual", "prediction")]
refFrame <- bikes

head(scores)

summary(scores)

# Criando um dataframe
inFrame[, c("dteday", "monthCount", "hr", "xformWorkHr")] <- refFrame[, c("dteday", "monthCount", "hr", "xformWorkHr")]

# Nomeando o dataframe
names(inFrame) <- c("cnt", "predicted", "dteday", "monthCount", "hr", "xformWorkHr")

#  Time series plot mostrando a diferença entre valores reais e valores previstos
inFrame <- inFrame[order(inFrame$dteday),]
s <- c(7, 9, 12, 15, 18, 20, 22)

lapply(s, function(s){
  ggplot() +
    geom_line(data = inFrame[inFrame$hr == s, ], 
              aes(x = dteday, y = cnt)) +
    geom_line(data = inFrame[inFrame$hr == s, ], 
              aes(x = dteday, y = predicted), color = "red") +
    ylab("Numero de Bikes") +
    labs(title = paste("Demanda de Bikes as ",
                       as.character(s), ":00", spe ="")) +
    theme(text = element_text(size = 20))
})

# Computando os resíduos
inFrame <-  mutate(inFrame, resids = predicted - cnt)

# Plotando os resíduos
ggplot(inFrame, aes(x = resids)) + 
  xlab('Resíduos') +
  ylab("Quantidade") +
  geom_histogram(binwidth = 1, fill = "white", color = "darkblue")

# Distribuição os resíduos
qqnorm(inFrame$resids)
qqline(inFrame$resids)

# Plotando os resíduos com as horas transformadas
inFrame <- mutate(inFrame, fact.hr = as.factor(hr),
                  fact.xformWorkHr = as.factor(xformWorkHr))

facts <- c("fact.hr", "fact.xformWorkHr") 

lapply(facts, function(x){ 
  ggplot(inFrame, aes_string(x = x, y = "resids")) + 
    geom_boxplot( ) + 
    ggtitle("Residuos - Demanda de Bikes por Hora - Atual vs Previsto")})

# conforme podemos observar a linha preta é exatamento os dados que temos nosso conjuto de dados original.
# A linha vermelha demonstra as nossas previsões com o modelo.

head(inFrame)

# Mediana dos resíduos por hora
evalFrame <- inFrame %>%
  group_by(hr) %>%
  summarise(medResidByHr = format(round(
    median(predicted - cnt), 2), 
    nsmall = 2)) 

# Computando a mediana dos resíduos
tempFrame <- inFrame %>%
  group_by(monthCount) %>%
  summarise(medResid = median(predicted - cnt)) 

summary(tempFrame)
head(evalFrame)

