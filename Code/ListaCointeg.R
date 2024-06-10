## Lista de Co-integração

# Abrindo os dados

library(readr)
cpi <- read.csv2("C:/Users/9775105/Downloads/cpi.csv")
View(cpi)

cpi = ts(log(cpi[,2]), start=strsplit(cpi[1,1],'-')[[1]][1:2], freq=12)

ipca = read.csv('C:/Users/9775105/Downloads/ipca.csv')
ipca = ts(log(ipca[,2]), start=strsplit(as.character(ipca[1,1]),'[.]')[[1]], freq=12)

cambio = read.csv2('C://Users/9775105/Downloads/tx_cambio.csv')
cambio = ts(log(cambio[,2]), start=strsplit(as.character(cambio[1,1]),'[.]')[[1]], freq=12)


dados = cbind(cpi,ipca,cambio)

#Vamos parar em mar/24, quando todas as séries estão disponíveis
dados = window(dados, start = c(2001,01),end = c(2024,03))

#Visualizando os dados
plot(dados)


#Vamos considerar os testes A 10% de significância
#install.packages("CADFtest")
library(CADFtest)

#install.packages("car")
library(car)

#install.packages("sandwich")
library(sandwich)

#install.packages("lmtest")
library(lmtest)

#CPI
teste = CADFtest(dados[,1], type = "trend", max.lag.y = ceiling(12*(length(dados[,1])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)

#Não rejeitamos a nula, nesta etapa
#Teste F para componente determinístico
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#Olhando a tabela correspondente ao nosso caso nos slides, não rejeitamos a nula do teste F a 10%.

#Rodando o teste no modelo com drift somente
teste = CADFtest(dados[,1], type = "drift", max.lag.y = ceiling(12*(length(dados[,1])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)


#Não rejeitamos a nula, aqui

#Teste F para componente determinístico
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#Valor crítico a 10%, nos slides, é 3.81 (temos 269 observações). Logo, não rejeitamos a nula


#Rodando o teste no modelo sem componente determinístico
teste = CADFtest(dados[,1], type = "none", max.lag.y = ceiling(12*(length(dados[,1])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)

#Testando pela presença de componentes determinísticos na diferença
trnd = 1:(nrow(dados)-1)
coeftest(lm(diff(dados[,1])~trnd), vcov. = vcovHAC)
