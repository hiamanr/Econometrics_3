## Cointegration - Hiaman Santos and JFCP2000-----------------------------------

## Defining directory-----------------------------------------------------------



## Reading databases------------------------------------------------------------

#install.packages('readr')
library(readr)

cpi <- read.csv2("C:/Users/lauro/Documents/GitHub/Econometrics_3/cpi.csv")
cpi = ts(log(cpi[,2]), start=strsplit(cpi[1,1],'-')[[1]][1:2], freq=12)

ipca = read.csv('C:/Users/lauro/Documents/GitHub/Econometrics_3/ipca.csv')
ipca = ts(log(ipca[,2]), start=strsplit(as.character(ipca[1,1]),'[.]')[[1]], freq=12)

cambio = read.csv2('C:/Users/lauro/Documents/GitHub/Econometrics_3/tx_cambio.csv')
cambio = ts(log(cambio[,2]), start=strsplit(as.character(cambio[1,1]),'[.]')[[1]], freq=12)


# Creating database-------------------------------------------------------------

dados = cbind(cpi,ipca,cambio)

## Creating window for data from jan./2001 to mar./2024-------------------------
dados = window(dados, start = c(2001,12),end = c(2024,03))

## Visualising data-------------------------------------------------------------
plot(dados)

## Uni root tests---------------------------------------------------------------

#Vamos considerar os testes A 10% de significância
#install.packages("CADFtest")
library(CADFtest)

#install.packages("car")
library(car)

#install.packages("sandwich")
library(sandwich)

#install.packages("lmtest")
library(lmtest)

## CPI -------------------------------------------------------------------------
teste = CADFtest(dados[,1], type = "trend", max.lag.y = 
                   ceiling(12*(length(dados[,1])/100)^(1/4)),criterion = "MAIC")

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

## IPCA-------------------------------------------------------------------------

# Uni root tests ---------------------------------------------------------------

#IPCA
teste = CADFtest(dados[,2], type = "trend", max.lag.y = 
                   ceiling(12*(length(dados[,2])/100)^(1/4)), criterion = "MAIC")
summary(teste)
print(teste)

#Não rejeitamos a nula do teste, nesta etapa

#Teste F para componente determinístico
linearHypothesis(teste$est.model,c("trnd= 0", "L(y, 1) = 0" ))

#Não rejeitamos a nula da F, a 10%, aqui


#Rodando modelo só com drift
teste = CADFtest(dados[,2], type = "drift", max.lag.y = ceiling(12*(length(dados[,2])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)

#Não rejeitamos a nula do teste, nesta etapa

#Teste F para componente determinístico
linearHypothesis(teste$est.model,c("(Intercept)= 0", "L(y, 1) = 0" ))

#Logo, devemos repetir o teste de raiz unitária com valores críticos normais:
#A 10% valor crítico é
qnorm(0.1)

#Estatística de teste é
teste$statistic

#Testando pela presença de componentes determinísticos na diferença
trnd = 1:(nrow(dados)-1)
coeftest(lm(diff(dados[,2])~trnd), vcov. = vcovHAC)

## Câmbio-----------------------------------------------------------------------

teste = CADFtest(dados[,3], type = "trend", max.lag.y = ceiling(12*(length(dados[,3])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)

#Não rejeitamos a nula do teste, nesta etapa

#Teste F para componente determinístico
linearHypothesis(teste$est.model,c("trnd= 0", "L(y, 1) = 0" ))

#Não rejeitamos a nula da F, a 10%, aqui (resposta da resolução proposta pelo professor)


#Rodando modelo só com drift
teste = CADFtest(dados[,3], type = "drift", max.lag.y = ceiling(12*(length(dados[,3])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)

#Não rejeitamos a nula do teste, nesta etapa

#Teste F para componente determinístico
linearHypothesis(teste$est.model,c("(Intercept)= 0", "L(y, 1) = 0" ))

#Rodando modelo sem componentes determinísticos
teste = CADFtest(dados[,3], type = "none", max.lag.y = ceiling(12*(length(dados[,3])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
print(teste)


#Testando pela presença de componentes determinísticos na diferença
trnd = 1:(nrow(dados)-1)
coeftest(lm(diff(dados[,3])~trnd), vcov. = vcovHAC)

## Item 2 ----------------------------------------------------------------------

teste = lm(cpi~.,data= dados)

res= ts(residuals(teste),start = c(2001,12),frequency=12)

library("urca")
ur.pp(res, type = c("Z-tau"))

modelo_diff = lm(diff(cpi)~diff(ipca)+diff(cambio),data=dados)

coefci(modelo_diff, level = 0.9, vcov. = vcovHAC)

### Item 3

acf(diff(dados),lag.max=48)

library(vars)

#Selecionando a ordem p
VARselect(dados, lag.max = 20, type = 'const')

#Vamos escolher a ordem indicada pelo BIC
var = VAR(dados, p=2, type = 'const')

#Teste de razão de verossimilhança para a nula de que última defasagem é irrelevante
lkl = 2*(logLik(var) - logLik(VAR(dados, p=1, type = 'const')))

#Rejeitaremos nula a 10% se lkl maior que o valor crítico, isto é, se:
lkl > qchisq(0.9, 9)

#Logo, rejeitamos a nula.


#Vamos analisar a autocorrelação dos erros
serial.test(var,  type = 'BG')


#Vamos escolher a ordem p=3
var = VAR(dados, p=3, type = 'const')

#Teste de razão de verossimilhança para a nula de que última defasagem é irrelevante
lkl = 2*(logLik(var) - logLik(VAR(dados, p=2, type = 'const')))

#Rejeitaremos nula a 10% se lkl maior que o valor crítico, isto é, se:
lkl > qchisq(0.9, 9)


#Não há evidência estatística suficiente para afirmar que última defasagem é relevante

#Vamos analisar a autocorrelação dos erros
serial.test(var,  type = 'BG')


joh = ca.jo(dados, type = 'eigen',ecdet = 'none', K = 3, spec = 'transitory')

summary(joh)
print(joh)

### Item 4

modelo = VAR(diff(dados), p=2)
#Projeções para variação de preços e câmbio
predict(modelo)
print(modelo)
