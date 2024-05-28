### Lista 6, de Econometria 3 (Metodologia  VAR) - Hiaman Santos----------------

### Resolução da questão 1 ------------------------------------------------- 

# Utilizaremos a série de PIB, como nas listas anteriores. 
# Ademais, trabalharemos com a série de desemprego, pois
# Faz sentido supor que o desemprego ajude a prever o PIB.

# Devemos conduzir todas as etapas de especificação e diagnóstico do
# modelo.


## Pacotes----------------------------------------------------------------------

# Podemos utilizar pacotes do R para fazer download de séries automaticamente 

# Para dados do BCB, instalamos o seguinte pacote
# Utilizaremos os dados do BCB para este exerício

#install.packages("GetBCBData")
library(GetBCBData)

#install.packages("tidyverse")
library(tidyverse)

#Instalando pacotes para decompor e dessazonalizar
#install.packages("seasonal")
library(seasonal) #Arima X13-Seats

#install.packages("mFilter")
library(mFilter) #Filtro HP

#Carregando dados disponíveis no pacote URCA
#install.packages("urca")
library("urca")

#install.packages("forecast")
library(forecast)

#install.packages("sandwich")
library(sandwich) #Erros padrão HAC

#install.packages("lmtest")
library(lmtest) #Testes com erros padrão robustos

#install.packages("PNADcIBGE")
library(PNADcIBGE)

#install.packages("survey")
library(survey)

#install.packages("convey")
library(convey)

#install.packages("dplyr")
library(dplyr)

#install.packages("knitr")
library(knitr)

#install.packages("kableExtra")
library(kableExtra)

#install.packages("zoo")
library(zoo)

#install.packages("ggplot2")
library(ggplot2)

## Carregando dados das variáveis de interesse------------------------------------
#Neste exercício utilizaremos os dados disponibilizados pelo BCB para o PIB 

pib <- 
  GetBCBData::gbcbd_get_series( #Comando para extrair dados
    id=22099, #código da série desejada (ver no site do Bacen - pib trimestral, índice)
    first.date = "1995-10-01", #data inicial desejada
    last.date = Sys.Date() #Default é Sys.Date() para extrairmos os dados 
    #disponibilizados para todo período
  ) %>% 
  as_tibble() %>% #função as_tibble para que dados sejam tratados pelo R 
  #como tibble (data frame)
  select(ref.date, value)


# Carregar tabela de desemprego (IBGE, 2024)

## Transformando séries

pib_ts <- ts(pib$value, c(1995,4), frequency = 4) #a função "ts" é usada para 
#criar objetos de séries temporais

log_pib <- log(pib_ts) #transformando índice em logartimo

pib_estacionarizado <- log(pib$value) - lag(log(pib$value))

pib_estac_ts <- ts(pib_estacionarizado, c(1996,1), frequency = 4)

plot(pib_estac_ts)

class(pib$value)

Desemprego_tri <- Desemprego_tri_1_

# Por recomendação do professor Luís, trabalharemos com variável em logaritmo.

desemprego_brasil = ts(data = Desemprego_tri$Brasil, start = c(2012,1), frequency = 4)

## Plotando gráficos dos objetos que temos até o momento


plot(desemprego_brasil)

## Testes de raiz unitária -----------------------------------------------------

# Pacotes-----------------------------------------------------------------------

install.packages("CADFtest")
library("CADFtest") # testes de raiz unitária

#install.packages("sandwich")
library("sandwich") #Erros padrão HAC

#install.packages("lmtest")
library("lmtest") #Teste robusto

install.packages("car")
library("car") #Teste de restrições lineares

## Desemprego


#Testando a presença de raízes unitárias
#Vamos conduzir os testes a 10% de significância

#Começamos pelo modelo mais completo:
teste = CADFtest(desemprego_brasil, type = "trend", max.lag.y = ceiling(12*(length(desemprego_brasil)/100)^(1/4)),
                 criterion = "MAIC")

print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.9941 > 0.10)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendência linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estatística F é de 2.4958. Usando o Caso 4 da tabela na página 11 dos slides, vemos que, a 10% de significância,
#o valor crítico para 48 observações é 5.61. Como F < 5.61, não rejeitamos a hipótese nula de que 
#a tendência e o coeficiente associado a y_{t-1} são ambos zero. Nesse caso, vamos para o modelo somente com
#intercepto

teste = CADFtest(desemprego_brasil, type = "drift", max.lag.y = ceiling(12*(length(desemprego_brasil)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste)
#O teste t NÃO rejeita a hipótese nula de uma raiz unitária, a 10% (p-valor é de 0.3233 > 0.05)
#Vamos conduzir o teste F para detectar se o modelo de fato apresenta intercepto.

print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estatística F é de 1.8389. Usando o Caso 2 da tabela na página 11 dos slides, vemos que, a 10% de significância,
#o valor crítico para 48 observações é 3.94. Como F < 3.94, não rejeitamos a hipótese nula de que 
#o intercepto e o coeficiente associado a y_{t-1} são ambos zero. Nesse caso, vamos para o modelo sem componentes
#determinísticos

teste = CADFtest(desemprego_brasil, type = "none", max.lag.y = ceiling(12*(length(desemprego_brasil)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste)

#Aqui, também não rejeitamos a hipótese nula. p-valor = 0,6696 > 0,10. Concluímos que a série apresenta UMA raiz unitária


#Vamos testar a presença de componentes determinísticos.
#Como os dados apresentam tendência estocástica, trabalhamos com a série diferenciadas
trend = 1:(length(desemprego_brasil)-1)
modelo = lm(diff(desemprego_brasil)~trend)

coeftest(modelo, vcov. = vcovHAC)

#Não rejeitamos a hipótese nula de que NÃO HÁ tendência linear determinística
#na série em primeira diferença 0.1058 > 0.10


#2. Ajuste via ARIMA X13-Seats

Desemprego_ts <- ts(Desemprego_tri$Brasil, c(2012,1), frequency = 4)

modelo_sazonal <- seas(Desemprego_ts)

plot(modelo_sazonal)

desemprego_x13 = predict(modelo_sazonal)

plot(desemprego_x13)

# estacionarizando

desemp_diff <- diff(desemprego_x13)

plot(desemp_diff)

Desemprego_estacionarizado <- as.vector(desemprego_x13) - lag(as.vector(desemprego_x13))


desemp_estac_ts <- ts(Desemprego_estacionarizado, c(2012,2), frequency = 4)


plot(desemp_estac_ts)

plot(desemprego_x13)

#Pacote para VAR
install.packages("vars")
library("vars")


dados = cbind(desemp_estac_ts[2:48], pib_estac_ts[67:113])


# Codigo do professor 

#Selecionando ordem do VAR inicial
VARselect(dados, type = "both", lag.max = ceiling(12*(nrow(dados)/100)^(1/4)))

#Vamos verificar o modelo do VAR com SC
modelo = VAR(dados,type = 'both', p = 8)

summary(modelo)
help(roots)
any(roots(modelo)>1)

# Processo estimado não apresenta raiz unitária - Isso é bom. 
# Modelo está bem ajustado. Modelo é estável. 
# 

#Vamos fazer o teste de razão de verossimilhança
stat = 2*(logLik(modelo) - logLik(VAR(dados,type = 'both', p = 7)))
print(stat)

#Valor crítico é, a 5%
qchisq(0.95, 4)

#Aceitamos nula de que oitava defasagem é inócua.Aceitamos h0 de que def
#defasagem é inócua. 
#Trabalhamos com p=1.

#Vamos verificar o teste de Portmanteau para autocorrelação dos resíduos
serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))
#Rejeitamos a hipótese nula: VAR(2) joga muita informação fora!!!
#h0 NÃO TEM AUTOCORREL

#p-valor 
#rever hipótese nula nos slides

#Vamos considerar o VAR com base no SC p =1
modelo = VAR(dados,type = 'both', p = 1)

summary(modelo)
help(roots)
any(roots(modelo)>1)
#Processo estimado não apresenta raiz unitária

#Vamos fazer o teste de razão de verossimilhança

# nÃO TEM COMO FAZER O TESTE SEM LAG.
#stat = 2*(logLik(modelo) - logLik(VAR(dados,type = 'both', p = 0)))
#print(stat)
##Valor crítico é, a 5%
#qchisq(0.95, 9)
##Evidência de que precisamos desta defasagem

serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))
#Parecemos melhorar algo em relação ao modelo menor

normality.test(modelo)
#Rejeitamos claramente normalidade dos erros

#Podemos procurar especificações com ordem maiores, mas a dimensionalidade vai tender a piorar
#Vamos ficar com o modelo do AIC (vale comparar os resultados com o do BIC e HQ também)

predicoes = predict(modelo, ci = 0.95)

fanchart(predict(modelo, ci = 0.95), plot.type = 'single')


#1 TERMINA AQUI

# REPOSTA 2

exp = readxl::read_xls("expectativas.xls")
exp = ts(exp[,2], start = c(2001, 7), frequency = 12)

juros = read.csv("selic_atualizado.csv")
juros = ts(juros[,2], start = c(juros[1,1]%/%1,juros[1,1]%%1*100), frequency = 12)

infl = read.csv("nucleo_ipca.csv")
infl = ts(infl[,2], start = c(infl[1,1]%/%1,infl[1,1]%%1*100), frequency = 12)

#Anualizando variação mensal
infl=100*((1+infl/100)^(12)-1)


dados = cbind(exp, juros, infl)
dados = window(dados, start = c(2001,07), end = c(2024,03))

plot(dados)


trnd = 1:nrow(dados)

resid = residuals(lm(dados[,1:2]~ trnd))
dados = cbind(dados,resid)


VARselect(dados[,1:3], type = "both", lag.max = ceiling(12*(nrow(dados)/100)^(1/4)))

#p selecionado = 2 (baseado no SC)
modelo = VAR(dados[,1:3],type = 'both', p = 2)


#Teste de RV

stat = 2*(logLik(modelo) - logLik(VAR(dados[,1:3],type = 'both', p = 1)))
print(stat)
qchisq(0.95, 9)

#Vamos verificar o teste de Portmanteau para autocorrelação dos resíduos
serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))

#Rejeitamos nula: VAR(2) joga muita informação fora

#Usamos então VAR selecionado por AIC: p = 14

modelo = VAR(dados[,1:3],type = 'both', p = 14)

stat = 2*(logLik(modelo) - logLik(VAR(dados[,1:3],type = 'both', p = 13)))
print(stat)
#Valor crítico é, a 5%
qchisq(0.95, 9)
#Evidência de que precisamos desta defasagem


serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))
#Melhorou

predicoes = predict(modelo, ci = 0.95)

fanchart(predict(modelo, ci = 0.95), plot.type = 'single')

#Previsões para abril e maio

forecast(modelo, h = 2)

#Podemos testar a hipótese de Granger-causalidade

library(bruceR)

granger_causality(modelo)

#Rejeitamos a nula (p-valr < 0.001)