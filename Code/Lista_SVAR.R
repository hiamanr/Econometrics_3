library(tidyverse)
library(vars)


######Directory

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

working_directory <- gsub("/Code", "", file_directory)

setwd(working_directory)

######Dados

options(scipen = 999)

nominal <- read.csv("Dados\\cambio_nominal.csv")

nominal <- ts(nominal[,2], start = strsplit(as.character(nominal[1,1]),"[.]")[[1]], 
           frequency = 12)

real <- read.csv("Dados\\cambio_real.csv")

real <- ts(real[,2], start = strsplit(as.character(real[1,1]),"[.]")[[1]], 
           frequency = 12)

diff_log_nominal <- diff(log(nominal))

diff_log_real <- diff(log(real))

dados <- cbind(diff_log_nominal, diff_log_real) %>% 
  window(start = c(2002, 01), end = c(2024, 03))

#PElo AIC, escolhemos p = 2

VARselect(dados, lag.max = ceiling(12*(nrow(dados)/100)^(1/4)),
          type = "none", season=12)

modelo_p2 <- VAR(dados, type = 'none', p = 2)

any(roots(modelo_p2)>1)

stat <- 2*(logLik(modelo_p2) - logLik(VAR(dados[,1:2],type = 'none', p = 1)))
print(stat)

qchisq(0.95, 4)

#Não rejeitamos a nula de que a última defasagem é inócua! Passamos então para p = 1

modelo_p1 <- VAR(dados, type = 'none', p = 1)

any(roots(modelo_p1)>1)

#Vamos verificar o teste de Portmanteau para autocorrelação dos resíduos
serial.test(modelo_p1, lags.pt = 20, type = c("PT.adjusted"))

#Mas agora rejeitamos a nula de que os erros não são serialmente-correlacionados...

serial.test(modelo_p2, lags.pt = 20, type = c("PT.adjusted"))

#E pra p = 2 não rejeitávamos. Talvez melhor escolher p = 2 mesmo

modelo_reduzido <-  VAR(dados, type = 'none', p = 2)

#Fixando semente para as simulações usadas no cálculo dos intervalos de confiança
set.seed(1812)
irf = irf(modelo_reduzido, impulse = "diff_log_real", 
          n.ahead = 12, ci = 0.95, runs = 1000)
plot(irf)


#Decomposição variância

decomp = fevd(modelo_reduzido, n.ahead = 1000)

decomp$diff_log_nominal[1:15,]

decomp$diff_log_real[1:15,]

