library(tidyverse)
library(vars)


######Directory

file_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

working_directory <- gsub("/Code", "", file_directory)

setwd(working_directory)

######Dados

options(scipen = 999)

nominal <- read.csv("Dados\\cambio_nominal.csv") %>% 
  mutate(Data = paste0(Data, ".01")) %>% 
  mutate(Data = as.Date(Data, format = "%Y.%m.%d"))%>% 
  filter(Data>= as.Date("2002.01.01", format = "%Y.%m.%d"))

nominal <- ts(nominal[,2], frequency = 12)

real <- read.csv("Dados\\cambio_real.csv") %>% 
  mutate(Data = paste0(Data, ".01")) %>% 
  mutate(Data = as.Date(Data, format = "%Y.%m.%d"))%>% 
  filter(Data>= as.Date("2002.01.01", format = "%Y.%m.%d"))

real <- ts(real[,2], frequency = 12)




