install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
install.packages("xlsx")
library(xlsx)

getwd()
setwd("~/MeusProjetos/R_Study_cases/UFPE/Math. Laboratory")

dados<-read_xlsx("~/MeusProjetos/R_Study_cases/UFPE/Math. Laboratory/Questão1_list2.xlsx")
attach(dados)
summary(dados)

plot(AA)
lines(EE)
lines(FF)
lines(DD, col = 2, lwd = 1)
lines(BB, col = 3, lwd = 2)
lines(CC, col = 4, lwd = 3)

#Parte 2
#Distribuições de probabilidades no R
#Pdistribuição retorna a probabilidade 

#Calcule a probabilidade de não caber em assento de 15 polegadas 
#numa população normal com média 14.4, e desvio padrão 1 
pnorm(15, 14.4, 1, lower.tail = FALSE)

#Tamanho de cadeira no qual 95% da população caiba
qnorm(0.95, 14.4, 1)

#Densidade probabilistica
dnorm(12, 14.4, 1)

#Individuos aléatorios
rnorm(20, 14.4, 1)
