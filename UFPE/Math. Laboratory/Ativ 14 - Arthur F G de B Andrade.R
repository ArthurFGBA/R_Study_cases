#Aula 04/07/2024
install.packages("readxl")
library(readxl)

getwd()
setwd("~/MeusProjetos/R_Study_cases/UFPE/Math. Laboratory")

dados<-read_xlsx("~/MeusProjetos/R_Study_cases/UFPE/Math. Laboratory/Importar Arquivo Excel.xlsx")
str(dados)
attach(dados)
summary(dados)
dados

plot(AA)

lines(EE)

lines(FF)

lines(DD,col=2,lwd=1)

lines(BB,col=3,lwd=2)

lines(CC,col=4,lwd=3)

lines(EE,lty=2,col=4,lwd=3)

lines(FF,lty=3,col= 3,lwd=4)

plot(AA,BB)
# gráfico
abline(lm(AA ~ BB)) 
# gráfico da regressão linear
ml<-lm(AA ~ BB) # regressão y com x

z<-20*ml[[1]][1]+30*ml[[1]][1]
z
